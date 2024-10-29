import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow.keras import layers

# Paso 1: Cargar los datos originales desde el archivo Excel
original_data = pd.read_excel('/Users/lugo/PycharmProjects/simulador/data.xlsx', engine='openpyxl')

# Identificar columnas numéricas y categóricas
numeric_columns = original_data.select_dtypes(include=[np.number]).columns.tolist()
categorical_columns = original_data.select_dtypes(include=['object']).columns.tolist()

# Normalizar solo las columnas numéricas
data_np = original_data.copy()
data_np[numeric_columns] = (data_np[numeric_columns] - data_np[numeric_columns].min()) / (
    data_np[numeric_columns].max() - data_np[numeric_columns].min())

# Definir dimensiones
input_dim = data_np.shape[1]  # Número de columnas
latent_dim = 100  # Dimensión del espacio latente

# Paso 2: Definir el modelo del Generador
generator = tf.keras.Sequential([
    layers.Input(shape=(latent_dim,)),
    layers.Dense(128, activation='relu'),
    layers.Dense(256, activation='relu'),
    layers.Dense(input_dim, activation='sigmoid')  # Usar sigmoide para normalizar la salida
])

# Paso 3: Definir el modelo del Discriminador
discriminator = tf.keras.Sequential([
    layers.Input(shape=(input_dim,)),
    layers.Dense(256, activation='relu'),
    layers.Dense(128, activation='relu'),
    layers.Dense(1, activation='sigmoid')  # Salida binaria
])

# Compilar el discriminador
discriminator.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

# Paso 4: Crear el modelo GAN combinando generador y discriminador
discriminator.trainable = False  # No entrenar el discriminador durante la fase de entrenamiento del generador
gan_input = layers.Input(shape=(latent_dim,))
generated_data = generator(gan_input)
gan_output = discriminator(generated_data)
gan = tf.keras.Model(gan_input, gan_output)

# Compilar el modelo GAN
gan.compile(loss='binary_crossentropy', optimizer='adam')

# Paso 5: Entrenar el modelo GAN
epochs = 10000
batch_size = 32

for epoch in range(epochs):
    noise = np.random.normal(0, 1, size=[batch_size, latent_dim])
    generated_samples = generator.predict(noise)
    real_samples = data_np.sample(batch_size).values
    X = np.concatenate([real_samples, generated_samples])
    y_dis = np.array([1] * batch_size + [0] * batch_size)
    try:
        d_loss = discriminator.train_on_batch(X, y_dis)
    except Exception as e:
        break
    noise = np.random.normal(0, 1, size=[batch_size, latent_dim])
    y_gen = np.array([1] * batch_size)
    try:
        g_loss = gan.train_on_batch(noise, y_gen)
    except Exception as e:
        break
    if epoch % 1000 == 0:
        print(f'Epoch: {epoch}, D Loss: {d_loss[0]}, G Loss: {g_loss[0]}')

# Paso 6: Generar nuevos datos
latent_samples = np.random.normal(0, 1, size=(1060, latent_dim))
generated_data = generator.predict(latent_samples)
generated_data_df = pd.DataFrame(generated_data, columns=original_data.columns)

# Desnormalizar los datos generados y convertir a enteros
for col in numeric_columns:
    generated_data_df[col] = generated_data_df[col] * (original_data[col].max() - original_data[col].min()) + original_data[col].min()
    generated_data_df[col] = generated_data_df[col].round(0).astype(int)

# Paso 7: Aplicar las restricciones de las variables

# Asegurar que T.CLAMP sea siempre menor que T.BOMB y mayor que 35
if 'T.CLAMP' in generated_data_df.columns and 'T.BOMB' in generated_data_df.columns:
    for index, row in generated_data_df.iterrows():
        t_bomb = row['T.BOMB']
        generated_data_df.at[index, 'T.BOMB'] = t_bomb
        generated_data_df.at[index, 'T.CLAMP'] = max(np.random.randint(35, t_bomb), 35)

# Edad entre 30 y 80, excepto cuando CIRUGIA es 'CIA' (18 a 40)
if 'edad' in generated_data_df.columns and 'CIRUGIA' in generated_data_df.columns:
    for index, row in generated_data_df.iterrows():
        cirugia = row['CIRUGIA']
        if cirugia == 'CIA':
            generated_data_df.at[index, 'edad'] = np.random.randint(18, 41)
        else:
            generated_data_df.at[index, 'edad'] = np.random.randint(30, 81)

# Las variables BYPASS, BYPASS.ARTERIALES, y BYPASS.VENOSOS solo tienen datos cuando CIRUGIA es CRM, COMBINADO, o 'BENTALL+CRM'
bypass_related_columns = ['BYPASS', 'BYPASS.ARTERIALES', 'BYPASS.VENOSOS']
valid_cirugia = ['CRM', 'COMBINADO', 'BENTALL+CRM']

# Aplicar las restricciones
if 'CIRUGIA' in generated_data_df.columns:
    for index, row in generated_data_df.iterrows():
        cirugia = str(row['CIRUGIA']).strip()  # Asegurarse de eliminar espacios adicionales
        if cirugia not in valid_cirugia:
            # Si no es una cirugía válida, establecer los valores de BYPASS relacionados como NaN
            for col in bypass_related_columns:
                if col in generated_data_df.columns:
                    generated_data_df.at[index, col] = np.nan
        else:
            # Si es una cirugía válida, asignar valores aleatorios para BYPASS
            for col in bypass_related_columns:
                if col in generated_data_df.columns:
                    generated_data_df.at[index, col] = np.random.randint(1, 100)

# FEY entre 20 y 70
if 'FEY' in generated_data_df.columns:
    generated_data_df['FEY'] = generated_data_df['FEY'].apply(lambda x: np.random.randint(20, 71))

# Paso 8: Convertir columnas categóricas a los tipos originales
for col in categorical_columns:
    generated_data_df[col] = original_data[col].astype('category')

# Guardar los nuevos datos generados
generated_data_df.to_excel('/Users/lugo/PycharmProjects/simulador/nuevo_conjunto_de_datos_gan_con_restricciones.xlsx', index=False)

print("Datos sintéticos generados usando GANs con restricciones y guardados correctamente.")























