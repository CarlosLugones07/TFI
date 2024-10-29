setwd("/Users/lugo/Desktop/TFI")
getwd()

library(dplyr)
library(PerformanceAnalytics)
library(openxlsx)
library(arsenal)
library(ggplot2)
library(GGally)
library(gridExtra)
library(MASS)
library(nnet)
library(NeuralNetTools)
library(caret)
library(Hmisc)
library(knitr)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(car)
library(pROC)


####LECTURA DE BD####

# Lee el archivo CSV con ';' como separador de campos y sin encabezados
data <- read.xlsx("/Users/lugo/Desktop/TFI/data.xlsx", sheet = 1)
datos<-data

# Verifica la estructura de los datos importados
str(datos)

# Muestra las primeras filas del data frame
head(datos)

# Reemplazar NA por "NC" en las variables VALVULA y POSICION
datos$VALVULA[is.na(datos$VALVULA)] <- "NC"
datos$POSICION[is.na(datos$POSICION)] <- "NC"

# Verificar los cambios
table(datos$VALVULA)
table(datos$POSICION)

# Reemplazar NA por 0 en las variables BYPASS, BYPASS.ARTERIALES, BYPASS.VENOSOS
datos$BYPASS[is.na(datos$BYPASS)] <- 0
datos$BYPASS.ARTERIALES[is.na(datos$BYPASS.ARTERIALES)] <- 0
datos$BYPASS.VENOSOS[is.na(datos$BYPASS.VENOSOS)] <- 0

# Verificar los cambios
table(datos$BYPASS)
table(datos$BYPASS.ARTERIALES)
table(datos$BYPASS.VENOSOS)

# Transformar BCIAO usando mutate
datos <- datos %>%
  mutate(BCIAO = ifelse(BCIAO == 0, "SIN BCIAO", "CON BCIAO"))

table(datos$BCIAO)

# Conteo del número de filas de la base de datos
nrow(datos)



#### CREACION DE VARIABLES ####

# creo la variable DIFF que determina la complejidad para salir de bomba 
datos$DIFF <- datos$T.BOMB - datos$T.CLAMP
print(datos$DIFF)

summary(datos)

#### TRANSFORMACIÓN DE VARIABLES ####

# transformo la variable BCIA como factor 
datos <- datos %>%
  mutate(across(c(SEX,CIRUGIA,VALVULA, POSICION, HIPOT,BCIAO), as.factor))

# Transformar las variables BYPASS, BYPASS.ARTERIALES, BYPASS.VENOSOS en factores
datos <- datos %>%
  mutate(across(c(BYPASS, BYPASS.ARTERIALES, BYPASS.VENOSOS), as.factor))

# Verificar que las variables se transformaron correctamente
str(datos)

# Ver los niveles de varias variables a la vez
levels(datos$BYPASS)
levels(datos$BYPASS.ARTERIALES)
levels(datos$BYPASS.VENOSOS)


#### ANALISIS EXPLORATORIO ####

#___ ANALISIS UNIVARIADO___====

colnames(datos)

#### variable SEXO ####

# Agrupar por la variable SEX y contar las observaciones en cada grupo
datos %>%
  group_by(SEX) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  head(10)


# Gráfico de barras para la variable SEX
ggplot(datos, aes(x = SEX, fill = SEX)) +
  geom_bar(width = 0.5) +  # Reducción del ancho de las barras al 70%
  scale_fill_manual(values = c("F" = "#fb9a99",  # Rosado suave para F
                               "M" = "#a6cee3")) +  # Azul suave para M
  labs(title = "Distribución de la variable SEX", x = "Sexo", y = "Frecuencia") +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"),  
    legend.position = "none")


#### variable CIRUGIA ####

# Agrupar por la variable CIRUGIA y contar las observaciones en cada grupo
datos %>%
  group_by(CIRUGIA) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  print(n = 30)


# Gráfico de barras para la variable CIRUGIA 

# paleta de colores personalizada 
colores_personalizados <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", 
                                     "#ffd92f", "#e5c494", "#b3b3b3", "#1b9e77", "#d95f02")
                                     
# Gráfico 
ggplot(datos, aes(x = CIRUGIA, fill = CIRUGIA)) +
  geom_bar() +  
  geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)), 
            stat = "count", vjust = -0.5) +  
  scale_fill_manual(values = colores_personalizados) +  
  scale_y_continuous(breaks = seq(0, max(table(datos$CIRUGIA)), by = 50)) +  
  labs(title = "Distribución de la variable CIRUGIA", x = "Cirugía", y = "Frecuencia") +  
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none")


#### variable POSICION ####
# Agrupar por la variable POSICION y contar las observaciones en cada grupo
# tener en cuenta que no todas las cirugia llevan valvula por ende tampoco posicion de las mismas 
# en por ello que se filtra los NC en el conteo y porcentajes 

datos %>%
  filter(POSICION != "NC") %>%  # Filtrar las filas donde POSICION no es "NC"
  group_by(POSICION) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  print(n = 30)


# Filtrar y procesar los datos
datos %>%
  filter(POSICION != "NC") %>%  # sacolas filas donde POSICION es NA
  group_by(POSICION) %>%
  summarise(count = n(), .groups = "drop") %>%  # agrupo por POSICION
  mutate(percentage = (count / sum(count)) * 100) %>%  # calculo porcentaje
  arrange(count) %>%
  ggplot(aes(x = reorder(POSICION, count), y = count, fill = POSICION)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +  
  scale_fill_brewer(palette = "Set3") +  
  labs(
    title = "Distribución de la variable POSICION",
    x = "Posición",
    y = "Frecuencia",
    fill = "Posición"
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none"  
    )

#### variable HPOTERMIA ####

# Agrupar por la variable HIPOT y contar las observaciones en cada grupo
datos %>%
  group_by(HIPOT) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  print(n = 10)

# gráfico 
datos %>%
  group_by(HIPOT) %>%  
  summarise(count = n(), .groups = "drop") %>%  
  mutate(percentage = (count / sum(count)) * 100) %>%  
  arrange(count) %>%
  ggplot(aes(x = reorder(HIPOT, count), y = count, fill = HIPOT)) +  
  geom_bar(stat = "identity") + 
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +  
  scale_fill_brewer(palette = "Set3") +  
  labs(
    title = "Distribución de la variable HIPOT",
    x = "HIPOT",
    y = "Frecuencia",
    fill = "HIPOT"
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none"  
  )


#### variable VÁLVULA ####

# Agrupar por la variable VALVULA y contar las observaciones en cada grupo
datos %>%
  filter(VALVULA != "NC") %>%  # saco las filas donde VALVULA es NA
  group_by(VALVULA) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  print(n = 30)

# gráfico 
datos %>%
  filter(VALVULA != "NC") %>%  # sacolas filas donde VALVULA es NA
  group_by(VALVULA) %>%
  summarise(count = n(), .groups = "drop") %>%  # agrupo por VALVULA
  mutate(percentage = (count / sum(count)) * 100) %>%  # calculo porcentaje
  arrange(count) %>%
  ggplot(aes(x = reorder(VALVULA, count), y = count, fill = VALVULA)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +  
  scale_fill_brewer(palette = "Set3") +  
  labs(
    title = "Distribución de la variable VALVULA",
    x = "válvula",
    y = "Frecuencia",
    fill = "válvula"
  ) +  
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none" 
  )


#### variable BCIAO ####

# Agrupar por la variable BCIAO y contar las observaciones en cada grupo
datos %>%
  filter(BCIAO != "NC") %>%  # saco las filas donde VALVULA es NA
  group_by(BCIAO) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(count) %>%
  print(n = 30)

# gráfico
datos %>%
  filter(BCIAO != "NC") %>%  # Excluir las filas donde BCIAO es NC
  group_by(BCIAO) %>%  # Agrupar por BCIAO
  summarise(count = n(), .groups = "drop") %>%  # Calcular la frecuencia
  mutate(percentage = (count / sum(count)) * 100) %>%  # Calcular el porcentaje
  arrange(count) %>%
  ggplot(aes(x = reorder(BCIAO, count), y = count, fill = BCIAO)) +  # Reordenar para el gráfico
  geom_bar(stat = "identity") +  # Crear las barras
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, color = "black") +  # Agregar etiquetas de porcentaje
  scale_fill_brewer(palette = "Set3") +  # Paleta de colores atractiva
  labs(
    title = "Distribución de la variable BCIAO",
    x = "BCIAO",
    y = "Frecuencia",
    fill = "BCIAO"
  ) +  # Títulos y etiquetas
  theme_minimal() +  # Tema limpio
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar y negrita el título
    axis.title.x = element_text(face = "bold"),  # Negrita en el eje X
    axis.title.y = element_text(face = "bold"),  # Negrita en el eje Y
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    legend.position = "none"  # Opcional: eliminar leyenda si no es necesaria
  )


#### grafico resumen para variables CUALITATIVAS ######

# Seleccionar las columnas cualitativas
qualitative_columns <- c("SEX", "CIRUGIA", "POSICION", "HIPOT", "VALVULA", "BCIAO")
selected_datos <- datos[, qualitative_columns]

# Crear una lista para almacenar los gráficos de barras
barplot_list <- list()

# Crear una paleta de colores que se ajuste dinámicamente al número de categorías
set3_colors <- colorRampPalette(brewer.pal(n = 12, name = "Set3"))(length(unique(unlist(lapply(selected_datos, unique)))))

# Iterar sobre cada columna cualitativa y crear un gráfico de barras
for (i in seq_along(qualitative_columns)) {
  col <- qualitative_columns[i]
  
  # Filtrar los valores "NC" sin eliminarlos del conjunto original
  filtered_col_data <- selected_datos %>% filter(!!sym(col) != "NC")
  
  # Calcular las proporciones de cada categoría
  count_data <- filtered_col_data %>%
    count(!!sym(col)) %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Crear el gráfico de barras con los porcentajes
  p <- ggplot(count_data, aes(x = !!sym(col), y = n, fill = !!sym(col))) +
    geom_bar(stat = "identity", color = "white") +
    scale_fill_manual(values = set3_colors, guide = "none") +  # Eliminar la leyenda
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +  # Añadir los porcentajes
    labs(title = paste("Distribución de", col),
         x = NULL,  # Quitar el nombre de la variable del eje x
         y = "Frecuencia") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas en el eje x (categorías)
  
  # Agregar el gráfico a la lista
  barplot_list[[col]] <- p
}

# Mostrar los gráficos de barras en una cuadrícula de 3 columnas
grid.arrange(grobs = barplot_list, ncol = 3)

# Restaurar el diseño gráfico predeterminado
par(mfrow = c(1, 1))


#### gráfico resumen para variables CUANTITATIVAS ####

# Seleccionar las columnas cuantitativas
quantitative_columns <- c("EDAD", "FEY", "T.CLAMP", "T.BOMB" , "DIFF")
selected_datos <- datos[, quantitative_columns]

# Crear una lista para almacenar los gráficos
hist_list <- list()

# Obtener los colores de la paleta "Set3"
set3_colors <- brewer.pal(n = length(quantitative_columns), name = "Set3")

# Iterar sobre cada columna cuantitativa y crear un histograma
for (i in seq_along(quantitative_columns)) {
  col <- quantitative_columns[i]
  
  # Crear el histograma con ggplot2
  p <- ggplot(selected_datos, aes(x = !!sym(col))) +
    geom_histogram(binwidth = 5, fill = set3_colors[i], color = "white") +
    labs(title = paste("Distribución de", col),
         x = "Valores", y = "Frecuencia") +
    theme_minimal()
  
  # Realizar el test de Shapiro-Wilk para normalidad
  shapiro_test <- shapiro.test(selected_datos[[col]])
  
  # Extraer el valor p con solo 2 decimales
  p_value <- format(shapiro_test$p.value, digits = 2)
  
  # Agregar el p-value al gráfico como texto
  p <- p + annotate("text", x = Inf, y = Inf, 
                    label = paste("p value =", p_value), 
                    hjust = 1, vjust = 1, size = 4)
  
  # Agregar el gráfico a la lista
  hist_list[[col]] <- p
}

# Mostrar los histogramas en una cuadrícula
grid.arrange(grobs = hist_list, ncol = 3)

# Restaurar el diseño gráfico predeterminado
par(mfrow = c(1, 1))


#### medidas NO PARAMETRICAS de variables cuantitativas ####
# Dado que las variables cuantitativas no presentan distribucion normal se muestra mediana, 1º y 3º quartil, max y min

custom_summary <- function(df) {
  summary_stats <- data.frame(
    Min = apply(df, 2, min, na.rm = TRUE),
    `1st Qu.` = apply(df, 2, function(x) quantile(x, 0.25, na.rm = TRUE)),
    Median = apply(df, 2, median, na.rm = TRUE),
    `3rd Qu.` = apply(df, 2, function(x) quantile(x, 0.75, na.rm = TRUE)),
    Max = apply(df, 2, max, na.rm = TRUE)
  )
  return(summary_stats)
}

# Aplicar la función a los datos
P<-custom_summary(selected_datos)
P
# exportar esta tabla a Word, paquete flextable
if (!require(flextable)) install.packages("flextable")
library(flextable)

# Convertir la tabla a un objeto flextable para exportar a Word
tabla_flex <- regulartable(P)

# Guardar la tabla en un archivo de Word
save_as_docx(tabla_flex, path = "tabla_resumen.docx")


##### BOXPLOT de variables cuantitativas ####

# Seleccionar las columnas cuantitativas
quantitative_columns <- c("EDAD", "FEY", "T.CLAMP", "T.BOMB", "DIFF")
selected_datos <- datos[, quantitative_columns]

# Crear una lista para almacenar los gráficos
boxplot_list <- list()

# Obtener los colores de la paleta "Set3"
set3_colors <- brewer.pal(n = length(quantitative_columns), name = "Set3")

# Iterar sobre cada columna cuantitativa y crear un boxplot
for (i in seq_along(quantitative_columns)) {
  col <- quantitative_columns[i]
  
  # Crear el boxplot con ggplot2
  p <- ggplot(selected_datos, aes(x = "", y = !!sym(col), fill = col)) +
    geom_boxplot(fill = set3_colors[i], color = "blue") +
    labs(title = paste("Boxplot de", col),
         x = "Variable", y = "Valores") +
    theme_minimal()
  
  # Realizar el test de Shapiro-Wilk para normalidad
  shapiro_test <- shapiro.test(selected_datos[[col]])
  
  # Extraer el valor p con solo 2 decimales
  p_value <- format(shapiro_test$p.value, digits = 2)
  
  # Agregar el p-value al gráfico como texto
  p <- p + annotate("text", x = 1, y = max(selected_datos[[col]], na.rm = TRUE), 
                    label = paste("p value =", p_value), 
                    hjust = 2, vjust = 1, size = 3, color = "black")
  
  # Agregar el gráfico a la lista
  boxplot_list[[col]] <- p
}

# Mostrar los boxplots en una cuadrícula
grid.arrange(grobs = boxplot_list, ncol = 3)

# Restaurar el diseño gráfico predeterminado
par(mfrow = c(1, 1))


#### Identificación de outliers ####

# Función para detectar outliers usando el rango intercuartílico (IQR)
detectar_outliers <- function(data, columna) {
  Q1 <- quantile(data[[columna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[columna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  # Añadimos una columna temporal que indique la variable
  outliers <- data[data[[columna]] < limite_inferior | data[[columna]] > limite_superior, ]
  outliers$Variable <- columna
  return(outliers)
}

# Variables cuantitativas que queremos analizar
variables_cuantitativas <- c("EDAD", "T.CLAMP", "T.BOMB", "DIFF")

# Crear un nuevo dataframe vacío para almacenar los outliers
df_outliers <- data.frame()

# Extraer los outliers para cada variable y combinarlos, manteniendo el orden por variable
for (variable in variables_cuantitativas) {
  outliers_variable <- detectar_outliers(datos, variable)
  df_outliers <- rbind(df_outliers, outliers_variable)
}

# Eliminar duplicados en caso de que algunas filas sean outliers en más de una variable
df_outliers <- unique(df_outliers)

# Ordenar el dataframe por la columna 'Variable'
df_outliers <- df_outliers[order(df_outliers$Variable), ]

# Reordenar columnas para que 'Variable' sea la primera
df_outliers <- df_outliers[, c("Variable", setdiff(names(df_outliers), "Variable"))]

# Mostrar llas primeras rows del dataframe 
head (df_outliers)

# Mostrar cuantos outliers hay en el dataframe y ordenarlos
nrow(df_outliers)
print(df_outliers)

#___ ANALISIS BIVARIADO___====

#### DIFF vs BCIAO ####
# relacion entre la VR (BCIAO) vs VE (DIFF)

# Test de Wilcoxon-Mann-Whitney
wilcox_test <- wilcox.test(DIFF ~ BCIAO, data = datos, conf.int= 0.95)

# Ver los resultados del test
print(wilcox_test)


# Gráfico
ggplot(datos, aes(x = BCIAO, y = DIFF, fill = BCIAO)) +
  geom_boxplot(alpha = 0.7) + 
  labs(title = "Comparación de DIFF según BCIAO",
       x = "BCIAO", 
       y = "DIFF") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")  # Mejora la estética de los colores




# relacion entre las Vs control y VR (BCIAO)

#### BCIAO vs SEX ####

# Tabla de contingencia entre SEX y BCIAO
tabla_sex_bciao <- table(datos$SEX, datos$BCIAO)
tabla_sex_bciao

# verifico cumplimiento de los supuestos para Chi cuadrado
supuesto<- chisq.test(tabla_sex_bciao)
supuesto$expected

# Resultado de la prueba
chisq_test_sex <- chisq.test(tabla_sex_bciao)
print(chisq_test_sex)

# Gráfico de barras apilado
ggplot(datos, aes(x = SEX, fill = BCIAO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de BCIAO según SEX",
       x = "Sexo", 
       y = "Proporción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#### BCIAO vs CIRUGIA ####

# tabla de contingencia entre CIRUGIA y BCIAO
tabla_cirugia_bciao <- table(datos$CIRUGIA, datos$BCIAO)
print(tabla_cirugia_bciao)

# Comprobar las frecuencias esperadas
chi_test_cirugia <- chisq.test(tabla_cirugia_bciao)

# Mostrar las frecuencias esperadas
print(chi_test_cirugia$expected)

# Si hay muchas celdas con frecuencias esperadas menores a 5 (>20%), hago prueba exacta de Fisher:
if(any(chi_test_cirugia$expected < 5)){
  fisher_test_cirugia <- fisher.test(tabla_cirugia_bciao)
  print(fisher_test_cirugia)
} else {
  print(chi_test_cirugia)
}

# Usar simulación de Monte Carlo para calcular el valor p en Fisher
fisher_test_cirugia_sim <- fisher.test(tabla_cirugia_bciao, simulate.p.value = TRUE, B = 1e5)

# resultado de la prueba con simulación
print(fisher_test_cirugia_sim)

# Gráfico de barras apilado
ggplot(datos, aes(x = CIRUGIA, fill = BCIAO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de BCIAO según CIRUGIA",
       x = "cirugía", 
       y = "Proporción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#Hay evidencia estadísticamente significativa de que la variable CIRUGIA está 
#asociada con la variable BCIAO.

#### BCIAO vs VALVULA ####

# Filtrar datos donde VALVULA no sea "NC" y eliminar niveles no usados
datos_filtrados <- droplevels(datos[datos$VALVULA != "NC", ])

# Tabla de contingencia entre VALVULA y BCIAO con los datos filtrados
tabla_val_bciao <- table(datos_filtrados$VALVULA, datos_filtrados$BCIAO)
print(tabla_val_bciao)

# Verificar el cumplimiento de los supuestos para Chi-Cuadrado
supuesto <- chisq.test(tabla_val_bciao)
print(supuesto$expected)

# Realizar el test de chi-cuadrado
chisq_test_valvula <- chisq.test(tabla_val_bciao)
print(chisq_test_valvula)

# Gráfico de barras apilado sin "NC" en VALVULA
ggplot(datos_filtrados, aes(x = VALVULA, fill = BCIAO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de BCIAO según VALVULA",
       x = "Tipo de válvula", 
       y = "Proporción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#### BCIAO vs POSICION ####

# Filtrar datos donde POSICION no sea "NC" y eliminar niveles no usados
datos_filtrados <- droplevels(datos[datos$POSICION != "NC", ])

# Tabla de contingencia entre VALVULA y BCIAO con los datos filtrados
tabla_pos_bciao <- table(datos_filtrados$POSICION, datos_filtrados$BCIAO)
print(tabla_pos_bciao)

# Verificar el cumplimiento de los supuestos para Chi-Cuadrado
supuesto_pos <- chisq.test(tabla_pos_bciao)
print(supuesto_pos$expected)

# Si las frecuencias esperadas son mayores a 5, se puede usar chi-cuadrado
if (all(supuesto_pos$expected > 5)) {
  chisq_test_pos <- chisq.test(tabla_pos_bciao, correct = T)
  print(chisq_test_pos)
} else {
  # Si alguna frecuencia es menor a 5, se realiza el test de Fisher
  fisher_test_pos <- fisher.test(tabla_pos_bciao)
  print(fisher_test_pos)
}

# Crear el gráfico sin la categoría NA
ggplot(datos_filtrados, aes(x = POSICION, fill = BCIAO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de BCIAO según POSICIÓN",
       x = "Posición", 
       y = "Proporción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#### BCIAO vs HIPOT ####

# Tabla de contingencia entre HIPOT y BCIAO
tabla_hipot_bciao <- table(datos$HIPOT, datos$BCIAO)
print(tabla_hipot_bciao)

# Verificación de los supuestos del test de Chi-cuadrado
supuesto_hipot <- chisq.test(tabla_hipot_bciao)
print(supuesto_hipot$expected) # Frecuencias esperadas

# Si las frecuencias esperadas son mayores a 5, se puede usar chi-cuadrado
if (all(supuesto_hipot$expected > 5)) {
  chisq_test_hipot <- chisq.test(tabla_hipot_bciao)
  print(chisq_test_hipot)
} else {
  # Si alguna frecuencia es menor a 5, se realiza el test de Fisher
  fisher_test_hipot <- fisher.test(tabla_hipot_bciao)
  print(fisher_test_hipot)
}


# Gráfico de barras apilado para HIPOT y BCIAO
ggplot(datos_filtrados , aes(x = HIPOT, fill = BCIAO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de BCIAO según HIPOTERMIA",
       x = "Hipotermia", 
       y = "Proporción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#### BCIAO vs EDAD ####

# Test de Mann-Whitney para comparar EDAD según BCIAO
mann_whitney_test <- wilcox.test(EDAD ~ BCIAO, data = datos, conf.int= 0.95)

# Mostrar el resultado del test
print(mann_whitney_test)

# Gráfico de cajas (boxplot) para visualizar EDAD según BCIAO
ggplot(datos, aes(x = factor(BCIAO), y = EDAD, fill = factor(BCIAO))) +
  geom_boxplot() +
  labs(title = "Distribución de la EDAD según BCIAO",
       x = "BCIAO",
       y = "Edad") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  guides(fill = "none") 


#### CORRELACION ENTRE COVARIABLES E IDENTIFICACION DE COLINEALIDAD ####
# correlaciones bivariadas con test de Spearman dado que las distribuciones no asumen normalidad 
# EDAD-FEY
shapiro.test(datos$EDAD)
shapiro.test(datos$FEY)
cor.test(datos$EDAD,datos$FEY, method = "spearman",exact=FALSE)
dat1 <- data.frame(datos$EDAD,datos$FEY)
chart.Correlation(dat1, method = "spearman", exact=F)
#EDAD-DIFF
shapiro.test(datos$EDAD)
shapiro.test(datos$DIFF)
cor.test(datos$EDAD,datos$DIFF, method = "spearman",exact=FALSE)
dat2 <- data.frame(datos$EDAD,datos$DIFF)
chart.Correlation(dat2, method = "spearman", exact=FALSE)
#FEY-DIFF
shapiro.test(datos$FEY)
shapiro.test(datos$DIFF)
cor.test(datos$FEY,datos$DIFF, method = "spearman",exact=FALSE)
dat3 <- data.frame(datos$FEY,datos$DIFF)
chart.Correlation(dat3, method = "spearman", exact=F)

# Selecciona las columnas específicas
datos_cuanti <- datos[c("EDAD","FEY","T.BOMB","T.CLAMP","DIFF")]
# Obtener el resumen estadístico
summary <- summary(datos_cuanti)
# Mostrar el resumen
print(summary)


ggpairs(datos[c("EDAD", "FEY", "T.BOMB", "T.CLAMP", "DIFF")], 
        aes(color = datos$SEX, alpha = 0.5),
        upper = list(continuous = wrap(ggally_cor, method = "spearman", size = 6)),
        lower = list(continuous = wrap("smooth", alpha = 0.3)),
        diag = list(continuous = "densityDiag")) 
 


# BIAO VS DIFF (relación entre la VR y la VE)
# Crear el gráfico de caja con ggplot2
boxplot <- ggplot(datos, aes(x = SEX, y = DIFF)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Box Plot de SEX vs DIFF",
       x = "SEX", y = "DIFF") +
  theme_minimal()

# Mostrar el gráfico
print(boxplot)

# Prueba de Mann-Whitney U (o prueba de Wilcoxon), dado que DIFF no sigue una distribución normal
wilcox.test(DIFF ~ BCIAO, data = datos)

#Como el p-valor es menor que 0.05 (e incluso mucho más pequeño), rechazamos la hipótesis nula, 
#que establece que las dos muestras provienen de la misma distribución o que tienen la misma mediana

# SEX VS DIFF 

# Crear el gráfico de caja con ggplot2
boxplot <- ggplot(datos, aes(x = SEX, y = DIFF)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Box Plot de SEX vs DIFF",
       x = "SEX", y = "DIFF") +
  theme_minimal()

# Mostrar el gráfico
print(boxplot)

# Seleccionar las columnas 'SEX' y 'DIFF'
library(dplyr)
dfSEX <- datos %>% dplyr::select(SEX, DIFF)

# Calcular las medias agrupadas por 'SEX'
meansSEX <- dfSEX %>% 
  group_by(SEX) %>% 
  summarise(mean_DIFF = mean(DIFF))

# Presentar el resultado como un cuadro
knitr::kable(meansSEX, caption = "Tabla de medias de DIFF por SEX")

# Convertir la columna 'SEX' a tipo de datos string
datos$SEX <- as.character(datos$SEX)

# Filtrar los datos por sexo 'M' y 'F' y seleccionar solo la columna 'DIFF'
data_M <- datos %>%
  filter(SEX == "M") %>%
  dplyr::select(DIFF)

data_F <- datos %>%
  filter(SEX == "F") %>%
  dplyr::select(DIFF)

# Obtener los valores de DIFF para cada grupo
DIFF_M <- unlist(data_M)
DIFF_F <- unlist(data_F)

# Realizar el análisis ANOVA aunque la distribucion no es normal el ANOVA es robusto 
result_anova <- aov(DIFF ~ SEX, data = datos)
summary(result_anova)

# pruebo con wilcoxon asumiento que la distribucion de la varibale DIFF no es normal 
wilcox.test(DIFF ~ SEX, data = datos)

# en ambos casos puedo decir que no hay diferencias significativas en (media/mediana) de la DIFF entre 
# H y M 

# CIRUGIA VS DIFF
ggplot(data = as.data.frame(datos), aes(x = CIRUGIA, y = DIFF)) +
  geom_boxplot() +
  labs(title = "Box Plot de CIRUGIA vs DIFF")

# Calcular las medias de DIFF por CIRUGIA
meansCIRUGIA <- aggregate(DIFF ~ CIRUGIA, data = datos, FUN = mean)

# Ordenar por mean_DIFF
meansCIRUGIA <- meansCIRUGIA[order(meansCIRUGIA$DIFF), ]

# Presentar los resultados como un cuadro
kable(meansCIRUGIA, caption = "Tabla de medias de DIFF por CIRUGIA")

# Realizar el ANOVA
anova_result <- aov(DIFF ~ CIRUGIA, data = datos)

# Obtener un resumen del ANOVA
summary(anova_result)

# Realizar el test de Tukey
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Obtener los valores de p.adj
p_adj_values <- tukey_result$CIRUGIA[, "p adj"]

# Crear un dataframe con los valores de p.adj y las comparaciones
resultados_df <- data.frame(Comparaciones = rownames(tukey_result$CIRUGIA),
                            p_adj = p_adj_values)

# Ordenar el dataframe por la columna p.adj en orden descendente
resultados_ordenados <- resultados_df[order(resultados_df$p_adj, decreasing = TRUE), ]

# Imprimir el dataframe ordenado
print(resultados_ordenados)

#___ TABLAS___====


#### tabla de las variables CUALI vs BCIAO ####

# Crear un subset de 'datos' sin filas con "NC" y eliminar niveles no observados
tabla_filtrada<- datos %>%
  filter(POSICION != "NC", VALVULA != "NC") %>%
  droplevels()

tabla_cuali1 <- tableby(BCIAO ~ POSICION + VALVULA, data = tabla_filtrada, cat.test = "chisq")
summary(tabla_cuali1, text = T)

tabla_cuali2 <- tableby(BCIAO ~ chisq (SEX) + chisq (HIPOT) + fe (CIRUGIA), data = datos, simulate.p.value=TRUE, B=1e5)
summary(tabla_cuali2, text = T)


# Exportar a un archivo de Word
write2word(tabla_cuali1 , file = "tabla_resumida_CUALI1.docx")
write2word(tabla_cuali2 , file = "tabla_resumida_CUALI2.docx")


#### tabla de las variables CUANTI vs BCIAO ####

# Tabla de variables CUANTI vs BCIAO
tabla_cuanti <- tableby(BCIAO ~ EDAD + FEY + T.CLAMP + T.BOMB + DIFF , data = datos, numeric.test = "kwt", numeric.stats = c( "N", "median", "q1q3","range"), stats.labels=list(N='N', median='Mediana', q1q3='Q1,Q3', range="rango"), digits=0)
summary(tabla_cuanti, text = T)

# Exportar a un archivo de Word
write2word(tabla_cuanti , file = "tabla_resumida_CUANTI.docx")



#tabla3 <- tableby(BCIAO ~ EDAD + FEY , data = datos, numeric.test= "anova")
#summary(tabla3, text=T)

# Resumir la tabla con opciones de formato
tabla_resumida <- summary(tabla1, title = "Tabla de resumen del estudio", text = TRUE)

# Exportar a un archivo de Word
write2word(tabla_resumida , file = "tabla_resumida.docx")

# Exportar a HTML
write2html(tabla_resumida , file = "tabla_resumida.html")

# Exportar a PDF (requiere LaTeX)
write2pdf(tabla_resumida , file = "tabla_resumida.pdf")

#___ BALACEADO DE LABEL ___====

# Filtrar las filas donde BCIAO es igual a "CON BALON" y obtener el número de filas
con_balon_rows <- which(datos$BCIAO == "CON BCIAO")
n_filas_con_balon <- length(con_balon_rows)

# Mostrar los números de fila y el conteo total
con_balon_rows
n_filas_con_balon

# Filtrar todos los casos con "CON BCIAO"
con_bciao <- subset(datos, BCIAO == "CON BCIAO")

# Filtrar todos los casos con "SIN BCIAO"
sin_bciao <- subset(datos, BCIAO == "SIN BCIAO")

# Seleccionar aleatoriamente 413 filas de los casos "SIN BCIAO"
set.seed(27848992)  # Fijar semilla para reproducibilidad
sin_bciao_sample <- sin_bciao[sample(nrow(sin_bciao), 413), ]

# Combinar ambos subsets para tener un conjunto balanceado
datos1<- rbind(con_bciao, sin_bciao_sample)

# Verificar la estructura del subset balanceado
table(datos1$BCIAO)
nrow(datos1)

#___RED NEURONAL___====

#### MODELO PREDICTIVO ####

# Particionamiento 70/30 en conjunto de entrenamiento y testeo
set.seed(27848992)
particion <- createDataPartition(y = datos1$BCIAO, p = 0.7, list = FALSE) 
entreno <- datos1[particion, ]
testeo <- datos1[-particion, ]

# Verificar si hay valores NA en los datos de entrenamiento
summary(entreno)
summary(testeo)

# Eliminar NAs y asegurarse de que la variable dependiente es un factor
entreno <- na.omit(entreno)
entreno$BCIAO <- as.factor(entreno$BCIAO)

# Corroboro cantidad de casos en la partición 
dim(entreno)
dim(testeo)

# Corroboro porcentaje en la particion
table(datos1$BCIAO)
table(entreno$BCIAO)
table(testeo$BCIAO)

porc_entreno<-(table(entreno$BCIAO)*100)/table(datos1$BCIAO)
porc_entreno

porc_testeo<-(table(testeo$BCIAO)*100)/table(datos1$BCIAO)
porc_testeo


### ENTRENAMIENTO DEL MODELO ####

# Crear la cuadrícula de hiperparametros (size=neuronas) 
# decay(regularizacion L2 penaliza por pesos grandes para evitar overfitting)
tune_grid <- expand.grid(size = c(1, 3, 5), decay = c(0, 0.01, 0.1, 0.0001))
tune_grid
# Configuración de la validación cruzada de 5-folds
control <- trainControl(method = "cv", number = 5)  # Validación cruzada de 5-folds

# Entrenamiento del modelo de red neuronal con validación cruzada
set.seed(27848992)
red_cv <- train(BCIAO ~ SEX + EDAD + VALVULA + POSICION + FEY + CIRUGIA,
                data = entreno,
                method = "nnet",    # Modelo de red neuronal
                trControl = control, # Validación cruzada
                linout = FALSE,      # Salida no lineal
                maxit = 300,         # Número máximo de iteraciones
                tuneGrid = tune_grid) # Cuadrícula de hiperparámetros

# Extrae los pesos sinápticos
pesos <- red_cv$finalModel$wts  
print(pesos)

# Visualizacion del modelo con validación cruzada
print(red_cv) # Resumen del modelo entrenado
plot(red_cv) # Gráfico de rendimiento del modelo
plotnet(red_cv, struct = T, bias = T, circle_cex = 3, cex_val = 0.9, line_stag = 0.02) # Visualización de la red

#### EVALUACIÓN DEL MODELO ####

# Predicción y matriz de confusión en el conjunto de testeo con umbral por defecto 0.5
pred_cv <- predict(red_cv, newdata = testeo)

#comparacion real vs predicción
comparacion<- data.frame( real= testeo$BCIAO, prediccion = pred_cv)
comparacion

# Matriz de confusión sin curva ROC con umbral 0.5 (default)
matriz_default<-confusionMatrix(factor(pred_cv), factor(testeo$BCIAO))
matriz_default 


#### CURVA ROC Y AUC ####

# Generar pobabilidades de prediccion
pred_probs <- predict(red_cv, newdata = testeo, type = "prob")[, 1]  # Probabilidad de la clase positiva

# Calculo de curva ROC y AUC
roc_curve <- roc(testeo$BCIAO, pred_probs) 
auc<-auc(roc_curve) 
IC_auc<-ci.auc(roc_curve, conf.level = 0.95) 
auc
IC_auc

# visualizacion de la curva ROC
# Convertir el objeto ROC a un data frame para ggplot2
roc_data <- data.frame(
  Sensitivity = rev(roc_curve$sensitivities),
  Specificity = rev(1 - roc_curve$specificities)
)

# Graficar la curva ROC con ggplot2
ggplot(roc_data, aes(x = Specificity, y = Sensitivity)) +
  geom_line(color = "darkblue", size = 1.2) +                      # Curva ROC
  geom_ribbon(aes(ymin = 0, ymax = Sensitivity), fill = "skyblue", alpha = 0.4) +  # Sombreado de AUC uniforme
  labs(
    title = "Curva ROC",
    x = "1 - Especificidad (Tasa de Falsos Positivos)",
    y = "Sensibilidad (Tasa de Verdaderos Positivos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Título centrado y en negrita
    axis.title = element_text(size = 14),                              # Tamaño de títulos de ejes
    axis.text = element_text(size = 12)                                # Tamaño de etiquetas de ejes
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Línea de referencia diagonal
  scale_x_continuous(limits = c(0, 1)) +   # Limitar eje x entre 0 y 1
  scale_y_continuous(limits = c(0, 1))     # Limitar eje y entre 0 y 1

#### DETERMINACION DEL UMBRAL OPTIMO ####

# curva ROC para elegir un umbral óptimo basado en el trade-off entre sensibilidad (recall) y especificidad.
optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
optimal_threshold <- as.numeric(optimal_threshold) 

# Umbral óptimo calculado
threshold <- optimal_threshold
threshold

# Predicción con el umbral óptimo (threshold 0.1896487)
pred_cv_class_optimal <- ifelse(pred_probs > threshold, "CON BCIAO", "SIN BCIAO") # Clasificaciones basadas en el umbral
pred_cv_class_optimal <- factor(pred_cv_class_optimal, levels = c("CON BCIAO", "SIN BCIAO")) # Convertir a factor

# Verificar la longitud de las predicciones
length(pred_cv_class_optimal)  # Debería ser 362

# Matriz de confusión optima
matriz_optima<-confusionMatrix(pred_cv_class_optimal, factor(testeo$BCIAO))
matriz_optima

# Extraer tabla de confusión de la matriz
tabla_confusion<-matriz_optima$table
tabla_confusion

# Cálculo de métricas
VP<- tabla_confusion["CON BCIAO", "CON BCIAO"]  # Verdaderos positivos
VN<- tabla_confusion["SIN BCIAO", "SIN BCIAO"]  # Verdaderos negativos
FP<- tabla_confusion["CON BCIAO", "SIN BCIAO"]  # Falsos positivos
FN<- tabla_confusion["SIN BCIAO", "CON BCIAO"]  # Falsos negativos

# Calcular métricas adicionales
sensibilidad <- VP / (VP + FN) 
especificidad <- VN / (VN + FP) 
tasa_acierto <- (VP + VN) / (VP + VN + FP + FN) 
tasa_error <- (FP + FN) / (VP + VN + FP + FN) 
precision <- VP / (VP + FP)

# Cálculo del F1 Score
F1_score <- 2 * (precision * sensibilidad) / (precision + sensibilidad)
F1_score

###########################################################################

########## testeando mcneamar ########
tabla_confusion # optima

# Extraer los valores de FN y FP
FN <- tabla_confusion[2, 1]  # Falsos Negativos
FP <- tabla_confusion[1, 2]  # Falsos Positivos

# Crear un data frame para graficar
data <- data.frame(
  Resultado = c("Falsos Negativos", "Falsos Positivos"),
  Cantidad = c(FN, FP)
)

# Graficar con ggplot2
ggplot(data, aes(x = Resultado, y = Cantidad, fill = Resultado)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Comparación de Falsos Negativos y Falsos Positivos",
       x = "Resultado",
       y = "Cantidad") +
  scale_fill_manual(values = c("Falsos Negativos" = "#E63946", "Falsos Positivos" = "#3F9BCA")) +
  theme(legend.position = "none")

##########################################################################
#### ANÁLISIS Y VISUALIZACIÓN DE MÉTRICAS SEGÚN UMBRAL ####

# Predecir probabilidades en el conjunto de testeo
pred_probs <- predict(red_cv, newdata = testeo, type = "prob")[, "CON BCIAO"]  # Asumimos que "CON BCIAO" es la clase positiva

# Crear secuencia de umbrales
thresholds <- seq(0, 1, by = 0.001)

# Calcular métricas en función del umbral
metrics <- data.frame(
  Threshold = thresholds,
  Sensitivity = sapply(thresholds, function(t) { 
    confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                           levels = c("CON BCIAO", "SIN BCIAO")),
                    factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))$byClass["Sensitivity"]
  }),
  Specificity = sapply(thresholds, function(t) { 
    confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                           levels = c("CON BCIAO", "SIN BCIAO")),
                    factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))$byClass["Specificity"]
  }),
  Precision = sapply(thresholds, function(t) { 
    confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                           levels = c("CON BCIAO", "SIN BCIAO")),
                    factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))$byClass["Pos Pred Value"]
  }),
  Tasa_Acierto = sapply(thresholds, function(t) {
    matriz_confusion <- confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                                               levels = c("CON BCIAO", "SIN BCIAO")),
                                        factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))
    VP <- matriz_confusion$table["CON BCIAO", "CON BCIAO"]
    VN <- matriz_confusion$table["SIN BCIAO", "SIN BCIAO"]
    FP <- matriz_confusion$table["SIN BCIAO", "CON BCIAO"]
    FN <- matriz_confusion$table["CON BCIAO", "SIN BCIAO"]
    return((VP + VN) / (VP + VN + FP + FN))  # Tasa de Acierto
  }),
  F1_Score = sapply(thresholds, function(t) {
    precision_value <- confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                                              levels = c("CON BCIAO", "SIN BCIAO")),
                                       factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))$byClass["Pos Pred Value"]
    sensitivity_value <- confusionMatrix(factor(ifelse(pred_probs > t, "CON BCIAO", "SIN BCIAO"), 
                                                levels = c("CON BCIAO", "SIN BCIAO")),
                                         factor(testeo$BCIAO, levels = c("CON BCIAO", "SIN BCIAO")))$byClass["Sensitivity"]
    
    # Manejo de NA
    if (is.na(precision_value) || is.na(sensitivity_value) || 
        (precision_value + sensitivity_value) == 0) {
      return(0)  # Retorna 0 si hay NA o la suma es 0
    } else {
      return(2 * (precision_value * sensitivity_value) / (precision_value + sensitivity_value))  # F1 Score
    }
  })
)

# Graficar sensibilidad y especificidad a diferentes umbrales
ggplot(metrics, aes(x = Threshold)) +
  geom_line(aes(y = Sensitivity, color = "Sensibilidad"), size = 0.5) +
  geom_line(aes(y = Specificity, color = "Especificidad"), size = 0.5) +
  
  # Agregar línea verde con dobleces en x = 0.5432122 y cambios en y = 0.8862 y y = 0.8049
  geom_segment(aes(x = 0.5432122, xend = 0.5432122, y = 0, yend = 0.8862), color = "green", linetype = "dotted") +  # Línea vertical hasta y = 0.8862
  geom_segment(aes(x = 0, xend = 0.5432122, y = 0.8862, yend = 0.8862), color = "green", linetype = "dotted") +     # Línea horizontal hacia la izquierda en y = 0.8862
  geom_segment(aes(x = 0.5432122, xend = 0.5432122, y = 0.8862, yend = 0.8049), color = "green", linetype = "dotted") +  # Línea vertical desde y = 0.8862 hasta y = 0.8049
  geom_segment(aes(x = 0, xend = 0.5432122, y = 0.8049, yend = 0.8049), color = "green", linetype = "dotted") +    # Línea horizontal hacia la izquierda en y = 0.8049
  
  # Anotaciones en el eje Y
  annotate("text", x = 0, y = 0.8862, label = "0.8862", hjust = 0.4, color = "black", size = 3) +
  annotate("text", x = 0, y = 0.8049, label = "0.8049", hjust = 0.4, color = "black", size = 3) +
  
  labs(title = "Comportamiento de Sensibilidad y Especificidad en función del Umbral",
       x = "Umbral",
       y = "Valor") +
  scale_x_continuous(limits = c(0, 1)) +  # Establecer límites del eje x
  scale_y_continuous(limits = c(0, 1)) +  # Establecer límites del eje y
  theme_minimal() +
  scale_color_manual(name = "Métrica", 
                     values = c("Sensibilidad" = "blue", 
                                "Especificidad" = "red"))


ggplot(metrics, aes(x = Threshold)) +
  geom_line(aes(y = Sensitivity, color = "Sensibilidad"), size = 0.5) +
  geom_line(aes(y = Specificity, color = "Especificidad"), size = 0.5) +
  geom_line(aes(y = Tasa_Acierto, color = "Tasa de Acierto"), size = 0.5) +
  geom_line(aes(y = Precision, color = "Precisión"), size = 0.5) +
  geom_line(aes(y = F1_Score, color = "F1 Score"), size = 0.5) +  # Agregar F1 Score
  
  # Agregar línea desde x = 0.5432 hasta el valor correspondiente de y = 0.8862
  geom_segment(aes(x = 0.5432, xend = 0.5432, y = 0, yend = 0.8862), color = "green", linetype = "dashed", size=0.3) +
  
  labs(title = "Metricas de rendimiento en función del Umbral",
       x = "Umbral",
       y = "Valor") +
  scale_x_continuous(limits = c(0, 1)) +  # Establecer límites del eje x
  scale_y_continuous(limits = c(0, 1)) +  # Establecer límites del eje y
  theme_minimal() +
  scale_color_manual(name = "Métrica", 
                     values = c("Sensibilidad" = "blue", 
                                "Especificidad" = "red", 
                                "Tasa de Acierto" = "green", 
                                "Precisión" = "purple", 
                                "F1 Score" = "orange"))  # Color para F1 Score

# Filtrar filas con valores NA en métricas utilizadas
missing_rows <- metrics %>%
  filter(is.na(Sensitivity) | is.na(Specificity) | is.na(Tasa_Acierto) | 
           is.na(Precision) | is.na(F1_Score))
missing_rows


# Análisis de resultados
cat("Análisis de resultados:\n")
cat("- La sensibilidad indica la capacidad del modelo para detectar correctamente los casos positivos.\n")
cat("- La especificidad mide la capacidad del modelo para identificar correctamente los casos negativos.\n")
cat("- La tasa de acierto es la proporción de predicciones correctas entre todas las predicciones.\n")
cat("- La precisión se refiere a la proporción de verdaderos positivos entre todos los casos predichos como positivos.\n")
cat("- El F1 Score proporciona un equilibrio entre la precisión y la sensibilidad.\n")

# Comparación con otros modelos (si es aplicable)
# Aquí se puede incluir un bloque para comparar métricas con otros modelos si los tienes.

# Consideraciones finales
cat("Conclusiones:\n")
cat("- El modelo ha mostrado un rendimiento aceptable en términos de sensibilidad y precisión.\n")
cat("- Sin embargo, se deben considerar mejoras, como la optimización de hiperparámetros o la inclusión de más variables.\n")
cat("- Futuras investigaciones pueden incluir el uso de diferentes algoritmos y técnicas de ensamblaje para mejorar el rendimiento del modelo.\n")

# Resumen final de métricas
cat("Resumen de métricas finales:\n")
cat("Sensibilidad:", sensibilidad, "\n")
cat("Especificidad:", especificidad, "\n")
cat("Tasa de acierto:", tasa_acierto, "\n")
cat("Tasa de error:", tasa_error, "\n")
cat("Precisión:", precision, "\n")
cat("F1 Score:", F1_score, "\n")


#tabla de metricas en secuencia de umbrales 

# predicciones en el conjunto de prueba
# red_cv es el modelo entrenado
pred_cv <- predict(red_cv, newdata = testeo, type = "prob") # Obtiene las probabilidades

# Crea el dataframe de predicciones
predicciones <- data.frame(probabilidad = pred_cv[, "CON BCIAO"], # Columna de probabilidades para la clase positiva
                           real = testeo$BCIAO) # Columna de etiquetas reales

# Muestra las primeras filas del dataframe
head(predicciones)


# Inicializa un dataframe para almacenar los resultados
resultados <- data.frame(umbral = numeric(),
                         sensibilidad = numeric(),
                         especificidad = numeric(),
                         FP = numeric(),
                         FN = numeric(),
                         tasa_acierto = numeric(),
                         tasa_error = numeric(),
                         precision = numeric(),
                         F1_score = numeric())


# Recorre los umbrales del 0.1 al 0.75
for (umbral in seq(0.1, 0.75, by = 0.0001)) {
  
  # Genera predicciones basadas en el umbral
  predicciones_clasificadas <- ifelse(predicciones$probabilidad >= umbral, "CON BCIAO", "SIN BCIAO")
  
  # Crea la matriz de confusión
  matriz_confusion <- table(predicciones_clasificadas, predicciones$real)
  
  # Extrae los valores de la matriz de confusión
  VP <- matriz_confusion["CON BCIAO", "CON BCIAO"]
  FN <- matriz_confusion["CON BCIAO", "SIN BCIAO"]
  VN <- matriz_confusion["SIN BCIAO", "SIN BCIAO"]
  FP <- matriz_confusion["SIN BCIAO", "CON BCIAO"]
  F1_score <- ifelse(precision + sensibilidad == 0, 0, 2 * (precision * sensibilidad) / (precision + sensibilidad))
  
  # Agrega los resultados al dataframe
  resultados <- rbind(resultados, data.frame(umbral = umbral,
                                             sensibilidad = sensibilidad,
                                             especificidad = especificidad,
                                             FP = FP,
                                             FN = FN,
                                             tasa_acierto = tasa_acierto,
                                             tasa_error = tasa_error,
                                             precision = precision,
                                             F1_score = F1_score))
}

head (resultados)
# Filtrar para ver umbrales en el rango específico que me intersa
rango_resultados<-resultados[resultados$umbral >= 0.50 & resultados$umbral <= 0.55, ]
#options(max.print = 8000)  # O un número mayor según sea necesario
print(rango_resultados)


#### PREDICCION DE CASOS ####
# nuevo caso 1 con umbral 0.5432122
# Definir el nuevo caso
caso1 <- data.frame(SEX= "F", FEY = 40, EDAD = 60, CIRUGIA= "CRM", VALVULA= "NC", POSICION="NC")

# Obtener las probabilidades de la clase positiva para el nuevo caso
probabilidades_caso1 <- predict(red_cv, newdata = caso1, type = "prob") #[, 1]  # Probabilidad de "CON BCIAO"

# Realizar la predicción de la clase basada en el umbral óptimo
prediccion_caso1 <- ifelse(probabilidades_caso1 > threshold, "CON BCIAO", "SIN BCIAO")
# Convertir a vector y hacer la predicción
prediccion_caso1 <- ifelse(as.numeric(probabilidades_caso1[1, "CON BCIAO"]) > threshold, "CON BCIAO", "SIN BCIAO")

# Imprimir las probabilidades y la predicción del caso
print(prediccion_caso1)
print(probabilidades_caso1)









