#-----------------------------------------------------
#                LEER REJILLA
#-----------------------------------------------------

library(readxl)
#install.packages("tidiverse")
library(tidyverse)
library(dplyr)



#repgrid_elementos <- read_excel("C:\\Users\\gilmo\\Desktop\\Ruta_archivos\\Repgrid_LUIS_DAVID_2024-01-09_20_40_23.xlsx")
#repgrid_elementos

repgrid_elementos <- read_excel("C:\\Users\\gilmo\\Desktop\\Ruta_archivos\\RTP1734508.xlsx")
repgrid_elementos

lista_vectores_e <- lapply(repgrid_elementos, as.vector)
lista_vectores_e


T_elementos_fin<- data.frame(lista_vectores_e[3:length(lista_vectores_e)-1])
T_elementos_fin


t_elementos_fin_R <- round(cor(T_elementos_fin),2)
t_elementos_fin_R #correlacion elementos

#------ ahora vectores constructos


#repgrid_constructos <- read_excel("C:\\Users\\gilmo\\Desktop\\Ruta_archivos\\Repgrid_LUIS_DAVID_2024-01-09_20_40_23.xlsx", skip = 1)


repgrid_elementos

matriz_e <- as.matrix(repgrid_elementos)
print(matriz_e)

ncol(matriz_e)
nrow(matriz_e)

vector_variables_c <- matriz_e[,1] #saco vector nombre variables
vector_variables_op_c <- matriz_e[,ncol(matriz_e)]
vector_variables_c
vector_variables_op_c

#elimiino las columnas
columnas_a_eliminar <- c(1, ncol(matriz_e))
matriz_e_d <- matriz_e[, -columnas_a_eliminar]
matriz_e_d

#matriz_e <- matrix(repgrid_elementos, nrow, ncol(matriz_e_d)-2)
#matriz_e

Tabla_e_d <- data.frame(matriz_e_d)
Tabla_e_d

# tabla constructos
matriz_c <- t(matriz_e_d)
matriz_c

matriz_cn <- as.numeric(matriz_c)
matriz_cn

matriz_c_def <- matrix(matriz_cn,nrow(matriz_e), ncol(matriz_e)-2)
matriz_c_def

tabla_c <- data.frame(matriz_c_def)
tabla_c

nuevos_nombres <- as.character(vector_variables_c)
colnames(tabla_c) <- nuevos_nombres # para cambiar nombres
tabla_c

tabla_c_R <- round(cor(tabla_c),2)
tabla_c_R #correlacion elementos

#TABLA DISTANCIAS "CONSTRUCTOS"
matriz_c_def_distancias <- round(dist(matriz_c_def),2)
matriz_c_def_distancias

#TABLA DISTANCIAS "ELEMENTOS"
matriz_e_d__distancias <- round(dist(matriz_e_d),2)
matriz_e_d__distancias

matriz_e_d__distancias2 <- round(dist(T_elementos_fin),2)
matriz_e_d__distancias2

#desde aqui ya no lo tengo tan claro otras pruebas

# INTENTO SACAR PRIMER FACTOR
resultado_pca <- prcomp(tabla_c_R, scale. = TRUE)
primer_factor <- resultado_pca$rotation [,1]
print("\nprimer factor (componente principal")
primer_factor

representación_lineal <- as.matrix(tabla_c_R) %% primer_factor
plot(representación_lineal, col = "blue", pch = 19, main = "Representación lineal del primer factor")

# INTENTO SACAR SEGUNDO FACTOR
resultado_pca2 <- prcomp(tabla_c_R, scale. = TRUE)
segundo_factor <- resultado_pca2$rotation [,2]
segundo_factor

representación_lineal2 <- as.matrix(tabla_c_R) %% segundo_factor
plot(representación_lineal2, col = "blue", pch = 19, main = "Representación lineal del segundo factor")



#------------GRAFICAS, NO MUY CLARO
num_conglomerados <- 3
resultado_kmeans <- kmeans(Tabla_e_d, centers = num_conglomerados)
df_conglomerados <- cbind(Tabla_e_d, conglomerado = as.factor(resultado_kmeans$cluster))
df_conglomerados
#plot(df_conglomerados[,1:2], col = resultado_kmeans$cluster, pch=19, main = "Gráfico de dispersión con conglomerados", xlab = "variable1", ylab "variable2")
plot(df_conglomerados[,1:3])

#install.packages("fmsb")
library(fmsb)
#df_conglomerados
#df_radar <- df_conglomerados[,2:3]
#df_radar_norm <- as.data.frame(scale(df_radar))
#df_radar_norm$cluster <- resultado_kmeans$cluster

#inteto modificar a ver si grabo algo