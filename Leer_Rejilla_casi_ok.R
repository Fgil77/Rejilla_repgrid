#-----------------------------------------------------
#                LEER REJILLA
#-----------------------------------------------------

library(readxl)
##install.packages("tidiverse")
library(tidyverse)
library(dplyr)
##install.packages("devtools") #porque lo pide el programa (no volver a instalar)
devtools::install_github("r-lib/conflicted")




#repgrid_elementos <- read_excel("C:\\Users\\gilmo\\Desktop\\Ruta_archivos\\Repgrid_LUIS_DAVID_2024-01-09_20_40_23.xlsx")
#repgrid_elementos

repgrid_elementos <- read_excel("C:\\Users\\gilmo\\Desktop\\Ruta_archivos\\Repgrid_fgil_2023-12-12_18_57_04.xlsx")
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

matriz_c_def <- matrix(matriz_cn, ncol(matriz_e)-2, nrow(matriz_e))
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
matriz_constructos_d__distancias <- round(dist(Tabla_e_d),2)
matriz_constructos_d__distancias

matriz_elementos_d__distancias2 <- round(dist(matriz_c_def),2)
matriz_elementos_d__distancias2
class(matriz_elementos_d__distancias2)

#----Preparo la tabla de ELEMENTOS distancias
names(Tabla_e_d) #PARA OBTENER UN VECTOR CON LOS NOMBRES DE LAS VARIBLES / TÍTULOS DE LAS COLUMNAS
vector_varible_elementos <- names(Tabla_e_d)
vector_varible_elementos
#-Montamos la tabla
matriz_regular_elementos_d_distancias2 <- as.matrix(matriz_elementos_d__distancias2)
matriz_regular_elementos_d_distancias2
colnames(matriz_regular_elementos_d_distancias2) <- vector_varible_elementos
colnames(matriz_regular_elementos_d_distancias2)
rownames(matriz_regular_elementos_d_distancias2) <- colnames(matriz_regular_elementos_d_distancias2)
tabla_regular_ELEMENTOS_d_distancias2 <- as.data.frame(matriz_regular_elementos_d_distancias2)
tabla_regular_ELEMENTOS_d_distancias2


#Preparo la tabla de CONSTRUCTOS distancias
matriz_regular_constructos_d_distancias2 <- as.matrix(matriz_constructos_d__distancias)
matriz_regular_constructos_d_distancias2
colnames(matriz_regular_constructos_d_distancias2) <- vector_variables_c
colnames(matriz_regular_constructos_d_distancias2)
rownames(matriz_regular_constructos_d_distancias2) <- colnames(matriz_regular_constructos_d_distancias2)
tabla_regular_CONSTRUCTOS_d_distancias2 <- as.data.frame(matriz_regular_constructos_d_distancias2)
tabla_regular_CONSTRUCTOS_d_distancias2
print(tabla_regular_CONSTRUCTOS_d_distancias2)

#---IMPRIMIR LA TABLA COMO OBJETO
library(gridExtra)
library(ggplot2)

#creamos tabla con tableGrob
tabla_image_elementos_distancias <- tableGrob(tabla_regular_ELEMENTOS_d_distancias2)
tabla_image_elementos_distancias
#class(tabla_image_elementos_distancias)

#CREAMOS ARCHIVO .PNG
imagen_dist_ELEMENTOS <- ggsave("Imagen_dist_ELEMENTOS.png", plot = tabla_image_elementos_distancias, device = "png", width = 49, height = 10, units = "in")
imagen_dist_ELEMENTOS

#otra forma------- CREAR ARCHIVO HTML 
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("webshot")


library(knitr)
library(kableExtra)
library(webshot)

tabla_kable_e_dis <- kable(tabla_regular_ELEMENTOS_d_distancias2, "html") %>% kable_styling("striped")
tabla_kable_e_dis # crea html a partir de la tabla de datos, el styling para dar estilo

temp_html_Elementos <- tempfile(fileext = ".html") 
temp_html_Elementos

#cat(tabla_kable_e_dis, file = temp_html) #el temporal html en un directorio c C:\\Users\\gilmo\\AppData\\Local\\Temp\\Rtmp6xjOC3\\filec907a7b703d.html
#la función cat es para concatenar y escribir el contenido en el archivo
#eliminar tempora html
#unlink(temp_html)

#install.packages("webshot2") # PARA IMPRMIR COMO IMAGEN EL ARCHIVO
#library(webshot)
#library(webshot2)
#webshot(temp_html,"tabla_imagen2.png", cliprect = "viewport") #(esto no funcionó)

#-----AHORA LO MISMO CON LA TABLA DE CONSTRUCTOS - IMPRIMIR OBJETO CONSTRUCTOS
#creamos tabla con tableGrob
tabla_image_constructos_distancias <- tableGrob(tabla_regular_CONSTRUCTOS_d_distancias2)
tabla_image_constructos_distancias
#class(tabla_image_constructos_distancias)

#CREAMOS ARCHIVO .PNG
imagen_dist_CONSTRUCTOS <- ggsave("Imagen_dist_CONSTRUCTOS.png", plot = tabla_image_constructos_distancias, device = "png", width = 49, height = 10, units = "in")
imagen_dist_CONSTRUCTOS

#otra forma------- CREAR ARCHIVO HTML 
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("webshot")


library(knitr)
library(kableExtra)
library(webshot)

tabla_kable_C_dis <- kable(tabla_regular_CONSTRUCTOS_d_distancias2, "html") %>% kable_styling("striped")
tabla_kable_C_dis # crea html a partir de la tabla de datos, el styling para dar estilo

temp_html_Constructos <- tempfile(fileext = ".html") 
temp_html_Constructos

#cat(tabla_kable_e_dis, file = temp_html) #el temporal html en un directorio c C:\\Users\\gilmo\\AppData\\Local\\Temp\\Rtmp6xjOC3\\filec907a7b703d.html
#la función cat es para concatenar y escribir el contenido en el archivo
#eliminar tempora html
#unlink(temp_html)

#install.packages("webshot2") # PARA IMPRMIR COMO IMAGEN EL ARCHIVO
#library(webshot)
#library(webshot2)
#webshot(temp_html,"tabla_imagen2.png", cliprect = "viewport") #(esto no funcionó)



########################################################







#install.packages("openrepgrid") (he intentado utilizar las funciones de opengrid con lo que tengo y nada)
#library(OpenRepGrid)
#rep_grid_formato <- repgrid(matriz_c_def, rownames = df$Item, description = "Ejemplo de Repertory Grid")
#rep_grid_formato (no reconoce la función repgrid)

#install.packages("OpenRepGrid")
#library(OpenRepGrid)




#desde aqui ya no lo tengo tan claro otras pruebas

# INTENTO SACAR PRIMER FACTOR
#resultado_pca <- prcomp(tabla_c_R, scale. = TRUE)
#primer_factor <- resultado_pca$rotation [,1]
#print("\nprimer factor (componente principal")
#primer_factor

#representación_lineal <- as.matrix(tabla_c_R) %% primer_factor
#plot(representación_lineal, col = "blue", pch = 19, main = "Representación lineal del primer factor")

# INTENTO SACAR SEGUNDO FACTOR
#resultado_pca2 <- prcomp(tabla_c_R, scale. = TRUE)
#segundo_factor <- resultado_pca2$rotation [,2]
#segundo_factor

#representación_lineal2 <- as.matrix(tabla_c_R) %% segundo_factor
#plot(representación_lineal2, col = "blue", pch = 19, main = "Representación lineal del segundo factor")



#------------GRAFICAS, NO MUY CLARO
#num_conglomerados <- 3
#resultado_kmeans <- kmeans(Tabla_e_d, centers = num_conglomerados)
#df_conglomerados <- cbind(Tabla_e_d, conglomerado = as.factor(resultado_kmeans$cluster))
#df_conglomerados
#plot(df_conglomerados[,1:2], col = resultado_kmeans$cluster, pch=19, main = "Gráfico de dispersión con conglomerados", xlab = "variable1", ylab "variable2")
#plot(df_conglomerados[,1:3])

#install.packages("fmsb")
#library(fmsb)
#df_conglomerados
#df_radar <- df_conglomerados[,2:3]
#df_radar_norm <- as.data.frame(scale(df_radar))
#df_radar_norm$cluster <- resultado_kmeans$cluster

#inteto modificar a ver si grabo algo