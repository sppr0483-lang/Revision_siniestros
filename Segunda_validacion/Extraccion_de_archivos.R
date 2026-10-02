##################
#####Librerias empleadas
library(fs)
library(dplyr)
#####Cambio de directorio
setwd("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/2da_revision_siniestros")
ruta_base <- "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/2da_revision_siniestros/E"
#############################################
###En el siguiente código se extraen las rutas de los archivos
archivos <- dir_ls(ruta_base, recurse = TRUE, type = "file")
#####Se declaran las siguientes listas para almacenaje
Asegurado = list()
Archivo = list()
Ruta = list()
###########################
###########################
#####Se recorre la lista de archivos
i=0
for (archivo in archivos) {
  i=i+1
  # Ruta relativa respecto al directorio principal
  #rel <- path_rel(archivo, ruta_base)
rel  =archivo
  # Primera carpeta debajo de ruta_base
  carpeta_principal <- strsplit(rel, .Platform$file.sep)[[1]]

  # Nombre actual del archivo
  nombre <- path_file(archivo)

  # Nuevo nombre
  nuevo_nombre <- paste0(carpeta_principal, "_", nombre)

  # Nueva ruta
  #nuevo_path <- path(path_dir(archivo), nuevo_nombre)

  #file.rename(archivo, nuevo_path)
  Asegurado[[i]] = carpeta_principal
  Archivo[[i]] = nombre
  Ruta[[i]]  = archivo  
}
########################
##################Data frame con datos de los archivos
df <- tibble(Asegurado = vector("list", length(Asegurado)))
for(k in seq_along(Asegurado)){
  df$Asegurado[[k]] <- Asegurado[[k]]
}
library(tibble)
df <- tibble(
  Asegurado = sapply(Asegurado, paste, collapse = "; "),
  Ruta = Ruta,
  Archivo = Archivo
)
############################
####################Resultado Sin.xlsx
####Exportación de resultados
#write.xlsx(df,"C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Sin.xlsx")
