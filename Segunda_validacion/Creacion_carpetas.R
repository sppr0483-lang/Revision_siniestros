#############
####Cruzador
##############
###El siguiente código mueve los archivos al entorno definitivo
###Se recibio la carpeta original 2 y se mueven los archivos
##segun Tipo a carpeta "E" en carpetas nomradas segun los tipos
errores <- tibble()
AS <- unique(trimws(df$Tipo))
for(i in seq_along(AS)) {
   destino <- file.path(
    "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/2da_revision_siniestros/E",
    AS[i]
  )
  dir_create(destino, recurse = TRUE)
}
#####Esta carpeta general E es la que permitirá 
