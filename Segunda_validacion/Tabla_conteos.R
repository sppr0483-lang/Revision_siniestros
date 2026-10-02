##############
#########
#######Conteos de los insumos
######Creación de la tabla de frecuencias
ruta_base <- "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/2da_revision_siniestros/VF"
# Crear carpeta si no existe
#dir_create(destino)

# Archivos
#archivos <- dir_ls(origen, type = "file")
archivos1 <- dir_ls(ruta_base, recurse = TRUE, type = "file")

length(archivos1)
n = list()
for(i in 1:length(archivos1)){
  archivo = archivos1[[i]]
 n[[i]] =  path_file(archivo)
}
n1 = list()
for(i in 1:length(archivos1)){
  archivo = archivos1[[i]]
 n1[[i]] =  path_file(archivo)
}
re = unique(df$arch)
head(re)
head(archivos1)
########
library(stringr)
##Extraer el contratante de lso archivos
resultado <- str_extract(archivos1, "(?<=/VF/)[^/]+/[^/]+")
head(resultado)
###Se crea el data frame definitivo
df1 = data.frame(nombre1 = resultado)
df1$nombre = unlist(n1)
df1$archiv = archivos1
df1
df1 = df1[grepl("\\.pdf$", df$arch),]
head(df1)
kj = which(df1$nombre  %in% re)
df2 = df1[kj,]
df2 = df2[c("nombre1","nombre")]
df2 = df2%>%unique()
Z = df%>%left_join(df2,by = c("arch" = "nombre"))
Tab = as.data.frame.matrix(table(Z$nombre1,Z$Tipo)) 
Tab
########################################
######A continuación se le añade el nombre del archivo
Tab$nombres = rownames(Tab)
####EXPORTACIÓN
#write.xlsx(Tab, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/2da_revision_siniestros/E/Tabla_insumos.xlsx")
