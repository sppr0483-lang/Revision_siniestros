###Etiquetamiento 
###################En el siguiente código
####Se crea una columna "Tipo" que contiene la clase de archivo
df = data.frame(arch = unlist(Archivo))
df$Tipo = 0  
df$Tipo[grepl(
"FINIQUITO",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "FINIQUITO"
df$Tipo[grepl(
"DETERMINACION",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "DETERMINACION"
df$Tipo[grepl(
"DETERMINACION",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "DETERMINACION"

df$Tipo[grepl(
"REQUISICION",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "REQUISICION"

df$Tipo[grepl(
"CONVENIO",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "CONVENIO"

df$Tipo[grepl(
"CHECKLIST",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "CHECKLIST"

df$Tipo[grepl(
"FACTURA",
stri_trans_general(df$arch, "Latin-ASCII"),
ignore.case = TRUE
)] = "FACTURA"
head(df)
