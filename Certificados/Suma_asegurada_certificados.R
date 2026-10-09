library(readxl)
Vigor <- read_excel("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/SINIESTROS_FINAL/Base_GH_30062026/Base_GH_30062026.xlsx", 
    sheet = "Base ")
Insumo_cer <- read_excel("CERTIFICADOS_GH/Insumo_cer.xlsx", 
    col_types = c("text", "text", "date", 
        "date", "numeric", "numeric", "numeric", 
        "text"))
###################################################3
r = unique(Insumo_cer$`No. Póliza`)
##################################################
lista = list()
#sort(names(Vigor))
for(i in 1:length(r)){
  K = Insumo_cer%>%filter(`No. Póliza` == r[i])
  r1 = unique(K$`No. Certificado`)
  K1 = Vigor%>%filter(`POL` == r[[i]], CERT %in% r1)
######################
B1 = K%>%select("No. Póliza","No. Certificado", "Suma asegurada")
B1 = B1%>%unique()
B = K1%>%select("POL","CERT","SA_END","COB")
#unique(K1$COB)
B = B%>%filter(COB =="GH-A.01 Estructura")
#B = B%>%unique()
Val1 = B%>%left_join(B1, by = c("POL" ="No. Póliza","CERT"="No. Certificado"))
Val1$diferencia = Val1$`SA_END`-Val1$`Suma asegurada`
lista[[i]] = Val1
}
######################################
df_final <- do.call(rbind, lista)
