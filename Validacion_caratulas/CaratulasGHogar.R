####Importación de librerias
library(readr)
library(readxl)
library(dplyr)
#################Observación previa
########################La suma asegurada que debe considerarse en productos GH es aquella correspondiente a la cobertura priuncipal
####la de mayor duración i.e. 10 años
###############################################Soportes Documentales Póliza maestra
Soporte <- read_excel("Nueva carpeta/Soporte.xlsx", 
                      col_types = c("text", "date", "date", 
                                    "date", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "numeric", "text", "text", "text", 
                                    "text", "text", "text", "numeric", 
                                    "numeric", "text", "text", "text", 
                                    "text", "text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "text", "text"))
################################
#############################Base histórica
library(readxl)
Vigor <- read_excel("SINIESTROS_FINAL/Base_GH_30062026/Base_GH_30062026.xlsx", 
                               sheet = "Base ", col_types = c("numeric", 
                                                              "numeric", "date", "text", "numeric", 
                                                              "numeric", "numeric", "text", "text", 
                                                              "text", "text", "text", "text", "numeric", 
                                                              "text", "numeric", "text", "numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "text", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "numeric", "text", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric"))
##############################################################################################
###########################################################################################
#########################################################################################
#########Validacion de Suma Asegurada
#####################################Selección de parámetros de interés
B1 = Soporte%>%select("No. Póliza", "Suma asegurada")
B1 = B1%>%unique()
###########################################
########################Selección de campos
B = Vigor%>%select("POL","SA_END","COB")
B = B%>%filter(COB =="GH-A.01 Estructura")
############################################
Val1 = B%>%left_join(B1, by = c("POL" ="No. Póliza"))
Val12 = Val1
#names(Val12)
###################################################
#####Validación principal
#########Discrepancia en suma asegurada
Val12 = Val12%>%group_by(`POL`)%>%summarise(sum(`SA_END`), max(`Suma asegurada`))
Val12$diferencia = Val12$`sum(SA_END)`-Val12$`max(\`Suma asegurada\`)`
####################################
#####################Validaciones optativas complementarias
#Polizas = Val12$POL[abs(Val12$diferencia)<100]
#Polizas = Polizas = Polizas[!is.na(Polizas)]
##################################################################################################
#################################################################################################
#################################################################################################
#########Validacion de prima neta
##########Filtrado básico
D1 = Soporte%>%select("No. Póliza", "Prima Neta")
D1 = D1%>%unique()
########################Selección de Campos
D = Vigor%>%select("POL","PT","CERT","SA_END","CVE_PROD")
########################Segmentación por producto
DF = D%>%filter(D$CVE_PROD == 23)
DI = D%>%filter(D$CVE_PROD == 22)
#D = D%>%unique()
####################FOVISSTE
Val2 = DF%>%left_join(D1, by = c("POL"="No. Póliza"))
Val22 = Val2
Val22 = Val22%>%group_by(`POL`)%>%summarise(max(`Prima Neta`), sum(`PT`),certif = n_distinct(CERT))
################Cruce con suma asegurada cobertura principal
Val22 = Val22 %>%left_join(Val12, by = c("POL"="POL"))
#############################Generación de prima neta mas 2 al millar SA
Val22$Prima = Val22$`sum(PT)`+(Val22$`max(\`Suma asegurada\`)`*2/1000)
Val22$diferenciaP = Val22$`max(\`Prima Neta\`)` -Val22$Prima
##############3INFONAVIT
####################FOVISSTE
Val2 = DI%>%left_join(D1, by = c("POL"="No. Póliza"))
Val22I = Val2
Val22I = Val22I%>%group_by(`POL`)%>%summarise(max(`Prima Neta`), sum(`PT`),certif = n_distinct(CERT))
################Cruce con suma asegurada cobertura principal
Val22I = Val22I %>%left_join(Val12, by = c("POL"="POL"))
Val22I$diferenciaP = Val22I$`max(\`Prima Neta\`)` -Val22I$`sum(PT)`


sort(names(Vigor))
