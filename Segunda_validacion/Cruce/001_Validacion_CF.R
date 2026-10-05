library(readxl)
#Se carga la base d esiniestros proporcionada
BASE_HIST <- read_excel("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/ASE_HIST.xlsx", 
    skip = 7)
#Generación de llave
####Se normaliza clave identificadora para cruces de base histórica
###
library(stringi)
BASE_HIST$llave1 = sapply(BASE_HIST$`No.de Siniestro`, function(x){x  |>
  stri_trans_general("Latin-ASCII") |>
  toupper() |>
  gsub("[^A-Z0-9]", "_", x = _) |>
  gsub("_+", "_", x = _) |>
  gsub("^_|_$", "", x = _)
  
})
###Se genera nueva base monto para aquellos valores no ausentes
Monto = BASE_HIST%>%filter(!is.na(BASE_HIST$`Monto pagado\r\nMoneda original`))
sort(unique(Monto$`No.de Siniestro`))
###Auxiliar para verificar el nombre d elos campos de base histórica
names(BASE_HIST)
#########Contabilizar registros
X = as.data.frame.matrix(table(Monto$`No.de Siniestro`,Monto$`No.de Siniestro`))
X = colSums(X)
which(X>1)
###############
######Importación de bases de datos para el cruce
library(readxl)
#####################Base 1 Base de Convenio Finiquito
Concentrado <- read_excel("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Concentrado.xlsx", 
    col_types = c("text", "text", "text", 
        "text", "date", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "text", "text"))
head(Concentrado)
##############################Base 2 Determinación de la pérdida
library(readxl)
Concentrado1 <- read_excel("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Concentrado.xlsx", 
    sheet = "Determinacion perdida", skip = 3)
###################################################
##########################Generacion de reportes 
Concentrado%>%left_join(Monto, by = c("No. de Siniestro"="No.de Siniestro"))
Concentrado$llave1 = sapply(Concentrado$`No. de Siniestro`, function(x){x  |>
  stri_trans_general("Latin-ASCII") |>
  toupper() |>
  gsub("[^A-Z0-9]", "_", x = _) |>
  gsub("_+", "_", x = _) |>
  gsub("^_|_$", "", x = _)
  
})
######################Valores Base histórica
unique(BASE_HIST$`No.de Siniestro`)
unique(Concentrado$llave1)
sort(setdiff(BASE_HIST$llave1,Concentrado$llave1))
sort(setdiff(Concentrado$llave1,BASE_HIST$llave1))
unique(Monto$llave1)
###################################################
###################################################
empresas <- c(
  "INGENIEROS CUEVAS ASOCIADOS, S.C",
  "PIMOSA S.A. DE C.V",
  "CAD & LAN MEXICO, S.A. DE C.V",
  "VOLTS LEASING",
  "CRAZY RECORDS AND PRODUCTIONS",
  "Global Process Agente de Seguros y de Fianzas, S.A. de C.V",
  "CENTRO ESCOLAR LANDAU, S.C",
  "MAQUINARIA CAMES S.A. DE C.V",
  "A TRABAJAR SOLUCIONES DE EMPLEO",
  "CIVIL GPA, S.A.P.I. DE C.V",
  "CSI LEASING MEXICO SA DE CV",
  "JESUS GUILLERMO ALVAREZ ROJO",
  "FINAMOL S DE RL DE CV",
  "PRGX DE MÉXICO S DE RL DE CV",
  "RENDAUTO, S.A. DE C.V",
  "RODRIGO DOMINGUEZ URQUIZO",
  "COMERCIALIZADORA DE TODO GARLLIN",
  "MOMENTUM MARKETING, S.A.S. DE C.V",
  "PHI TOOLS S.A. DE C.V",
  "EMERGING METHANE SOLUTIONS S.A. DE C.V",
  "CONCEPTO LIBRE MEXICANO S DE RL DE C.V",
  "FUNDACION PRO EMPLEO PRODUCTIVO",
  "LUIS CARLOS PINTO VELAZQUEZ",
  "CARLOS ANDRES ARROYO AGUILAR",
  "CHUFANI CONSTRUCTORA, S.A. DE C.V",
  "397 CAP, S.A. DE C.V. SOFOM ENR",
  "ELIZABETH ACOSTA JURADO",
  "Impulsora del Deportivo Necaxa, S.A. de C.V",
  "WORLD WILDLIFE FIND INC",
  "XS PRODUCTIONS, SRL DE CV"
)
library(stringi)

normalizar <- function(x) {
  x %>%
    tolower() %>%
    stri_trans_general("Latin-ASCII") %>%  # quita acentos
    gsub("[^a-z0-9]", "", .)               # deja solo letras y números
}

empresas = normalizar(empresas)
BASE_HIST$llave2 = sapply(BASE_HIST$Contratante, normalizar) 
ZA = BASE_HIST%>%filter(BASE_HIST$llave2 %in% empresas)
ZA1  =unique(ZA$llave1)
###################################################
##################################################
#####Cruce Monto pagado Convenio Finiquito Base histórica
library(openxlsx)
Z1 = Concentrado%>%left_join(Monto, by = c("llave1"="llave1"))
Z1$Proporcionada = ifelse(Z1$llave1 %in% ZA1,1,0)
#write.xlsx(Z1, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Monto.xlsx")
Z1$`Monto pagado\r\nMoneda original`
Z4 = Z1
#####################################3
RE = Z4%>%filter(is.na(`Monto pagado\r\nMoneda original`))
#write.xlsx(RE, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Monto_Ausentes.xlsx")
Z1 = Z1[!is.na(Z1$`Monto pagado\r\nMoneda original`),]
#write.xlsx(Z1, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Montos_comparables.xlsx")
Z1$CORRECT_MONT =ifelse(Z1$`Monto pagado\r\nMoneda original` == Z1$`Monto a indemnizar`,1,0)
##############################################################
###################Segundo reporte
Z2 = Z1%>%filter(CORRECT_MONT ==0) 
#write.xlsx(Z2, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Montos_discrepantes.xlsx")
Z2$discrepancia = abs(Z2$`Monto pagado\r\nMoneda original` - Z2$`Monto a indemnizar`)
Rango = range(Z2$discrepancia)
#write.xlsx(Rango, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Rango_discrepancia.xlsx")
Z2 = Z2%>%arrange(discrepancia)
#write.xlsx(Z2, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Medidicon_discrepancia.xlsx")
WE = Z2[,c("No. de Siniestro" ,"Monto a indemnizar","Monto pagado\r\nMoneda original" ,"discrepancia")]
#write.xlsx(WE, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Reporte_Medidicon_DISCREPANCIA_RESUMIDO.xlsx")
names(Z2)
###################################################
####de Z3 revisar el segundo error y discrepancia de criterios es nacional o moneda original???
JU1 = Z3 = Z2%>%filter(!is.na(`Monto pagado\r\nM.N.`))
JU2 = Z3[,c("Monto a indemnizar" ,"Monto pagado\r\nM.N.")]
#write.xlsx(JU1, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Hallazgo1_Registros_erroneos_siniestros.xlsx")
#write.xlsx(JU2, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Hallazgo1_Registros_erroneos_siniestros2.xlsx")
#############################################################################
Z2%>%arrange(llave1)
Z1[,c("Monto a indemnizar" ,"Monto pagado\r\nMoneda original")]
names(Z1)
