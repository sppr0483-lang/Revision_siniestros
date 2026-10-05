Fecha_ocurrido = Concentrado%>%group_by(`No. de Siniestro`, Asegurado, `Fecha ocurrido`,llave1)
library(modeest)
Asegura = Asegura%>%group_by(`No.de Siniestro`, Asegurado, `Fecha Ocurrido`,llave1,Proporcionada)%>%summarise(mfv(`Asegurado`))

K3  =Fecha_ocurrido%>%left_join(Asegura, by= c("llave1" = "llave1") )
K3$Discrepa = ifelse(K3$`Fecha ocurrido` != K3$`Fecha Ocurrido`, 0, 1)
Report  = K3[which(K3$Discrepa == 0),c("No.de Siniestro","Fecha ocurrido","Fecha Ocurrido")]
#write.xlsx(Report, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Hallazgo2_Registros_erroneos_Fecha_ocurrido.xlsx")
Concentrado
Asegura$`Fecha Ocurrido`
