library(readxl)
####Se retoma determinación de la pérdida
Concentrado1 <- read_excel("C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Concentrado.xlsx", 
    sheet = "Determinacion perdida", skip = 3)
Concentrado1$llave1 = sapply(Concentrado1$Siniestro, function(x){x  |>
  stri_trans_general("Latin-ASCII") |>
  toupper() |>
  gsub("[^A-Z0-9]", "_", x = _) |>
  gsub("_+", "_", x = _) |>
  gsub("^_|_$", "", x = _)
})
Concentrado1$Proporcionada = ifelse(Concentrado1$llave1 %in% ZA1,1,0)
#####Campos de interés
Determinacion = Concentrado1%>%group_by(Siniestro,`Monto ajustado`,Deducible, `Monto reclamado`, `Pérdida indemnizable`,llave1,Proporcionada)
names(Concentrado1)
#####################Trabajo con Base histórica
AJUSTE = BASE_HIST
library(dplyr)
AJUSTE_SEL <- AJUSTE %>%
  select(
    `No.de Siniestro`,
    `Estimacion inicial`,
    `Aumento de reserva`,
    `Disminución de reserva`,
    `Gastos de ajuste`,
    `Monto salvamentos/recuperaciones (Moneda original)`,
    `Monto deducible`,
    `Monto coaseguro`,
    llave1
  )
AJUSTE_RES <- AJUSTE_SEL %>%
  group_by(`No.de Siniestro`,llave1) %>%
  summarise(
    `Estimacion inicial` = if(all(is.na(`Estimacion inicial`))) 0 else sum(`Estimacion inicial`, na.rm = TRUE),
    `Aumento de reserva` = if(all(is.na(`Aumento de reserva`))) 0 else sum(`Aumento de reserva`, na.rm = TRUE),
    `Disminución de reserva` = if(all(is.na(`Disminución de reserva`))) 0 else sum(`Disminución de reserva`, na.rm = TRUE),
    `Gastos de ajuste` = if(all(is.na(`Gastos de ajuste`))) 0 else sum(`Gastos de ajuste`, na.rm = TRUE),
    `Monto salvamentos/recuperaciones (Moneda original)` =
      if(all(is.na(`Monto salvamentos/recuperaciones (Moneda original)`))) 0
      else sum(`Monto salvamentos/recuperaciones (Moneda original)`, na.rm = TRUE),
    `Monto deducible` = if(all(is.na(`Monto deducible`))) 0 else sum(`Monto deducible`, na.rm = TRUE),
    `Monto coaseguro` = if(all(is.na(`Monto coaseguro`))) 0 else sum(`Monto coaseguro`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ajuste =
      `Estimacion inicial` +
      `Aumento de reserva` +
      `Disminución de reserva` +
      `Gastos de ajuste` +
      `Monto salvamentos/recuperaciones (Moneda original)` -
      `Monto deducible` -
      `Monto coaseguro`,
    ajuste_sn_ded =
      `Estimacion inicial` +
      `Aumento de reserva` -
      `Disminución de reserva` +
      `Gastos de ajuste` -
      `Monto salvamentos/recuperaciones (Moneda original)` 
  )
##################Validacion de la determinacion
Ajustes = AJUSTE_RES%>%left_join(Determinacion , by= c("llave1" = "llave1") )
Ajustes1 = Ajustes[!is.na(Ajustes$`Monto ajustado`),]
Ajustes2 = Ajustes[is.na(Ajustes$`Monto ajustado`),]
############Revision discrepancias
h = setdiff(Ajustes$llave1,Ajustes2$llave1)
Determinacion%>%filter(llave1 %in% h)
Ajustes$Discrepa = ifelse(Ajustes$`Monto ajustado`!=Ajustes$ajuste_sn_ded, 0, 1)
Ajustes$Discrepa2 = ifelse(Ajustes$`Monto ajustado`!=Ajustes$ajuste, 0, 1)
###############################Primer reporte
Report1  = Ajustes[which(Ajustes$Discrepa == 0),c("Monto ajustado","ajuste_sn_ded","ajuste","llave1","Proporcionada")]
Report1$dicrepancia = abs(Report1$ajuste_sn_ded-Report1$`Monto ajustado`)
Report1 = Report1%>%arrange(dicrepancia)
#write.xlsx(Report1, "C:/Users/agarciadeleon/R_Studio/Scripts/Carp1/Validacion_PREG_80/Validaciones/Revision_Siniestros/Hallazgo3_Reporte_ajustes_D.xlsx")
##########################################Segundo reporte
Report2  = Ajustes[which(Ajustes$Discrepa2 == 0),c("Monto ajustado","ajuste_sn_ded","llave1","ajuste")]
AJUSTE$`No.de Siniestro`
AJUSTE$ajuste_DED_COA = AJUSTE$`Estimacion inicial` + AJUSTE$`Aumento de reserva` -AJUSTE$`Disminución de reserva` + AJUSTE$`Gastos de ajuste` + AJUSTE$`Monto salvamentos/recuperaciones (Moneda original)`-AJUSTE$`Monto deducible`-AJUSTE$`Monto coaseguro`
