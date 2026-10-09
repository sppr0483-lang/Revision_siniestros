########################Auditoría interna
lista1 = list()
Insumo_cer$`Prima Neta`=as.numeric(Insumo_cer$`Prima Neta`)
for(i in 1:length(r)){
  K = Insumo_cer%>%filter(`No. Póliza` == r[i])
  r1 = unique(K$`No. Certificado`)
  K1 = Vigor%>%filter(`POL` == r[[i]], CERT %in% r1)
######################
B1 = K%>%select("No. Póliza","No. Certificado", "Prima Neta")
B1 = B1%>%unique()
B = K1%>%select("POL","CERT","PT","COB")
#############################################################
##############################################
A = K1%>%select("POL","CERT","SA_END","COB")
#unique(K1$COB)
A = A%>%filter(COB =="GH-A.01 Estructura")
Val1 = A%>%left_join(B1, by = c("POL" ="No. Póliza","CERT"="No. Certificado"))
####################################
#unique(K1$Cobertura)
Val2 = B%>%left_join(B1, by = c("POL"="No. Póliza", "CERT" = "No. Certificado"))
Val22 = Val2#%>%unique()
Val22 = Val22%>%group_by(`POL`,`CERT`)%>%summarise(max(`Prima Neta`), sum(`PT`),certif = n_distinct(CERT))
Val22 = Val22 %>%left_join(Val1, by = c("POL"="POL", "CERT"="CERT"))
Val22$Prima = Val22$`sum(PT)`+(Val22$SA_END*2/1000)
Val22$diferenciaP = Val22$`max(\`Prima Neta\`)` -Val22$Prima
lista1[[i]] = Val22
}
####################################################################
df_final1 <- do.call(rbind, lista1)
