# http://www.gesetze-im-internet.de/estg/__32a.html

library(tidyverse)

NettoRente <- function(BruttoRente, 
                       Grundfreibetrag = 9408, Zone2 = 14532, 
                       GKV = 14.6, ZusatzB = 1.1, PV = 3.05) {
  
  JahresBruttoRente = BruttoRente*12
  WerbungKP = 102
  SonderausP = 36
  
  Vorsorgeaufwendungen = (GKV/2 + ZusatzB/2 + PV)/100*JahresBruttoRente
  zvE = (JahresBruttoRente - Vorsorgeaufwendungen - WerbungKP - SonderausP) %>% floor()
  y = (zvE-Grundfreibetrag)/10000
  z = (zvE-Zone2)/10000
  
  Einkommenssteuer = ifelse(zvE <= Grundfreibetrag, 0,# 1. Zone
                            ifelse(zvE > Grundfreibetrag & zvE <= Zone2,
                                   (972.87*y+400)*y, # 2. Zone
                                   (212.02*z+2397)*z+972.79) # 3. Zone
  )
  
  JahresNettoRente = JahresBruttoRente - Vorsorgeaufwendungen - Einkommenssteuer
  NettoRente = round(JahresNettoRente/12,2)
  NettoRente
}

RenteEKS <- function(BruttoRente, 
                       Grundfreibetrag = 9408, Zone2 = 14532, 
                       GKV = 0, ZusatzB = 0, PV = 0) {
  
  JahresBruttoRente = BruttoRente*12
  WerbungKP = 102
  SonderausP = 36
  
  Vorsorgeaufwendungen = (GKV/2 + ZusatzB/2 + PV)/100*JahresBruttoRente
  zvE = (JahresBruttoRente - Vorsorgeaufwendungen - WerbungKP - SonderausP) %>% floor()
  y = (zvE-Grundfreibetrag)/10000
  z = (zvE-Zone2)/10000
  
  Einkommenssteuer = ifelse(zvE <= Grundfreibetrag, 0,# 1. Zone
                            ifelse(zvE > Grundfreibetrag & zvE <= Zone2,
                                   (972.87*y+400)*y, # 2. Zone
                                   (212.02*z+2397)*z+972.79) # 3. Zone
  )
  
  JahresNettoRente = JahresBruttoRente - Vorsorgeaufwendungen - Einkommenssteuer
  NettoRente = round(JahresNettoRente/12,2)
  NettoRente
}