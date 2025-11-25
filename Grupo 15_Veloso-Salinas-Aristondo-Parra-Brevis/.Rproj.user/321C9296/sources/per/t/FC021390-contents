if (!require("pacman")) install.packages("pacman") # instalar pacman

pacman::p_load(tidyverse, # Manipulacion datos
               ggplot2, # Gráficos
               sjPlot, # Tablas y gráficos
               ggpubr, # Gráficos
               ggmosaicdplyr, # para manipular datos
               psych, # para analizar datos
               sjmisc,
               haven,
               knitr, # Render y tablas
               kableExtra, # Formateo tablas
               summarytools, # Tablas
               stargazer, # Tablas
               janitor, # Tablas y formateo
               crosstable, # Tablas
               table1,
               broom,
               rempsyc,
               sjstats,
               gginference,
               gtools,
               foreign
               )


            

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

Casenproc1 <- read_dta("input (3)/Casenproc1.dta") #subir base de datos

Casenproc2 <- Casenproc1 #base de datos para trabajar numericamente

Casenproc2 <- Casenproc2[c("Gen","ingresos","NivEduc", "SistPrev","ConcAtencionSM")]

#Revisión
sum(is.na(Casenproc2))
Casenproc2 <- Casenproc2 %>%
  mutate( Gen = ifelse( Gen < 0, NA, Gen))
Casenproc2 <- na.omit(Casenproc2)
table(Casenproc2$ConcAtencionSM)

#Limpieza casos perdidos

##Variable Sistema Previsional
table(Casenproc2$SistPrev)
Casenproc2 <- Casenproc2 %>%
  mutate( SistPrev = ifelse( SistPrev < 0, NA, SistPrev))
table(Casenproc2$SistPrev)

##Variable Ingresos
table(Casenproc2$ingresos)
Casenproc2 <- Casenproc2 %>% 
  mutate(ingresos = ifelse(ingresos < 0, NA, ingresos))

##Variable Concurrencia a atención por SM
Casenproc2 <- Casenproc2 %>% 
  mutate(ConcAtencionSM = ifelse(ConcAtencionSM < 0, NA, ConcAtencionSM))



#Revisión
table(Casenproc2$ingresos)
table(Casenproc2$NivEduc)          
table(Casenproc2$SistPrev)
table(Casenproc2$Gen)
table(Casenproc2$ConcAtencionSM)
sum(is.na(Casenproc2))
summary(Casenproc2$ConcAtencionSM)
Casenproc2 <- na.omit(Casenproc2)
summary(Casenproc2$ConcAtencionSM)

sum(is.na(Casenproc2))

#Procesamiento Variables numericas

Casenproc3 <- Casenproc2 #copiar base de datos para respaldar y codificar

Casenproc2 <- na.omit(Casenproc2)

#Recodificación

##Variable Genero
Casenproc3$Gen <- recode(Casenproc3$Gen, '1' = 'Masculino', '2' = 'Femenino', '3' = 'Transmasculino', '4' = 'Transfemenino', '5' = 'No binario', '6' = 'Otro genero')
Casenproc3$Gen <- recode(Casenproc3$Gen, '1. Masculino' = 'Masculino', '2. Femenino' = 'Femenino', '3. Transmasculino' = 'Transmasc', '4. Transfemenino' = 'Transfem', '5. No binario' = 'No binario', '6. Otro genero' = 'Otro genero')


##Variable Ingresos
summary(Casenproc3$ingresos)

Casenproc3 <- Casenproc3 %>%
mutate(Casenproc3$ingresos, case_when(
    ingresos < 1 ~ '01. No percibe ingresos',
      ingresos < 500001 ~ '02. 0 a 500000',
      ingresos > 500001 & ingresos < 1000001 ~ '03. 500.000 a 1.000.000',
      ingresos > 1000001 & ingresos < 2000001 ~ '04. 1.000.000 a 2.000.000', 
      ingresos > 2000001 & ingresos < 3000001 ~ '05. 2.000.000 a 3.000.000', 
      ingresos > 3000001 & ingresos < 4000001 ~ '06. 3.000.000 a 4.000.000',
      ingresos > 4000001 & ingresos < 5000001 ~ '07. 4.000.000 a 5.000.000', 
      ingresos > 5000001 & ingresos < 10000001 ~ '08. 5.000.000 a 10.000.000', 
      ingresos > 10000001 & ingresos < 15000001 ~ '09. 10.000.000 a 15.000.000',
      ingresos > 15000001 & ingresos < 20000001 ~ '10. 15.000.000 a 20.000.000',
      ingresos > 20000001 & ingresos < 25000001 ~ '11. 20.000.000 a 25.000.000'))

Casenproc3 <- Casenproc3[c("NivEduc" , "SistPrev", "Gen", "case_when(...)", "ConcAtencionSM" )]
Casenproc3 <- Casenproc3 %>%
  rename(Ingresos = `case_when(...)`)
table(Casenproc3$Ingresos)


##Variable Nivel educacional alcanzado
Casenproc3 <- Casenproc3 %>% 
  mutate(Casenproc3$NivEduc, case_when(
    NivEduc == 1 ~ '01. No Asistió',
    NivEduc == 2 ~ '02. Sala Cuna',
    NivEduc == 3 ~ '03. Jardin Infantil',
    NivEduc == 4 ~ '04. Prekinder/Kinder',
    NivEduc == 5 ~ '05. Ed Especial',
    NivEduc == 6 ~ '06. Primaria Sist Antiguo',
    NivEduc == 7 ~ '07. Ed Basica',
    NivEduc == 8 ~ '08. Humanidades Sist Antiguo',
    NivEduc == 9 ~ '09. Ed Media Cientifico Humanista',
    NivEduc == 10 ~ '10. Tecnica, Comercial, Industrial o Normalista Sist Antiguo',
    NivEduc == 11 ~ '11. Ed Media Tecnico Profesional',
    NivEduc == 12 ~ '12. Tecnico Superior',
    NivEduc == 13 ~ '13. Profesional',
    NivEduc == 14 ~ '14. Magister o Maestria',
    NivEduc == 15 ~ '15. Doctorado'
  ))
Casenproc3 <- Casenproc3[c("SistPrev", "Gen", "Ingresos", "case_when(...)", "ConcAtencionSM" )]
Casenproc3 <- Casenproc3 %>%
  rename(NivEduc = 'case_when(...)')
table(Casenproc3$NivEduc)


##Variable Sist. Previsional
Casenproc3 <- Casenproc3 %>%
  mutate(Casenproc3$SistPrev, case_when(
    SistPrev == 1 ~ 'Fonasa',
    SistPrev == 2 ~ 'Isapre',
    SistPrev == 3 ~ 'FFAA y del Orden',
    SistPrev == 4 ~ 'Ninguno/Particular',
    SistPrev == 5 ~ 'Otro Sistema'
  ))
Casenproc3 <- Casenproc3[c("Gen", "Ingresos", "NivEduc", "case_when(...)", "ConcAtencionSM")]
Casenproc3 <- Casenproc3 %>%
  rename(SistPrev = 'case_when(...)')
table(Casenproc3$SistPrev)

Casenproc3 <- Casenproc3[c("Gen","Ingresos","NivEduc", "SistPrev","ConcAtencionSM"  )]
summary(Casenproc3)
summary(Casenproc2)
colnames(Casenproc3)

#Ultima revisión de casos perdidos
sum(is.na(Casenproc3))
summary(Casenproc3$ConcAtencionSM)
Casenproc3 <- na.omit(Casenproc3)
summary(Casenproc3$ConcAtencionSM)

sum(is.na(Casenproc3))

#Tablas

write.dta(Casenproc3, "Casen_codificada.dta")

##Tablas descriptivas Univariadas
summary(Casenproc2$Gen) #Genero
summary(Casenproc2$NivEduc) #Nivel educacional
summary(Casenproc3$ConcAtencionSM) #Concurrencia
summary(Casenproc2$SistPrev) #Sist PRevisional
summary(Casenproc2$ingresos) #ingresos

##Tablas de frecuencia
###Variable Concurrencia atención salud mental
sjmisc::frq(Casenproc3$ConcAtencionSM) %>%
  kable()
###Variable Ingresos
sjmisc::frq(Casenproc3$Ingresos) %>%
  kable()
###Variable Sistema previsional
sjmisc::frq(Casenproc3$SistPrev) %>%
  kable() 
###Variable Genero
sjmisc::frq(Casenproc3$Gen) %>%
  kable() 
###Variable Nivel educacional
sjmisc::frq(Casenproc3$NivEduc) %>%
  kable() 

##Tablas descriptivas Bivariadas (Con variable Concurrencia a Atención SM)
###Variable Ingresos
sjPlot::sjt.xtab(var.row = Casenproc3$ConcAtencionSM, var.col = Casenproc3$Ingresos, 
                 show.summary = F, emph.total = T)
###Variable Sistema previsional
sjPlot::sjt.xtab(var.row = Casenproc3$ConcAtencionSM, var.col = Casenproc3$SistPrev, 
                 show.summary = F, emph.total = T)
###Variable Genero
sjPlot::sjt.xtab(var.row = Casenproc3$ConcAtencionSM, var.col = Casenproc3$Gen, 
                 show.summary = F, emph.total = T)
###Variable Nivel Educacional
sjPlot::sjt.xtab(var.row = Casenproc3$ConcAtencionSM, var.col = Casenproc3$NivEduc, 
                 show.summary = F, emph.total = T)
##Correlacionar

#Variable Ingresos (pearson)
coringconc <- cor.test( x = Casenproc2$ingresos,
                        y = Casenproc2$ConcAtencionSM,
                        method = 'pearson',
                        use = 'complete.obs')

stats.table <- tidy(coringconc)

stats.table %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    statistic = round(statistic, 2),
    ic_95 = paste0("[", round(conf.low, 2), ",", round(conf.high, 2), "]"),
    stars = gtools::stars.pval(p.value),
    p_value = case_when(
      p.value < 0.05 & p.value > 0.01 ~ "< 0.05",
      p.value < 0.01 & p.value > 0.001 ~ "< 0.01",
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ ""
    ),
    p_value = paste0(p_value, stars)
  ) %>%
  dplyr::select(estimate, statistic, p_value, parameter, method, alternative, ic_95) %>%
  kableExtra::kable(
    col.names = c("Estimación", "t", "p-value", "df", "Método", "Alternativa", "95% IC"),
    booktabs = T
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = T,
    latex_options = "hold_position",
    position = "center"
  )

###Variable Sistema Previsional
corsist <- chisq.test(table(Casenproc3$SistPrev, Casenproc3$ConcAtencionSM))
corsist1 <- tidy(corsist, conf_int = T )
nice_table(corsist1)

###Variable Genero
corGen <- chisq.test(table(Casenproc3$Gen, Casenproc3$ConcAtencionSM))
corGen1 <- tidy(corGen, conf_int = T)
nice_table(corGen1)

##Variable Nivel educacional
cornived <- chisq.test(table(Casenproc3$NivEduc, Casenproc3$ConcAtencionSM))
cornived1 <- tidy(cornived, conf_int = T )
nice_table(cornived1)


sjPlot::sjt.xtab(var.row = Casenproc3$ConcAtencionSM, var.col = Casenproc3$Gen, 
                 show.summary = F, emph.total = T)

descr(Casenproc2$Ingresos)


#--------------------------Fin----------------------------------------

