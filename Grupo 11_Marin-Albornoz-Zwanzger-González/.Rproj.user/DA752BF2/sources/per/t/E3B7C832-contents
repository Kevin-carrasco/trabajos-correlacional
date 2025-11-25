#Preparacion de datos


# Librerias

install.packages("pacman")

pacman::p_load(dplyr, # para sintaxis
               Publish,# para IC)       
               gginference, # visualización 
               rempsyc, # reporte
               kableExtra, # tablas
               broom, # varios
               flextable, # nice table
               rempsyc,# tablas
               tidyverse, # manipulación datos
               sjPlot, # gráficos
               rstatix, # test estadísticos
               labelled,# para labels
               sjmisc, # descriptivos
               corrplot, # correlaciones
               psych, # test estadísticos 
               sjstats, # tamaños de efecto
               gginference,# visualización
               gtools) #tablas

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


load("ELSOC_Long_2016_2023.RData")


elsoc_subset <- elsoc_long_2016_2023 %>% filter(ola == 2)


elsoc_subset <- elsoc_subset[, c("r13_ideol_01", "m0_edad", "r16", "m0_sexo")]


library(dplyr)

elsoc_subset <- elsoc_subset %>%
  mutate(across(everything(),
                ~ replace(.x, .x %in% c(-999, -888, -777, -666), NA)))

frq(elsoc_subset)







#--------Analisis Descriptivo (UNIVARIADO)




#---Tablas de frecuencia 


#--1.1 Ideologia y confianza


# Ideología
frq(elsoc_subset$r13_ideol_01)

# Confianza en inmigrantes
frq(elsoc_subset$r16)  

#----Graficos

plot_frq(elsoc_subset$r13_ideol_01)
plot_frq(elsoc_subset$r16)




#----1.2 Edad (continua)

descr(elsoc_subset$m0_edad) 

#-----Gráfico (histograma + densidad):


ggplot(elsoc_subset, aes(x = m0_edad)) +
  geom_histogram(bins = 30) +
  labs(x = "Edad", y = "Frecuencia")


Publish::ci.mean(elsoc_subset$m0_edad) #95 de confianza



#------1.3 Sexo (dicotomica)
frq(elsoc_subset$m0_sexo)

#------Gráfico
plot_frq(elsoc_subset$m0_sexo)






#----Análisis descriptivo bivariado


#-----1.1 Confianza en inmigrantes × ideología

plot_xtab(elsoc_subset$r13_ideol_01,
          elsoc_subset$r16,
          margin = "row")  









#-----Análisis estadístico bivariado





#----1.1 Ideología  x Confianza 

test_chi <- chisq.test(elsoc_subset$r16, elsoc_subset$r13_ideol_01)
print(test_chi)


#--Tamano del efecto (V de Cramer)


library(rstatix)
cramer_v(elsoc_subset$r16, elsoc_subset$r13_ideol_01)




#--------1.2 Edad  x Confianza


cor_test_edad_conf <- cor.test(elsoc_subset$m0_edad, 
                               as.numeric(elsoc_subset$r16), 
                               method = "spearman")
print(cor_test_edad_conf)






#--------1.3 Sexo x confianza


# 1. Convertir confianza a número

elsoc_subset$r16_num <- as.numeric(elsoc_subset$r16)


# 2. Prueba T de Student 

t_test_result <- t.test(r16_num ~ m0_sexo, 
                        data = elsoc_subset, 
                        var.equal = FALSE) 
print(t_test_result)

# 3. Tamaño del Efecto (Cohen's d)

elsoc_subset %>% 
  cohens_d(r16_num ~ m0_sexo, var.equal = FALSE)
