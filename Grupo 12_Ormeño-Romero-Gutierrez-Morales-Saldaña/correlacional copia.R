pacman::p_load(tidyverse, # Manipulacion datos
               sjPlot, # Graficos y tablas
               sjmisc, # Descriptivos
               corrplot, # Correlaciones
               psych,
               dyplr, # Test estadísticos
               kableExtra,
               haven,
               broom,
               haven,
               forcats,
               ggpubr,
               gginference,
               rempsyc,
               flextable,
               ggplot2)


Base_de_datos_EBS_2023 <- read_sav("C:/Users/sebag/OneDrive/Escritorio/Trabajo Correlación FINAL/Base de datos EBS 2023.SAV")

options(scipen = 999)
base_de_datos <- Base_de_datos_EBS_2023

names(Base_de_datos_EBS_2023)
#recodificar variables

Base_recodificada <- Base_de_datos_EBS_2023 %>% select( 
  tnr,
  sg01,
  tramoebs2,
  qaut_casen,
  educ_recat_casen
  )

Base_recodificada %>% mutate(
  across(everything(), ~ na_if(., -99)),
  across(everything(), ~ na_if(., -88)),
  across(everything(), ~ na_if(., -89)),
 )

#calcular porcentaje na con la recodificación

porcentaje_na_real <- sapply(Base_recodificada, function(X) sum(is.na(X)) / length(X) * 100)
print(porcentaje_na_real)

#convertir codigos en NA

Base_preparada <- Base_recodificada %>% mutate(
  across(everything(), ~ na_if(., -99)), #no responde
  across(everything(), ~ na_if(., -88)), #no sabe
  across(everything(), ~ na_if(., -89)) #no aplica
)

porcentaje_na_final <- sapply(Base_preparada, function(x) sum(is.na(x)) / length(x) * 100)
print(porcentaje_na_final)
                            

#cambio de nombres en variables
Base_final <- Base_preparada %>% rename( 
  Tiempo_no_renumerado = tnr,
  Sexo = sg01,
  Edad_tramos = tramoebs2,
  Quintil_ingreso = qaut_casen,
  Nivel_educacional = educ_recat_casen
  )

#quitar NA listwise
Base_final_final <- na.omit(Base_final)

names(Base_final_final)

rm(
  Base_de_datos_EBS_2023,
  Base_preparada,
  Base_recodificada,
  conteo_na,
  porcentaje_na
)
#porcentaje final de NA
porcentaje_final <- sapply(Base_final_final, function(x) sum(is.na(x)) / length(x) * 100)
print(porcentaje_final)

Base_final_final <- Base_final_final %>% 
  mutate( Nivel_educacional = na_if(Nivel_educacional, -77.)) %>% 
  drop_na(Nivel_educacional)


#cambiar minutos a horas variable de trabajo no remunerado 

Base_final_final <- Base_final_final %>% 
  mutate(
    tiempo_no_renumerado_horas = Tiempo_no_renumerado / 60 )


#limite en horas de trabajo no renumerado 

# dejamos fuera los casos insólitos (> 18 horas)

Base_final_final <- Base_final_final %>%
  filter(tiempo_no_renumerado_horas <= 18)

print("Nuevo resumen de la variable TNR:")
summary(Base_final_final$tiempo_no_renumerado_horas)

print(paste("La nueva base de datos cuenta con", nrow(Base_final_final), "observaciones."))

#preparacion analisis /// RECODIFICACIÓN VARIABLES

Base_final_final <- Base_final_final %>%
  mutate(
    #EDAD TRAMOS
    # (haven factor para categorizar
    Edad_tramos = haven::as_factor(Edad_tramos),
    Edad_tramos = fct_relevel(Edad_tramos,
                              "1. 18 a 29 años", 
                              "2. 30 a 44 años", 
                              "3. 45 a 59 años", 
                              "4. 60 A 79 años", 
                              "5. 80 años o más"),
    
    # QUINTIL INGRESO 
    
    Quintil_ingreso = haven::as_factor(Quintil_ingreso),
    Quintil_ingreso = fct_relevel(Quintil_ingreso,
                                  "1. I", "2. II", "3. III", "4. IV", "5. V"),
    
    #  SEXO 
    Sexo = haven::as_factor(Sexo),
    Sexo = fct_relevel(Sexo, "1. Hombre", "2. Mujer")
  )


#categorizar Nivel eduacional

Base_final_final <- Base_final_final %>% 
  mutate(
    Nivel_educacional = haven::as_factor(Nivel_educacional),
    Nivel_educacional = fct_relevel(Nivel_educacional, 
                                    "1. Educación básica",
                                    "2. Educación media",
                                    "3. Educación superior"
                                    )
  )

#verifico 
glimpse(Base_final_final)

tabla_descriptivos <- Base_final_final %>%
  select(Sexo, Edad_tramos, Quintil_ingreso, Nivel_educacional, tiempo_no_renumerado_horas) %>%
  sjmisc::descr(show = c("label", "range", "mean", "sd", "NA.prc", "n")) %>%
  as.data.frame()

# tabla
tabla_plot <- ggtexttable(
  tabla_descriptivos,
  rows = NULL, 
  theme = ttheme("light")
)

# Mostrar tabla como un gráfico
print(tabla_plot)


  #correlaciones

print("Resultado T-Test (Sexo vs TNR):")
t.test(tiempo_no_renumerado_horas ~ Sexo, 
       data = Base_final_final, 
       var.equal = FALSE)


#grafico primera correlacion

datos_resumen <- Base_final_final %>%
  group_by(Sexo) %>%
  summarise(
    Media_Horas = mean(tiempo_no_renumerado_horas, na.rm = TRUE),
    Error_Estandar = sd(tiempo_no_renumerado_horas, na.rm = TRUE) / sqrt(n()) 
  )

# gráfico
ggplot(datos_resumen, aes(x = Sexo, y = Media_Horas, fill = Sexo)) +
  # Barras que representan el promedio
  geom_col(width = 0.6, alpha = 0.8) + 
  geom_errorbar(aes(ymin = Media_Horas - Error_Estandar, 
                    ymax = Media_Horas + Error_Estandar), 
                width = 0.2, size = 1) +

  geom_text(aes(label = round(Media_Horas, 1)), 
            vjust = -1.5, size = 5, fontface = "bold") +
  
  scale_y_continuous(limits = c(0, max(datos_resumen$Media_Horas) + 1)) +
  scale_fill_manual(values = c("1. Hombre" = "#FD8D3C", "2. Mujer" = "#31A354")) +
  
  labs(title = "Diferencia de Medias: Trabajo No Remunerado por Sexo",
       subtitle = "Visualización de los resultados del T-Test (Media + Error Estándar)",
       y = "Promedio de Horas (TNR)",
       x = "Sexo") +
  theme_minimal()


#2. CORRELACION 2 TNR CON VARIABLES ORDINALES


# --- 1. TNR vs. Nivel Educacional ---
print("Correlación Spearman (Educación vs TNR):")
cor.test(as.numeric(Base_final_final$Nivel_educacional), 
         Base_final_final$tiempo_no_renumerado_horas, 
         method = "spearman")

#GRAFICO TNR / NIVEL EDUACIONAL 

# calculo de media y error estandar 
datos_resumen_educ <- Base_final_final %>%
  group_by(Nivel_educacional) %>%
  summarise(
    Media_Horas = mean(tiempo_no_renumerado_horas, na.rm = TRUE),
    Error_Estandar = sd(tiempo_no_renumerado_horas, na.rm = TRUE) / sqrt(n())
  )

# grafico de puntos y lineas
ggplot(datos_resumen_educ, aes(x = Nivel_educacional, y = Media_Horas, group = 1)) +
  
  geom_line(color = "#E31A1C", size = 1) +
  
  geom_point(size = 3, color = "#E31A1C") +
  
  geom_errorbar(aes(ymin = Media_Horas - Error_Estandar, 
                    ymax = Media_Horas + Error_Estandar), 
                width = 0.2, color = "black") +
  
  geom_text(aes(label = round(Media_Horas, 1)), 
            vjust = -1.5, size = 3.5) +
  
  labs(title = "Tendencia del Tiempo No Remunerado por Nivel Educacional",
       subtitle = "Relación entre Educación y promedio de horas de TNR",
       y = "Promedio de Horas (TNR)",
       x = "Nivel Educacional") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#correlacion 3

print("Correlación Spearman (Ingreso vs TNR):")
cor.test(as.numeric(Base_final_final$Quintil_ingreso), 
         Base_final_final$tiempo_no_renumerado_horas, 
         method = "spearman")

#grafico corerlacion 3 tnr ingresos

datos_resumen_ingreso <- Base_final_final %>%
  group_by(Quintil_ingreso) %>%
  summarise(
    Media_Horas = mean(tiempo_no_renumerado_horas, na.rm = TRUE),
    Error_Estandar = sd(tiempo_no_renumerado_horas, na.rm = TRUE) / sqrt(n())
  )

ggplot(datos_resumen_ingreso, aes(x = Quintil_ingreso, y = Media_Horas, group = 1)) +
  
  # linea de tendencia
  geom_line(color = "#542788", size = 1) + 
  
  geom_point(size = 3, color = "#542788") +
  geom_errorbar(aes(ymin = Media_Horas - Error_Estandar, 
                    ymax = Media_Horas + Error_Estandar), 
                width = 0.2, color = "black") +
  
  geom_text(aes(label = round(Media_Horas, 1)), 
            vjust = -1.5, size = 3.5) +
  
  labs(title = "Tendencia de TNR según Nivel Socioeconómico",
       subtitle = "Visualización de la Correlación de Spearman (Medias por Quintil)",
       y = "Promedio de Horas (TNR)",
       x = "Quintil de Ingreso") +
  
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))



#4 correlación tnr / tramos de edad

print("Correlación Spearman (Edad vs TNR):")
cor.test(as.numeric(Base_final_final$Edad_tramos), 
         Base_final_final$tiempo_no_renumerado_horas, 
         method = "spearman")

#grafico tnr tramos de ead

# resumen de datos con media y error estandar
datos_resumen_edad <- Base_final_final %>%
  group_by(Edad_tramos) %>%
  summarise(
    Media_Horas = mean(tiempo_no_renumerado_horas, na.rm = TRUE),
    Error_Estandar = sd(tiempo_no_renumerado_horas, na.rm = TRUE) / sqrt(n())
  )

# Grafico
ggplot(datos_resumen_edad, aes(x = Edad_tramos, y = Media_Horas, group = 1)) +
  
  geom_line(color = "#2C7FB8", size = 1) + 
  
  geom_point(size = 3, color = "#2C7FB8") +
  
  geom_errorbar(aes(ymin = Media_Horas - Error_Estandar, 
                    ymax = Media_Horas + Error_Estandar), 
                width = 0.2, color = "black") +
  
  geom_text(aes(label = round(Media_Horas, 1)), 
            vjust = -1.5, size = 3.5) +
  
  labs(title = "Tendencia del TNR a lo largo del Ciclo Vital",
       subtitle = "Correlación de Spearman: Medias por Tramo de Edad",
       y = "Promedio de Horas (TNR)",
       x = "Tramo de Edad") +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))


#5 chi test sexo ingresos 

#_---------------------------------------------

# 2. Prueba de Hipótesis (Chi-Cuadrado)

#tabla contingencia 

Tabla_Cruzada <- Base_final_final %>%
  count(Quintil_ingreso, Sexo) %>%
  group_by(Sexo) %>%
  mutate(Porcentaje = round(n / sum(n) * 100, 1)) %>%
  pivot_wider(names_from = Sexo, values_from = c(n, Porcentaje), names_glue = "{Sexo}_{.value}") %>%
  select(Quintil_ingreso, `1. Hombre_n`, `1. Hombre_Porcentaje`, `2. Mujer_n`, `2. Mujer_Porcentaje`) %>%
  rename(
    "Quintil" = Quintil_ingreso,
    "Hombres (N)" = `1. Hombre_n`, "Hombres (%)" = `1. Hombre_Porcentaje`,
    "Mujeres (N)" = `2. Mujer_n`, "Mujeres (%)" = `2. Mujer_Porcentaje`
  )

# Mostrar Tabla
kable(Tabla_Cruzada, caption = "Tabla 2. Distribución de Ingresos por Sexo")

#asociación chi cuadrado para ver si hay correlación

# Tabla de contingencia cruda para el calculo ya que con la anterior dio errores

tabla_base <- table(Base_final_final$Quintil_ingreso, Base_final_final$Sexo)

# Chicuadrado
test_chi <- chisq.test(tabla_base)

# resultados de la prueba
print("--- Resultado Chi-Cuadrado ---")
print(test_chi)

#  calculamos V de Cramer
v_cramer <- sqrt(test_chi$statistic / sum(tabla_base))

print(paste("V de Cramer:", round(v_cramer, 3)))

#grafico 

ggplot(Base_final_final, aes(x = Sexo, fill = Quintil_ingreso)) +

  geom_bar(position = "fill", width = 0.6) +
  
  labs(title = "Distribución de Quintiles de Ingreso según Sexo",
       subtitle = "Asociación entre variables categóricas (Chi-Cuadrado)",
       y = "Proporción (%)",
       x = "Sexo",
       fill = "Quintil") +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_minimal() +
  scale_fill_brewer(palette = "Purples")












