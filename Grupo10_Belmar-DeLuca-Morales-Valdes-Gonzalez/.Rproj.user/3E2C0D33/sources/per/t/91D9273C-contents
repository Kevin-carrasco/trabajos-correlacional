#Entrega Final Estadistica Correlacional 
#descriptivos correlación #medidas de asosiación v.categóricas


if (!require("pacman")) install.packages("pacman")
if (!require("BiocManager")) install.packages("BiocManager")
pacman::p_load(tidyverse,    # conjunto de paquetes, contiene dplyr y ggplot2
               haven,        # Leer archivo dta
               dplyr,        # para sintaxis
               ggplot2,
               kable,
               kableExtra,
               knitr,        # Render y tablas
               stargazer,    # Tablas
               janitor,      # Tablas y formateo
               crosstable,   # Tablas
               table1,       # Tablas
               gt,
               Publish,      # para IC
               summarytools, # para descriptivos
               sjmisc,       # para descriptivos
               sjPlot,       # para tablas
               corrplot,     # Correlaciones
               rstatix,      # Test estadísticos
               psych,        # para  alfa de cronbach
               labelled,     # Para labels
               broom,        # Tablas
               gginference,  # Visualizacion 
               rempsyc,      # Reporte
               flextable,    # nice table
               scales,
               sjstats,
               ltm,
               ggpubr,       # Gráficos
               ggmosaic, reshape2,rcompanion, lsr)     # Gráficos

options(scipen = 999)        # para desactivar notacion cientifica
rm(list = ls())              # para limpiar el entorno de trabajo (Elimina todos los objetos)


# cargar base y crear subset-----------
casen <- read_sav("input/casen22.sav")

casen2022_subset <- casen %>% 
  dplyr::select(
    v36c, 
    area, 
    e6a_no_asiste, 
    e6a_asiste, 
    y1, 
    s19e
  ) %>%
  mutate(
    nivel_educ = coalesce(e6a_asiste, e6a_no_asiste)
  ) %>%
  rename(
    freq_traf_drog = v36c,
    area_tipo      = area,
    ingresos       = y1,
    acc_med        = s19e
  ) %>%
  dplyr::select(-e6a_no_asiste, -e6a_asiste)

rm(casen)


# limpieza de missings solo sobre variables numéricas originales----------
proc_casen_subset <- casen2022_subset %>%
  mutate(across(
    .cols = c(freq_traf_drog, area_tipo, nivel_educ, ingresos, acc_med),
    ~ na_if(.x, -88)
  )) %>%
  mutate(across(
    .cols = c(freq_traf_drog, area_tipo, nivel_educ, ingresos, acc_med),
    ~ na_if(.x, -99)
  )) %>%
  mutate(across(
    .cols = c(freq_traf_drog, area_tipo, nivel_educ, ingresos, acc_med),
    ~ na_if(.x, -66)
  )) %>%
  mutate(across(
    .cols = c(freq_traf_drog, area_tipo, nivel_educ, ingresos, acc_med),
    .fns = as.numeric
  )) %>%
  # NO eliminamos nivel_educ para no perder casos; sí exigimos ingresos y freq_traf_drog y acc_med
  drop_na(freq_traf_drog, ingresos, acc_med)


# recodificaciones
proc_casen_subset <- proc_casen_subset %>%
  
  mutate(
    area_reco = case_when(
      area_tipo == 1 ~ "Urbana",
      area_tipo == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    freq_traf = case_when(
      freq_traf_drog == 1       ~ "No",
      freq_traf_drog %in% 2:4   ~ "Sí",
      TRUE                       ~ NA_character_
    ),
    freq_traf_numeric = case_when(
      freq_traf_drog == 1       ~ 0,
      freq_traf_drog %in% 2:4   ~ 1,
      TRUE                       ~ NA_real_
    ),
    nivel_educ_reco = case_when(
      nivel_educ == 2  ~ "Sala cuna",
      nivel_educ == 3  ~ "Jardín Infantil",
      nivel_educ == 4  ~ "Prekínder / Kínder",
      nivel_educ == 5  ~ "Educación Especial",
      nivel_educ == 6  ~ "Primaria o preparatoria (Sistema Antiguo)",
      nivel_educ == 7  ~ "Educación Básica",
      nivel_educ == 8  ~ "Humanidades (Sistema Antiguo)",
      nivel_educ == 9  ~ "Educación Media Científico-Humanista",
      nivel_educ == 10 ~ "Técnica, Comercial, Industrial o Normalista (Sistema Antiguo)",
      nivel_educ == 11 ~ "Educación Media Técnica Profesional",
      nivel_educ == 12 ~ "Técnico Nivel Superior",
      nivel_educ == 13 ~ "Profesional",
      nivel_educ == 14 ~ "Magíster",
      nivel_educ == 15 ~ "Doctorado",
      TRUE ~ NA_character_
    ),
    acceso_meds_reco = case_when(
      acc_med == 1 ~ "Sí",
      acc_med == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    acc_med = case_when(
      acc_med == 1 ~ 1,
      acc_med == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  
  #Conversión a factores 
  mutate(
    area_reco = factor(area_reco, levels = c("Urbana", "Rural"), ordered = TRUE),
    freq_traf = factor(freq_traf, levels = c("No", "Sí")),
    nivel_educ_reco = factor(nivel_educ_reco, 
                             levels = c(
                               "Sala cuna",
                               "Jardín Infantil",
                               "Prekínder / Kínder",
                               "Educación Especial",
                               "Primaria o preparatoria (Sistema Antiguo)",
                               "Educación Básica",
                               "Humanidades (Sistema Antiguo)",
                               "Educación Media Científico-Humanista",
                               "Técnica, Comercial, Industrial o Normalista (Sistema Antiguo)",
                               "Educación Media Técnica Profesional",
                               "Técnico Nivel Superior",
                               "Profesional",
                               "Magíster",
                               "Doctorado"
                             ), ordered = TRUE),
    acceso_meds_reco = factor(acceso_meds_reco, levels = c("Sí","No"), ordered = TRUE)
  )



# descriptivos----


#tabla de frecuencias
freq_area <- proc_casen_subset %>%
  filter(!is.na(area_reco)) %>%
  count(area_reco) %>%
  mutate(percent = round(100 * n / sum(n), 2))

freq_traf <- proc_casen_subset %>%
  filter(!is.na(freq_traf)) %>%
  count(freq_traf) %>%
  mutate(percent = round(100 * n / sum(n), 2))

freq_nivel_educ <- proc_casen_subset %>%
  filter(!is.na(nivel_educ_reco)) %>%
  count(nivel_educ_reco) %>%
  mutate(percent = round(100 * n / sum(n), 2))

freq_acc_med <- proc_casen_subset %>%
  filter(!is.na(acceso_meds_reco)) %>%
  count(acceso_meds_reco) %>%
  mutate(percent = round(100 * n / sum(n), 2))

# mostrar tablas 
freq_area %>% kbl(col.names = c("Categoría","Frecuencia","Porcentaje (%)"),
                  caption = "Tipo de Área") %>% kable_classic(full_width = FALSE, html_font = "Times New Roman")

freq_traf %>% kbl(col.names = c("Categoría","Frecuencia","Porcentaje (%)"),
                  caption = "Frecuencia Tráfico de Drogas") %>% kable_classic(full_width = FALSE, html_font = "Times New Roman")

freq_nivel_educ %>% kbl(col.names = c("Categoría","Frecuencia","Porcentaje (%)"),
                        caption = "Nivel Educacional") %>% kable_classic(full_width = FALSE, html_font = "Times New Roman")

freq_acc_med %>% kbl(col.names = c("Categoría","Frecuencia","Porcentaje (%)"),
                     caption = "Acceso a Medicamentos") %>% kable_classic(full_width = FALSE, html_font = "Times New Roman")


ggplot(proc_casen_subset %>% filter(!is.na(area_reco)),
       aes(x = area_reco, fill = area_reco)) +
  geom_bar(position = "dodge2") +
  labs(title = "Frecuencias de área de residencia",
       x = "Área de residencia", 
       y = "Frecuencia", 
       caption = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "red", 
            vjust = 1.5, position = position_dodge(.9)) # agregamos freq de cada barra por grupo


ggplot(proc_casen_subset %>% filter(!is.na(freq_traf)),
       aes(x = freq_traf, fill = freq_traf)) +
  geom_bar(position = "dodge2") +
  labs(title = "Frecuencias de tráfico de drogas presenciado",
       x = "Tráfico de drogas presenciado", 
       y = "Frecuencia", 
       caption = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "black", 
            vjust = 1.5, position = position_dodge(.9)) # agregamos freq de cada barra por grupo

ggplot(proc_casen_subset %>% filter(!is.na(nivel_educ_reco)), 
       aes(x = "", fill = nivel_educ_reco)) +  # Eliminamos la variable en eje X
  geom_bar(position = "dodge") +
  labs(title = "Frecuencias Nivel Educacional", 
       x = NULL,  # Quitamos label del eje X
       y = "Frecuencia", 
       caption = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.") +
  geom_text(aes(label = after_stat(count)), 
            stat = "count", 
            position = position_dodge(0.9),
            vjust = -0.5,
            size = 3) +
  scale_fill_viridis_d() +  # Paleta de colores HD
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Quitamos texto del eje X
        axis.ticks.x = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10))
ggplot(proc_casen_subset %>% filter(!is.na(acceso_meds_reco)),
       aes(x = acceso_meds_reco, fill = acceso_meds_reco)) +
  geom_bar(position = "dodge2") +
  labs(title = "Frecuencias de problemas para la entrega o acceso a medicamentos por su costo",
       x = "Problemas", 
       y = "Frecuencia", 
       caption = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.") +
  geom_text(aes(label = after_stat(count)), stat = "count", colour = "red", 
            vjust = 1.5, position = position_dodge(.9)) # agregamos freq de cada barra por grupo

ggplot(proc_casen_subset %>% filter(!is.na(ingresos)),
       aes(x = ingresos)) +
  geom_density() +
  labs(title = "Distribución de ingresos de ls participantes",
       x = "Ingresos", 
       caption = "Fuente: Elaboración propia en base a Encuesta CASEN 2022.")




#area y frecuencia tráfico. tabla y barplot
tabla_area_traf <- proc_casen_subset %>%
  filter(!is.na(area_reco), !is.na(freq_traf)) %>%
  count(area_reco, freq_traf) %>%
  group_by(area_reco) %>%
  mutate(percent = round(100 * n / sum(n), 2))

tabla_area_traf %>% kbl(col.names = c("Área","Frecuencia tráfico","N","% (por área)"),
                        caption = "Área vs Frecuencia de Tráfico") %>% kable_classic(full_width = FALSE)

ggplot(proc_casen_subset %>% filter(!is.na(area_reco), !is.na(freq_traf)),
       aes(x = area_reco, fill = freq_traf)) +
  geom_bar(position = "fill") +
  labs(title = "Área vs Frecuencia de Tráfico de Drogas", x = "Área", y = "Proporción")

# area y nivel educacional
ggplot(proc_casen_subset %>% filter(!is.na(area_reco), !is.na(nivel_educ_reco)),
       aes(x = area_reco, fill = nivel_educ_reco)) +
  geom_bar(position = "fill") +
  labs(title = "Área vs Nivel Educacional", x = "Área", y = "Proporción") +
  guides(fill = guide_legend(ncol = 2))

# frecuencia tráfico y nivel educacional
tabla_traf_nivel <- proc_casen_subset %>%
  filter(!is.na(freq_traf), !is.na(nivel_educ_reco)) %>%
  count(freq_traf, nivel_educ_reco) %>%
  group_by(freq_traf) %>%
  mutate(percent = round(100 * n / sum(n), 2))

tabla_traf_nivel %>% kbl(col.names = c("Frecuencia tráfico","Nivel educ","N","% (por tráfico)"),
                         caption = "Frecuencia Tráfico vs Nivel Educacional") %>% kable_classic(full_width = FALSE)

#descriptivo ingresos tabla

desc_ingresos <- psych::describe(proc_casen_subset$ingresos)

tabla_ingresos <- desc_ingresos[, c("n", "min", "max", "mean", "sd", "median")]
tabla_ingresos$cv <- tabla_ingresos$sd / tabla_ingresos$mean

tabla_ingresos <- as_tibble(tabla_ingresos)

kbl(
  tabla_ingresos,
  col.names = c(
    "Tamaño muestral",
    "Mínimo",
    "Máximo",
    "Media",
    "Desv. estándar",
    "Mediana",
    "Coef. variación"
  ),
  caption = "Tabla de Descriptivo Ingresos"
) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman")

#correlaciones---------

# Correlación punto biserial entre ingresos y tráfico de drogas H1
biserial_traf <- biserial.cor(
  proc_casen_subset$ingresos,
  proc_casen_subset$freq_traf_numeric,  # Variable numérica 0/1
  use = "complete.obs"
)
print(paste("Biserial ingresos-tráfico:", round(biserial_traf, 4)))
t_test_traf <- t.test(ingresos ~ freq_traf_numeric, data = proc_casen_subset)
print(t_test_traf)

# correlación entre tráfico y área (ambas dicotomicas)H2
tabla_traf_area <- table(
  proc_casen_subset$freq_traf_numeric,
  proc_casen_subset$area_tipo
)

chisq_test <- chisq.test(tabla_traf_area)
chisq_test


phi_value <- cramersV(tabla_traf_area)

phi_value


# Correlación entre tráfico y acceso a medicamentos (ambas dicotomicas)H4
chisq.test(table(proc_casen_subset$freq_traf_numeric,
                 proc_casen_subset$acc_med))

cramerV(table(proc_casen_subset$freq_traf_numeric,
              proc_casen_subset$acc_med))

# Correlación biserial puntual entre tráfico de drogas (dicotomica) y educación (ordinal)H3

cor.tespt(proc_casen_subset$nivel_educ,
         proc_casen_subset$freq_traf_numeric,
         method = "spearman")

#matriz de correlacion----------
vars_cor <- proc_casen_subset %>% 
  dplyr::select(where(is.numeric))

library(sjPlot)
sjPlot::tab_corr(vars_cor, triangle = "lower")
