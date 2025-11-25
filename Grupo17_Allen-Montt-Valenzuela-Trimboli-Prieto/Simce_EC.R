# 1. Cargar librerías necesarias
library(tidyverse) # Para manipulación de datos y carga (readr, dplyr)

# 2. Cargar las bases de datos

# Base A: Resultados de Aprendizaje (SIMCE)
# Contiene: RBD, Dependencia, GSE, Promedios Mate/Lectura

datos_simce <- read_csv2 ("documents\Simce_EC\input\simce2m2024_rbd_preliminar.csv")

# Base B: Indicadores de Desarrollo Personal y Social (IDPS)
# Contiene: RBD, Indicadores de Clima (CC), Autoestima, etc.
datos_idps <- read_csv2("documents\simce_EC\input\idps2M2024_rbd_preliminar.csv")

# 3. Procesamiento de la Base IDPS (Filtro para Hipótesis 3)
datos_clima <- datos_idps %>%
  filter(ind == "CC") %>%    # Filtramos solo el indicador 'Clima de Convivencia'
  select(rbd, prom) %>%      # Seleccionamos solo el ID del colegio y el puntaje
  rename(clima_convivencia = prom)    # Renombramos

# 4. Unión de Bases (Merge)
# Unimos la base SIMCE con la columna de Clima usando el RBD.
base_maestra <- datos_simce %>%
  left_join(datos_clima, by = "rbd")

# 5. Selección y Recodificación
base_proc <- base_maestra %>%
  #Seleccionamos las columnas originales y les damos nombres
  select(rbd,
         prom_mate = prom_mate2m_rbd,   # Nombre nuevo = Nombre original
         prom_lect = prom_lect2m_rbd,
         cod_depe2,
         cod_grupo,
         clima_convivencia
  ) %>%
  
  # 6. Corrección de sintaxis (FILTRAR)
  filter(!is.na(prom_mate) & !is.na(prom_lect)) %>%
  
  # 7. TRANSFORMACIÓN
  mutate(
    # Asegurar que sean numéricos
    prom_mate = as.numeric(prom_mate),
    
    # Recodificar Dependencia
    cod_depe2 = as.numeric(as.character(cod_depe2)),
    dependencia = case_when(
      cod_depe2 %in% c(1, 4) ~ "municipal/público",
      cod_depe2 == 2 ~ "part. subvencionado",
      cod_depe2 == 3 ~ "part. pagado",
      TRUE ~ NA_character_
    ),
    dependencia = factor(dependencia, levels = c("municipal/público", "part. subvencionado", "part. pagado")),
    
    # Recodificar GSE
    gse = factor(cod_grupo,
                 levels = c(1, 2, 3, 4, 5),
                 labels = c("bajo", "medio bajo", "medio", "medio alto", "alto"),
                 ordered = TRUE),
    
    # Asegurar que clima sea numérico
    clima_convivencia = as.numeric(clima_convivencia))

# 8. Guardamos el archivo listo para analizar en la carpeta 'proc' previamente creada
saveRDS(base_proc, "proc/base_final_simce2024.rds")

# Creamos el promedio general SIMCE y una versión numérica del GSE para el análisis.
base_analisis <- base_proc %>%
  mutate(   # Promedio simple entre lectura y matemáticas
    prom_simce = (prom_mate + prom_lect) / 2,
    
    # GSE numérico para correlaciones
    gse_num = as.numeric(gse)) %>%
  
  # Eliminamos casos que no tengan el clima de convivencia
  filter(!is.na(clima_convivencia))

# Cargar librerías para tablas y gráficos
library(sjPlot)
library(ggpubr)

# Caracterización de la Muestra
library(sjPlot)
sjt.xtab(
  base_analisis$dependencia,
  base_analisis$gse,
  show.row.prc = TRUE,
  show.summary = TRUE,
  title = "tabla de contingencia: dependencia administrativa según nivel socioeconómico")




# DISTRIBUCIÓN DE LA VARIABLE DEPENDIENTE (SIMCE)

library(ggplot2)
ggplot(base_analisis, aes(x = prom_simce)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue3", color = "white") +
  geom_density(color = "pink4", size = 1) +
  labs(title = "distribución del rendimiento SIMCE",
       x = "puntaje promedio", y = "densidad") +
  theme_minimal()


# 8.1. ANÁLISIS DESCRIPTIVO
# A) Descriptivos de variables numéricas
tabla_numerica <- base_analisis %>%
  select(prom_simce, clima_convivencia) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, min, max) %>%
  round(2)

tab_df(tabla_numerica,
       title = "Estadísticos desc, de variables numéricas",
       show.rownames = TRUE)

# B) Frecuencias para Variables Categóricas
# Para DEPENDENCIA:
tabla_dep <- base_analisis %>%
  count(dependencia) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1)
  )

tab_df(tabla_dep,
       title = "distribución por dependencia",
       col.header = c("dependencia", "N", "%"))

# Para GSE:
tabla_gse <- base_analisis %>%
  count(gse) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1)
  )

tab_df(tabla_gse,
       title = "distribución por grupo socioeconómico (GSE)",
       col.header = c("GSE", "N", "%"))


# 9. PRUEBA DE HIPÓTESIS 1:
# H1: Part. Pagado > Municipal y Subvencionado

# Medias por grupo
base_analisis %>%
  group_by(dependencia) %>%
  summarise(
    media = mean(prom_simce, na.rm = TRUE),
    desv_est = sd(prom_simce, na.rm = TRUE),
    N = n()
  )

# Visualización (Boxplot)
ggplot(base_analisis, aes(x = dependencia, y = prom_simce, fill = dependencia)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "distribución del puntaje SIMCE según dependencia",
       x = "", y = "promedio SIMCE (lect + mate)") +
  theme_minimal() +
  theme(legend.position = "none")

# 10. PRUEBA DE HIPÓTESIS 2:
# H2: A mayor GSE, mayor puntaje
# Visualización

ggplot(base_analisis, aes(x = gse_num, y = prom_simce)) +
  geom_jitter(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "red4") +
  scale_x_continuous(breaks = 1:5, labels = levels(base_analisis$gse)) +
  labs(title = "correlación entre nivel socioeconómico y SIMCE",
       x = "grupo socioeconómico", y = "puntaje simce") +
  theme_minimal()

# Prueba de Correlación de Spearman
cor_test <- cor.test(base_analisis$gse_num, base_analisis$prom_simce, method = "spearman")
cor_test


# 11. PRUEBA DE HIPÓTESIS 3:
# H3: Clima influye independientemente de la dependencia

# Ajustamos el modelo lineal
# Y = prom_simce
# X1 = clima_convivencia
# X2 = dependencia
modelo_h3 <- lm(prom_simce ~ clima_convivencia + dependencia, data = base_analisis)

# Tabla de resultados
tab_model(modelo_h3,
          show.std = TRUE,
          title = "modelo de regresión lineal múltiple",
          dv.labels = "puntaje simce promedio")

# Base procesada
base_analisis <- base_final_simce2024 %>%
  mutate(prom_simce = (prom_mate + prom_lect) / 2,
         gse_num = as.numeric(gse)) %>%
  filter(!is.na(clima_convivencia))
