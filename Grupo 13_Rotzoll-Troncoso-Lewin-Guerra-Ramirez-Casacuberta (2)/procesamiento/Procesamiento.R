# -------------------------------------------------------------------------
# 1. Cargar librerías ------------------------------------------------------
# -------------------------------------------------------------------------

pacman::p_load(
  dplyr, sjPlot, sjmisc, kableExtra, psych, broom, tidyr, rempsyc,
  sjstats, gginference, ggplot2, gtsummary, xfun, scales, glue, flextable
)

options(scipen = 999)
rm(list = ls())

# -------------------------------------------------------------------------
# 2. Abrir base de datos ---------------------------------------------------
# -------------------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

# -------------------------------------------------------------------------
# 3. Filtrar ola 2023 y seleccionar variables -----------------------------
# -------------------------------------------------------------------------

Datos_ELSOC <- elsoc_long_2016_2023 %>%
  filter(ola == 7) %>%
  select(
    idencuesta,
    m0_sexo,
    m13,
    depresion_1 = s11_01,
    depresion_2 = s11_02,
    depresion_3 = s11_03,
    depresion_4 = s11_04,
    depresion_5 = s11_05,
    depresion_6 = s11_06,
    depresion_7 = s11_07,
    depresion_8 = s11_09
  )

# -------------------------------------------------------------------------
# 4. Recodificación de variables ------------------------------------------
# -------------------------------------------------------------------------

Datos_ELSOC <- Datos_ELSOC %>%
  # 4.1 Recodificar ítems de depresión (1–5 → 0–4)
  mutate(across(
    starts_with("depresion"),
    ~ case_when(
      .x %in% 1:5 ~ .x - 1,
      .x %in% c(-999, -888, -666) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )) %>%
  
  # 4.2 Recodificar género
  mutate(
    genero = case_when(
      m0_sexo == 1 ~ "Hombre",
      m0_sexo == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),
    genero = factor(genero, levels = c("Hombre", "Mujer"))
  ) %>%
  
  # 4.3 Limpiar ingreso m13 (solo reemplazar códigos de error)
  mutate(
    m13 = ifelse(m13 %in% c(-999, -888, -666), NA, m13)
  )

# -------------------------------------------------------------------------
# 5. Análisis psicométrico de depresión -----------------------------------
# -------------------------------------------------------------------------

variables_depresion <- Datos_ELSOC %>% 
  select(starts_with("depresion"))

# Promedios por ítem
promedios_dep <- variables_depresion %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
promedios_dep

# Correlaciones
matriz_cor <- cor(variables_depresion, use = "pairwise.complete.obs")
matriz_cor

sjPlot::tab_corr(
  variables_depresion,
  na.deletion = "pairwise",
  triangle = "lower"
)

# Alpha de Cronbach
alpha_dep <- psych::alpha(variables_depresion)
alpha_dep

# -------------------------------------------------------------------------
# 6. Crear escala de depresión (rangos fijos) ------------------------------
# -------------------------------------------------------------------------

Datos_ELSOC <- Datos_ELSOC %>%
  mutate(
    # 1. Escala continua: promedio de los ítems
    depresion = rowMeans(
      select(., starts_with("depresion")),
      na.rm = TRUE
    ),
    
    # 2. Escala ordinal (4 niveles)
    depresion_ordinal = case_when(
      depresion >= 0 & depresion < 1 ~ "Bajo",
      depresion >= 1 & depresion < 2 ~ "Medio bajo",
      depresion >= 2 & depresion < 3 ~ "Medio alto",
      depresion >= 3 & depresion <= 4 ~ "Alto",
      TRUE ~ NA_character_
    ),
    
    # 3. Convertir a factor ordenado SIN generar niveles extra
    depresion_ordinal = factor(
      depresion_ordinal,
      levels = c("Bajo", "Medio bajo", "Medio alto", "Alto"),
      ordered = TRUE
    ),
    
    # 4. Eliminar niveles fantasma como "Medio"
    depresion_ordinal = droplevels(depresion_ordinal)
  )

# -------------------------------------------------------------------------
# 7. Quintiles de ingreso                  --------------------------------
# -------------------------------------------------------------------------
Datos_ELSOC <- Datos_ELSOC %>%
  mutate(
    # Reemplazar códigos de no respuesta por NA
    m13 = ifelse(m13 %in% c(-666, -888, -999), NA_real_, m13),
    
    # Crear quintiles
    m13_quintil = ntile(m13, 5),
    m13_quintil = factor(
      m13_quintil,
      levels = 1:5,
      labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)"),
      ordered = TRUE
    )
  )
# -------------------------------------------------------------------------
# 8. Mostrar cortes reales de quintiles -----------------------------------
# -------------------------------------------------------------------------

quintiles_m13 <- quantile(Datos_ELSOC$m13, probs = seq(0, 1, 0.2), na.rm = TRUE)
quintiles_m13