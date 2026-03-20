pacman::p_load(readxl,writexl,
               tidyverse,
               janitor,
               gt,
               scales,
               stringr,
               responsePatterns,
               ggplot2,
               patchwork,
               knitr)

merge_raw_data <- read_excel(here::here("data/merge_raw_data.xlsx"))|>
  clean_names()

data <- merge_raw_data

rm(merge_raw_data)

data <- data %>%
  mutate(
    education_recoded = case_when(
      # 1. Media incompleta
      str_detect(tolower(educacion), "media incompleta") ~ 1,
      
      # 2. Media completa (incluye técnico nivel medio)
      str_detect(tolower(educacion), "media completa") |
        str_detect(tolower(educacion), "enseñanza media completa") |
        str_detect(tolower(educacion), "bachiller") |
        str_detect(tolower(educacion), "básica completa") |
        str_detect(tolower(educacion), "educación básica completa") |
        str_detect(tolower(educacion), "técnico nivel medio") |
        str_detect(tolower(educacion), "tecnico nivel medio") ~ 2,
      
      # 3. Técnico superior (excluye técnico nivel medio)
      (str_detect(tolower(educacion), "tecnico") |
         str_detect(tolower(educacion), "técnico")) &
        !str_detect(tolower(educacion), "nivel medio") ~ 3,
      
      # 4. Universitaria / Profesional (completa o incompleta)
      str_detect(tolower(educacion), "universit") |
        str_detect(tolower(educacion), "educación superior") |
        str_detect(tolower(educacion), "superior completa") |
        str_detect(tolower(educacion), "superior incompleta") |
        str_detect(tolower(educacion), "profesional") ~ 3,
      
      # 5. Posgrado
      str_detect(tolower(educacion), "magister") |
        str_detect(tolower(educacion), "máster") |
        str_detect(tolower(educacion), "master") |
        str_detect(tolower(educacion), "doctor") |
        str_detect(tolower(educacion), "postgrado") |
        str_detect(tolower(educacion), "posgrado") ~ 3,
      
      TRUE ~ NA_real_
    ),
    
    # Convertir a factor con etiquetas (opcional)
    education_recoded = factor(
      education_recoded,
      levels = 1:3,
      labels = c(
        "Media incompleta",
        "Media completa",
        "Educación superior"
      )
    )
  )

data <- data %>%
  mutate(
    comuna_rm = case_when(
      str_detect(tolower(comuna), "stgo") |
        str_detect(tolower(comuna), "santiago") |
        str_detect(tolower(comuna), "rm") |
        str_detect(tolower(comuna), "metropolitana") ~ 1,
      TRUE ~ 0
    ),
    comuna_rm = factor(comuna_rm, levels = c(0,1),
                       labels = c("Región", "RM"))
  )

# Transformar edad a numérica
data$edad_num <- as.numeric(data$edad)

# Crear variable de tramos
data$edad_tramo <- cut(
  data$edad_num,
  breaks = c(-Inf, 30, 45, 65),
  labels = c("18–30", "31–45", "46–65"),
  right = TRUE
)

# Crear variable dicotómica como código numérico 1–4
data$edad_tramo_num <- as.numeric(data$edad_tramo)

# Verificar
table(data$edad_tramo, data$edad_tramo_num)


# Crear columnas de strike

data <- data %>%
  mutate(across(
    c(comun_atencion_check_1, comun_atencion_check_2,
      sae_atencion_check_1, sae_atencion_check_2,
      fonasa_atencion_check_1, fonasa_atencion_check_2),
    ~ if_else(.x == 1, 0, 1)
  ))

data <- data %>%
  mutate(
    sum_atencion_check =  
      comun_atencion_check_1 +
      comun_atencion_check_2 +
      sae_atencion_check_1 +
      sae_atencion_check_2 +
      fonasa_atencion_check_1 +
      fonasa_atencion_check_2,
    strike_double_atencion = if_else(sum_atencion_check >= 2, 1, 0),
    strike_time = ifelse(time_in_seconds < 1200 | time_in_seconds > 5400, 1, 0),
    #Varianza intrasujeto
    var_sae_pre  = var(c_across(starts_with("sae_pre"))),
    var_sae_pos  = var(c_across(starts_with("sae_pos"))),
    var_fon_pre  = var(c_across(starts_with("fonasa_pre"))),
    var_fon_pos  = var(c_across(starts_with("fonasa_pos"))),
    # Nuevo strike_variance
    strike_variance = if_else(
      var_sae_pre < 0.15 |
        var_sae_pos < 0.15 |
        var_fon_pre < 0.15 |
        var_fon_pos < 0.15,
      1, 0),
    strike_one_atencion = case_when(
      # caso 1: exactamente un 1 y está en comun → asignar 1
      sum_atencion_check == 1 & (comun_atencion_check_1 == 1 | comun_atencion_check_2 == 1) ~ 1,
      
      # caso 2: exactamente un 1 y está en sae o fonasa → asignar 2
      sum_atencion_check == 1 & (sae_atencion_check_1 == 1 | sae_atencion_check_2 == 1 |
                           fonasa_atencion_check_1 == 1 | fonasa_atencion_check_2 == 1) ~ 2,
      
      # caso 3: más de un 1 o todos 0 → asignar 0
      TRUE ~ 0),
    total_strike = strike_double_atencion + strike_time + strike_variance + strike_one_atencion
      ) %>%
  mutate(
    # Criterio final de eliminación:
    # 1) Eliminados automáticamente por strike_double_atencion = 1
    # 2) Eliminados si tienen 2 o más strikes (pero excluyendo strike_double_atencion para este conteo)
    strike_count_no_double = strike_time + strike_variance + 
      if_else(strike_one_atencion %in% c(1,2), 1, 0),
    
    eliminado = case_when(
      strike_double_atencion == 1 ~ 1,
      strike_count_no_double >= 2 ~ 1,
      TRUE ~ 0
    )
  )

data <- data |>
  mutate(
    genero = ifelse(genero == "m", "Mujer", "Hombre"),
    genero = factor(genero, levels = c("Hombre", "Mujer"))
  )

# Create variable of informed trust
data <- data %>%
  mutate(
    z_fonasa_comp_obj_pos = as.numeric(scale(fonasa_comp_objetiva_pos))
  )

trust_fonasa_pos <- c(
  "fonasa_conf_belief_funcional_pos",
  "fonasa_conf_belief_moral_pos",
  "fonasa_conf_word_funcional_pos",
  "fonasa_conf_word_moral_pos"
)


data <- data %>%
  mutate(
    across(
      all_of(trust_fonasa_pos),
      ~ {
        z_trust <- as.numeric(scale(.x))
        z_know  <- z_fonasa_comp_obj_pos
        
        informed_part   <- z_trust * pmax(z_know, 0)
        misinformed_part <- z_trust * abs(pmin(z_know, 0))
        
        informed_part - misinformed_part
      },
      .names = "{.col}_calibrated"
    )
  )


data$fonasa_conf_belief_funcional_pos_calibrated

data <- data %>%
  mutate(
    z_sae_comp_obj_pos = as.numeric(scale(sae_comp_objetiva_pos))
  )

trust_sae_pos <- c(
  "sae_conf_belief_funcional_pos",
  "sae_conf_belief_moral_pos",
  "sae_conf_word_funcional_pos",
  "sae_conf_word_moral_pos"
)


data <- data %>%
  mutate(
    across(
      all_of(trust_sae_pos),
      ~ {
        z_trust <- as.numeric(scale(.x))
        z_know <- z_sae_comp_obj_pos

        informed_part <- z_trust * pmax(z_know, 0)
        misinformed_part <- z_trust * abs(pmin(z_know, 0))

        informed_part - misinformed_part
      },
      .names = "{.col}_calibrated"
    )
  )


data <- data %>%
  mutate(
    attitudes_tech = rowMeans(
      select(., starts_with("pre_general_attitudes_tech")),
      na.rm = TRUE
    ),
    alfabetizacion_digital = rowMeans(
      select(., starts_with("alfabetizacion_digital")),
      na.rm = TRUE
    ),
    ai_skills = rowMeans(
      select(., starts_with("ai_skills")),
      na.rm = TRUE
    ),
    attitudes_ai = rowMeans(
      select(., starts_with("general_attitudes_ai")),
      na.rm = TRUE
    ),
    get_variable_ord_sae = ifelse(get_variable_ord == 1, "Fonasa", "SAE"),
    uso_sae = rowMeans(
      select(., starts_with("sae_familiaridad_uso")),
      na.rm = TRUE
    ),
    sae_familiaridad_recode = ifelse(
      sae_familiaridad_3 == "No sé.",
      "No.",
      sae_familiaridad_3
    )
  )
