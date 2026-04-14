# =============================================================================
# PATH ANALYSIS - Experimento SAE: Control Humano y Corrección de Decisiones
# =============================================================================
#
# MODELO (cadena de mediación con moderación):
#
#   comp_pre → decision_error (–)
#   comp_pre → comp_post      (+, efecto directo secundario)
#   decision_error × disclaimer → comp_post  (moderación: el disclaimer
#       aumenta comprensión post CONDICIONADO a haber cometido un error)
#   disclaimer → comp_post    (+)
#   comp_post  → decision_corregida (+)
#
# Variables:
#   sae_comp_subjetiva_pre  [310] → Comprensión ex-ante  (continua)
#   sae_comp_subjetiva_pos  [311] → Comprensión ex-post  (continua)
#   control_humano          [293] → Disclaimer            (dicotómica: 0/1)
#   cantidad_colegios < 5        → Decisión errónea       (dicotomiza)
#   x402b_repostulacion == "Incluiría más escuelas" → Decisión corregida
# =============================================================================


# --- 0. Instalación y carga de paquetes -------------------------------------

packages <- c("lavaan", "semTools", "semPlot", "tidyverse",
              "ggplot2", "scales", "gridExtra")

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(lavaan)
  library(semTools)
  library(semPlot)
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(gridExtra)
})


# =============================================================================
# 1. PREPARACIÓN DE DATOS
# =============================================================================

df <- data %>%
  mutate(
    # Decisión errónea: menos de 5 colegios = error (1), sino (0)
    decision_error = if_else(cantidad_colegios < 5, 1L, 0L, missing = NA_integer_),

    # Decisión corregida: eligió incluir más escuelas
    decision_corregida = if_else(
      x402b_repostulacion == "Incluiría más escuelas", 1L, 0L,
      missing = NA_integer_
    ),

    # Estandarizar variables continuas
    comp_pre_z  = scale(sae_comp_subjetiva_pre)[, 1],
    comp_post_z = scale(sae_comp_subjetiva_pos)[, 1],

    # Disclaimer como numérico (0/1)
    disclaimer = as.numeric(control_humano),

    # Término de interacción: moderación decision_error × disclaimer
    # Captura si el disclaimer tiene un efecto adicional en quienes erraron
    error_x_disclaimer = decision_error * disclaimer
  ) %>%
  filter(!is.na(decision_error), !is.na(disclaimer))

cat("=== Resumen del dataset ===\n")
cat("N total:", nrow(df), "\n")
cat("Decisiones erróneas:", sum(df$decision_error, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$decision_error, na.rm = TRUE) * 100))
cat("Con disclaimer:", sum(df$disclaimer, na.rm = TRUE), "\n")
cat("Erraron Y tienen disclaimer:",
    sum(df$decision_error == 1 & df$disclaimer == 1, na.rm = TRUE), "\n")
cat("Decisión corregida:", sum(df$decision_corregida, na.rm = TRUE), "\n\n")


# =============================================================================
# 2. PATH ANALYSIS CON LAVAAN
# =============================================================================
# La moderación (decision_error × disclaimer) se incluye como término
# de interacción explícito en la ecuación de comp_post_z.
#
# Cadena principal:
#   comp_pre_z → decision_error → (modera) → comp_post_z → decision_corregida
#                                disclaimer ↗
#
# Estimador MLR: robusto a no-normalidad de variables dicotómicas.

model_path <- '
  # ── Ecuación 1: Decisión errónea ──────────────────────────────────────────
  # Comprensión ex-ante predice negativamente el error de decisión
  decision_error ~ b1*comp_pre_z

  # ── Ecuación 2: Comprensión ex-post ───────────────────────────────────────
  # Predictores:
  #   (a1) disclaimer: efecto directo del control humano sobre comprensión
  #   (a2) error_x_disclaimer: MODERACIÓN (disclaimer × error)
  #         → el disclaimer es más efectivo en quienes erraron
  #   (a3) comp_pre_z: efecto directo de comprensión previa sobre post
  #   (a4) decision_error: efecto del error sobre comprensión post
  comp_post_z ~ a1*disclaimer + a2*error_x_disclaimer +
                a3*comp_pre_z + a4*decision_error

  # ── Ecuación 3: Decisión corregida ────────────────────────────────────────
  # La corrección depende principalmente de la comprensión ex-post
  decision_corregida ~ c1*comp_post_z + c2*comp_pre_z + c3*decision_error

  # ── Efectos indirectos ────────────────────────────────────────────────────

  # 1. Cadena completa: comp_pre → error → comp_post → dec.corregida
  ind_compre_error_post := b1 * a4 * c1

  # 2. comp_pre → comp_post → dec.corregida (vía directa al mediador)
  ind_compre_post := a3 * c1

  # 3. disclaimer → comp_post → dec.corregida
  ind_disclaimer_post := a1 * c1

  # 4. Efecto de la moderación sobre dec.corregida vía comp_post
  #    (cuánto agrega el disclaimer a quienes erraron, vía comprensión)
  ind_mod_post := a2 * c1

  # 5. Efecto total de comp_pre sobre dec.corregida
  total_compre := (b1 * a4 * c1) + (a3 * c1) + c2

  # 6. Efecto total de disclaimer sobre dec.corregida
  total_disclaimer := (a1 * c1) + (a2 * c1)
'

fit_path <- sem(
  model_path,
  data      = df,
  estimator = "MLR",
  missing   = "ML",
  se        = "robust"
)

cat("=== RESULTADOS DEL PATH ANALYSIS ===\n\n")
summary(fit_path,
        fit.measures = TRUE,
        standardized = TRUE,
        rsquare      = TRUE)


# =============================================================================
# 3. ÍNDICES DE AJUSTE
# =============================================================================

fit_indices <- fitMeasures(fit_path, c("chisq", "df", "pvalue",
                                        "cfi", "tli", "rmsea",
                                        "rmsea.ci.lower", "rmsea.ci.upper",
                                        "srmr", "aic", "bic"))

cat("\n=== ÍNDICES DE AJUSTE ===\n")
cat(sprintf("Chi²  = %.3f (df = %d, p = %.3f)\n",
            fit_indices["chisq"], fit_indices["df"], fit_indices["pvalue"]))
cat(sprintf("CFI   = %.3f  |  TLI = %.3f\n", fit_indices["cfi"], fit_indices["tli"]))
cat(sprintf("RMSEA = %.3f [%.3f, %.3f]\n",
            fit_indices["rmsea"],
            fit_indices["rmsea.ci.lower"],
            fit_indices["rmsea.ci.upper"]))
cat(sprintf("SRMR  = %.3f\n", fit_indices["srmr"]))
cat(sprintf("AIC   = %.1f  |  BIC = %.1f\n", fit_indices["aic"], fit_indices["bic"]))

cat("\n=== EFECTOS INDIRECTOS Y TOTALES ===\n")
indirect <- parameterEstimates(fit_path, standardized = TRUE) %>%
  filter(op == ":=") %>%
  select(label, est, se, z, pvalue, std.all) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(indirect, n = Inf)


# =============================================================================
# 4. ANÁLISIS COMPLEMENTARIO: MODERACIÓN CON MODELOS SIMPLES
# =============================================================================

cat("\n=== MODELO LOGÍSTICO: Decisión errónea ~ comp_pre ===\n")
logit_error <- glm(decision_error ~ comp_pre_z,
                   data = df, family = binomial())
summary(logit_error)
cat("Odds Ratios:\n"); print(round(exp(coef(logit_error)), 3))

cat("\n=== MODELO LOGÍSTICO: Decisión corregida ~ comp_post ===\n")
logit_corr <- glm(decision_corregida ~ comp_post_z + comp_pre_z + decision_error,
                  data = df %>% filter(!is.na(decision_corregida)),
                  family = binomial())
summary(logit_corr)
cat("Odds Ratios:\n"); print(round(exp(coef(logit_corr)), 3))

cat("\n=== MODELO LINEAL: comp_post ~ disclaimer × decision_error ===\n")
lm_mod <- lm(comp_post_z ~ disclaimer * decision_error + comp_pre_z, data = df)
summary(lm_mod)

# Efectos simples del disclaimer por grupo
cat("\n--- Efecto simple del disclaimer según si hubo error ---\n")
df_no_error <- df %>% filter(decision_error == 0, !is.na(comp_post_z))
m0 <- lm(comp_post_z ~ disclaimer + comp_pre_z, data = df_no_error)
cat(sprintf("Entre quienes NO erraron: β = %.3f (p = %.3f)\n",
            coef(m0)["disclaimer"],
            summary(m0)$coefficients["disclaimer", "Pr(>|t|)"]))

df_si_error <- df %>% filter(decision_error == 1, !is.na(comp_post_z))
m1 <- lm(comp_post_z ~ disclaimer + comp_pre_z, data = df_si_error)
cat(sprintf("Entre quienes SÍ erraron: β = %.3f (p = %.3f)\n",
            coef(m1)["disclaimer"],
            summary(m1)$coefficients["disclaimer", "Pr(>|t|)"]))


# =============================================================================
# 5. VISUALIZACIÓN: semPlot
# =============================================================================

cat("\n=== Generando gráficos... ===\n")

png("path_diagram_semplot.png", width = 1600, height = 900, res = 130)
semPaths(
  fit_path,
  what        = "std",
  whatLabels  = "std",
  layout      = "tree2",
  rotation    = 2,
  style       = "ram",
  node.width  = 2.2,
  node.height = 1.3,
  edge.label.cex = 0.85,
  fade        = FALSE,
  residuals   = FALSE,
  intercepts  = FALSE,
  curve       = 0.35,
  color       = list(lat = "#F7DC6F", man = "#AED6F1"),
  nodeLabels  = c("Comp\nPre", "Dec.\nErrorada",
                  "Comp\nPost", "Dec.\nCorregida",
                  "Disclaimer", "Error×\nDisclaimer")
)
title("Path Analysis SAE — Cadena de mediación con moderación\n(coeficientes estandarizados)",
      cex.main = 1.1)
dev.off()


# =============================================================================
# 6. VISUALIZACIÓN PERSONALIZADA CON GGPLOT2 (para presentación)
# =============================================================================

params <- parameterEstimates(fit_path, standardized = TRUE) %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, std.all) %>%
  mutate(
    sig   = case_when(pvalue < .001 ~ "***",
                      pvalue < .01  ~ "**",
                      pvalue < .05  ~ "*",
                      TRUE          ~ "n.s."),
    label = paste0(sprintf("%.2f", std.all), sig),
    color = case_when(std.all > 0 & pvalue < .05 ~ "positivo",
                      std.all < 0 & pvalue < .05 ~ "negativo",
                      TRUE                        ~ "n.s.")
  )

# Coordenadas de nodos según el diagrama corregido:
# comp_pre (izq) → decision_error (centro-arriba)
#                  disclaimer (centro-abajo)   → comp_post (der-centro) → decision_corregida (der)
# error_x_disclaimer (centro-medio) ↗
nodes <- tibble(
  id    = c("comp_pre_z", "decision_error", "disclaimer",
            "error_x_disclaimer", "comp_post_z", "decision_corregida"),
  label = c("Comprensión\nex-ante", "Decisión\nerrónea",
            "Disclaimer\ncontrol humano", "Error ×\nDisclaimer\n(moderación)",
            "Comprensión\nex-post", "Decisión\ncorregida"),
  x     = c(1.0,  2.8,  2.8,  3.7,  4.5,  5.8),
  y     = c(2.0,  3.0,  1.0,  1.6,  2.0,  2.0),
  tipo  = c("exog", "endog", "exog", "mod", "mediador", "resultado")
)

edges <- params %>%
  left_join(nodes %>% select(id, x, y), by = c("rhs" = "id")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(nodes %>% select(id, x, y), by = c("lhs" = "id")) %>%
  rename(x_to = x, y_to = y) %>%
  filter(!is.na(x_from), !is.na(x_to))

p_path <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#12122A", color = NA),
    plot.title      = element_text(color = "white", size = 15, face = "bold",
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(color = "#9090B0", size = 10, hjust = 0.5,
                                   margin = margin(b = 14)),
    plot.margin     = margin(20, 20, 20, 20),
    legend.position = "bottom",
    legend.text     = element_text(color = "white", size = 9),
    legend.title    = element_text(color = "white", size = 9, face = "bold")
  ) +
  geom_segment(
    data = edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
        color = color, linewidth = abs(std.all)),
    arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
    alpha = 0.85
  ) +
  geom_label(
    data = edges,
    aes(x = (x_from + x_to) / 2,
        y = (y_from + y_to) / 2 + 0.13,
        label = label, color = color),
    fill = "#12122A", size = 3.3, fontface = "bold",
    label.padding = unit(0.12, "lines"), label.size = 0
  ) +
  geom_point(
    data = nodes,
    aes(x = x, y = y, fill = tipo),
    shape = 22, size = 20, color = "white", stroke = 1.2
  ) +
  geom_text(
    data = nodes,
    aes(x = x, y = y, label = label),
    color = "white", size = 2.9, fontface = "bold", lineheight = 0.85
  ) +
  scale_color_manual(
    name   = "Dirección",
    values = c("positivo" = "#2ECC71", "negativo" = "#E74C3C", "n.s." = "#7F8C8D"),
    labels = c("positivo" = "Positivo (p<.05)",
               "negativo" = "Negativo (p<.05)",
               "n.s."     = "No significativo")
  ) +
  scale_fill_manual(
    values = c("exog"      = "#2471A3",
               "endog"     = "#CB4335",
               "mod"       = "#7D3C98",
               "mediador"  = "#148F77",
               "resultado" = "#1E8449"),
    guide = "none"
  ) +
  scale_linewidth(range = c(0.4, 2.5), guide = "none") +
  coord_cartesian(xlim = c(0.4, 6.4), ylim = c(0.4, 3.6)) +
  labs(
    title    = "Path Analysis — Experimento SAE: Control Humano",
    subtitle = "Cadena de mediación con moderación (error × disclaimer) | * p<.05  ** p<.01  *** p<.001"
  )


# ── Gráfico de coeficientes estandarizados ──
p_coef <- params %>%
  mutate(
    pathway = case_when(
      rhs == "error_x_disclaimer" ~ "Error×Disclaimer → Comp.post",
      TRUE ~ paste(rhs, "→", lhs)
    ),
    pathway = str_replace_all(pathway, c(
      "comp_pre_z"         = "Comp.pre",
      "decision_error"     = "Dec.error",
      "disclaimer"         = "Disclaimer",
      "comp_post_z"        = "Comp.post",
      "decision_corregida" = "Dec.corregida"
    ))
  ) %>%
  ggplot(aes(x = reorder(pathway, std.all), y = std.all, fill = color)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = label,
                hjust = ifelse(std.all >= 0, -0.1, 1.1)),
            color = "white", size = 3.4, fontface = "bold") +
  geom_hline(yintercept = 0, color = "white", linetype = "dashed", alpha = 0.4) +
  coord_flip(ylim = c(-0.8, 0.8)) +
  scale_fill_manual(
    values = c("positivo" = "#2ECC71", "negativo" = "#E74C3C", "n.s." = "#7F8C8D"),
    guide  = "none"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#12122A", color = NA),
    panel.background   = element_rect(fill = "#12122A", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#2A2A4E"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    plot.title         = element_text(color = "white", face = "bold", size = 13)
  ) +
  labs(title = "Coeficientes estandarizados por trayectoria",
       x = NULL, y = "β estandarizado")

ggsave("path_diagram_custom.png",      plot = p_path, width = 14, height = 8,
       dpi = 180, bg = "#12122A")
ggsave("coeficientes_trayectoria.png", plot = p_coef, width = 10, height = 6,
       dpi = 180, bg = "#12122A")


# =============================================================================
# 7. TABLA RESUMEN DE RESULTADOS
# =============================================================================

tabla_params <- parameterEstimates(fit_path, standardized = TRUE) %>%
  filter(op %in% c("~", ":=")) %>%
  mutate(
    Trayectoria = case_when(
      op == ":=" ~ paste("INDIRECTO:", label),
      TRUE       ~ paste(rhs, "→", lhs)
    ),
    B        = round(est, 3),
    SE       = round(se, 3),
    z        = round(z, 3),
    p        = round(pvalue, 3),
    `β(std)` = round(std.all, 3),
    Sig      = case_when(pvalue < .001 ~ "***",
                         pvalue < .01  ~ "**",
                         pvalue < .05  ~ "*",
                         TRUE          ~ "n.s.")
  ) %>%
  select(Trayectoria, B, SE, z, p, `β(std)`, Sig)

cat("\n=== TABLA COMPLETA DE PARÁMETROS ===\n")
print(tabla_params, n = Inf)

write_csv(tabla_params, "resultados_path_analysis.csv")

cat("\n=== Archivos generados ===\n")
cat("  ✓ path_diagram_custom.png\n")
cat("  ✓ coeficientes_trayectoria.png\n")
cat("  ✓ path_diagram_semplot.png\n")
cat("  ✓ resultados_path_analysis.csv\n")
cat("\n¡Análisis completado!\n")
