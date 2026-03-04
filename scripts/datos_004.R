# ============================================================
# 004_preparar_variables.R
# ============================================================

rm(list = ls()); gc()

library(tidyverse)

panel <- readRDS("outputs/panel_cohortes.rds")

# ── PASO 1: periodo continuo ─────────────────────────────────
# 2005T1 = 2005.00 | 2005T2 = 2005.25 | 2005T3 = 2005.50 | 2005T4 = 2005.75
panel <- panel |>
  mutate(periodo = year + (trim - 1) / 4)

# ── PASO 2: log-odds con winsorización ───────────────────────
# p = 0 da -Inf, p = 1 da +Inf. Truncar en [eps, 1-eps].
# COVID (2020T2-T3): tasas altas de desocupación son reales.
# log-odds(0.40) = -0.41, perfectamente manejable. Se mantienen.
logodds <- function(p, eps = 0.001) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}

panel <- panel |>
  mutate(
    lo_part       = logodds(tasa_part),
    lo_desocu     = logodds(tasa_desocu),
    lo_formal     = logodds(formal),
    lo_informal   = logodds(informal),
    lo_autoempleo = logodds(auto_empleo)
  )

# ── PASO 4: verificaciones ───────────────────────────────────

cat("\n=== 1. Log-odds: no debe haber Inf ni NaN ===\n")
panel |>
  select(starts_with("lo_")) |>
  summarise(across(everything(), list(
    n_inf = ~sum(is.infinite(.)),
    n_nan = ~sum(is.nan(.)),
    min   = ~round(min(., na.rm = TRUE), 3),
    max   = ~round(max(., na.rm = TRUE), 3)
  ))) |>
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = stat, values_from = value) |>
  print()
# Esperado: n_inf = 0, n_nan = 0 en las cinco variables

cat("\n=== 2. Dimensiones clave ===\n")
cat("Edades distintas  :", n_distinct(panel$edad),     "\n")
cat("Cohortes distintas:", n_distinct(panel$year_nac), "\n")
cat("Periodos distintos:", n_distinct(panel$periodo),  "\n")

cat("\n=== 3. Filas por grupo ===\n")
panel |>
  count(genero, niv_esc) |>
  pivot_wider(names_from = niv_esc, values_from = n) |>
  print()

cat("\n=== 4. Periodos cubiertos ===\n")
panel |>
  summarise(
    primer     = paste0(min(year), "T", trim[which.min(periodo)]),
    ultimo     = paste0(max(year), "T", trim[which.max(periodo)]),
    n_periodos = n_distinct(periodo)
  ) |>
  print()

# ── GUARDAR ──────────────────────────────────────────────────
saveRDS(panel, "outputs/panel_estimacion.rds")
cat("\nGuardado: outputs/panel_estimacion.rds\n")
cat("Dimensiones:", nrow(panel), "x", ncol(panel), "\n")