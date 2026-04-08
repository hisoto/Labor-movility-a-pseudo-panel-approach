# ============================================================
# 005_APC_estimacion.R
# Estimación del modelo APC de Deaton (1997) aplicado a
# Duval-Hernández & Orraca Romano (2009)
#
# Modelo:  ln(p_ct/(1-p_ct)) = θ + α_a + κ_c + τ_t + ε_ct
# Método:  WLS ponderado por tamaño de celda (n_obs)
# ID:      Normalización de Deaton — dummies de periodo
#          ortogonalizadas a [1, t] antes de estimar.
#
#   Por qué este enfoque:
#   edad = year − year_nac exactamente en el pseudo-panel,
#   lo que hace el modelo no restringido rango-deficiente.
#   Solución: reemplazar los T-1 dummies de periodo por
#   T-2 dummies ortogonales a [1,t] (espacio M_B).
#   Los efectos resultantes satisfacen Σ τ = 0 y Σ t·τ = 0
#   por construcción → normalización de Deaton incorporada.
#
# Input:  outputs/panel_cohortes.rds
# Output: outputs/efectos_apc.rds
# ============================================================

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(tidyverse, beepr)

# ── Inputs / Outputs ─────────────────────────────────────────
out_path <- "outputs/efectos_apc.rds"
# ── Preparación del panel ────────────────────────────────────
logodds <- function(p, eps = 0.001) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}

panel_est <- readRDS("outputs/panel_cohortes.rds") |>
  filter(n_obs >= 100, !is.na(niv_esc), !is.na(genero)) |>
  mutate(
    periodo       = year + (trim - 1) / 4,
    lo_part       = logodds(tasa_part),
    lo_desocu     = logodds(tasa_desocu),
    lo_formal     = logodds(formal),
    lo_informal   = logodds(informal),
    lo_autoempleo   = logodds(auto_empleo),
    lo_sobreocupado = logodds(sobreocupado)
  )


cat("\n=== 1. Log-odds: no debe haber Inf ni NaN ===\n")
panel_est |>
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
cat("Edades distintas  :", n_distinct(panel_est$edad),     "\n")
cat("Cohortes distintas:", n_distinct(panel_est$year_nac), "\n")
cat("Periodos distintos:", n_distinct(panel_est$periodo),  "\n")

cat("\n=== 3. Filas por grupo ===\n")
panel_est |>
  count(genero, niv_esc) |>
  pivot_wider(names_from = niv_esc, values_from = n) |>
  print()

cat("\n=== 4. Periodos cubiertos ===\n")
panel_est |>
  summarise(
    primer     = paste0(min(year), "T", trim[which.min(periodo)]),
    ultimo     = paste0(max(year), "T", trim[which.max(periodo)]),
    n_periodos = n_distinct(periodo)
  ) |>
  print()

# ── Variables dependientes ────────────────────────────────────
vars_dep <- c(
  part         = "lo_part",
  desocu       = "lo_desocu",
  formal       = "lo_formal",
  informal     = "lo_informal",
  autoempleo   = "lo_autoempleo",
  sobreocupado = "lo_sobreocupado"
)

# ╔══════════════════════════════════════════════════════════╗
# ║  FUNCIONES AUXILIARES                                    ║
# ╚══════════════════════════════════════════════════════════╝

# ── 1. Dummies de periodo ortogonalizadas (Deaton) ───────────
# Devuelve una matriz T × (T-2).
# Cada columna es un dummy de periodo proyectado fuera de [1, t]:
#   D_orth = M_B × I   donde  M_B = I - B(B'B)⁻¹B',  B = [1, t]
# Los coeficientes sobre estas columnas satisfacen
#   Σ τ_t = 0  y  Σ t·τ_t = 0  por construcción.
crear_dummies_deaton <- function(T) {
  t_num <- seq_len(T)
  B     <- cbind(1, t_num)
  M_B   <- diag(T) - B %*% solve(crossprod(B)) %*% t(B)
  cols  <- qr(M_B)$pivot[seq_len(qr(M_B)$rank)]
  M_B[, cols, drop = FALSE]
}

# ── 2. Extraer efectos de edad o cohorte ─────────────────────
# idx = 1 es la categoría de referencia (efecto fijado en 0).
# Devuelve tibble(idx, valor) ordenado por idx.
extraer_efectos <- function(fit, patron) {
  coefs    <- coef(fit)
  sel      <- grepl(patron, names(coefs))
  idx_vals <- as.integer(sub(patron, "", names(coefs)[sel]))

  tibble(
    idx   = c(1L, idx_vals),
    valor = c(0.0, unname(coefs[sel]))
  ) |>
    arrange(idx)
}

# ── 3. Estimación de un modelo APC ───────────────────────────
estima_apc <- function(df, var_dep) {

  # Filtrar NAs en la variable dependiente
  df_sub <- df |> filter(!is.na(.data[[var_dep]]))

  if (nrow(df_sub) < 30L) {
    warning(sprintf("[%s] Menos de 30 obs. Omitido.", var_dep))
    return(NULL)
  }

  # Periodos observados por cohorte (diagnóstico de cohortes frágiles)
  periodos_por_cohorte <- df_sub |>
    summarise(n_periodos = n_distinct(periodo), .by = year_nac)

  # Re-indexar dentro del subgrupo (garantiza 1..N sin huecos)
  periodos_uniq <- sort(unique(df_sub$periodo))
  edades_uniq   <- sort(unique(df_sub$edad))
  cohortes_uniq <- sort(unique(df_sub$year_nac))

  T <- length(periodos_uniq)
  A <- length(edades_uniq)
  C <- length(cohortes_uniq)

  # Parámetros libres: 1 + (A-1) + (C-1) + (T-2) = A + C + T - 3
  if (nrow(df_sub) <= A + C + T - 3L) {
    warning(sprintf("[%s] Grados de libertad insuficientes. Omitido.", var_dep))
    return(NULL)
  }

  # Mappings índice → valor real
  map_periodo <- tibble(periodo_idx = seq_len(T), periodo = periodos_uniq)
  map_edad    <- tibble(edad_idx    = seq_len(A), edad    = edades_uniq)
  map_cohorte <- tibble(cohorte_idx = seq_len(C), year_nac = cohortes_uniq)

  df_sub <- df_sub |>
    mutate(
      edad_idx    = as.integer(factor(edad,     levels = edades_uniq)),
      cohorte_idx = as.integer(factor(year_nac, levels = cohortes_uniq)),
      periodo_idx = as.integer(factor(periodo,  levels = periodos_uniq))
    )

  # ── Dummies de periodo ortogonalizadas ──────────────────────
  D_deaton <- crear_dummies_deaton(T)           # T × (T-2)
  nms_d    <- paste0("d_", seq_len(ncol(D_deaton)))
  colnames(D_deaton) <- nms_d

  # Unir al data frame por periodo_idx
  D_df <- as.data.frame(D_deaton)
  D_df$periodo_idx <- seq_len(T)
  df_aug <- left_join(df_sub, D_df, by = "periodo_idx")

  # ── Modelo único (rango completo) ────────────────────────────
  f_con <- as.formula(paste0(
    var_dep,
    " ~ factor(edad_idx) + factor(cohorte_idx) + ",
    paste(nms_d, collapse = " + ")
  ))
  fit_con <- lm(f_con, data = df_aug, weights = df_aug$n_pond)

  # ── Recuperar τ_t (T valores, ya normalizados) ───────────────
  # τ = D_deaton %*% coef_d   (proyección inversa al espacio ⊥[1,t])
  coef_d <- coef(fit_con)[nms_d]
  tau    <- as.numeric(D_deaton %*% coef_d)

  # ── Empaquetar resultados ────────────────────────────────────

  tau_tbl <- tibble(
    periodo_idx = seq_len(T),
    tau         = tau
  ) |>
    left_join(map_periodo, by = "periodo_idx") |>
    mutate(
      year = as.integer(floor(periodo)),
      trim = as.integer(round((periodo - floor(periodo)) * 4)) + 1L
    )

  alpha_tbl <- extraer_efectos(fit_con, "^factor\\(edad_idx\\)") |>
    left_join(map_edad, by = c("idx" = "edad_idx"))

  kappa_tbl <- extraer_efectos(fit_con, "^factor\\(cohorte_idx\\)") |>
    left_join(map_cohorte, by = c("idx" = "cohorte_idx")) |>
    left_join(periodos_por_cohorte, by = "year_nac")

  list(
    tau       = tau_tbl,
    alpha     = alpha_tbl,
    kappa     = kappa_tbl,
    intercept = unname(coef(fit_con)[["(Intercept)"]]),
    r2        = summary(fit_con)$r.squared,
    n         = nrow(df_sub),
    dims      = c(T = T, A = A, C = C)
  )
}

estima_apc_safe <- safely(estima_apc, otherwise = NULL)

# ╔══════════════════════════════════════════════════════════╗
# ║  LOOP DE ESTIMACIÓN (6 grupos × 6 variables = 36 modelos)║
# ╚══════════════════════════════════════════════════════════╝

grupos <- expand_grid(
  genero  = c("Hombre", "Mujer"),
  niv_esc = c("Básica", "Intermedia", "Superior")
)

resultados <- map(seq_len(nrow(grupos)), function(i) {
  g     <- grupos[i, ]
  g_key <- paste(g$genero, g$niv_esc, sep = "_")
  df_g  <- panel_est |>
    filter(genero == g$genero, niv_esc == g$niv_esc)

  cat(sprintf("\n── %s (%d filas) ──\n", g_key, nrow(df_g)))

  map(names(vars_dep), function(v) {
    cat(sprintf("   %-12s ...", v))
    res <- estima_apc_safe(df_g, vars_dep[[v]])

    if (!is.null(res$error)) {
      cat(sprintf(" ERROR: %s\n", res$error$message))
      return(NULL)
    }
    if (is.null(res$result)) {
      cat(" omitido\n")
      return(NULL)
    }
    m <- res$result
    cat(sprintf(" R²=%.3f  (n=%d, T=%d, A=%d, C=%d)\n",
                m$r2, m$n, m$dims["T"], m$dims["A"], m$dims["C"]))
    m
  }) |>
    set_names(names(vars_dep))
}) |>
  set_names(paste(grupos$genero, grupos$niv_esc, sep = "_"))

# ╔══════════════════════════════════════════════════════════╗
# ║  VERIFICACIÓN RESTRICCIONES DEATON                       ║
# ╚══════════════════════════════════════════════════════════╝

cat("\n=== Verificación restricciones Deaton (Hombre_Básica / part) ===\n")
check_tau <- resultados[["Hombre_Básica"]][["part"]]$tau
if (!is.null(check_tau)) {
  t_idx <- seq_len(nrow(check_tau))
  cat(sprintf("  Σ τ_t      = %+.2e  (debe ser ≈ 0)\n", sum(check_tau$tau)))
  cat(sprintf("  Σ t·τ_t    = %+.2e  (debe ser ≈ 0)\n", sum(t_idx * check_tau$tau)))
}

# ╔══════════════════════════════════════════════════════════╗
# ║  DIAGNÓSTICO: COHORTES FRÁGILES                          ║
# ╚══════════════════════════════════════════════════════════╝

cat("\n=== Diagnóstico: periodos observados por cohorte (part) ===\n")
for (g in names(resultados)) {
  k <- resultados[[g]][["part"]]$kappa
  if (is.null(k) || !"n_periodos" %in% names(k)) next
  fragiles <- k |> filter(n_periodos < 20)
  cat(sprintf("  %-22s n_periodos rango [%d, %d]",
              g, min(k$n_periodos), max(k$n_periodos)))
  if (nrow(fragiles) > 0) {
    cat(sprintf(" | < 20 per: %d cohortes (year_nac %d–%d)\n",
                nrow(fragiles), min(fragiles$year_nac), max(fragiles$year_nac)))
  } else {
    cat(" | todas las cohortes tienen >= 20 periodos\n")
  }
}

# ╔══════════════════════════════════════════════════════════╗
# ║  RESUMEN DE ESTIMACIONES                                 ║
# ╚══════════════════════════════════════════════════════════╝

cat("\n=== R² por grupo y variable ===\n")
r2_mat <- sapply(resultados, function(g_res) {
  sapply(g_res, function(m) {
    if (is.null(m)) NA_real_ else round(m$r2, 3)
  })
})
print(r2_mat)

# ── Guardar ───────────────────────────────────────────────────
saveRDS(resultados, out_path)
cat(sprintf("\nGuardado: %s\n", out_path))
cat(sprintf("Grupos: %d | Variables: %d | Modelos: %d\n",
            length(resultados), length(vars_dep),
            length(resultados) * length(vars_dep)))

beepr::beep()
