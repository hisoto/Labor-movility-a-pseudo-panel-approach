# ============================================================
# 003_cohortes_panel.R
# Panel completo ENOE: cohortes sintéticas por año de
# nacimiento, nivel educativo y género
#
# Estructura basada en 002_sdem.R — solo requiere SDEM
# Output: un RDS con una fila por year × trim × year_nac ×
#         niv_esc × genero
# ============================================================

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(
  tidyverse,
  haven,
  future,
  furrr,
  beepr,
  data.table
)

# ── inputs / outputs ─────────────────────────────────────────
catalogo_path <- "outputs/catalogo_enoe_sdem_coe1.rds"
out_path      <- "outputs/panel_cohortes.rds"
inpc_path <- "inputs/incp_2000_2026.csv"

# Solo necesitamos SDEM — no filtramos por existe_coe1
catalogo <- readRDS(catalogo_path)

# -- Inpc 

inpc <- fread(inpc_path)

trim_mes <- c("1" = 2L, "2" = 5L, "3" = 8L, "4" = 11L)

# Valor base: enero 2026
inpc_base <- inpc |>
  filter(year == 2026L, month == 1L) |>
  pull(valor)

catalogo <- catalogo |>
  mutate(mes_ref = trim_mes[as.character(trim)]) |>
  left_join(
    inpc |>
      select(year, month, valor) |>
      rename(mes_ref = month, inpc_val = valor),
    by = c("year", "mes_ref")
  )

# ── función principal ─────────────────────────────────────────
procesa_fila <- function(row) {

  year      <- row$year[[1]]
  trim      <- row$trim[[1]]
  peso      <- row$peso[[1]]      # "fac" o "fac_tri" según tramo
  path_sdem <- row$path_sdem[[1]]
  inpc_val  <- row$inpc_val[[1]]


  # ── leer y filtrar SDEM ────────────────────────────────────
  sdemt <- read_dta(path_sdem)
  names(sdemt) <- tolower(names(sdemt))

  sdemt <- sdemt |>
    filter(
      r_def == 0,
      c_res %in% c(1, 3),
      eda >= 20,
      eda <= 70
    ) |>
    mutate(
      # year_nac dinámico: usa el año del trimestre que se está procesando
      year_nac = as.integer(year - eda),
      niv_esc = case_when(
        anios_esc >= 0 & anios_esc <= 6  ~ "Básica",
        anios_esc > 6  & anios_esc <= 12 ~ "Intermedia",
        anios_esc > 12                   ~ "Superior",
        TRUE                             ~ NA_character_
      ),
      genero = case_when(
        sex == 1 ~ "Hombre",
        sex == 2 ~ "Mujer",
        TRUE     ~ NA_character_
      ),
      ingreso = ifelse(
        clase1 == 1 & clase2 == 1 & ingocup > 0,
        ingocup * (inpc_base / inpc_val), 
        NA_real_
      )
    )

  # ── agregar por cohorte ────────────────────────────────────
  cohortes <- sdemt |>
    group_by(year_nac, niv_esc, genero) |>
    summarise(
    edad         = as.integer(round(weighted.mean(eda, .data[[peso]], na.rm = TRUE))),
    n_obs        = n(),
    n_pond       = as.integer(sum(.data[[peso]], na.rm = TRUE)),
    n_ocu        = as.integer(sum(.data[[peso]][clase1 == 1 & clase2 == 1],
                                  na.rm = TRUE)),
    n_eco_activa = as.integer(sum(.data[[peso]][clase1 == 1],
                                  na.rm = TRUE)),
    # Tasas (denominador = poblacion economicamente activa del cohorte) ----------
    tasa_part   = weighted.mean(
                    clase1 == 1,
                    .data[[peso]], na.rm = TRUE),
    tasa_desocu = weighted.mean(
                    clase2 == 2,
                    .data[[peso]] * (clase1 == 1),  # denominador = PEA
                    na.rm = TRUE),
    tasa_ocu = weighted.mean(
                    clase2 == 1,
                    .data[[peso]],  # denominador = población total del cohorte 
                    na.rm = TRUE),
    # Shares (denominador = ocupados del cohorte) ----------------
    # Las cuatro categorías son mutuamente excluyentes y exhaustivas:
    # formal + informal + auto_empleo + no_remunerado = 1
    # Nota: seg_soc == 3 (n pequeño) se asigna a informal siguiendo
    # el criterio del paper (ausencia de seguridad social laboral).
    formal      = weighted.mean(
                    pos_ocu == 1 & seg_soc == 1,
                    .data[[peso]] * (clase1 == 1 & clase2 == 1),
                    na.rm = TRUE),
    informal    = weighted.mean(
                    pos_ocu == 1 & seg_soc %in% c(2, 3),
                    .data[[peso]] * (clase1 == 1 & clase2 == 1),
                    na.rm = TRUE),
    auto_empleo = weighted.mean(
                    pos_ocu %in% c(2, 3),
                    .data[[peso]] * (clase1 == 1 & clase2 == 1),
                    na.rm = TRUE),
    no_remunerado = weighted.mean(
                    pos_ocu == 4,
                    .data[[peso]] * (clase1 == 1 & clase2 == 1),
                    na.rm = TRUE),
    sobreocupado  = weighted.mean(
                    hrsocup > 40,
                    .data[[peso]] * (clase1 == 1 & clase2 == 1 & pos_ocu == 1),
                    na.rm = TRUE),
    ingreso_mean = weighted.mean(
                     ingreso[!is.na(ingreso)],
                     .data[[peso]][!is.na(ingreso)],
                     na.rm = TRUE),
    .groups = "drop"
    ) |>
    mutate(
      year = year, 
      trim = trim,
      cohort_id = str_glue("{year_nac}_{niv_esc}_{genero}")
    ) |>
    relocate(cohort_id, year, trim, year_nac, niv_esc, genero, edad)

  cohortes
}

procesa_fila_safe <- purrr::safely(procesa_fila, otherwise = NULL)

# ── paralelización ────────────────────────────────────────────

plan(multisession, workers = 8)

jobs <- split(catalogo, seq_len(nrow(catalogo)))

res <- future_map(
  jobs,
  procesa_fila_safe,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

plan(sequential)

# ── separar éxitos y errores ──────────────────────────────────
ok  <- map(res, "result") %>% compact()
err <- map(res, "error")  %>% keep(~ !is.null(.x))

panel <- bind_rows(ok) |>
  arrange(year, trim, year_nac, niv_esc, genero) |>
  # Duval-Hernández & Orraca Romano (2009): celdas con >= 100 observaciones
  # (n_obs es el conteo no ponderado de encuestados en la celda)
  filter(n_obs >= 100)

# ── log de errores ────────────────────────────────────────────
if (length(err) > 0) {
  log_err <- tibble(
    idx = which(map_lgl(res, ~ !is.null(.x$error))),
    msg = map_chr(err, ~ .x$message)
  )
  message("⚠️  ", nrow(log_err), " trimestre(s) fallaron")
  saveRDS(log_err, "outputs/log_errores_panel_cohortes.rds")
}

# ── guardar ───────────────────────────────────────────────────
saveRDS(panel, out_path)

# ── resumen ───────────────────────────────────────────────────
cat("\n=== Resumen del panel ===\n")
cat(sprintf("Filas totales        : %s\n", format(nrow(panel), big.mark = ",")))
cat(sprintf("Trimestres cubiertos : %d\n", n_distinct(paste(panel$year, panel$trim))))
cat(sprintf("Cohortes (años nac)  : %d  [%d – %d]\n",
            n_distinct(panel$year_nac),
            min(panel$year_nac),
            max(panel$year_nac)))
cat(sprintf("Niveles educativos   : %s\n",
            paste(unique(panel$niv_esc), collapse = " / ")))

cat("\n=== Ingreso real promedio por nivel educativo (todos los trimestres) ===\n")
panel |>
  filter(!is.na(ingreso_mean)) |>
  group_by(niv_esc) |>
  summarise(
    ingreso = weighted.mean(ingreso_mean, n_ocu, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ingreso = scales::dollar(ingreso, prefix = "$", big.mark = ",")) |>
  print()

cat("\n=== Primeras filas ===\n")
print(head(panel, 10))

beepr::beep()
