# ============================================================
# 002_sdem.R
# Series de tiempo por educación y género — ENOE 2005–2025
# Replica Figuras 1 y 2 de Duval-Hernández & Orraca Romano (2009)
#
# Estructura basada en 003_pseudo_panel.R — solo requiere SDEM
# Output: un RDS con una fila por year × trim × niv_esc × genero
# Figuras: PNG en outputs/
# ============================================================

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(
  tidyverse,
  haven,
  future,
  furrr,
  beepr
)

# ── inputs / outputs ─────────────────────────────────────────
catalogo_path <- "outputs/catalogo_enoe_sdem_coe1.rds"
out_path      <- "outputs/serie_educacion_genero.rds"

# Solo necesitamos SDEM
catalogo <- readRDS(catalogo_path)

# ── función principal ─────────────────────────────────────────
procesa_fila <- function(row) {

  year      <- row$year[[1]]
  trim      <- row$trim[[1]]
  peso      <- row$peso[[1]]      # "fac" o "fac_tri" según tramo
  path_sdem <- row$path_sdem[[1]]

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
      )
    ) |>
    filter(!is.na(niv_esc), !is.na(genero))

  # ── agregar por educación y género (serie de tiempo) ───────
  serie <- sdemt |>
    group_by(niv_esc, genero) |>
    summarise(
      n_obs       = n(),
      tasa_part   = weighted.mean(
                      clase1 == 1,
                      .data[[peso]], na.rm = TRUE),
      tasa_desocu = weighted.mean(
                      clase2 == 2,
                      .data[[peso]] * (clase1 == 1),
                      na.rm = TRUE),
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
      .groups = "drop"
    ) |>
    mutate(year = year, trim = trim) |>
    relocate(year, trim, niv_esc, genero)

  serie
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

if (length(err) > 0) {
  log_err <- tibble(
    idx = which(map_lgl(res, ~ !is.null(.x$error))),
    msg = map_chr(err, ~ .x$message)
  )
  message("⚠️  ", nrow(log_err), " trimestre(s) fallaron")
  saveRDS(log_err, "outputs/log_errores_serie_educ_genero.rds")
}

serie <- bind_rows(ok) |>
  arrange(year, trim, niv_esc, genero) |>
  mutate(
    fecha   = year + (trim - 1) / 4,
    niv_esc = factor(niv_esc, levels = c("Básica", "Intermedia", "Superior"))
  )

saveRDS(serie, out_path)

beepr::beep()

# ── Figura 1: Participación laboral y desempleo ───────────────
# Replica el panel 2×2 del paper (Figura 1):
#   filas = indicador (participación / desempleo)
#   columnas = género (Hombre / Mujer)
#   líneas = nivel educativo

fig1_data <- serie |>
  select(fecha, niv_esc, genero, tasa_part, tasa_desocu) |>
  pivot_longer(
    cols      = c(tasa_part, tasa_desocu),
    names_to  = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = recode(indicador,
      tasa_part   = "Participación laboral",
      tasa_desocu = "Desempleo"
    ),
    indicador = factor(indicador,
      levels = c("Participación laboral", "Desempleo"))
  )

fig1 <- ggplot(fig1_data,
               aes(x = fecha, y = valor,
                   color = niv_esc, linetype = niv_esc)) +
  geom_line(linewidth = 0.7) +
  facet_grid(indicador ~ genero, scales = "free_y") +
  scale_color_manual(
    values = c("Básica" = "black", "Intermedia" = "#555555", "Superior" = "#999999"),
    name   = "Educación"
  ) +
  scale_linetype_manual(
    values = c("Básica" = "solid", "Intermedia" = "dashed", "Superior" = "dotted"),
    name   = "Educación"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = ""
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave("outputs/figura1_participacion_desempleo.png", fig1,
       width = 10, height = 7, dpi = 300)

# ── Figura 2: Shares sectoriales ─────────────────────────────
# Panel 2×2: filas = sector (formal / informal asalariado)
#             columnas = género
#             líneas = nivel educativo

fig2_data <- serie |>
  select(fecha, niv_esc, genero, formal, informal) |>
  pivot_longer(
    cols      = c(formal, informal),
    names_to  = "sector",
    values_to = "share"
  ) |>
  mutate(
    sector = recode(sector,
      formal   = "Sector formal",
      informal = "Informal asalariado"
    ),
    sector = factor(sector,
      levels = c("Sector formal", "Informal asalariado"))
  )

fig2 <- ggplot(fig2_data,
               aes(x = fecha, y = share,
                   color = niv_esc, linetype = niv_esc)) +
  geom_line(linewidth = 0.7) +
  facet_grid(sector ~ genero, scales = "free_y") +
  scale_color_manual(
    values = c("Básica" = "black", "Intermedia" = "#555555", "Superior" = "#999999"),
    name   = "Educación"
  ) +
  scale_linetype_manual(
    values = c("Básica" = "solid", "Intermedia" = "dashed", "Superior" = "dotted"),
    name   = "Educación"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = ""
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave("outputs/figura2_shares_sectoriales.png", fig2,
       width = 10, height = 7, dpi = 300)

cat("✓ Figuras guardadas en outputs/\n")
cat(sprintf("  Trimestres cubiertos: %d\n",
            n_distinct(paste(serie$year, serie$trim))))
cat(sprintf("  Rango: %d T%d – %d T%d\n",
            min(serie$year), min(serie$trim[serie$year == min(serie$year)]),
            max(serie$year), max(serie$trim[serie$year == max(serie$year)])))
