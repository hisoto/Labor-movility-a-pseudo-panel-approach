# ============================================================
# 007_fig_educacion_cohorte.R
# Replica Figure A-1 de Duval-Hernández & Orraca Romano (2009)
#
# Figura: promedio de años de educación por año de nacimiento,
#         separado por género (Hombres sólida, Mujeres punteada)
#
# Input:  outputs/catalogo_enoe_sdem_coe1.rds
# Output: outputs/figuras/fig_educacion_cohorte.png
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
source("scripts/theme_conasami.R")

# ── Inputs / Outputs ─────────────────────────────────────────
catalogo_path <- "outputs/catalogo_enoe_sdem_coe1.rds"
out_fig       <- "outputs/figuras/fig_educacion_cohorte.png"

catalogo <- readRDS(catalogo_path)

# ── Función principal ─────────────────────────────────────────
# Para cada trimestre: calcula el promedio ponderado de anios_esc
# agrupado por año de nacimiento (year_nac = year_encuesta - eda) y género.
procesa_fila <- function(row) {

  year      <- row$year[[1]]
  trim      <- row$trim[[1]]
  peso      <- row$peso[[1]]
  path_sdem <- row$path_sdem[[1]]

  sdemt <- read_dta(path_sdem)
  names(sdemt) <- tolower(names(sdemt))

  sdemt <- sdemt |>
    filter(
      r_def == 0,
      c_res %in% c(1, 3),
      eda   >= 20,
      eda   <= 70,
      !is.na(anios_esc)
    ) |>
    mutate(
      year_nac = year - eda,
      genero   = case_when(
        sex == 1 ~ "Hombre",
        sex == 2 ~ "Mujer",
        TRUE     ~ NA_character_
      )
    ) |>
    filter(!is.na(genero))

  sdemt |>
    group_by(year_nac, genero) |>
    summarise(
      anios_esc_medio = weighted.mean(anios_esc, .data[[peso]], na.rm = TRUE),
      n_obs           = n(),
      .groups         = "drop"
    ) |>
    mutate(year = year, trim = trim)
}

procesa_fila_safe <- purrr::safely(procesa_fila, otherwise = NULL)

# ── Paralelización ────────────────────────────────────────────
plan(multisession, workers = 8)

jobs <- split(catalogo, seq_len(nrow(catalogo)))

res <- future_map(
  jobs,
  procesa_fila_safe,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

plan(sequential)

# ── Separar éxitos y errores ─────────────────────────────────
ok  <- map(res, "result") |> compact()
err <- map(res, "error")  |> keep(~ !is.null(.x))

if (length(err) > 0) {
  message("⚠  ", length(err), " trimestre(s) fallaron")
}

# ── Agregar: promedio entre trimestres para cada cohorte ─────
# Cada trimestre observa la misma cohorte a distintas edades.
# Se promedia anios_esc_medio entre todos los trimestres disponibles.
educ_cohorte <- bind_rows(ok) |>
  group_by(year_nac, genero) |>
  summarise(
    anios_esc = mean(anios_esc_medio, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  filter(year_nac >= 1935, year_nac <= 2025) |>
  mutate(
    genero = factor(genero, levels = c("Hombre", "Mujer"))
  )

cat(sprintf("\nCohortes cubiertas: %d a %d\n",
            min(educ_cohorte$year_nac), max(educ_cohorte$year_nac)))

# ── Figura ────────────────────────────────────────────────────
fig <- ggplot(educ_cohorte,
              aes(year_nac, anios_esc,
                  color    = genero,
                  linetype = genero)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(
    values = c("Hombre" = "black", "Mujer" = "#555555"),
    labels = c("Hombre" = "Males", "Mujer" = "Females")
  ) +
  scale_linetype_manual(
    values = c("Hombre" = "solid", "Mujer" = "dashed"),
    labels = c("Hombre" = "Males", "Mujer" = "Females")
  ) +
  scale_x_continuous(breaks = seq(1935, 2005, 10)) +
  scale_y_continuous(breaks = seq(4, 14, 2)) +
  labs(
    title    = "Figure A-1. Years of Education by Cohort",
    subtitle = "ENOE 2005–2025 — Ages 20–70",
    x        = "Yr of Birth",
    y        = "Average Yrs of Education",
    color    = NULL,
    linetype = NULL
  ) +
  theme_conasami(base_size = 11)

ggsave(out_fig, fig, width = 8, height = 6, dpi = 300)
cat(sprintf("✓ Figura guardada en %s\n", out_fig))

beepr::beep()
