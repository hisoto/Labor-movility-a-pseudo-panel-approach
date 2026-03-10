# ============================================================
# 004_figuras_perfiles_brutos.R
# Perfiles brutos del ciclo de vida por cohorte y género
# Replica Figuras 3 y 4 de Duval-Hernández & Orraca Romano (2009)
#
# Requiere: outputs/panel_cohortes.rds  (003_pseudo_panel.R)
# Output  : outputs/figura3_perfiles_participacion.png
#           outputs/figura4_perfiles_sectores.png
# ============================================================

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(tidyverse)
source("scripts/theme_conasami.R")

# ── cargar panel ──────────────────────────────────────────────
panel <- readRDS("outputs/panel_cohortes.rds")

# ── re-agregar colapsando niv_esc ─────────────────────────────
# Las tasas se reconstruyen desde conteos para que el promedio
# ponderado entre grupos educativos sea correcto.
#
# n_ocu * formal  = sum(fac[trabajadores formales])   → n_formal
# n_ocu * informal = ...                              → n_informal
# n_ocu * auto_empleo = ...                           → n_auto

perfiles_trim <- panel |>
  group_by(year_nac, genero, year, trim) |>
  summarise(
    # ORDEN IMPORTA en dplyr::summarise(): las expresiones pueden referenciar
    # columnas ya creadas en la misma llamada. Los conteos sectoriales deben
    # calcularse ANTES de redefinir n_ocu como escalar; de lo contrario,
    # "n_ocu" en sum(formal * n_ocu) sería el total agregado (un escalar),
    # no el vector por grupo educativo, produciendo shares > 1.
    edad          = as.integer(round(weighted.mean(edad, n_pond, na.rm = TRUE))),
    n_formal      = sum(formal      * n_ocu,       na.rm = TRUE),
    n_informal    = sum(informal    * n_ocu,       na.rm = TRUE),
    n_auto_empleo = sum(auto_empleo * n_ocu,       na.rm = TRUE),
    n_pond        = sum(n_pond,                    na.rm = TRUE),
    n_eco_activa  = sum(n_eco_activa,              na.rm = TRUE),
    n_ocu         = sum(n_ocu,                     na.rm = TRUE),  # va al final
    .groups = "drop"
  ) |>
  mutate(
    tasa_part   = n_eco_activa / n_pond,
    tasa_desocu = ifelse(n_eco_activa > 0,
                         1 - n_ocu / n_eco_activa, NA_real_),
    formal      = ifelse(n_ocu > 0, n_formal      / n_ocu, NA_real_),
    informal    = ifelse(n_ocu > 0, n_informal    / n_ocu, NA_real_),
    auto_empleo = ifelse(n_ocu > 0, n_auto_empleo / n_ocu, NA_real_)
  )

# Si en un mismo año una cohorte tiene dos trimestres con la misma
# edad redondeada, promediamos ponderando por n_pond
perfiles <- perfiles_trim |>
  group_by(year_nac, genero, edad) |>
  summarise(
    across(
      c(tasa_part, tasa_desocu, formal, informal, auto_empleo),
      ~ weighted.mean(.x, n_pond, na.rm = TRUE)
    ),
    n_pond = sum(n_pond),
    .groups = "drop"
  ) |>
  arrange(year_nac, genero, edad)

cat(sprintf("Cohortes (años nac): %d  [%d – %d]\n",
            n_distinct(perfiles$year_nac),
            min(perfiles$year_nac),
            max(perfiles$year_nac)))
cat(sprintf("Edades cubiertas   : %d – %d\n",
            min(perfiles$edad), max(perfiles$edad)))

# ── Figura 3: Participación y Desempleo ───────────────────────
# Panel 2×2: filas = indicador, columnas = género
# Líneas = cohortes (una por año de nacimiento)

fig3_data <- perfiles |>
  select(year_nac, genero, edad, tasa_part, tasa_desocu) |>
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

fig3 <- ggplot(fig3_data,
               aes(x = edad, y = valor, group = year_nac)) +
  geom_line(alpha = 0.35, linewidth = 0.4, color = "black") +
  facet_grid(indicador ~ genero, scales = "free_y") +
  scale_x_continuous(breaks = seq(20, 70, 10), limits = c(20, 70)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Figura 3. Perfiles brutos del ciclo de vida I",
    subtitle = "Cada línea es una cohorte (año de nacimiento) — ENOE 2005–2025",
    x        = "Edad",
    y        = "Tasa"
  ) +
  theme_conasami(base_size = 11)

ggsave("outputs/figura3_perfiles_participacion.png", fig3,
       width = 10, height = 7, dpi = 300)

# ── Figura 4: Shares sectoriales ─────────────────────────────
# Panel 3×2: filas = sector, columnas = género
# Líneas = cohortes

fig4_data <- perfiles |>
  select(year_nac, genero, edad, formal, informal, auto_empleo) |>
  pivot_longer(
    cols      = c(formal, informal, auto_empleo),
    names_to  = "sector",
    values_to = "share"
  ) |>
  mutate(
    sector = recode(sector,
      formal      = "Sector formal",
      informal    = "Informal asalariado",
      auto_empleo = "Autoempleo"
    ),
    sector = factor(sector,
      levels = c("Sector formal", "Informal asalariado", "Autoempleo"))
  )

fig4 <- ggplot(fig4_data,
               aes(x = edad, y = share, group = year_nac)) +
  geom_line(alpha = 0.35, linewidth = 0.4, color = "black") +
  facet_grid(sector ~ genero, scales = "free_y") +
  scale_x_continuous(breaks = seq(20, 70, 10), limits = c(20, 70)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title    = "Figura 4. Perfiles brutos del ciclo de vida II",
    subtitle = "Cada línea es una cohorte (año de nacimiento) — ENOE 2005–2025",
    x        = "Edad",
    y        = "Proporción"
  ) +
  theme_conasami(base_size = 11)

ggsave("outputs/figura4_perfiles_sectores.png", fig4,
       width = 10, height = 10, dpi = 300)

cat("✓ Figuras guardadas en outputs/\n")
