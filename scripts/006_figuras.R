# ============================================================
# 006_figuras.R
# Gráficas de efectos APC
# Replicando estilo de Duval-Hernández & Orraca Romano (2009)
#
# Para cada variable dependiente se genera una figura con
# tres paneles apilados:
#   Fila 1 — Efecto edad (α_a)  vs. edad
#   Fila 2 — Efecto cohorte (κ_c) vs. año de nacimiento
#   Fila 3 — Efecto periodo (τ_t) vs. trimestre
# Cada panel tiene dos facetas (Hombre / Mujer) y tres líneas
# (una por nivel educativo).
#
# Input:  outputs/efectos_apc.rds
# Output: outputs/figuras/fig_[variable].pdf
# ============================================================

rm(list = ls()); gc()

pacman::p_load(tidyverse, patchwork)
source("scripts/theme_conasami.R")

dir.create("outputs/figuras", showWarnings = FALSE, recursive = TRUE)

resultados <- readRDS("outputs/efectos_apc.rds")

# ╔══════════════════════════════════════════════════════════╗
# ║  ETIQUETAS Y ESTÉTICA                                    ║
# ╚══════════════════════════════════════════════════════════╝

etiq_var <- c(
  part         = "Tasa de participación laboral",
  desocu       = "Tasa de desempleo",
  formal       = "Empleo formal",
  informal     = "Empleo informal",
  autoempleo   = "Autoempleo",
  sobreocupado = "Jornada > 40 h (asalariados)"
)

etiq_niv <- c(
  "Básica"     = "Básica (≤ 6 años)",
  "Intermedia" = "Intermedia (7–12 años)",
  "Superior"   = "Superior (> 12 años)"
)

etiq_gen <- c("Hombre" = "Hombres", "Mujer" = "Mujeres")

colores     <- c("Básica" = "#1b7837", "Intermedia" = "#762a83", "Superior" = "#e08214")
tipos_linea <- c("Básica" = "solid",   "Intermedia" = "dashed",  "Superior" = "dotdash")

# ╔══════════════════════════════════════════════════════════╗
# ║  APLANAR RESULTADOS → TIBBLES LARGOS                     ║
# ╚══════════════════════════════════════════════════════════╝

aplanar <- function(tipo) {
  # tipo: "alpha" | "kappa" | "tau"
  map_dfr(names(resultados), function(g) {
    partes  <- strsplit(g, "_")[[1]]
    genero  <- partes[1]
    niv_esc <- paste(partes[-1], collapse = "_")

    map_dfr(names(resultados[[g]]), function(v) {
      m <- resultados[[g]][[v]]
      if (is.null(m)) return(NULL)

      tbl <- m[[tipo]]

      # Unificar columna de valores: tau_tbl usa "tau", los demás usan "valor"
      if ("tau" %in% names(tbl)) tbl <- rename(tbl, valor = tau)

      tbl |> mutate(genero = genero, niv_esc = niv_esc, variable = v)
    })
  }) |>
    mutate(
      genero  = factor(genero,  levels = c("Hombre", "Mujer")),
      niv_esc = factor(niv_esc, levels = c("Básica", "Intermedia", "Superior"))
    )
}

df_alpha <- aplanar("alpha")   # idx, valor, edad,     genero, niv_esc, variable
df_kappa <- aplanar("kappa")   # idx, valor, year_nac, genero, niv_esc, variable
df_tau   <- aplanar("tau")     # ..., valor, periodo,  genero, niv_esc, variable

# ── Intercepto por grupo × variable ──────────────────────────
df_intercept <- map_dfr(names(resultados), function(g) {
  partes  <- strsplit(g, "_")[[1]]
  genero  <- partes[1]
  niv_esc <- paste(partes[-1], collapse = "_")
  map_dfr(names(resultados[[g]]), function(v) {
    m <- resultados[[g]][[v]]
    if (is.null(m)) return(NULL)
    tibble(genero = genero, niv_esc = niv_esc, variable = v,
           intercept = m$intercept)
  })
}) |>
  mutate(
    genero  = factor(genero,  levels = c("Hombre", "Mujer")),
    niv_esc = factor(niv_esc, levels = c("Básica", "Intermedia", "Superior"))
  )

# ── Efectos de referencia (convención del paper) ─────────────
# Perfil edad:    κ de la cohorte más cercana a 1956
# Perfil cohorte: α de la edad más cercana a 42
kappa_ref <- df_kappa |>
  group_by(genero, niv_esc, variable) |>
  slice_min(abs(year_nac - 1956L), n = 1, with_ties = FALSE) |>
  select(genero, niv_esc, variable, kappa_ref = valor)

alpha_ref <- df_alpha |>
  group_by(genero, niv_esc, variable) |>
  slice_min(abs(edad - 42L), n = 1, with_ties = FALSE) |>
  select(genero, niv_esc, variable, alpha_ref = valor)

# ── Tasas predichas en probabilidad: p̂ = plogis(θ + α_a + κ_ref) ──
df_alpha <- df_alpha |>
  left_join(df_intercept, by = c("genero", "niv_esc", "variable")) |>
  left_join(kappa_ref,    by = c("genero", "niv_esc", "variable")) |>
  mutate(valor_prob = plogis(intercept + valor + kappa_ref))

df_kappa <- df_kappa |>
  left_join(df_intercept, by = c("genero", "niv_esc", "variable")) |>
  left_join(alpha_ref,    by = c("genero", "niv_esc", "variable")) |>
  mutate(valor_prob = plogis(intercept + alpha_ref + valor))

# ╔══════════════════════════════════════════════════════════╗
# ║  COMPONENTES COMUNES DE GGPLOT                           ║
# ╚══════════════════════════════════════════════════════════╝

tema_apc <- theme_conasami(
    base_size       = 10,
    title_size      = 10,
    axis_title_size = 9,
    strip_text_size = 9
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.title     = element_blank(),
    legend.key.width = unit(1.8, "cm")
  )

escala_color <- scale_color_manual(values = colores,     labels = etiq_niv)
escala_tipo  <- scale_linetype_manual(values = tipos_linea, labels = etiq_niv)
escala_gen   <- labeller(genero = etiq_gen)
hline_cero   <- geom_hline(yintercept = 0, linetype = "dashed",
                            color = "grey55", linewidth = 0.35)

# ╔══════════════════════════════════════════════════════════╗
# ║  FUNCIÓN PRINCIPAL: UNA FIGURA POR VARIABLE              ║
# ╚══════════════════════════════════════════════════════════╝

figura_apc <- function(var) {

  d_a <- df_alpha |> filter(variable == var)
  d_k <- df_kappa |> filter(variable == var)
  d_t <- df_tau   |> filter(variable == var)

  # ── Panel 1: efecto edad (α_a) — escala probabilidad ────────
  p_alpha <- ggplot(d_a, aes(edad, valor_prob, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(20, 70, 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x        = "Edad",
      y        = "Tasa predicha",
      title    = expression(paste("Efecto edad   ", alpha[a])),
      subtitle = "Cohorte de referencia: 1956"
    ) +
    tema_apc

  # ── Panel 2: efecto cohorte (κ_c) — escala probabilidad ─────
  p_kappa <- ggplot(d_k, aes(year_nac, valor_prob, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(1935, 2005, 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x        = "Año de nacimiento",
      y        = "Tasa predicha",
      title    = expression(paste("Efecto cohorte   ", kappa[c])),
      subtitle = "Edad de referencia: 42"
    ) +
    tema_apc

  # ── Panel 3: efecto periodo (τ_t) ───────────────────────────
  p_tau <- ggplot(d_t, aes(periodo, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(2005, 2025, 5)) +
    labs(
      x     = "Periodo",
      y     = "Log-odds",
      title = expression(paste("Efecto periodo   ", tau[t]))
    ) +
    tema_apc

  # ── Combinar con patchwork ───────────────────────────────────
  combinada <- (p_alpha / p_kappa / p_tau) +
    plot_annotation(
      title    = etiq_var[var],
      subtitle = "Descomposición APC — Deaton (1997)",
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "grey40")
      )
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  list(combined = combinada,
       alpha    = p_alpha,
       kappa    = p_kappa,
       tau      = p_tau)
}

# ╔══════════════════════════════════════════════════════════╗
# ║  GUARDAR (una figura por variable)                        ║
# ╚══════════════════════════════════════════════════════════╝

walk(names(etiq_var), function(v) {
  cat(sprintf("Generando: %s ... ", etiq_var[v]))

  fig  <- figura_apc(v)
  path <- sprintf("outputs/figuras/fig_%s.pdf", v)

  ggsave(
    filename = path,
    plot     = fig$combined,
    width    = 18,
    height   = 28,
    units    = "cm",
    device   = cairo_pdf
  )

  cat(sprintf("guardada en %s\n", path))
})

# ── PNGs individuales por panel (5 vars × 3 efectos = 15 archivos) ──
walk(names(etiq_var), function(v) {
  fig <- figura_apc(v)

  walk(c("alpha", "kappa", "tau"), function(ef) {
    path <- sprintf("outputs/figuras/fig_%s_%s.png", v, ef)
    ggsave(
      filename = path,
      plot     = fig[[ef]],
      width    = 18,
      height   = 11,
      units    = "cm",
      dpi      = 200,
      device   = "png"
    )
    cat(sprintf("  PNG: %s\n", path))
  })
})

cat("\nListo. Figuras en outputs/figuras/\n")
