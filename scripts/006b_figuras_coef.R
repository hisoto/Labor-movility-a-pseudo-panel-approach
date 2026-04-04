# ============================================================
# 006b_figuras_coef.R
# Gráficas de coeficientes APC en escala log-odds
#
# Complemento a 006_figuras.R. En lugar de convertir α y κ a
# escala de probabilidad (plogis), se grafican las desviaciones
# crudas α_a, κ_c, τ_t directamente en log-odds.
#
# Convención de paneles:
#   Fila 1 — Efecto edad (α_a)    vs. edad
#   Fila 2 — Efecto cohorte (κ_c) vs. año de nacimiento
#   Fila 3 — Efecto periodo (τ_t) vs. trimestre
# Líneas por nivel educativo; facetas por género (Hombre/Mujer).
#
# Input:  outputs/efectos_apc.rds
# Output: outputs/figuras/fig_[variable]_coef.pdf
#         outputs/figuras/fig_[variable]_[alpha|kappa|tau]_coef.png
# ============================================================

rm(list = ls()); gc()
options(scipen = 999)

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

colores     <- c("Básica" = "#9b2247", "Intermedia" = "#1e5b4f", "Superior" = "#a57f2c")
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

figura_coef <- function(var) {

  d_a <- df_alpha |> filter(variable == var)
  d_k <- df_kappa |> filter(variable == var)
  d_t <- df_tau   |> filter(variable == var)

  # ── Panel 1: efecto edad (α_a) — log-odds crudo ─────────
  p_alpha <- ggplot(d_a, aes(edad, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(20, 70, 10)) +
    labs(
      x = "Edad",
      y = "Log-odds (desviación)"
    ) +
    tema_apc

  # ── Panel 2: efecto cohorte (κ_c) — log-odds crudo ──────
  p_kappa <- ggplot(d_k, aes(year_nac, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(1935, 2005, 10)) +
    labs(
      x = "Año de nacimiento",
      y = "Log-odds (desviación)"
    ) +
    tema_apc

  # ── Panel 3: efecto periodo (τ_t) — log-odds ────────────
  p_tau <- ggplot(d_t, aes(periodo, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(2005, 2025, 5)) +
    labs(
      x = "Periodo",
      y = "Log-odds (desviación)"
    ) +
    tema_apc

  # ── Combinar con patchwork ───────────────────────────────
  combinada <- (p_alpha / p_kappa / p_tau) +
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

  fig  <- figura_coef(v)
  path <- sprintf("outputs/figuras/fig_%s_coef.pdf", v)

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

# ── PNGs individuales por panel (6 vars × 3 efectos = 18 archivos) ──
walk(names(etiq_var), function(v) {
  fig <- figura_coef(v)

  walk(c("alpha", "kappa", "tau"), function(ef) {
    path <- sprintf("outputs/figuras/fig_%s_%s_coef.png", v, ef)
    ggsave(
      filename = path,
      plot     = fig[[ef]],
      width    = 25,
      height   = 10,
      units    = "cm",
      dpi      = 300,
      device   = "png"
    )
    cat(sprintf("  PNG: %s\n", path))
  })
})

cat("\nListo. Figuras en outputs/figuras/\n")
