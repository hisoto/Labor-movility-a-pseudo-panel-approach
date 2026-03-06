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

dir.create("outputs/figuras", showWarnings = FALSE, recursive = TRUE)

resultados <- readRDS("outputs/efectos_apc.rds")

# ╔══════════════════════════════════════════════════════════╗
# ║  ETIQUETAS Y ESTÉTICA                                    ║
# ╚══════════════════════════════════════════════════════════╝

etiq_var <- c(
  part       = "Tasa de participación laboral",
  desocu     = "Tasa de desempleo",
  formal     = "Empleo formal",
  informal   = "Empleo informal",
  autoempleo = "Autoempleo"
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

# ╔══════════════════════════════════════════════════════════╗
# ║  COMPONENTES COMUNES DE GGPLOT                           ║
# ╚══════════════════════════════════════════════════════════╝

tema_apc <- theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text       = element_text(face = "bold", size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.width = unit(1.8, "cm"),
    plot.title       = element_text(size = 10, face = "bold"),
    axis.title       = element_text(size = 9)
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

  # ── Panel 1: efecto edad (α_a) ──────────────────────────────
  p_alpha <- ggplot(d_a, aes(edad, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(20, 70, 10)) +
    labs(
      x     = "Edad",
      y     = "Log-odds",
      title = expression(paste("Efecto edad   ", alpha[a]))
    ) +
    tema_apc

  # ── Panel 2: efecto cohorte (κ_c) ───────────────────────────
  p_kappa <- ggplot(d_k, aes(year_nac, valor, color = niv_esc, linetype = niv_esc)) +
    geom_line(linewidth = 0.65) +
    hline_cero +
    facet_wrap(~genero, labeller = escala_gen) +
    escala_color + escala_tipo +
    scale_x_continuous(breaks = seq(1935, 2005, 10)) +
    labs(
      x     = "Año de nacimiento",
      y     = "Log-odds",
      title = expression(paste("Efecto cohorte   ", kappa[c]))
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
  (p_alpha / p_kappa / p_tau) +
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
    plot     = fig,
    width    = 18,
    height   = 22,
    units    = "cm",
    device   = "pdf"
  )

  cat(sprintf("guardada en %s\n", path))
})

cat("\nListo. Figuras en outputs/figuras/\n")
