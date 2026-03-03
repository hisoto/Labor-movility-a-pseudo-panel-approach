# Gráficas de cohortes 

pacman::p_load(
  tidyverse, 
  haven,
  ggrepel
)

panel <- readRDS("outputs/panel_cohortes.rds")

# Ingreso  

ingreso_decada <- panel |> 
  filter(year_nac %% 10 == 0) |>
  mutate(edad = as.numeric(edad))

glimpse(ingreso_decada)

etiquetas <- ingreso_decada |>
  filter(trim == 4, n_obs >= 100, niv_esc == "Superior") |>
  group_by(cohort_id) |>
  slice_max(edad, n = 1) |>
  ungroup() |>
  mutate(label = as.character(year_nac))

ggplot(
  ingreso_decada |> 
    filter(trim == 4 & n_obs >= 100 & niv_esc == "Superior")
) +
  geom_line(
    mapping = aes(
      x = edad, 
      y = ingreso_mean,
      color = cohort_id
    ), 
    show.legend = FALSE
  ) + 
  geom_text_repel(
    data          = etiquetas,
    aes(x = edad, y = ingreso_mean, label = label, color = cohort_id),
    size          = 3,
    direction     = "y",        # solo desplaza verticalmente
    nudge_x       = 2,          # empuja la etiqueta a la derecha del último punto
    segment.size  = 0.3,        # línea de conexión delgada
    segment.alpha = 0.5,
    show.legend   = FALSE
  ) +
  facet_wrap(~genero) + 
  theme_minimal()

# 