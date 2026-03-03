# cohortes para el último trimestre disponible 

rm(list = ls())

pacman::p_load(
  tidyverse,
  haven
)

sdemt <- read_dta("inputs/enoe_descomprimidos/ENOE_SDEMT425.dta")

glimpse(sdemt)

sdemt <- sdemt |> 
  filter(
# Filtros necesarios para usar las variables precodificadas
    r_def == 0 &  
    (c_res == 1 | c_res == 3) &
    eda >= 15 & eda <= 98 
) |> 
  mutate(
    niv_esc = case_when(
      anios_esc >= 0 & anios_esc <= 6 ~ "Básica",
      anios_esc > 6 & anios_esc <= 12 ~ "Intermedia",
      anios_esc > 12 ~ "Superior",
    ), 
    condicion = case_when(
      clase1 == 1 & clase2 == 1 & seg_soc == 1 ~ "Formal",
      clase1 == 1 & clase2 == 1 & seg_soc == 2 ~ "Informal",
      clase1 == 1 & clase2 == 2 ~ "Desocupado",
      clase1 == 2 ~ "Inactivo",
      TRUE ~ NA_character_
    ),
    genero = case_when(
      sex == 1 ~ "Hombre",
      sex == 2 ~ "Mujer",
      TRUE     ~ NA_character_
    ),
    nac_anio = as.integer(nac_anio),
    year_nac = as.integer(2025 - eda)
  ) 



cohortes <- sdemt |>
  group_by(year_nac, niv_esc, genero) |>
  summarise(
    edad   = as.integer(first(eda)),
    n_pond = as.integer(sum(fac_tri, na.rm = TRUE)),
    n_ocu = as.integer(sum(fac_tri[clase1 == 1 & clase2 == 1], na.rm = TRUE)),
    n_eco_activa = as.integer(sum(fac_tri[clase1 == 1], na.rm = TRUE)),
    tasa_ocu = weighted.mean(clase2 == 1, fac_tri, na.rm = TRUE),
    tasa_desocu = weighted.mean(clase1== 1 & clase2 == 2, fac_tri, na.rm = TRUE),
    formal = weighted.mean(pos_ocu == 1 & seg_soc == 1, fac_tri, na.rm = TRUE),
    informal = weighted.mean(pos_ocu == 1 & seg_soc == 2, fac_tri, na.rm = TRUE),
    auto_empleo = weighted.mean(pos_ocu == 3, fac_tri,na.rm = TRUE),
    .groups = "drop"
  ) 


ggplot(cohortes |> filter(edad >= 15 & edad <= 90)) + 
  geom_line(
    mapping = aes(
      x = edad, 
      y = tasa_desocu,
      color = genero
    )
  ) +
  facet_wrap(~ niv_esc) +
  theme_bw()
