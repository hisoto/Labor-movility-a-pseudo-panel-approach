# Catálogo ENOE/ENOE_N: empareja SDEM y COE1 por trimestre ------
# y asigna llave + factor de expansión según tramo

rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(data.table)
library(haven)

# ----------------------- PATHS ------------------------------
path_in  <- "inputs/enoe_descomprimidos/"

# ----------------------- HELPERS ----------------------------

yt_index <- function(year, trim) year * 10L + trim

get_reglas <- function(year, trim) {
  idx <- yt_index(year, trim)
  
  if (idx >= yt_index(2005L, 1L) && idx <= yt_index(2020L, 1L)) {
    return(list(
      tramo = "ENOE_2005T1_2020T1",
      llave = c("cd_a","ent","con","v_sel","n_hog","h_mud","n_ren"),
      peso  = "fac"
    ))
  }
  
  if (idx >= yt_index(2020L, 3L) && idx <= yt_index(2021L, 2L)) {
    return(list(
      tramo = "ENOE_N_2020T3_2021T2",
      llave = c("cd_a","ent","con","v_sel","tipo","mes_cal","ca","n_hog","h_mud","n_ren"),
      peso  = "fac_tri"
    ))
  }
  
  if (idx >= yt_index(2021L, 3L) && idx <= yt_index(2022L, 4L)) {
    return(list(
      tramo = "ENOE_N_2021T3_2022T4",
      llave = c("cd_a","ent","con","v_sel","tipo","mes_cal","n_hog","h_mud","n_ren"),
      peso  = "fac_tri"
    ))
  }
  
  if (idx >= yt_index(2023L, 1L) && idx <= yt_index(2025L, 2L)) {
    return(list(
      tramo = "ENOE_2023T1_2025T2",
      llave = c("tipo","mes_cal","cd_a","ent","con","v_sel","n_hog","h_mud","n_ren"),
      peso  = "fac_tri"
    ))
  }
  
  if (idx >= yt_index(2025L, 3L)) {
    return(list(
      tramo = "ENOE_2025T3_plus",
      llave = c("tipo","mes_cal","cd_a","cve_ent","con","v_sel","n_hog","h_mud","n_ren"),
      peso  = "fac_tri"
    ))
  }
  
  stop("Trimestre fuera de rango: ", year, "T", trim)
}

extrae_matches_3 <- function(path) {
  str_extract(basename(path), "\\d{3}(?=\\.dta$)")
}

# ----------------------- LISTADO DE ARCHIVOS ----------------

sdem_files <- list.files(
  path = path_in,
  pattern = "(?i)SDEMT\\d{3}\\.dta$",
  full.names = TRUE
)

coe1_files <- list.files(
  path = path_in,
  pattern = "(?i)COE1.*\\d{3}\\.dta$",
  full.names = TRUE
)

# ----------------------- ARMAR ÍNDICES ----------------------

sdem_idx <- tibble(path_sdem = sdem_files) %>%
  mutate(matches = map_chr(path_sdem, extrae_matches_3)) %>%
  filter(!is.na(matches)) %>%
  mutate(
    trim = as.integer(str_sub(matches, 1, 1)),
    year = 2000L + as.integer(str_sub(matches, 2, 3)),
    trimestre_id = sprintf("%04dT%d", year, trim)
  )

coe1_idx <- tibble(path_coe1 = coe1_files) %>%
  mutate(matches = map_chr(path_coe1, extrae_matches_3)) %>%
  filter(!is.na(matches)) %>%
  mutate(
    trim = as.integer(str_sub(matches, 1, 1)),
    year = 2000L + as.integer(str_sub(matches, 2, 3)),
    trimestre_id = sprintf("%04dT%d", year, trim)
  )

# ----------------------- CATÁLOGO ----------------------------

catalogo <- sdem_idx %>%
  select(trimestre_id, year, trim, path_sdem) %>%
  left_join(coe1_idx %>% select(trimestre_id, path_coe1), by = "trimestre_id") %>%
  arrange(year, trim) %>%
  mutate(
    reglas = map2(year, trim, get_reglas),
    tramo  = map_chr(reglas, "tramo"),
    llave  = map(reglas, "llave"),
    peso   = map_chr(reglas, "peso")
  ) %>%
  select(-reglas) %>%
  mutate(
    existe_sdem = file.exists(path_sdem),
    existe_coe1 = if_else(is.na(path_coe1), FALSE, file.exists(path_coe1))
  )

# ----------------------- VALIDACIÓN ESTRUCTURAL (NUEVO) ----------------
# Aquí verificamos que las llaves existan, ignorando mayúsculas/minúsculas

catalogo <- catalogo %>%
  mutate(
    llaves_existen = pmap_lgl(
      list(path_coe1, llave),
      function(path, llv) {
        if (is.na(path) || !file.exists(path)) return(FALSE)
        vars <- names(read_dta(path, n_max = 0))
        vars <- tolower(vars)
        all(llv %in% vars)
      }
    )
  )

# ----------------------- CHEQUEOS ----------------------------

dups <- catalogo %>% count(trimestre_id) %>% filter(n > 1)

sin_coe1 <- catalogo %>% filter(is.na(path_coe1) | !existe_coe1)

print(catalogo %>% summarise(
  n_trimestres = n(),
  n_sin_coe1   = sum(is.na(path_coe1) | !existe_coe1),
  n_sin_sdem   = sum(!existe_sdem),
  n_llave_falla = sum(!llaves_existen)
))

if (nrow(dups) > 0) {
  message("⚠️ Hay trimestres duplicados en SDEM/COE1 (revisar):")
  print(dups)
}

# Guardar catálogo
saveRDS(catalogo, "outputs/catalogo_enoe_sdem_coe1.rds")

# Opcional: subconjunto 2005T1–2020T1
catalogo_2005_2020t1 <- catalogo %>%
  filter(yt_index(year, trim) >= yt_index(2005L, 1L),
         yt_index(year, trim) <= yt_index(2020L, 1L))

saveRDS(catalogo_2005_2020t1, "outputs/catalogo_enoe_2005t1_2020t1.rds")

print(head(catalogo, 10))
