# ------------------------------------------------------------
# Pipeline ENOE: SDEM + COE1 (join por llave del catálogo)
# + filtros + empleo ponderado
# Paraleliza con furrr::future_map_dfr()
# ------------------------------------------------------------

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(
  tidyverse,
  haven,
  future,
  furrr,
  beepr
)

# ----------------------- INPUTS -----------------------------
catalogo_path <- "outputs/catalogo_enoe_sdem_coe1.rds"
out_path      <- "outputs/serie_empleo_construccion.rds"

catalogo <- readRDS(catalogo_path)

# Si COE1 es indispensable para tu filtro:
catalogo <- catalogo %>% filter(existe_coe1)

# ----------------------- SETTINGS ---------------------------
plan(multisession, workers = 4)

# ----------------------- FUNCIÓN ----------------------------
procesa_fila <- function(row) {
  
  year  <- row$year[[1]]
  trim  <- row$trim[[1]]
  peso  <- row$peso[[1]]          
  llave <- row$llave[[1]]         
  
  path_sdem <- row$path_sdem[[1]]
  path_coe1 <- row$path_coe1[[1]]
  
  # ---- leer SDEM ----
  sdem <- read_dta(path_sdem)
  names(sdem) <- tolower(names(sdem))   # 🔹 NORMALIZACIÓN
  
  vars_sdem_filtros <- c("r_def","eda","clase1","clase2",
                         "c_res","pos_ocu","rama")
  
  vars_sdem_need <- unique(c(llave, peso, vars_sdem_filtros))
  
  sdem <- sdem %>%
    select(any_of(vars_sdem_need))
  
  # ---- leer COE1 ----
  coe1 <- NULL
  
  if (!is.na(path_coe1) && file.exists(path_coe1)) {
    
    coe1_raw <- read_dta(path_coe1)
    names(coe1_raw) <- tolower(names(coe1_raw))  # 🔹 NORMALIZACIÓN
    
    # Variable(s) COE1 usadas para filtro
    vars_coe1_filtro <- c("p4a")
    
    vars_coe1_need <- unique(c(llave, vars_coe1_filtro))
    
    coe1 <- coe1_raw %>%
      select(any_of(vars_coe1_need))
  }
  
  # ---- join ----
  if (!is.null(coe1)) {
    datos <- sdem %>% left_join(coe1, by = llave)
  } else {
    datos <- sdem
  }
  
  n0 <- nrow(datos)
  
  # ---- filtros SDEM + COE1 ----
  datos <- datos %>%
    filter(r_def == 0) %>%
    filter(eda >= 15, eda <= 97) %>%
    filter(clase1 == 1, clase2 == 1) %>%
    filter(c_res != 2) %>%
    filter(pos_ocu == 1) %>%
    filter(rama == 1) %>%
    filter(p4a %in% c(2361, 2363, 2370,
                      2381, 2382, 2399))
  
  n1 <- nrow(datos)
  
  # ---- empleo ponderado ----
  ocupados <- datos %>%
    summarise(
      ocupados = sum(.data[[peso]], na.rm = TRUE)
    ) %>%
    pull(ocupados)
  
  tibble(
    year = year,
    trim = trim,
    ocupados = ocupados,
    n_before = n0,
    n_after  = n1,
    tiene_coe1 = !is.null(coe1)
  )
}

# Envolver en safely para que un trimestre malo no tumbe todo
procesa_fila_safe <- purrr::safely(procesa_fila, otherwise = NULL)

# Split del catálogo en lista de 1 fila por elemento
jobs <- split(catalogo, seq_len(nrow(catalogo)))

# ----------------------- RUN -------------------------------
res <- future_map(
  jobs,
  procesa_fila_safe,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

# Separar éxitos y errores
ok  <- map(res, "result") %>% compact()
err <- map(res, "error")  %>% keep(~ !is.null(.x))

serie <- bind_rows(ok) %>%
  arrange(year, trim)

# Guardar log de errores si existen
if (length(err) > 0) {
  log_err <- tibble(
    idx = which(map_lgl(res, ~ !is.null(.x$error))),
    msg = map_chr(err, ~ .x$message)
  )
  saveRDS(log_err, "outputs/log_errores_empleo_construccion.rds")
}

saveRDS(serie, out_path)

# Reset plan
plan(sequential)

beepr::beep()

serie
