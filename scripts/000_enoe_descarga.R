# ============================================================
# 000_enoe_descarga.R
# Descarga automática de microdatos ENOE (SDEM, DTA)
# Cubre 2005T1 → trimestre más reciente disponible
#
# Tres patrones de URL según tramo metodológico:
#   2005T1–2020T1  : {year}trim{N}_dta.zip
#   2020T3–2022T4  : enoe_n_{year}_trim{N}_dta.zip
#   2023T1–hoy     : enoe_{year}_trim{N}_dta.zip
#
# Lógica de markers: si ._downloaded_{zip_name} existe en
# dir_out, el trimestre se considera descargado y se omite.
# ============================================================

rm(list = ls())
options(scipen = 999)

# ── librerías ────────────────────────────────────────────────
pacman::p_load(
  purrr,   # walk, safely
  dplyr,
  fs,      # dir_create, file_create, file_exists
  httr2    # request / req_perform con manejo de errores HTTP
)

# ── directorios ──────────────────────────────────────────────
dir_out <- "inputs/enoe_zip"
dir_create(dir_out)

# ── helpers ──────────────────────────────────────────────────

yt_index <- function(year, trim) year * 10L + trim

# Construye la URL del ZIP según el tramo metodológico.
# Espeja la lógica de get_reglas() en 001_catalogo.R
get_url <- function(year, trim) {
  idx      <- yt_index(year, trim)
  base     <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos"

  if (idx >= yt_index(2005L, 1L) && idx <= yt_index(2020L, 1L)) {
    # Patrón clásico: 2005trim1_dta.zip
    return(sprintf("%s/%dtrim%d_dta.zip", base, year, trim))
  }

  if (idx >= yt_index(2020L, 3L) && idx <= yt_index(2022L, 4L)) {
    # ENOE_N COVID: enoe_n_2021_trim2_dta.zip
    return(sprintf("%s/enoe_n_%d_trim%d_dta.zip", base, year, trim))
  }

  if (idx >= yt_index(2023L, 1L)) {
    # Rediseño 2023+: enoe_2025_trim4_dta.zip
    return(sprintf("%s/enoe_%d_trim%d_dta.zip", base, year, trim))
  }

  # 2020T2 se canceló por COVID — no tiene archivo
  return(NA_character_)
}

# ── trimestres a descargar ───────────────────────────────────
# Genera la grilla completa 2005T1 → trim_fin
# Ajusta trim_fin al último trimestre publicado
trim_fin_year <- 2025L
trim_fin_trim <- 4L   # <-- actualiza cuando salga un nuevo trimestre

trimestres <- expand.grid(
  year = 2005L:trim_fin_year,
  trim = 1L:4L
) |>
  as_tibble() |>
  arrange(year, trim) |>
  filter(yt_index(year, trim) >= yt_index(2005L, 1L)) |>
  filter(yt_index(year, trim) <= yt_index(trim_fin_year, trim_fin_trim)) |>
  # Excluir 2020T2: cancelado por COVID
  filter(!(year == 2020L & trim == 2L)) |>
  mutate(
    url      = map2_chr(year, trim, get_url),
    zip_name = sprintf("enoe_%dT%d", year, trim),
    zip_file = path(dir_out, paste0(zip_name, ".zip")),
    marker   = path(dir_out, paste0("._downloaded_", zip_name))
  )

cat(sprintf("Trimestres en scope: %d\n", nrow(trimestres)))

# ── función de descarga (un trimestre) ───────────────────────
descarga_trimestre <- function(row) {

  zip_name <- row$zip_name
  url      <- row$url
  zip_file <- row$zip_file
  marker   <- row$marker

  # Trimestre sin URL (ej. 2020T2)
  if (is.na(url)) {
    message("⏭  Sin URL (omitido): ", zip_name)
    return(invisible(NULL))
  }

  # Ya descargado previamente
  if (file_exists(marker)) {
    message("✔  Ya descargado: ", zip_name)
    return(invisible(NULL))
  }

  message("⬇  Descargando: ", zip_name)

  # httr2: manejo explícito de errores HTTP
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(120) |>       # 2 min por archivo
      httr2::req_retry(max_tries = 3,
                       backoff = ~ 5) |>  # reintentos con espera
      httr2::req_perform(path = zip_file),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    # Limpiar archivo parcial si quedó
    if (file_exists(zip_file)) file_delete(zip_file)
    stop("Error de red: ", conditionMessage(resp))
  }

  # Verificar que el archivo descargado sea un ZIP real
  # (INEGI devuelve HTML 200 aunque el trimestre no exista)
  sig <- readBin(zip_file, "raw", n = 4L)
  es_zip <- identical(sig[1:2], as.raw(c(0x50, 0x4B)))  # PK header

  if (!es_zip) {
    file_delete(zip_file)
    stop("Respuesta no es un ZIP (¿trimestre aún no publicado?): ", url)
  }

  # Crear marker solo si todo salió bien
  file_create(marker)
  message("✅ Listo: ", zip_name,
          sprintf(" (%.1f MB)", file_size(zip_file) / 1e6))
}

descarga_safe <- purrr::safely(descarga_trimestre, otherwise = NULL)

# ── ejecutar ─────────────────────────────────────────────────
jobs <- split(trimestres, seq_len(nrow(trimestres)))

res <- walk(jobs, function(row) {
  out <- descarga_safe(row)
  if (!is.null(out$error)) {
    message("❌ FALLO [", row$zip_name, "]: ", out$error$message)
  }
})

# ── log de errores ───────────────────────────────────────────
errores <- map(
  seq_along(jobs),
  function(i) {
    err <- descarga_safe(jobs[[i]])$error
    if (!is.null(err)) tibble(zip_name = jobs[[i]]$zip_name, msg = err$message)
  }
) |>
  compact() |>
  bind_rows()

if (nrow(errores) > 0) {
  message("\n⚠️  ", nrow(errores), " trimestre(s) fallaron:")
  print(errores)
  saveRDS(errores, "outputs/log_errores_descarga.rds")
} else {
  message("\n🎉 Todas las descargas completadas sin errores.")
}

# ── resumen final ─────────────────────────────────────────────
descargados <- trimestres |>
  mutate(descargado = file_exists(marker)) |>
  summarise(
    total      = n(),
    descargado = sum(descargado),
    pendiente  = sum(!descargado)
  )

cat("\n=== Resumen ===\n")
print(descargados)

beepr::beep()