# OO descomprimir automaticamente cada zip

rm(list = ls()) #para borrar todos los objetos en la memoria de la consola
options(scipen=999) #para desactivar la notación científica

# librerias ---------------------------------------------------------------

pacman::p_load(
  purrr,
  fs
)


# directorios -------------------------------------------------------------

# Carpetas
dir_zip  <- "inputs/enoe_zip"
dir_out  <- "inputs/enoe_descomprimidos"

# Crear carpeta de salida si no existe
dir_create(dir_out)

# Listar ZIPs disponibles
zips <- dir_ls(dir_zip, regexp = "\\.zip$")


# purrr -------------------------------------------------------------------

walk(zips, function(zip_path) {
  
  # Nombre base del ZIP (sin extensión)
  zip_name <- path_ext_remove(path_file(zip_path))
  
  # Definir marcador
  marker <- path(dir_out, paste0("._unzipped_", zip_name))
  
  # Si ya existe el marcador, no hacer nada
  if (file_exists(marker)) {
    message("✔ Ya descomprimido: ", zip_name)
    return(invisible(NULL))
  }
  
  message("Descomprimiendo: ", zip_name)
  
  # Descomprimir
  unzip(zip_path, exdir = dir_out)
  
  # Crear marcador SOLO si todo salió bien
  file_create(marker)
  
  message("✅ Listo: ", zip_name)
})

beepr::beep()
