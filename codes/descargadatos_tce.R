# Instalar y cargar paquetes
install.packages(c("rvest","httr","readr","readxl","janitor","dplyr","stringr","purrr","naniar","tidyr"))
library(rvest)
library(httr)
library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(purrr)
library(naniar)
library(tidyr)

# 1️⃣ URL base
base_url <- "https://tribunalcalificador.cl"
index_url <- paste0(base_url, "/resultados_de_elecciones/")
dir.create("tce_descargas", showWarnings = FALSE)

# 2️⃣ Extraer subpáginas desde la página índice
index_html <- read_html(index_url)
links_all <- index_html %>% html_nodes("a") %>% html_attr("href") %>% na.omit()
links_all <- unique(ifelse(grepl("^https?://", links_all), links_all, paste0(base_url, links_all)))

# Filtrar subpáginas relevantes (evitar PDFs directos)
subpages <- links_all[grepl("resultados", links_all) & !grepl("\\.(pdf|zip)$", links_all, ignore.case = TRUE)]

# 3️⃣ Función para extraer enlaces a archivos .xlsx o .csv
extraer_archivos <- function(url) {
  message("Raspando: ", url)
  h <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(h)) return(character(0))
  hrefs <- h %>% html_nodes("a") %>% html_attr("href") %>% na.omit()
  hrefs_full <- ifelse(grepl("^https?://", hrefs), hrefs, paste0(base_url, hrefs))
  archivos <- unique(hrefs_full[grepl("\\.(xlsx|xls|csv)$", hrefs_full, ignore.case = TRUE)])
  return(archivos)
}

# 4️⃣ Raspado recursivo
archivos_encontrados <- unique(unlist(map(subpages, extraer_archivos)))

# 5️⃣ Descargar archivos
descargar_archivo <- function(url) {
  nombre <- basename(url)
  destino <- file.path("tce_descargas", nombre)
  message("Descargando: ", nombre)
  tryCatch({
    GET(url, user_agent("Mozilla/5.0"), write_disk(destino, overwrite = TRUE), timeout(60))
    destino
  }, error = function(e) {
    message("Error: ", e$message)
    return(NA_character_)
  })
}

rutas_descargadas <- map_chr(archivos_encontrados, descargar_archivo)
rutas_descargadas <- rutas_descargadas[file.exists(rutas_descargadas)]

# 6️⃣ Función para leer y limpiar cada archivo
leer_y_limpiar <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- read_csv(path, locale = locale(encoding = "UTF-8"))
  } else if (ext %in% c("xls","xlsx")) {
    df <- read_excel(path)
  } else return(NULL)
  
  df <- df %>% as_tibble() %>%
    janitor::clean_names() %>%                      # 2️⃣ Limpia nombres
    mutate(across(where(is.character), str_squish)) # 9️⃣ Elimina espacios
  
  # 4️⃣ Corrige tipos numéricos mal cargados
  cols_num <- names(df)[grepl("_num$", names(df))]
  if (length(cols_num) > 0) {
    df <- df %>% mutate(across(all_of(cols_num), as.numeric))
  }
  
  # 7️⃣ Armoniza categorías
  if ("respuesta" %in% names(df)) {
    df <- df %>% mutate(respuesta = str_to_lower(respuesta))
  }
  
  # 6️⃣ Elimina duplicados
  df <- df %>% distinct()
  
  # 8️⃣ Separar columnas pegadas (ejemplo)
  if ("region_comuna" %in% names(df)) {
    df <- df %>% separate(region_comuna, into = c("region","comuna"), sep = "\\s*-\\s*")
  }
  
  # Añade columna origen
  df <- df %>% mutate(archivo_origen = basename(path))
  return(df)
}

# 7️⃣ Leer y combinar
tablas <- map(rutas_descargadas, leer_y_limpiar)
tablas <- tablas[!map_lgl(tablas, is.null)]
datos_combinados <- bind_rows(tablas)

# 8️⃣ Analizar faltantes
naniar::vis_miss(datos_combinados)

# 🔟 Validación rápida
print(datos_combinados %>% summarise(across(everything(), ~sum(is.na(.)))))

# Exportar consolidado
write_csv(datos_combinados, "tce_resultados_consolidado.csv")
cat("Archivo combinado guardado como 'tce_resultados_consolidado.csv'.\n")
