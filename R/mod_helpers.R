# ==============================================================================
# SERVER HELPERS - Utilidades puras del servidor (sin dependencias reactivas)
# Contiene: operador %||%, diálogos de archivo/carpeta, presets CSF
# ==============================================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) || identical(a, "")) b else a
}

# Convierte "Desc (*.ext1 *.ext2)" al matrix que espera utils::choose.files()
.filtro_win <- function(filtro_str) {
  m <- regexec("^(.+?)\\s*\\((.+?)\\)$", trimws(filtro_str), perl = TRUE)
  partes <- regmatches(filtro_str, m)[[1]]
  if (length(partes) < 3) {
    return(matrix(c("All Files", "*.*"), ncol = 2, byrow = TRUE))
  }
  desc <- trimws(partes[2])
  exts <- gsub(" ", ";", trimws(partes[3]))  # "*.las *.laz" → "*.las;*.laz"
  matrix(c(desc, exts, "All Files", "*.*"), ncol = 2, byrow = TRUE)
}

elegir_archivo <- function(caption, filtros_rstudio, dir_inicio) {
  if (rstudioapi::isAvailable()) {
    tryCatch(
      rstudioapi::selectFile(
        caption = caption,
        filter  = filtros_rstudio,
        path    = dir_inicio
      ),
      error = function(e) character(0)
    )
  } else {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    tryCatch({
      if (!is.null(dir_inicio) && nzchar(dir_inicio %||% "") && dir.exists(dir_inicio))
        setwd(dir_inicio)
      resultado <- utils::choose.files(
        default = "",
        caption = caption,
        multi   = FALSE,
        filters = .filtro_win(filtros_rstudio),
        index   = 1L
      )
      if (length(resultado) == 0 || !nzchar(resultado[1])) character(0) else resultado[1]
    }, error = function(e) character(0))
  }
}

elegir_carpeta <- function(caption, dir_inicio) {
  if (rstudioapi::isAvailable()) {
    tryCatch(
      rstudioapi::selectDirectory(
        caption = caption,
        path    = dir_inicio
      ),
      error = function(e) NA_character_
    )
  } else {
    tryCatch({
      resultado <- utils::choose.dir(
        default = dir_inicio %||% "",
        caption = caption
      )
      if (is.na(resultado) || !nzchar(resultado)) NA_character_ else resultado
    }, error = function(e) NA_character_)
  }
}

csf_presets <- list(
  llano    = list(rigidness = 3L, threshold = 0.5, cloth_res = 1.0, sloop_smooth = FALSE),
  ondulado = list(rigidness = 2L, threshold = 0.5, cloth_res = 1.0, sloop_smooth = FALSE),
  quebrado = list(rigidness = 1L, threshold = 0.5, cloth_res = 0.5, sloop_smooth = TRUE)
)
