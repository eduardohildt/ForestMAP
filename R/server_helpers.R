# ==============================================================================
# SERVER HELPERS - Utilidades puras del servidor (sin dependencias reactivas)
# Contiene: operador %||%, diálogos de archivo/carpeta, presets CSF
# ==============================================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) || identical(a, "")) b else a
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
    tryCatch({
      tcltk::tkfocus(tcltk::tktoplevel())
      tcltk::tk_choose.files(
        default = dir_inicio,
        caption = caption,
        multi   = FALSE
      )
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
      tcltk::tk_choose.dir(default = dir_inicio, caption = caption)
    }, error = function(e) NA_character_)
  }
}

csf_presets <- list(
  llano    = list(rigidness = 3L, threshold = 0.5, cloth_res = 1.0, sloop_smooth = FALSE),
  ondulado = list(rigidness = 2L, threshold = 0.5, cloth_res = 1.0, sloop_smooth = FALSE),
  quebrado = list(rigidness = 1L, threshold = 0.5, cloth_res = 0.5, sloop_smooth = TRUE)
)
