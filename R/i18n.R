# ==============================================================================
# I18N — Carga y fusión de archivos de traducción
# Lee i18n/es.json, en.json, pt.json → expone .i18n_flat para lookup en tr()
# ==============================================================================

library(jsonlite)

# Aplana JSON anidado a lista plana con claves dot-separated
.flatten_json <- function(x, prefix = "") {
  result <- list()
  for (nm in names(x)) {
    key <- if (nchar(prefix) > 0L) paste0(prefix, ".", nm) else nm
    val <- x[[nm]]
    if (is.list(val) && !is.null(names(val))) {
      result <- c(result, .flatten_json(val, key))
    } else {
      result[[key]] <- as.character(val)
    }
  }
  result
}

# Lee y aplana los tres archivos de idioma
.load_flat <- function(base_dir = "i18n") {
  langs <- c("es", "en", "pt")
  flat  <- vector("list", length(langs))
  names(flat) <- langs
  for (l in langs) {
    path <- file.path(base_dir, paste0(l, ".json"))
    if (!file.exists(path)) stop("Archivo de traducción no encontrado: ", path)
    flat[[l]] <- .flatten_json(fromJSON(path, simplifyVector = FALSE))
  }
  # Completar EN y PT con fallback a ES para claves faltantes
  keys <- names(flat[["es"]])
  for (l in c("en", "pt")) {
    missing <- setdiff(keys, names(flat[[l]]))
    for (k in missing) flat[[l]][[k]] <- flat[["es"]][[k]]
  }
  flat
}

# Inicialización — exponer .i18n_flat usado en tr()
.i18n_flat <- .load_flat()
