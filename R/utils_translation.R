# ==============================================================================
# UTILS TRANSLATION — función tr() para lookup de traducciones
# Usa .i18n_flat (inicializado en i18n.R) para lookup directo sin mutación.
# Soporta strings parametrizados: tr("key.with.%d", lang, n)
# ==============================================================================

tr <- function(key, lang = "es", ...) {
  flat <- .i18n_flat[[lang]]
  text <- flat[[key]]
  if (is.null(text)) text <- .i18n_flat[["es"]][[key]]
  if (is.null(text)) text <- key  # fallback: mostrar la clave
  if (length(list(...)) > 0L) sprintf(text, ...) else text
}
