# ==============================================================================
# LAUNCHER - ForestMAP INTA
# ==============================================================================
# Ejecutar desde terminal: Rscript run.R
# O abrir app.R directamente en RStudio 
# ==============================================================================
  
cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - INTA EEA Montecarlo\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Detectar ruta del script en ejecución (run.R está en dev/, la raíz es un nivel arriba)
if (interactive()) {
  if ("rstudioapi" %in% rownames(installed.packages())) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (script_path != "") {
      setwd(dirname(dirname(normalizePath(script_path))))
    }
  }
}

# Verificar paquetes críticos
required <- c("shiny", "lidR", "terra", "sf", "RCSF", "plotly")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  cat("❌ ERROR: Paquetes faltantes:\n")
  cat("   ", paste(missing, collapse = ", "), "\n\n")
  cat("   Ejecute primero: Rscript dev/install.R\n\n")
  stop("Dependencias faltantes")
}

# Forzar la detección del número de nucleos existentes en la CPU
N_CORES_MAX <- parallel::detectCores()

# Configurar opciones
options(
  shiny.port = 3838,
  shiny.launch.browser = TRUE,
  shiny.autoreload = TRUE,
  shiny.maxRequestSize = 25 * 1024^3
)

# Lanzar aplicación
shiny::runApp("app.R")
