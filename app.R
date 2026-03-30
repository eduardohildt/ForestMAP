# ==============================================================================
# ForestMAP - Herramienta INTA para Análisis LiDAR y Fotogrametría Forestal
# ==============================================================================
# Autor      : Dr. Eduardo Hildt
# Institución: INTA EEA Montecarlo - Grupo Forestal
# Email      : hildt.eduardo@inta.gob.ar
# Versión    : 2026.5
# ==============================================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  INTA ForestMap - Cargando aplicación...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# LIBRERÍAS
# ══════════════════════════════════════════════════════════════════════════════
cat("📦 Cargando bibliotecas...\n")
library(shiny)
library(rstudioapi)
library(bslib)
library(DT)
library(plotly)
library(lidR)
library(terra)
library(sf)
library(RCSF)
library(htmlwidgets)
library(ggplot2)
library(parallel)
cat("   ✓ Bibliotecas cargadas\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURACIÓN GLOBAL
# ══════════════════════════════════════════════════════════════════════════════
cat("⚙️  Configuración global...\n")
options(
  shiny.maxRequestSize = 25 * 1024^3,
  shiny.launch.browser = TRUE
)

N_CORES_MAX <- parallel::detectCores()
cat("   ✓ Núcleos CPU detectados:", N_CORES_MAX, "\n")
cat("   ✓ Tamaño máximo archivo: 25 GB\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# DIRECTORIO DE TRABAJO
# ══════════════════════════════════════════════════════════════════════════════
script_dir <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) getwd()
)

if (!is.null(script_dir) && script_dir != "" && dir.exists(script_dir)) {
  setwd(script_dir)
}

cat("📁 Directorio:", getwd(), "\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# HELPER: CARGAR ARCHIVOS
# ══════════════════════════════════════════════════════════════════════════════
cargar_archivo <- function(nombre_archivo, obligatorio = TRUE) {
  if (file.exists(nombre_archivo)) {
    source(nombre_archivo, encoding = "UTF-8", local = FALSE)
    cat(sprintf("   ✓ %s\n", nombre_archivo))
    return(TRUE)
  } else {
    if (obligatorio) {
      cat(sprintf("   ✗ %s - NO ENCONTRADO\n", nombre_archivo))
      stop(sprintf("Error: No se encontró el archivo obligatorio: %s", nombre_archivo))
    } else {
      cat(sprintf("   ⚠ %s - no encontrado (opcional)\n", nombre_archivo))
      return(FALSE)
    }
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# CARGAR COMPONENTES
# ══════════════════════════════════════════════════════════════════════════════
cat("🎨 Cargando paleta de colores...\n")
cargar_archivo("R/colors_light.R", obligatorio = TRUE)
cat("\n")

cat("🔧 Cargando módulos de procesamiento...\n")
cargar_archivo("R/processing.R", obligatorio = TRUE)
cargar_archivo("R/visualization.R", obligatorio = TRUE)
cargar_archivo("R/export.R", obligatorio = TRUE)
cat("\n")

cat("📄 Cargando generador de informes...\n")
cargar_archivo("R/report_generator.R", obligatorio = FALSE)
cat("\n")

cat("🎨 Cargando interfaz de usuario...\n")
cargar_archivo("R/ui.R", obligatorio = TRUE)
cat("\n")

cat("⚙️  Cargando lógica del servidor...\n")
cargar_archivo("R/server.R", obligatorio = TRUE)
cat("\n")

# ══════════════════════════════════════════════════════════════════════════════
# LANZAR APLICACIÓN
# ══════════════════════════════════════════════════════════════════════════════
cat("═══════════════════════════════════════════════════════════\n")
cat("  ✅ Todo cargado correctamente\n")
cat("  🚀 Lanzando aplicación...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

shinyApp(ui = ui, server = server)
