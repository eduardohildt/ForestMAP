# ==============================================================================
# INSTALADOR AUTOMÁTICO - ForestMAP INTA
# ==============================================================================
# Instala todas las dependencias necesarias desde CRAN
# Ejecutar una sola vez antes del primer uso
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - Instalador de Dependencias\n")
cat("  INTA EEA Montecarlo - Grupo Forestal\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Lista completa de paquetes requeridos
required_packages <- c(
  "shiny",        # Framework web interactivo
  "bslib",        # Bootstrap 5 para Shiny
  "DT",           # Tablas interactivas DataTables
  "plotly",       # Gráficos interactivos 3D/2D
  "lidR",         # Procesamiento LiDAR
  "terra",        # Manipulación raster/vector moderna
  "sf",           # Geometrías vectoriales (simple features)
  "RCSF",         # Cloth Simulation Filter (clasificación suelo)
  "htmlwidgets",  # Widgets HTML/JS en R
  "ggplot2",      # Gráficos estáticos
  "parallel",     # Procesamiento paralelo (base R)
  "rstudioapi",   # Integración con RStudio
  "knitr",        # Generación de informes
  "rmarkdown",    # Renderizado PDF/HTML
  "fancyhdr"      # Renderizado PDF/HTML
)

cat("📦 Verificando", length(required_packages), "paquetes...\n\n")

# Detectar paquetes faltantes
missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) == 0) {
  cat("✅ Todas las dependencias ya están instaladas.\n")
  cat("   Ejecute 'Rscript run.R' o abra 'app.R' en RStudio.\n\n")
} else {
  cat("📥 Instalando", length(missing_packages), "paquetes faltantes:\n")
  cat("   ", paste(missing_packages, collapse = ", "), "\n\n")
  
  install.packages(
    missing_packages,
    dependencies = TRUE,
    repos = "https://cloud.r-project.org"
  )
  
  cat("\n✅ Instalación completa.\n")
  cat("   Ejecute 'Rscript run.R' o abra 'app.R' en RStudio.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")
