# ==============================================================================
# INSTALADOR AUTOMÁTICO - ForestMAP INTA
# ==============================================================================
# Instala todas las dependencias necesarias desde CRAN o desde binarios (06/08/26 por caída de lidR y rlas de CRAN)
# Ejecutar una sola vez antes del primer uso
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - Instalador de Dependencias\n")
cat("  INTA EEA Montecarlo - Grupo Forestal\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Lista completa de paquetes requeridos
required_packages <- c(
  "remotes",      # Instalación de paquetes desde codigo fuente
  "shiny",        # Framework web interactivo
  "shiny.i18n",   # Paquete para traducciones
  "bslib",        # Bootstrap 5 para Shiny
  "DT",           # Tablas interactivas DataTables
  "plotly",       # Gráficos interactivos 3D/2D
  "rlas",         # Procesamiento LiDAR
  "lidR",         # Procesamiento LiDAR
  "terra",        # Manipulación raster/vector moderna
  "sf",           # Geometrías vectoriales (simple features)
  "RCSF",         # Cloth Simulation Filter (clasificación suelo)
  "htmlwidgets",  # Widgets HTML/JS en R
  "ggplot2",      # Gráficos estáticos
  "parallel",     # Procesamiento paralelo (base R)
  "rstudioapi",   # Integración con RStudio
  "knitr",        # Generación de informes
  "pagedown",     # Transformación de HTML a PDF usando motor Chromium
  "rmarkdown"     # Renderizado HTML
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
  
  # r-lidar.r-universe.dev provee binarios de lidR/rlas aunque estén
  # archivados en CRAN (evita compilación local)
  repos <- c(
    rlidar = "https://r-lidar.r-universe.dev",
    CRAN   = "https://cloud.r-project.org"
  )
  
  install.packages(
    missing_packages,
    dependencies = TRUE,
    repos = repos
  )
  
  # Fallback: si lidR/rlas siguen sin instalarse, usar GitHub (requiere
  # herramientas de compilación: Rtools en Windows, Xcode CLT en Mac)
  still_missing <- missing_packages[
    !sapply(missing_packages, requireNamespace, quietly = TRUE)
  ]
  if (any(c("rlas", "lidR") %in% still_missing)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes", repos = "https://cloud.r-project.org")
    }
    cat("\n⚠️  Instalando lidR/rlas desde GitHub (fallback, requiere compilador)...\n")
    remotes::install_github("r-lidar/rlas")
    remotes::install_github("r-lidar/lidR")
  }
  
  cat("\n✅ Instalación completa.\n")
  cat("   Ejecute 'Rscript run.R' o abra 'run.R' en RStudio.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")

