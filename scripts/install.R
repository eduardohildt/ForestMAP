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
  "tinytex"       # Paquete que contiene herramientas para renderizas con LaTeX
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


# ------------------------------------------------------------------------------
# Verificar e instalar distribución LaTeX (TinyTeX) si no está disponible
# ------------------------------------------------------------------------------
cat("🔍 Verificando instalación de LaTeX...\n")

latex_available <- nchar(Sys.which("xelatex")) > 0 || 
  nchar(Sys.which("pdflatex")) > 0

if (!latex_available) {
  cat("📥 LaTeX no detectado. Instalando TinyTeX...\n")
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    install.packages("tinytex", repos = "https://cloud.r-project.org")
  }
  
  os <- .Platform$OS.type
  sysname <- Sys.info()[["sysname"]]
  
  # Descargar e instalar manualmente desde GitHub Releases (URL estable)
  tinytex_version <- "2026.03.02"
  tmpdir <- tempdir()
  
  if (sysname == "Windows") {
    url  <- paste0("https://github.com/rstudio/tinytex-releases/releases/download/v",
                   tinytex_version, "/TinyTeX-1-v", tinytex_version, ".zip")
    dest <- file.path(tmpdir, "TinyTeX-1.zip")
    download.file(url, dest, mode = "wb")
    tinytex:::install_prebuilt(dest)
    
  } else if (sysname == "Darwin") {
    url  <- paste0("https://github.com/rstudio/tinytex-releases/releases/download/v",
                   tinytex_version, "/TinyTeX-1-v", tinytex_version, ".tgz")
    dest <- file.path(tmpdir, "TinyTeX-1.tgz")
    download.file(url, dest, mode = "wb")
    tinytex:::install_prebuilt(dest)
    
  } else {
    url  <- paste0("https://github.com/rstudio/tinytex-releases/releases/download/v",
                   tinytex_version, "/TinyTeX-1-v", tinytex_version, ".tar.gz")
    dest <- file.path(tmpdir, "TinyTeX-1.tar.gz")
    download.file(url, dest, mode = "wb")
    tinytex:::install_prebuilt(dest)
  }
  
  cat("✅ TinyTeX instalado. Reinicie R antes de generar informes PDF.\n\n")
} else {
  cat("✅ LaTeX ya disponible en el sistema.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")

