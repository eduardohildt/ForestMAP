# ==============================================================================
# INSTALADOR AUTOMÁTICO - ForestMAP INTA
# ==============================================================================
# Instala todas las dependencias necesarias desde CRAN o desde binarios.
# BH, sf y terra se instalan primero desde CRAN puro para garantizar binarios.
# rlas y lidR se obtienen de r-lidar.r-universe.dev (binarios pre-compilados).
# Ejecutar una sola vez antes del primer uso.
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - Instalador de Dependencias\n")
cat("  INTA EEA Montecarlo - Grupo Forestal\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# En Windows, nunca compilar desde fuente (requeriría Rtools)
if (.Platform$OS.type == "windows") {
  options(install.packages.compile.from.source = "never")
  cat("ℹ️  Windows detectado: se instalarán solo binarios pre-compilados.\n\n")
}

repo_cran    <- c(CRAN = "https://cloud.r-project.org")
repo_lidar   <- c(rlidar = "https://r-lidar.r-universe.dev",
                  CRAN   = "https://cloud.r-project.org")

# ------------------------------------------------------------
# Fase 1: dependencias pesadas — instalar desde CRAN puro
# para garantizar binarios (no dejar que r-universe las sirva
# como fuente al resolver dependencias de lidR).
# ------------------------------------------------------------
phase1 <- c(
  "BH",    # Headers C++ de Boost (dependencia de rlas/lidR)
  "sf",    # Geometrías vectoriales (GDAL/GEOS/PROJ bundleados en Windows)
  "terra"  # Raster/vector moderno
)

# ------------------------------------------------------------
# Fase 2: resto de paquetes de aplicación
# ------------------------------------------------------------
phase2 <- c(
  "remotes",      # Instalación desde repositorios alternativos
  "shiny",        # Framework web interactivo
  "bslib",        # Bootstrap 5 para Shiny
  "DT",           # Tablas interactivas DataTables
  "plotly",       # Gráficos interactivos 3D/2D
  "RCSF",         # Cloth Simulation Filter (clasificación suelo)
  "htmlwidgets",  # Widgets HTML/JS en R
  "ggplot2",      # Gráficos estáticos
  "parallel",     # Procesamiento paralelo (base R)
  "rstudioapi",   # Integración con RStudio
  "knitr",        # Generación de informes
  "pagedown",     # HTML a PDF via Chromium
  "rmarkdown"     # Renderizado HTML
)

# ------------------------------------------------------------
# Fase 2b: GitHub — paquetes R puro no disponibles en CRAN
# No requieren compilación (sin código C/C++)
# ------------------------------------------------------------
failed2b <- character(0)
if (!requireNamespace("shiny.i18n", quietly = TRUE)) {
  cat("📥 [Fase 2b - GitHub] Instalando: shiny.i18n\n")
  # remotes ya debería estar instalado desde fase 2
  remotes::install_github("Appsilon/shiny.i18n", upgrade = "never")
  if (!requireNamespace("shiny.i18n", quietly = TRUE)) {
    failed2b <- c(failed2b, "shiny.i18n")
  }
}

# ------------------------------------------------------------
# Fase 3: LiDAR — binarios desde r-universe
# rlas va antes que lidR (es su dependencia directa)
# ------------------------------------------------------------
phase3 <- c("rlas", "lidR")

# ------------------------------------------------------------
# Fase 4: Bioconductor — EBImage (procesamiento de imágenes)
# Bioconductor provee binarios Windows via BiocManager
# ------------------------------------------------------------
phase4_bioc <- c("EBImage")

all_packages <- c(phase1, phase2, "shiny.i18n", phase3, phase4_bioc)

cat("📦 Verificando", length(all_packages), "paquetes...\n\n")

# Helper: instala solo los faltantes de un vector dado
install_missing <- function(pkgs, repos, label) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) == 0) return(invisible(character(0)))
  cat("📥 [", label, "] Instalando:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, dependencies = TRUE, repos = repos)
  still <- missing[!sapply(missing, requireNamespace, quietly = TRUE)]
  invisible(still)
}

# Ejecutar las tres fases en orden
failed1 <- install_missing(phase1, repo_cran,  "Fase 1 - CRAN")
failed2 <- install_missing(phase2, repo_cran,  "Fase 2 - CRAN")
failed3 <- install_missing(phase3, repo_lidar, "Fase 3 - r-universe")

# Fase 4: Bioconductor (EBImage y dependencias)
failed4 <- character(0)
bioc_missing <- phase4_bioc[!sapply(phase4_bioc, requireNamespace, quietly = TRUE)]
if (length(bioc_missing) > 0) {
  cat("📥 [Fase 4 - Bioconductor] Instalando:", paste(bioc_missing, collapse = ", "), "\n")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = repo_cran)
  }
  BiocManager::install(bioc_missing, ask = FALSE, update = FALSE)
  failed4 <- bioc_missing[!sapply(bioc_missing, requireNamespace, quietly = TRUE)]
}

all_failed <- c(failed1, failed2, failed2b, failed3, failed4)

cat("\n")
if (length(all_failed) == 0) {
  cat("✅ Instalación completa.\n")
  cat("   Ejecute 'Rscript run.R' o abra 'run.R' en RStudio.\n\n")
} else {
  cat("❌ Los siguientes paquetes NO pudieron instalarse:\n")
  cat("   ", paste(all_failed, collapse = ", "), "\n\n")
  cat("   Posibles causas:\n")
  cat("   - Sin conexión a internet o repositorio no disponible\n")
  cat("   - Versión de R incompatible (se recomienda R >= 4.3)\n")
  cat("   - Paquete temporalmente fuera de CRAN o r-universe\n\n")
  cat("   Consulte al administrador si el problema persiste.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")