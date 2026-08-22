# ==============================================================================
# INSTALADOR AUTOMÁTICO - ForestMAP INTA
# ==============================================================================
# Orden de fuentes (nunca se compila desde fuente):
#   1. CRAN          — paquetes con binarios Windows disponibles
#   2. r-universe    — rlas / lidR (binarios pre-compilados más actualizados)
#   3. GitHub        — shiny.i18n (R puro, sin compilación)
#   4. Bioconductor  — EBImage (binarios via BiocManager)
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - Instalador de Dependencias\n")
cat("  INTA EEA Montecarlo - Grupo Forestal\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Forzar binarios globalmente (sin Rtools no se puede compilar)
options(
  install.packages.compile.from.source = "never",
  pkgType = if (.Platform$OS.type == "windows") "win.binary" else "binary"
)

if (.Platform$OS.type == "windows") {
  cat("ℹ️  Windows: solo se instalarán binarios pre-compilados.\n\n")
}

# Si la librería del sistema no admite escritura, usar/crear librería personal
# sin preguntar (evita el prompt interactivo que CMD desatendido no puede responder)
libs <- .libPaths()
if (file.access(libs[1], mode = 2) != 0) {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
  .libPaths(user_lib)
  cat("ℹ️  Librería del sistema no escribible. Usando librería personal:\n   ", user_lib, "\n\n")
}

# ------------------------------------------------------------------------------
# Repositorios
# ------------------------------------------------------------------------------
repo_cran    <- c(CRAN  = "https://cloud.r-project.org")
repo_lidar   <- c(rlidar = "https://r-lidar.r-universe.dev",
                  CRAN   = "https://cloud.r-project.org")

# ------------------------------------------------------------------------------
# Helper
# ------------------------------------------------------------------------------
install_missing <- function(pkgs, repos, label) {
  needed <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(needed) == 0) {
    cat("✔  [", label, "] Todo ya instalado.\n")
    return(invisible(character(0)))
  }
  cat("📥 [", label, "] Instalando:", paste(needed, collapse = ", "), "\n")
  install.packages(needed, dependencies = TRUE, repos = repos)
  still_missing <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
  invisible(still_missing)
}

# ------------------------------------------------------------------------------
# Fuente 1 — CRAN (binarios)
# Incluye remotes para poder usar las fuentes siguientes
# ------------------------------------------------------------------------------
pkgs_cran <- c(
  "remotes",      # necesario para GitHub
  "BH",           # headers Boost (dependencia de rlas/lidR)
  "sf",           # vectorial (GDAL/GEOS/PROJ bundleados en Windows)
  "terra",        # raster/vector moderno
  "shiny",
  "shiny.i18n",   # Primera alternativa desde CRAN
  "bslib",
  "DT",
  "plotly",
  "RCSF",
  "htmlwidgets",
  "ggplot2",
  "rstudioapi",
  "knitr",
  "pagedown",
  "rmarkdown",
  "pandoc",      # Primera alternativa desde CRAN
  "BiocManager"  # Primera alternativa desde CRAN
)

failed_cran <- install_missing(pkgs_cran, repo_cran, "CRAN")

# ------------------------------------------------------------------------------
# Fuente 2 — r-universe (binarios de rlas/lidR más actualizados que CRAN)
# ------------------------------------------------------------------------------
pkgs_lidar <- c("rlas", "lidR")

failed_lidar <- install_missing(pkgs_lidar, repo_lidar, "r-universe / LiDAR")

# ------------------------------------------------------------------------------
# Fuente 3 — GitHub (shiny.i18n: R puro, sin compilación)
# ------------------------------------------------------------------------------
failed_github <- character(0)
if (!requireNamespace("shiny.i18n", quietly = TRUE)) {
  cat("📥 [GitHub] Instalando: shiny.i18n\n")
  remotes::install_github("Appsilon/shiny.i18n", upgrade = "never")
  if (!requireNamespace("shiny.i18n", quietly = TRUE)) {
    failed_github <- "shiny.i18n"
  }
} else {
  cat("✔  [GitHub] shiny.i18n ya instalado.\n")
}

# ------------------------------------------------------------------------------
# Fuente 4 — Bioconductor (EBImage, binarios via BiocManager)
# ------------------------------------------------------------------------------
pkgs_bioc   <- c("EBImage")
failed_bioc <- character(0)
needed_bioc <- pkgs_bioc[!sapply(pkgs_bioc, requireNamespace, quietly = TRUE)]

if (length(needed_bioc) > 0) {
  cat("📥 [Bioconductor] Instalando:", paste(needed_bioc, collapse = ", "), "\n")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = repo_cran)
  }
  BiocManager::install(needed_bioc, ask = FALSE, update = FALSE)
  failed_bioc <- needed_bioc[!sapply(needed_bioc, requireNamespace, quietly = TRUE)]
} else {
  cat("✔  [Bioconductor] EBImage ya instalado.\n")
}

# ------------------------------------------------------------------------------
# Fuente 5 — Pandoc (requerido por rmarkdown/knitr para renderizar)
# ------------------------------------------------------------------------------
failed_pandoc <- character(0)
if (!nzchar(Sys.which("pandoc")) && !rmarkdown::pandoc_available()) {
  cat("📥 [Pandoc] Instalando...\n")
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr", repos = repo_cran)
  }
  if (.Platform$OS.type == "windows") {
    installr::install.pandoc()
  } else {
    cat("⚠️  Instalación automática de Pandoc solo soportada en Windows.\n")
    cat("   Instale manualmente: https://pandoc.org/installing.html\n")
  }
  if (!nzchar(Sys.which("pandoc")) && !rmarkdown::pandoc_available()) {
    failed_pandoc <- "pandoc"
  }
} else {
  cat("✔  [Pandoc] Ya instalado.\n")
}

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
all_failed <- c(failed_cran, failed_lidar, failed_github, failed_bioc, failed_pandoc)

cat("\n")
if (length(all_failed) == 0) {
  cat("✅ Instalación completa.\n")
  cat("   Ejecute 'Ejecutar_ForestMap.cmd' para lanzar la aplicación.\n\n")
} else {
  cat("❌ Los siguientes paquetes NO pudieron instalarse:\n")
  cat("   ", paste(all_failed, collapse = ", "), "\n\n")
  cat("   Posibles causas:\n")
  cat("   - Sin conexión a internet o repositorio no disponible\n")
  cat("   - Versión de R incompatible (se recomienda R >= 4.3)\n")
  cat("   - Binario no disponible para esta versión de R\n\n")
  cat("   Consulte al administrador si el problema persiste.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")