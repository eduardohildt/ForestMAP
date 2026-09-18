# ==============================================================================
# INSTALADOR AUTOMÁTICO - ForestMAP INTA
# ==============================================================================
# Orden de fuentes (nunca se compila desde fuente):
#   1. CRAN          — paquetes con binarios Windows disponibles
#   2. r-universe    — rlas / lidR (binarios pre-compilados más actualizados)
#   3. Pandoc        — requerido por rmarkdown/knitr para renderizar el informe
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  ForestMAP - Instalador de Dependencias\n")
cat("  ForestMAP - Dependency Installer\n")
cat("  INTA EEA Montecarlo - Grupo Forestal\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Forzar binarios globalmente (sin Rtools no se puede compilar)
options(
  install.packages.compile.from.source = "never",
  pkgType = if (.Platform$OS.type == "windows") "win.binary" else "binary"
)

if (.Platform$OS.type == "windows") {
  cat("ℹ️  Windows: solo se instalarán binarios pre-compilados.\n")
  cat("ℹ️  Windows: only pre-compiled binaries will be installed.\n\n")
}

# Si la librería del sistema no admite escritura, usar/crear librería personal
# sin preguntar (evita el prompt interactivo que CMD desatendido no puede responder)
libs <- .libPaths()
if (file.access(libs[1], mode = 2) != 0) {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
  .libPaths(user_lib)
  cat("ℹ️  Librería del sistema no escribible. Usando librería personal:\n   ", user_lib, "\n")
  cat("ℹ️  System library not writable. Using personal library:\n   ", user_lib, "\n\n")
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
    cat("✔  [", label, "] Already installed.\n")
    return(invisible(character(0)))
  }
  cat("📥 [", label, "] Instalando:", paste(needed, collapse = ", "), "\n")
  cat("📥 [", label, "] Installing:", paste(needed, collapse = ", "), "\n")
  install.packages(needed, dependencies = TRUE, repos = repos)
  still_missing <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
  invisible(still_missing)
}

# ------------------------------------------------------------------------------
# Fuente 1 — CRAN (binarios)
# Incluye remotes para poder usar las fuentes siguientes
# ------------------------------------------------------------------------------
pkgs_cran <- c(
  "BH",           # headers Boost (dependencia de rlas/lidR)
  "sf",           # vectorial (GDAL/GEOS/PROJ bundleados en Windows)
  "terra",        # raster/vector moderno
  "shiny",
  "bslib",
  "DT",
  "plotly",
  "RCSF",
  "htmlwidgets",
  "ggplot2",
  "rstudioapi",   # opcional: solo mejora los diálogos de archivo si se corre desde RStudio
  "knitr",
  "pagedown",
  "rmarkdown",
  "pandoc"        # Primera alternativa desde CRAN
)

failed_cran <- install_missing(pkgs_cran, repo_cran, "CRAN")

# ------------------------------------------------------------------------------
# Fuente 2 — r-universe (binarios de rlas/lidR más actualizados que CRAN)
# ------------------------------------------------------------------------------
pkgs_lidar <- c("rlas", "lidR")

failed_lidar <- install_missing(pkgs_lidar, repo_lidar, "r-universe / LiDAR")

# ------------------------------------------------------------------------------
# Fuente 3 — Pandoc (requerido por rmarkdown/knitr para renderizar)
# ------------------------------------------------------------------------------
failed_pandoc <- character(0)
if (!nzchar(Sys.which("pandoc")) && !rmarkdown::pandoc_available()) {
  cat("📥 [Pandoc] Instalando...\n")
  cat("📥 [Pandoc] Installing...\n")
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr", repos = repo_cran)
  }
  if (.Platform$OS.type == "windows") {
    installr::install.pandoc()
  } else {
    cat("⚠️  Instalación automática de Pandoc solo soportada en Windows.\n")
    cat("⚠️  Automatic Pandoc installation is only supported on Windows.\n")
    cat("   Instale manualmente: https://pandoc.org/installing.html\n")
    cat("   Install it manually: https://pandoc.org/installing.html\n")
  }
  if (!nzchar(Sys.which("pandoc")) && !rmarkdown::pandoc_available()) {
    failed_pandoc <- "pandoc"
  }
} else {
  cat("✔  [Pandoc] Ya instalado.\n")
  cat("✔  [Pandoc] Already installed.\n")
}

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
all_failed <- c(failed_cran, failed_lidar, failed_pandoc)

cat("\n")
if (length(all_failed) == 0) {
  cat("✅ Instalación completa.\n")
  cat("✅ Installation complete.\n")
  cat("   Ejecute 'Ejecutar_ForestMap.cmd' para lanzar la aplicación.\n")
  cat("   Run 'Ejecutar_ForestMap.cmd' to launch the application.\n\n")
} else {
  cat("❌ Los siguientes paquetes NO pudieron instalarse:\n")
  cat("❌ The following packages could NOT be installed:\n")
  cat("   ", paste(all_failed, collapse = ", "), "\n\n")
  cat("   Posibles causas:\n")
  cat("   Possible causes:\n")
  cat("   - Sin conexión a internet o repositorio no disponible\n")
  cat("   - No internet connection or repository unavailable\n")
  cat("   - Versión de R incompatible (se recomienda R >= 4.3)\n")
  cat("   - Incompatible R version (R >= 4.3 recommended)\n")
  cat("   - Binario no disponible para esta versión de R\n\n")
  cat("   - Binary not available for this R version\n\n")
  cat("   Consulte al administrador si el problema persiste.\n")
  cat("   Contact your administrator if the problem persists.\n\n")
}

cat("═══════════════════════════════════════════════════════════\n\n")