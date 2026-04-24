# ==============================================================================
# PALETA DE COLORES - ForestMap INTA
# ==============================================================================
# Paleta científica optimizada para visualización LiDAR/forestal
# Fondo claro + verde forestal + alta legibilidad
# ==============================================================================

# ──────────────────────────────────────────────────────────────────────────────
# UI PRINCIPAL
# ──────────────────────────────────────────────────────────────────────────────
BG_PRIMARY      <- "#f8faf8"
BG_CARD         <- "#ffffff"
BG_DARK         <- "#eef2ee"

TEXT_PRIMARY    <- "#1a2e1a"
TEXT_SECONDARY  <- "#3d5a3d"
TEXT_MUTED      <- "#6b8a6b"

ACCENT_PRIMARY  <- "#2d7a3d"
ACCENT_HOVER    <- "#1f5f2f"
ACCENT_SECONDARY<- "#52a862"
TEXTO_PIE       <- "#FFFFFF"

BORDER_COLOR    <- "#d4e4d4"
BORDER_LIGHT    <- "#e8f0e8"

STATUS_SUCCESS  <- "#2d7a3d"
STATUS_WARNING  <- "#c87f0a"
STATUS_ERROR    <- "#c73838"

# ──────────────────────────────────────────────────────────────────────────────
# GRADIENTES CIENTÍFICOS
# ──────────────────────────────────────────────────────────────────────────────
# Gradiente TERRENO (DEM/DTM): agua → humedad → suelo → vegetación → dosel
COL_TERRAIN_1 <- "#5D737E"
COL_TERRAIN_2 <- "#468966"
COL_TERRAIN_3 <- "#A3C161"
COL_TERRAIN_4 <- "#E7E247"
COL_TERRAIN_5 <- "#FC913A"
COL_TERRAIN_6 <- "#F34A30"
COL_TERRAIN_7 <- "#ED1C24"

# Gradiente DOSEL (CHM/DSM): sin vegetación → emergentes
COL_CANOPY_1    <- "#5D737E"
COL_CANOPY_2    <- "#468966"
COL_CANOPY_3    <- "#A3C161"
COL_CANOPY_4    <- "#E7E247"
COL_CANOPY_5    <- "#FC913A"
COL_CANOPY_6    <- "#F34A30"
COL_CANOPY_7    <- "#ED1C24"

# Clasificación terreno/vegetación en nube de puntos
COL_TERRENO     <- "#FC913A"
COL_VEGETACION  <- "#2f5d3a"

# ──────────────────────────────────────────────────────────────────────────────
# COLORES ADICIONALES
# ──────────────────────────────────────────────────────────────────────────────
GREEN           <- ACCENT_PRIMARY
GOLD            <- STATUS_WARNING
PURPLE          <- "#7d5ba6"
DARK            <- TEXT_PRIMARY
DEEP            <- "#333533"
BORDER          <- BORDER_COLOR
MUTED           <- TEXT_MUTED
BLACK           <- "#000000"

MARKER_TREE_COLOR <- "#d64545"
MARKER_TREE_LINE  <- "#f4a442"

# ──────────────────────────────────────────────────────────────────────────────
# PALETAS PLOTLY (heatmaps interactivos)
# ──────────────────────────────────────────────────────────────────────────────
PAL_TERRAIN <- list(
  c(0,    COL_TERRAIN_1),
  c(0.16, COL_TERRAIN_2),
  c(0.33, COL_TERRAIN_3),
  c(0.50, COL_TERRAIN_4),
  c(0.66, COL_TERRAIN_5),
  c(0.83, COL_TERRAIN_6),
  c(1,    COL_TERRAIN_7)
)

PAL_CHM <- list(
  c(0,    COL_CANOPY_1),
  c(0.16, COL_CANOPY_2),
  c(0.33, COL_CANOPY_3),
  c(0.50, COL_CANOPY_4),
  c(0.66, COL_CANOPY_5),
  c(0.83, COL_CANOPY_6),
  c(1,    COL_CANOPY_7)
)

PAL_TERRENO_VEGETACION <- list(
  c(0, COL_TERRENO),
  c(1, COL_VEGETACION)
)

PAL_HILLSHADE <- list(
  c(0, "#3d3d3d"),
  c(1, "#ffffff")
)

# ──────────────────────────────────────────────────────────────────────────────
# PALETAS GGPLOT2 (exportación PNG/HTML)
# ──────────────────────────────────────────────────────────────────────────────
PAL_TERRAIN_PNG <- c(COL_TERRAIN_1, COL_TERRAIN_2, COL_TERRAIN_3, 
                     COL_TERRAIN_4, COL_TERRAIN_5, COL_TERRAIN_6, COL_TERRAIN_7)

PAL_CHM_PNG <- c(COL_CANOPY_1, COL_CANOPY_2, COL_CANOPY_3, 
                 COL_CANOPY_4, COL_CANOPY_5, COL_CANOPY_6, COL_CANOPY_7)

PAL_DEM_PNG <- PAL_TERRAIN_PNG  # Alias compatibilidad

# ──────────────────────────────────────────────────────────────────────────────
# ELEMENTOS ESPECÍFICOS
# ──────────────────────────────────────────────────────────────────────────────
HIST_FILL      <- ACCENT_PRIMARY
HIST_BORDER    <- TEXT_PRIMARY
MARKER_COLOR   <- ACCENT_PRIMARY
MARKER_LINE    <- ACCENT_HOVER
MARKER_SIZE    <- 1.2

TABLE_BG       <- BG_CARD
TABLE_TEXT     <- TEXT_PRIMARY

# ──────────────────────────────────────────────────────────────────────────────
# BOOTSTRAP 5 THEME
# ──────────────────────────────────────────────────────────────────────────────
BS_PRIMARY     <- ACCENT_PRIMARY
BS_SECONDARY   <- ACCENT_SECONDARY
BS_DARK        <- TEXT_PRIMARY
BS_LIGHT       <- BG_PRIMARY
BS_DANGER      <- STATUS_ERROR
BS_BODY_BG     <- BG_PRIMARY
BS_BORDER      <- BORDER_COLOR

# ──────────────────────────────────────────────────────────────────────────────
# FUNCIONES AUXILIARES
# ──────────────────────────────────────────────────────────────────────────────
palette_canopy <- function(n = 7, reverse = FALSE) {
  cols <- c(COL_CANOPY_1, COL_CANOPY_2, COL_CANOPY_3, COL_CANOPY_4, 
            COL_CANOPY_5, COL_CANOPY_6, COL_CANOPY_7)
  if (reverse) cols <- rev(cols)
  colorRampPalette(cols)(n)
}

palette_terrain <- function(n = 7, reverse = FALSE) {
  cols <- c(COL_TERRAIN_1, COL_TERRAIN_2, COL_TERRAIN_3, COL_TERRAIN_4, 
            COL_TERRAIN_5, COL_TERRAIN_6, COL_TERRAIN_7)
  if (reverse) cols <- rev(cols)
  colorRampPalette(cols)(n)
}

# ──────────────────────────────────────────────────────────────────────────────
# METADATA
# ──────────────────────────────────────────────────────────────────────────────
THEME_NAME        <- "Scientific Forest Light"
THEME_DESCRIPTION <- "Fondo claro + verde forestal para visualización científica LiDAR/forestal"
THEME_VERSION     <- "5.1"
THEME_DATE        <- "2026-03-26"
