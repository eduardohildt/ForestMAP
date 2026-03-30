# ==============================================================================
# MÓDULO DE VISUALIZACIÓN
# ==============================================================================
# Funciones para generar gráficos plotly y ggplot2 de nubes de puntos,
# rasters y datos forestales
# ==============================================================================

# ──────────────────────────────────────────────────────────────────────────────
# CONVERSIÓN DE DATOS
# ──────────────────────────────────────────────────────────────────────────────
raster_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "valor"
  df
}

# ──────────────────────────────────────────────────────────────────────────────
# HEATMAPS 2D PLOTLY
# ──────────────────────────────────────────────────────────────────────────────
plotly_raster <- function(df, titulo, colores, legend_title = "Valor") {
  plot_ly(df, x = ~x, y = ~y, z = ~valor,
          type = "heatmap",
          colorscale = colores,
          colorbar   = list(title     = legend_title,
                            tickfont  = list(color=TEXT_PRIMARY),
                            titlefont = list(color=TEXT_PRIMARY)),
          hovertemplate = "X: %{x:.1f}<br>Y: %{y:.1f}<br>Val: %{z:.2f}<extra></extra>") |>
    layout(
      title         = list(text=titulo, font=list(color=ACCENT_PRIMARY, size=13)),
      paper_bgcolor = BG_CARD,
      plot_bgcolor  = BG_CARD,
      xaxis = list(title="X (m)", color=TEXT_PRIMARY, scaleanchor="y", scaleratio=1),
      yaxis = list(title="Y (m)", color=TEXT_PRIMARY),
      margin   = list(l=50,r=20,t=40,b=50),
      dragmode = "zoom"
    ) |>
    config(
      scrollZoom = TRUE,
      displayModeBar = TRUE,
      modeBarButtonsToAdd = list("resetViews"),
      modeBarButtonsToRemove = c("lasso2d","select2d","toImage")
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# VISOR 3D NUBES DE PUNTOS
# ──────────────────────────────────────────────────────────────────────────────
plotly_nube_3d <- function(las_obj, max_pts = 80000,
                           color_var = "Z", titulo = "Nube de Puntos") {
  df <- as.data.frame(las_obj@data)
  if (nrow(df) > max_pts) df <- df[sample(nrow(df), max_pts), ]
  
  if (color_var == "Z") {
    col_vals <- df$Z
    col_name <- "Elevación (m)"
    pal      <- PAL_CHM
  } else {
    col_vals <- ifelse(df$Classification == 2L, 0, 1)
    col_name <- "Clase"
    pal      <- PAL_TERRENO_VEGETACION
  }
  
  plot_ly(
    x = ~df$X, y = ~df$Y, z = ~df$Z,
    type   = "scatter3d", mode = "markers",
    marker = list(
      size       = 1.5,
      color      = col_vals,
      colorscale = pal,
      colorbar   = list(title     = col_name,
                        tickfont  = list(color = TEXT_PRIMARY),
                        titlefont = list(color = TEXT_PRIMARY)),
      opacity    = 0.85
    ),
    hovertemplate = "X:%{x:.1f} Y:%{y:.1f} Z:%{z:.2f}<extra></extra>"
  ) |>
    layout(
      title        = list(text = titulo, font = list(color = ACCENT_PRIMARY, size = 13)),
      paper_bgcolor = BG_CARD,
      scene = list(
        bgcolor    = BG_CARD,
        xaxis      = list(title = "X (m)", color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        yaxis      = list(title = "Y (m)", color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        zaxis      = list(title = "Z (m)", color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        camera     = list(eye = list(x = 1.4, y = -1.4, z = 1.1)),
        aspectmode = "data"
      )
    ) |>
    config(displayModeBar = TRUE)
}

# ──────────────────────────────────────────────────────────────────────────────
# HELPERS UI
# ──────────────────────────────────────────────────────────────────────────────
tip_label <- function(label_text, tip_text) {
  tags$div(style = "margin-bottom:2px;",
    tags$span(style = paste0("color:", TEXT_PRIMARY, "; font-size:12px;"), label_text),
    tags$span(style = "cursor:help; margin-left:5px;",
              title = tip_text,
              tags$span(style = paste0("color:", GOLD, "; font-size:11px;
                background:", BORDER_COLOR, "; border-radius:50%;
                padding:0 4px; font-weight:bold;"), "?"))
  )
}
