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
plotly_raster <- function(df, titulo, colores, legend_title = "Valor", lang = "es") {
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
      xaxis = list(title = tr("plot.axis.x_m", lang), color=TEXT_PRIMARY, scaleanchor="y", scaleratio=1),
      yaxis = list(title = tr("plot.axis.y_m", lang), color=TEXT_PRIMARY),
      margin   = list(l=50,r=20,t=40,b=50),
      dragmode = "pan"
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
plotly_nube_3d <- function(las_obj, max_pts = 120000,
                           color_var = "Z", titulo = "Nube de Puntos", lang = "es") {
  df <- as.data.frame(las_obj@data)
  if (nrow(df) > max_pts) df <- df[sample(nrow(df), max_pts), ]

  if (color_var == "Z") {
    col_vals <- df$Z
    col_name <- tr("plot.colorbar.elevation", lang)
    pal      <- PAL_CHM
  } else {
    col_vals <- ifelse(df$Classification == 2L, 0, 1)
    col_name <- tr("plot.colorbar.class", lang)
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
        xaxis      = list(title = tr("plot.axis.x_m", lang), color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        yaxis      = list(title = tr("plot.axis.y_m", lang), color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        zaxis      = list(title = tr("plot.axis.z_m", lang), color = TEXT_PRIMARY,
                          gridcolor = BORDER_COLOR, zerolinecolor = BORDER_COLOR),
        camera     = list(eye = list(x = 1.4, y = -1.4, z = 1.1)),
        aspectmode = "data"
      )
    ) |>
    config(displayModeBar = TRUE)
}

# ──────────────────────────────────────────────────────────────────────────────
# MAPAS WEB (LEAFLET)
# ──────────────────────────────────────────────────────────────────────────────
# Registro único de proveedores de mapa base. Para agregar o quitar un proveedor
# basta con editar esta lista y su clave map.layer.<id> en los archivos i18n.
# El primero de la lista es el mapa base por defecto.
# Solo proveedores sin API key y con licencia abierta.
MAPAS_BASE <- list(
  list(
    id              = "ign",
    url             = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG%3A3857@png/{z}/{x}/{-y}.png",
    attribution     = '<a href="https://www.ign.gob.ar/AreaServicios/Argenmap/Introduccion" target="_blank">Instituto Geográfico Nacional</a> (CC BY 4.0)',
    max_native_zoom = 18
  ),
  list(
    id              = "osm",
    url             = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution     = '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a> contributors (ODbL)',
    max_native_zoom = 19
  ),
  list(
    id              = "satellite",
    url             = "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless_3857/default/g/{z}/{y}/{x}.jpg",
    attribution     = '<a href="https://s2maps.eu" target="_blank">Sentinel-2 cloudless</a> by EOX IT Services GmbH (Contains modified Copernicus Sentinel data 2016, CC BY 4.0)',
    max_native_zoom = 18
  )
)

MAPA_ZOOM_MAX       <- 21
MAPA_RASTER_MAX_CEL <- 2e6   # celdas máximas del raster dibujado
MAPA_QUERY_MAX_CEL  <- 5e5   # celdas máximas del raster de consulta de valores
MAPA_RASTER_ZINDEX  <- 100   # los mapas base usan el zIndex por defecto (1)

# Agrega al mapa todos los proveedores de MAPAS_BASE, el selector de capas y el
# aviso de fondo no disponible (sin conexión o servicio caído).
agregar_mapas_base <- function(map, lang = "es") {
  grupos <- vapply(MAPAS_BASE, function(p) tr(paste0("map.layer.", p$id), lang), "")
  for (i in seq_along(MAPAS_BASE)) {
    p   <- MAPAS_BASE[[i]]
    map <- leaflet::addTiles(map,
      urlTemplate = p$url,
      attribution = p$attribution,
      group       = grupos[i],
      options     = leaflet::tileOptions(maxNativeZoom = p$max_native_zoom,
                                         maxZoom       = MAPA_ZOOM_MAX))
  }
  if (length(grupos) > 1) {
    map <- map |>
      leaflet::addLayersControl(baseGroups = grupos,
                                position   = "topleft",
                                options    = leaflet::layersControlOptions(collapsed = TRUE)) |>
      leaflet::hideGroup(grupos[-1])
  }
  htmlwidgets::onRender(map, "
    function(el, x, data) {
      var map = this, visible = false;
      var aviso = L.control({position: 'bottomleft'});
      aviso.onAdd = function() {
        var d = L.DomUtil.create('div', 'fm-map-offline');
        d.innerHTML = data.msg;
        d.style.cssText = 'background:rgba(255,255,255,.92);padding:3px 8px;border-radius:4px;' +
                          'font-size:11px;color:#8a5a00;border:1px solid #e0c080;';
        return d;
      };
      function enganchar(capa) {
        if (!(capa instanceof L.TileLayer) || capa._fmAviso) return;
        capa._fmAviso = true;
        capa.on('tileerror', function() { if (!visible) { aviso.addTo(map); visible = true; } });
        capa.on('tileload',  function() { if (visible)  { aviso.remove();   visible = false; } });
      }
      map.eachLayer(enganchar);
      map.on('layeradd', function(e) { enganchar(e.layer); });
    }", data = list(msg = tr("map.offline_notice", lang)))
}

# Mapa vacío con los mapas base: punto de partida de todas las vistas.
leaflet_base <- function(lang = "es") {
  leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE,
                                                     maxZoom      = MAPA_ZOOM_MAX)) |>
    agregar_mapas_base(lang) |>
    leaflet::addScaleBar(position = "bottomleft",
                         options  = leaflet::scaleBarOptions(imperial = FALSE))
}

# TRUE si el objeto (sf, SpatRaster o SpatVector) tiene un CRS utilizable.
tiene_crs <- function(x) {
  if (is.null(x)) return(FALSE)
  if (inherits(x, c("SpatRaster", "SpatVector"))) return(nzchar(terra::crs(x)))
  !is.na(sf::st_crs(x))
}

# Convierte sf / SpatVector a sf en EPSG:4326 (sin Z/M).
a_wgs84 <- function(x) {
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  sf::st_transform(sf::st_zm(x), 4326)
}

# Reduce la resolución hasta no superar max_celdas.
limitar_celdas <- function(r, max_celdas, fun = "mean") {
  fact <- ceiling(sqrt(terra::ncell(r) / max_celdas))
  if (fact > 1) terra::aggregate(r, fact = fact, fun = fun, na.rm = TRUE) else r
}

leaflet_titulo <- function(map, texto) {
  leaflet::addControl(map,
    html     = tags$div(style = paste0("font-weight:600;font-size:12px;color:", ACCENT_PRIMARY, ";"), texto),
    position = "topright", layerId = "titulo", className = "info legend")
}

# Raster con paleta continua, leyenda y valor del píxel bajo el cursor.
# `capa` identifica la capa y rotula el valor consultado; debe ser única entre
# los mapas de la app porque leafem guarda los valores en un objeto JS global.
leaflet_raster <- function(map, r, colores, titulo_leyenda, capa = titulo_leyenda,
                           method = "bilinear", opacity = 0.85, digits = 2) {
  r   <- limitar_celdas(r[[1]], MAPA_RASTER_MAX_CEL)
  rng <- as.numeric(unlist(terra::global(r, c("min", "max"), na.rm = TRUE)))
  # margen mínimo: la reproyección pasa a float32 y puede rozar fuera del rango
  rng <- rng + c(-1, 1) * max(1e-9, 1e-6 * max(abs(rng)))
  pal     <- leaflet::colorNumeric(colores, domain = rng, na.color = "transparent")
  pal_inv <- leaflet::colorNumeric(colores, domain = rng, na.color = "transparent", reverse = TRUE)
  map |>
    leaflet::addRasterImage(r, colors = pal, opacity = opacity, method = method,
                            group = capa, layerId = capa,
                            maxBytes = 4 * MAPA_RASTER_MAX_CEL * 1.5,
                            # comparte panel con los mapas base: zIndex mayor para
                            # que el fondo no lo tape al cambiar de proveedor
                            options  = leaflet::gridOptions(zIndex = MAPA_RASTER_ZINDEX)) |>
    leafem::addImageQuery(limitar_celdas(r, MAPA_QUERY_MAX_CEL),
                          group = capa, layerId = capa, digits = digits,
                          prefix = "", position = "topright",
                          className = "fm-query") |>
    leaflet::addLegend(pal = pal_inv, values = rng, title = titulo_leyenda,
                       opacity = 1, position = "bottomright",
                       labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
}

# El ROI de "extensión completa" se crea sin CRS: se toma el del raster de referencia.
roi_con_crs <- function(roi, ref = NULL) {
  if (is.null(roi) || tiene_crs(roi) || is.null(ref) || !tiene_crs(ref)) return(roi)
  terra::crs(roi) <- terra::crs(ref)
  roi
}

# Contorno del área de análisis.
leaflet_roi <- function(map, roi, relleno = NA, opacidad_relleno = 0) {
  if (is.null(roi) || !tiene_crs(roi)) return(map)
  leaflet::addPolygons(map, data = a_wgs84(roi),
                       color = ACCENT_PRIMARY, weight = 2, opacity = 1,
                       fill = !is.na(relleno), fillColor = relleno,
                       fillOpacity = opacidad_relleno, group = "roi")
}

# ── Vistas ────────────────────────────────────────────────────────────────────
mapa_shp <- function(sf_all, idx, titulo, lang = "es") {
  if (!tiene_crs(sf_all)) return(NULL)
  sf_all <- a_wgs84(sf_all)
  leaflet_base(lang) |>
    leaflet::addPolygons(data = sf_all, color = "#888888", weight = 1,
                         fillColor = "#e8f4e8", fillOpacity = 0.25, group = "todos") |>
    mapa_shp_seleccion(sf_all, idx, titulo)
}

# Resalta el polígono seleccionado y encuadra el mapa (también usado con leafletProxy).
mapa_shp_seleccion <- function(map, sf_all, idx, titulo) {
  sel <- sf_all[min(idx, nrow(sf_all)), ]
  bb  <- as.numeric(sf::st_bbox(sel))
  map |>
    leaflet::clearGroup("seleccion") |>
    leaflet::addPolygons(data = sel, color = ACCENT_PRIMARY, weight = 3,
                         fillColor = ACCENT_PRIMARY, fillOpacity = 0.3, group = "seleccion") |>
    leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4]) |>
    leaflet_titulo(titulo)
}

mapa_raster <- function(r, colores, titulo, titulo_leyenda, roi = NULL,
                        opacity = 0.85, lang = "es") {
  if (!tiene_crs(r)) return(NULL)
  leaflet_base(lang) |>
    leaflet_raster(r, colores, titulo_leyenda, capa = titulo, opacity = opacity) |>
    leaflet_roi(roi_con_crs(roi, r)) |>
    leaflet_titulo(titulo)
}

mapa_curvas <- function(dem, curvas, titulo, titulo_leyenda, roi = NULL, lang = "es") {
  if (!tiene_crs(dem)) return(NULL)
  m <- leaflet_base(lang) |>
    leaflet_raster(dem, PAL_TERRAIN_PNG, titulo_leyenda, capa = titulo, opacity = 0.7)
  if (!is.null(curvas) && tiene_crs(curvas)) {
    cv <- a_wgs84(curvas)
    m  <- leaflet::addPolylines(m, data = cv, color = "black", weight = 0.8, opacity = 0.9,
                                label = sprintf(tr("map.popup.contour", lang), cv$level),
                                group = "curvas")
  }
  m |> leaflet_roi(roi_con_crs(roi, dem)) |> leaflet_titulo(titulo)
}

mapa_arboles <- function(chm, arboles, titulo, titulo_leyenda, roi = NULL, lang = "es") {
  if (!tiene_crs(chm) || !tiene_crs(arboles)) return(NULL)
  arb <- a_wgs84(arboles)
  leaflet_base(lang) |>
    leaflet_raster(chm, PAL_CHM_PNG, titulo_leyenda, capa = titulo, opacity = 0.75) |>
    leaflet_roi(roi_con_crs(roi, chm)) |>
    leaflet::addCircleMarkers(data = arb, radius = 4, weight = 1.2,
                              color = MARKER_TREE_LINE, opacity = 1,
                              fillColor = MARKER_TREE_COLOR, fillOpacity = 0.9,
                              label = sprintf(tr("plot.hover.apex", lang), round(arboles$Z, 1)),
                              group = "arboles") |>
    leaflet_titulo(titulo)
}

mapa_cobertura <- function(copas, roi, chm, titulo, lang = "es") {
  roi <- roi_con_crs(roi, chm)
  if (is.null(roi) || !tiene_crs(roi)) return(NULL)
  m <- leaflet_base(lang) |>
    leaflet_roi(roi, relleno = COL_TERRENO, opacidad_relleno = 0.45)
  if (!is.null(copas) && tiene_crs(copas)) {
    m <- leaflet::addPolygons(m, data = a_wgs84(copas), stroke = FALSE,
                              fillColor = COL_VEGETACION, fillOpacity = 0.75, group = "copas")
  }
  m |>
    leaflet::addLegend(colors = c(COL_VEGETACION, COL_TERRENO),
                       labels = c(tr("plot.canopy.canopy", lang), tr("plot.canopy.no_canopy", lang)),
                       title = tr("plot.colorbar.coverage", lang),
                       opacity = 0.8, position = "bottomright") |>
    leaflet_titulo(titulo)
}

# Registra una salida que muestra el mapa y, si no puede construirse (sin CRS o
# error de reproyección), el gráfico anterior como respaldo.
#   mapa():     expresión reactiva que devuelve un mapa leaflet o NULL
#   respaldo(): expresión reactiva del gráfico de respaldo (plotly o plot base/ggplot)
registrar_mapa <- function(output, id, height, mapa, respaldo, tipo_respaldo = "plotly") {
  mapa_r <- reactive({
    tryCatch(mapa(), error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      warning(sprintf("[%s] mapa no disponible: %s", id, conditionMessage(e)), call. = FALSE)
      NULL
    })
  })
  output[[id]] <- renderUI({
    if (!is.null(mapa_r()))              leaflet::leafletOutput(paste0(id, "_mapa"),     height = height)
    else if (tipo_respaldo == "plotly")  plotly::plotlyOutput(paste0(id, "_respaldo"),   height = height)
    else                                 plotOutput(paste0(id, "_respaldo"),             height = height)
  })
  output[[paste0(id, "_mapa")]] <- leaflet::renderLeaflet(req(mapa_r()))
  output[[paste0(id, "_respaldo")]] <- if (tipo_respaldo == "plotly")
    plotly::renderPlotly(respaldo()) else renderPlot(respaldo())
  invisible(mapa_r)
}

# ──────────────────────────────────────────────────────────────────────────────
# HELPERS UI
# ──────────────────────────────────────────────────────────────────────────────
tip_label <- function(label_text, tip_text, label_id = NULL) {
  lspan <- tags$span(style = paste0("color:", TEXT_PRIMARY, "; font-size:12px;"), label_text)
  if (!is.null(label_id)) lspan$attribs$id <- label_id
  tags$div(style = "margin-bottom:2px;",
    lspan,
    tags$span(style = "cursor:help; margin-left:5px;",
              title = tip_text,
              tags$span(style = paste0("color:", GOLD, "; font-size:11px;
                background:", BORDER_COLOR, "; border-radius:50%;
                padding:0 4px; font-weight:bold;"), "?"))
  )
}
