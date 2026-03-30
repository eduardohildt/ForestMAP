# ==============================================================================
# UI - Interfaz de usuario
# ==============================================================================
# Este archivo requiere que se carguen primero:
# - Bibliotecas: shiny, bslib, DT, plotly
# - Variables de colores desde colors_light.R
# - Función tip_label() desde visualization.R
# ==============================================================================
# Versión de la aplicación (se usa en footer y sección "Acerca de")
NVersion <- "2026.5"
addResourcePath("assets", "./assets")
# Las bibliotecas se cargan en app.R, pero las incluimos aquí por si se
# ejecuta ui.R de forma independiente para pruebas
if (!exists("page_navbar")) {
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
}

# Helper UI: Etiqueta con tooltip (normalmente viene de visualization.R)
if (!exists("tip_label")) {
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
}

ui <- page_navbar(
  title = "INTA ForestMap",
	theme = bs_theme(
	  version     = 5,
	  bg          = BG_PRIMARY,      # #f8faf8 (fondo claro)
	  fg          = TEXT_PRIMARY,    # #1a2e1a (texto oscuro)
	  primary     = BS_PRIMARY,
	  secondary   = BS_SECONDARY,
	  success     = STATUS_SUCCESS,
	  info        = ACCENT_SECONDARY,
	  warning     = STATUS_WARNING,
	  danger      = BS_DANGER,
	  base_font   = font_google("IBM Plex Sans"),
	  heading_font = font_google("Exo 2"),
	  code_font   = font_google("IBM Plex Mono"),
	  `navbar-bg`         = ACCENT_PRIMARY,  # Navbar verde forestal en lugar de negro
	  `card-bg`           = BG_CARD,         # #ffffff (blanco)
	  `card-border-color` = BORDER_COLOR     # #d4e4d4 (verde claro)
	),
  header = tags$head(tags$style(HTML(paste0("
    body{background-color:",BG_PRIMARY,";padding-bottom:140px;}
    .stage-badge{display:inline-flex;align-items:center;justify-content:center;
      width:26px;height:26px;border-radius:50%;
      background:",BG_CARD,";border:2px solid ",ACCENT_PRIMARY,";
      color:",ACCENT_PRIMARY,";font-weight:700;font-size:11px;margin-right:5px;}
    .log-box{background:",BG_CARD,";border:1px solid ",BORDER_COLOR,";border-radius:6px;
      padding:10px;height:170px;overflow-y:auto;
      font-family:'IBM Plex Mono',monospace;font-size:11px;color:",PURPLE,";
      white-space:pre-wrap;}
    .metric-card{background:",BG_CARD,";border:1px solid ",BORDER_COLOR,";border-radius:8px;
      padding:12px;text-align:center;min-height:95px;display:flex;flex-direction:column;justify-content:center;}
    .metric-value{font-size:24px;font-weight:700;color:",ACCENT_PRIMARY,";}
    .metric-label{font-size:10px;color:",TEXT_PRIMARY,";text-transform:uppercase;letter-spacing:1px;}
    .btn-run{background:linear-gradient(135deg,",ACCENT_PRIMARY,",",ACCENT_HOVER,");
      color:",BG_CARD,";font-weight:700;border:none;
      padding:9px 22px;border-radius:6px;font-size:13.5px;
      transition:all .2s;}
    .btn-run:hover{transform:translateY(-2px);
      box-shadow:0 5px 16px rgba(45,122,61,.35);color:",BG_CARD,";}
    hr{border-color:",BORDER_COLOR,";}
    .shiny-input-container label{color:",TEXT_PRIMARY,";font-size:12.5px;}
    .form-control,.form-select{background:",BG_CARD," !important;
      border-color:",BORDER_COLOR," !important;color:",TEXT_PRIMARY," !important;}
    .form-control:focus{border-color:",ACCENT_PRIMARY," !important;
      box-shadow:0 0 0 .18rem rgba(45,122,61,.2) !important;}
    .card{border:1px solid ",BORDER_COLOR," !important;}
    .card-header{background:",BG_DARK," !important;
      border-bottom:1px solid ",BORDER_COLOR," !important;}}
    h5.card-title{color:",ACCENT_PRIMARY,";font-weight:600;}
    .info-banner{background:linear-gradient(135deg,",BG_CARD,",",BG_PRIMARY,");
      border:1px solid ",ACCENT_PRIMARY,";border-radius:8px;padding:16px;margin-bottom:16px;}}
    .param-section{border-left:3px solid ",ACCENT_PRIMARY,";
      padding:6px 0 4px 10px;margin:8px 0 4px 0;}
    .param-section-label{color:",ACCENT_PRIMARY,";font-size:11px;font-weight:700;
      text-transform:uppercase;letter-spacing:1px;}
    .tip-box{background:",BG_DARK,";border-radius:5px;padding:6px 8px;
      font-size:11px;color:",TEXT_SECONDARY,";margin-top:2px;margin-bottom:6px;
      border-left:2px solid ",GOLD,";}
    .ref-box{background:",BG_DARK,";border-radius:5px;padding:8px 10px;
      font-size:11px;color:",TEXT_SECONDARY,";border-left:3px solid ",PURPLE,";}
    .nav-link{font-size:15px;font-weight:500;padding:8px 16px !important;}
        .navbar-nav .nav-link.active{font-weight:600;}
  ")))),



  # ── 1: CONFIGURACIÓN ────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","1"),"Configuración"),
    value="tab_config",
    div(class="container-fluid py-4",
      # Banner superior
      div(class="row justify-content-center mb-4",
        div(class="col-12",
          div(class="info-banner text-center",
            tags$h2(style=paste0("color:",GREEN,";font-weight:800;letter-spacing:-1px;"),
                    "Análisis integral de relevamientos aéreos"),
            tags$p(style=paste0("color:",MUTED,";font-size:15px;margin:4px 0 0;"),
                   "Aplicado a paisajes productivos forestales")
          )
        )
      ),
      # Layout de dos columnas: Izquierda = Proyecto (40%) | Derecha = Carga de archivos (60%)
      # RESPONSIVO: col-12 en móvil, col-lg-5/7 en desktop
      div(class="row g-3",
        # COLUMNA IZQUIERDA: Configuración del proyecto
        div(class="col-12 col-lg-5",
          card(card_header(tags$h5(class="card-title","⚙️ Configuración del proyecto")),
            card_body(
              div(class="row g-3",
                div(class="col-12 col-md-6",
                  tags$label(style=paste0("color:",GREEN,";font-size:11px;font-weight:600;"),
                             "Autor"),
                  textInput("cfg_autor", "", value="Autor del análisis", 
                            placeholder="Ingrese el autor"),
                  
                  tags$label(style=paste0("color:",GREEN,";font-size:11px;font-weight:600;margin-top:8px;"),
                             "Institución"),
                  textInput("cfg_institucion", "", value="Institución", 
                            placeholder="Ingrese la institución")
                ),
                div(class="col-12 col-md-6",
                  tags$label(style=paste0("color:",GREEN,";font-size:11px;font-weight:600;"),
                             "Email"),
                  textInput("cfg_email", "", value="email@email.com", 
                            placeholder="Ingrese el email"),
                  
                  tags$label(style=paste0("color:",GREEN,";font-size:11px;font-weight:600;margin-top:8px;"),
                             "Destinatario"),
                  textInput("cfg_destinatario", "", value="Destinatario del informe", 
                            placeholder="Ingrese el destinatario")
                )
              ),
              div(style="margin-top:12px;",
                actionButton("btn_guardar_cfg","💾  Guardar",class="btn btn-sm btn-outline-success w-100"))
            )
          )
        ),
        
        # COLUMNA DERECHA: Carga de archivos
        div(class="col-12 col-lg-7",
          card(card_header(tags$h5(class="card-title","📁 Selección de archivos")),
            card_body(
              # Sección 1: LAS/LAZ
              div(class="row g-2 mb-2",
                div(class="col-12 col-lg-8",
                  actionButton("btn_las", "📂  Seleccionar LAS/LAZ",
                    class = "btn btn-outline-success w-100 mb-1"),
                  verbatimTextOutput("txt_ruta_las"),

                ),
                div(class="col-12 col-lg-4",
                  div(class="metric-card",
                    div(class="metric-value",textOutput("met_las")),
                    div(class="metric-label","📁 LAS/LAZ"))
                )
              ),
              # Sección 2: SHP
              div(class="row g-2 mb-2",
                div(class="col-12 col-lg-8",
                  actionButton("btn_shp", "🗺️  Seleccionar área (SHP)",
                    class = "btn btn-outline-info w-100 mb-1"),
                  verbatimTextOutput("txt_ruta_shp"),

                ),
                div(class="col-12 col-lg-4",
                  div(class="metric-card",
                    div(class="metric-value",textOutput("met_shp")),
                    div(class="metric-label","🗺️ Área"))
                )
              ),
              
              # Sección 4: Salida
              div(class="row g-2 mb-2",
                div(class="col-12 col-lg-8",
                  actionButton("btn_dir","🗂️  Carpeta de salida",
                    class="btn btn-outline-secondary w-100 mb-1"),
                  verbatimTextOutput("txt_ruta_dir")
                ),
                div(class="col-12 col-lg-4",
                  div(class="metric-card",
                    div(class="metric-value",textOutput("met_dir")),
                    div(class="metric-label","📂 Salida"))
                )
              )
            )
          )
        )
      )
    )
  ),

  # ── 2: CARGA DE DATOS ────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","2"),"Carga de datos"),
    value="tab_prepro",
    div(class="container-fluid py-4",
      div(class="row g-3",
        # Columna izquierda: Parámetros (más ancha)
        div(class="col-lg-5",
          # Parámetros generales
          card(card_header(tags$h5(class="card-title","⚙️ Parámetros generales")),
            card_body(
              div(class="row g-3",
                div(class="col-md-4",
                  tip_label("Núcleos CPU", paste0("0 = todos (", N_CORES_MAX, ")")),
                  sliderInput("n_hilos","",0,N_CORES_MAX,2,step=1)
                ),
                div(class="col-md-4",
                  tip_label("Área buffer (m)", "Extensión perimetral del área a recortar"),
                  sliderInput("buffer_m","",0,75,20,step=5)
                ),
                div(class="col-md-4",
                  tip_label("Densidad objetivo (pts/m²)", "Densidad deseada para el submuestreo aleatorio de la nube de puntos"),
                  sliderInput("densidad","",2,200,10,step=1)
                )
              )
            )
          ),
          # Clasificación del suelo
          card(card_header(tags$h5(class="card-title","🏔️ Clasificación del suelo (Algoritmo CSF)")),
            card_body(
              div(class="tip-box mb-3",
                "El algoritmo CSF simula una malla que cae sobre los puntos bajos del terreno para separar suelo de vegetación."),
              
              div(class="row g-3",
                div(class="col-md-6",
                  tip_label("Rigidez de la malla", "Según topografía. Una malla más suave captura mejor el micro relieve, mientras que una mas tensa interpola mejor áreas donde no se observó el terreno"),
                  selectInput("csf_rigidness","",
                    choices=c("1 — Quebrado"=1,
                              "2 — Ondulado (común)"=2,
                              "3 — Llano"=3),
                    selected=2)
                ),
                div(class="col-md-6",
                  tip_label("Umbral (m)", "Altura para considerar que un punto corresponde al terreno"),
                  sliderInput("csf_threshold","",0.05,2,0.5, step=0.05)
                )
              ),
              
              div(class="row g-3",
                div(class="col-md-6",
                  tip_label("Resolución de la malla (m)", "Una mayor resolución (menor valor) captura mejor el micro relieve. Recomendado: 1  a 2 m"),
                  sliderInput("csf_cloth_res","",0.5,5,1,step=0.25)
                ),
                div(class="col-md-6",
                  tags$br(),
                  tip_label("Suavizado de pendientes abruptas", "Activarlo si el terreno presenta cambios abruptos de nivel (terrazas, terraplenes, albardones)"),
                  checkboxInput("csf_sloop_smooth","Suavizar pendientes",value=FALSE)
                )
              )
            )
          ),
          # Botón ejecutar
          div(class="d-grid mt-3",
            actionButton("btn_preprocesar","▶  Ejecutar carga y clasificación",
              class="btn-run w-100", style="padding:12px;font-size:15px;"))
        ),
        
        # Columna derecha: Resultados
        div(class="col-lg-7",
          # Métricas
          div(class="row g-2 mb-3",
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_orig")),
                div(class="metric-label","Puntos cargados"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_filt")),
                div(class="metric-label","Filtrados"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_suelo")),
                div(class="metric-label","Suelo"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_veg")),
                div(class="metric-label","Vegetación")))
          ),
          
          # Pestañas de visualización
          navset_card_tab(
            nav_panel("📊 Log de procesamiento",
              div(class="log-box", style="height:420px;",
                verbatimTextOutput("log_prepro"))
            ),
            nav_panel("☁️ Nube original",
              div(class="tip-box mb-2","🖱️ Girar: clic+arrastrar · Zoom: rueda · Trasladar: clic derecho"),
              plotlyOutput("plot3d_orig",height="390px")
            ),
            nav_panel("🗺️ Nube clasificada",
              div(class="tip-box mb-2","🎨 Colores: suelo vs. vegetación"),
              plotlyOutput("plot3d_clasf",height="390px")
            )
          )
        )
      )
    )
  ),

  # ── 3: MODELOS DIGITALES ─────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","3"),"Modelos digitales"),
    value="tab_modelos",
    div(class="container-fluid py-4",
      div(class="row g-3",
        # Columna izquierda: Parámetros
        div(class="col-lg-4",
          # DEM
          card(card_header(tags$h5(class="card-title","🏔️ DEM — Modelo de elevación")),
            card_body(
              div(class="row g-3",
                div(class="col-md-4",
                  tip_label("Resolución DEM (m)", "Tamaño píxel deseado para el DEM"),
                  sliderInput("dem_res","",0.25,10,1,step=0.25,ticks=TRUE)
                ),
                div(class="col-md-4",
                  tip_label("Filtro de minimización (px)", "Filtro de minimización para eliminar pixeles altos por vegetación baja (impar)"),
                  sliderInput("dem_win_min","",3,15,3,step=2,ticks=TRUE)
                ),
                div(class="col-md-4",
                  tip_label("Filtro de media (px)", "Filtro focal para suavizar el DEM (impar)"),
                  sliderInput("dem_win_mean","",3,81,9,step=2,ticks=TRUE)
                )
              )
            )
          ),
          
          # Curvas y Hillshade
          card(card_header(tags$h5(class="card-title","📏 Curvas de nivel y sombreado")),
            card_body(
              div(class="row g-3",
                div(class="col-md-4",
                  tip_label("Equidistancia curvas (m)", "Separación entre curvas de nivel"),
                  sliderInput("cn_equidist","", 0.1, 10, 1, step=0.1, ticks=TRUE)
                ),
                div(class="col-md-4",
                  tip_label("Ángulo solar (°)", "Elevación"),
                  sliderInput("hs_angulo","",10,80,45,step=5,ticks=TRUE)
                ),
                div(class="col-md-4",
                  tip_label("Azimut (°)", "Dirección"),
                  sliderInput("hs_azimut","",0,360,315,step=15,ticks=TRUE)
                )
              )
            )
          ),
          
          # CHM
          card(card_header(tags$h5(class="card-title","🌿 CHM — Modelo de altura de copas")),
            card_body(
              div(class="row g-3",
                div(class="col-md-6",
                  tip_label("Resolución (m)", "Tamaño de píxel del CHM"),
                  sliderInput("chm_res","",0.1,2,0.5,step=0.1, ticks=TRUE)
                ),
                div(class="col-md-6",
                  tip_label("Ventana de interpolación (m)", "Area de búsqueda de puntos cercanos para el relleno de huecos"),
                  sliderInput("chm_subcirc","",0,0.5,0.025,0.005,ticks=TRUE)
                )
              )
            )
          ),
          
          # Botón ejecutar
          div(class="d-grid mt-3",
            actionButton("btn_modelos","▶  Generar todos los modelos",
              class="btn-run w-100", style="padding:12px;font-size:15px;"))
        ),
        
        # Columna derecha: Visualizaciones
        div(class="col-lg-8",
          navset_card_tab(
            nav_panel("🗺️ DEM",
              div(class="tip-box mb-2","Zoom con rueda o selección. Botón 🏠 para restablecer."),
              div(class="row g-2",
                div(class="col-md-6",
                  tags$small(style=paste0("color:",ACCENT_PRIMARY,";font-weight:600;"),"DEM bruto"),
                  plotlyOutput("plot_dem_bruto",height="360px")),
                div(class="col-md-6",
                  tags$small(style=paste0("color:",ACCENT_PRIMARY,";font-weight:600;"),"DEM suavizado"),
                  plotlyOutput("plot_dem_suav",height="360px"))
              )
            ),
            nav_panel("📏 Curvas nivel",
             div(class="tip-box mb-2","Curvas de nivel graficadas sobre el DEM"),
              plotOutput("plot_curvas",height="480px")
            ),
            nav_panel("🌓 Relieve sombreado",
              div(class="tip-box mb-2","Sombreado del relieve según ángulo solar."),
              plotlyOutput("plot_hillshade",height="480px")
            ),
            nav_panel("🌿 CHM — Modelo de altura de copas",
              div(class="tip-box mb-2","Gris=suelo · Azul=baja · Verde=media · Amarillo=alta · Rojo=emergentes"),
              plotlyOutput("plot_chm",height="480px")
            ),
            nav_panel("📊 Perfil vertical",
             div(class="tip-box mb-2","Distribución vertical de las alturas en la nube de puntos"),
              plotOutput("plot_hist_z",height="400px")
            ),
            nav_panel("📝 Log",
              div(class="log-box", style="height:440px;",
                verbatimTextOutput("log_modelos"))
            )
          )
        )
      )
    )
  ),

  # ── 4: ÁRBOLES ──────────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","4"),"Árboles"),
    value="tab_arboles",
    div(class="container-fluid py-4",
      div(class="row g-3",
        # Columna izquierda: Parámetros
        div(class="col-lg-4",
          card(card_header(tags$h5(class="card-title","🌲 Detección de ápices")),
            card_body(
              div(class="tip-box mb-3",
                "El algoritmo busca el punto más alto en ventanas móviles. Cada máximo representa un árbol."),
              
              div(class="row g-3",
                div(class="col-md-6",
                  tip_label("Diámetro de la ventana (m)", "Diámetro de la ventana de búsqueda, similar al tamaño de la copa de los árboles"),
                  sliderInput("lmf_ws","",1,10,4,step=0.5)
                ),
                div(class="col-md-6",
                  tip_label("Altura mínima de los árboles (m)", "Evita buscar árboles muy bajos. Reduce falsos positivos y agiliza la búsqueda"),
                  sliderInput("lmf_hmin","",1,40,10,step=1)
                )
              ),
              
              div(class="row g-3",
                div(class="col-12",
                  tip_label("Altura de corte para cobertura (m)", "Umbral para clasificar áreas cubiertas por copas. Las áreas del CHM por encima de este valor se consideran cubiertas"),
                  sliderInput("canopy_height_cutoff","",0,20,4,step=0.5)
                )
              ),
              
              div(class="tip-box", style="font-size:10px;",
                "💡 Si se detectan demasiados árboles, incrementar el Tamaño de la ventana o la altura mínima")
            )
          ),
          
          # Métricas
          div(class="row g-2 mt-3",
            div(class="col-12",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_n_arb")),
                div(class="metric-label","🌲 Árboles detectados"))
            ),
            div(class="col-6",
              div(class="metric-card",
                div(class="metric-value", style="font-size:20px;", textOutput("met_alt_med")),
                div(class="metric-label","Altura media (m)"))
            ),
            div(class="col-6",
              div(class="metric-card",
                div(class="metric-value", style="font-size:20px;", textOutput("met_alt_max")),
                div(class="metric-label","Altura máx (m)"))
            )
          ),
          
          # Botón ejecutar
          div(class="d-grid mt-3",
            actionButton("btn_arboles","▶  Detectar árboles",
              class="btn-run w-100", style="padding:12px;font-size:15px;"))
        ),
        
        # Columna derecha: Visualizaciones
        div(class="col-lg-8",
          navset_card_tab(
            nav_panel("📍 Mapa de ápices",
              div(class="tip-box mb-2","Cruces rojas = ápices sobre CHM. Zoom con rueda o selección."),
              plotlyOutput("plot_arb_chm",height="460px")
            ),
            nav_panel("📊 Distribución",
              plotOutput("plot_hist_arb",height="280px"),
              hr(),

            ),
            nav_panel("🌳 Cobertura de copas",
              div(class="tip-box mb-2","Análisis del área cubierta por las copas usando el CHM y el umbral de altura configurado."),
              plotlyOutput("plot_canopy_coverage",height="320px"),
              hr(),
              div(class="row g-2",
                div(class="col-6",
                  div(class="metric-card",
                    div(class="metric-value", style="font-size:18px;", textOutput("met_canopy_area")),
                    div(class="metric-label","Área copas (ha)"))
                ),
                div(class="col-6",
                  div(class="metric-card",
                    div(class="metric-value", style="font-size:18px;", textOutput("met_canopy_pct")),
                    div(class="metric-label","% Cubierto"))
                )
              )
            ),
            nav_panel("📝 Log",
              div(class="log-box", style="height:480px;",
                verbatimTextOutput("log_arboles"))
            )
          )
        )
      )
    )
  ),

  # ── 5: EXPORTACIÓN ──────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","5"),"Exportar"),
    value="tab_exportar",
    div(class="container-fluid py-4",
      div(class="row justify-content-center",
        div(class="col-lg-10",
          # Resumen de productos
          div(class="row g-3 mb-4",
            div(class="col-md-4",
              card(
                card_body(
                  tags$h6(style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
                    "☁️ Nubes de puntos"),
                  tags$ul(style=paste0("color:",TEXT_SECONDARY,";font-size:13px;list-style:none;padding:0;"),
                    tags$li(style="padding:4px 0;","✓ puntos_clasificados.laz"),
                    tags$li(style="padding:4px 0;","✓ puntos_normalizados.laz"))
                )
              )
            ),
            div(class="col-md-4",
              card(
                card_body(
                  tags$h6(style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
                    "🗺️ Modelos raster"),
                  tags$ul(style=paste0("color:",TEXT_SECONDARY,";font-size:13px;list-style:none;padding:0;"),
                    tags$li(style="padding:4px 0;","✓ DEM.tif"),
                    tags$li(style="padding:4px 0;","✓ CHM.tif"),
                    tags$li(style="padding:4px 0;","✓ Hillshade.tif"))
                )
              )
            ),
            div(class="col-md-4",
              card(
                card_body(
                  tags$h6(style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
                    "📐 Capas vectoriales"),
                  tags$ul(style=paste0("color:",TEXT_SECONDARY,";font-size:13px;list-style:none;padding:0;"),
                    tags$li(style="padding:4px 0;","✓ Curvas_Nivel.shp"),
                    tags$li(style="padding:4px 0;","✓ Arboles.shp"),
                    tags$li(style="padding:4px 0;","✓ Cobertura_Copas.shp"),
                    tags$li(style="padding:4px 0;","✓ Area_Interes.shp"))
                )
              )
            )
          ),
          
          # Botones de acción
          card(
            card_body(
              div(class="row g-3",
                div(class="col-md-4",
                  actionButton("btn_exportar","💾  Exportar productos",
                    class="btn-run w-100", style="padding:14px;font-size:15px;")
                ),
                div(class="col-md-4",
                  actionButton("btn_informe","📄  Generar informe PDF",
                    class="btn btn-outline-info w-100", style="padding:14px;font-size:15px;")
                ),
                div(class="col-md-4",
                  actionButton("btn_abrir_carpeta","📂  Abrir carpeta",
                    class="btn btn-outline-warning w-100", style="padding:14px;font-size:15px;")
                )
              )
            )
          ),
          
          # Log
          card(card_header(tags$h5(class="card-title","📝 Log de exportación")),
            card_body(
              div(class="log-box", style="height:300px;",
                verbatimTextOutput("log_exportar"))
            )
          )
        )
      )
    )
  ),

  # ── 6: REFERENCIAS Y CRÉDITOS ────────────────────────────────────────────
  nav_panel("📚 Acerca de ...", value="tab_referencias",
    div(class="container-fluid py-4",
      div(class="row justify-content-center",
        div(class="col-lg-10",
          card(card_header(tags$h5(class="card-title","📚 Referencias bibliográficas")),
            card_body(
              div(class="ref-box",
                tags$b(style=paste0("color:",PURPLE,";"),"Paquete lidR"),tags$br(),
                tags$p(style="margin:4px 0 0;",
                  "Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2021). ",
                  tags$em("lidR: An R package for analysis of Airborne LiDAR Data."),
                  " Remote Sensing of Environment, 251, 112061.",
                  tags$a(href="https://doi.org/10.1016/j.rse.2020.112061",
                         "doi:10.1016/j.rse.2020.112061", target="_blank",
                         style=paste0("color:",GREEN,";"))
                ),
                tags$p(style="margin:6px 0 0;",
                  "Roussel J-R, Auty D (2023). ",
                  tags$em("Airborne LiDAR Data Manipulation and Visualization for Forestry Applications."),
                  " R package version 4.x. ",
                  tags$a(href="https://cran.r-project.org/package=lidR",
                         "CRAN/lidR", target="_blank",
                         style=paste0("color:",GREEN,";"))
                )
              ),
              
              div(class="ref-box",
                tags$b(style=paste0("color:",PURPLE,";"),"Algoritmo CSF (suelo)"),tags$br(),
                tags$p(style="margin:4px 0 0;",
                  "Zhang W et al. (2016). An easy-to-use airborne LiDAR data filtering method based on cloth simulation. ",
                  tags$em("Remote Sensing"), " 8(6), 501.",
                  tags$a(href="https://doi.org/10.3390/rs8060501",
                         "doi:10.3390/rs8060501", target="_blank",
                         style=paste0("color:",GREEN,";"))
                )
              )
            )
          ),
          tags$br(),
          card(
              card_header(
                tags$h5(
                  class="card-title",
                  "ℹ️ Acerca de la Herramienta INTA para el análisis integral de relevamientos aerofotogramétricos y LiDAR en paisajes productivos forestales"
                )
              ),
              
              card_body(
                
                tags$p("Desarrollada por el Grupo Forestal de la Estación Experimental Agropecuaria Montecarlo del INTA, esta herramienta presenta un flujo de trabajo para el análisis de nubes de puntos generadas por relevamientos aerofotogramétricos o LiDAR."),
                
                tags$p(
                  style=paste0("color:",GREEN,";font-size:13px;font-weight:600;margin:12px 0 8px;"),
                  "Funciones incorporadas:"
                ),
                
                # 🔹 ROW CORRECTO
                div(class="row",
                    
                    # 🔸 Columna izquierda (lista)
                    div(class="col-9",
                        tags$ul(
                          style=paste0("color:",MUTED,";font-size:12px;"),
                          tags$li("Recorte y submuestreo de la nube de puntos"),
                          tags$li("Clasificación automática del terreno"),
                          tags$li("Generación de modelos digitales (DEM, CHM)"),
                          tags$li("Extracción de curvas de nivel"),
                          tags$li("Detección de árboles individuales"),
                          tags$li("Informe descriptivo en PDF")
                        )
                    ),
                    
                    # 🔸 Columna derecha (logo)
                    div(class="col-3 d-flex align-items-start",
                        img(
                          src = "assets/logo_INTA.png",
                          alt = "INTA Logo",
                          width = "120px",
                          style = "opacity:0.9; margin-left:10px;"
                        )
                    )
                ),
                
                tags$table(
                  class="table table-sm",
                  style=paste0("color:",MUTED,";margin-top:10px;"),
                  tags$tr(tags$td(tags$b("Versión:")),      tags$td(NVersion)),
                  tags$tr(tags$td(tags$b("Última actualización:")), tags$td("Marzo 2026")),
                  tags$tr(tags$td(tags$b("Contacto:")),     tags$td(tags$a(
                    href="mailto:hildt.eduardo@inta.gob.ar",
                    "hildt.eduardo@inta.gob.ar",
                    style=paste0("color:",GREEN,";")
                  )))
                )
              )
            )
        )
      )
    )
  ),
  
  footer = tags$footer(
    style=paste0("position:fixed; bottom:0; left:0; right:0; width:100%; background:",DEEP,"; border-top:1px solid ",BORDER,"; padding:16px 20px; text-align:center; font-size:12px; color:",MUTED,"; z-index:1000; box-shadow:0 -2px 8px rgba(0,0,0,0.1);"),
    tags$div(
      "INTA ForestMap — Análisis integral de relevamientos aéreos — Dr. Eduardo Hildt — INTA EEA Montecarlo — ",
      tags$a(href="mailto:hildt.eduardo@inta.gob.ar", "Contacto: hildt.eduardo@inta.gob.ar", style=paste0("color:",TEXTO_PIE,";")),
      paste0(" — Red de Drones INTA — Versión ", NVersion),
      style=paste0("color:",TEXTO_PIE,";")
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# Fin de UI
