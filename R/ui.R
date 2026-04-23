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
}

ui <- page_navbar(
  id       = "main_nav",
  title    = "INTA ForestMap",
  fillable = FALSE,
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
    .nav-link.tab-locked{pointer-events:none !important;opacity:.45 !important;cursor:not-allowed !important;}
    .pipeline-block{border-left:4px solid ",BORDER_COLOR,";padding-left:18px;margin-bottom:28px;}
    .pipeline-block.block-complete{border-left-color:",STATUS_SUCCESS,";}
    .pipeline-step-header{display:flex;align-items:center;justify-content:space-between;margin-bottom:12px;}
    .pipeline-step-title{display:flex;align-items:center;gap:10px;}
    .step-badge{display:inline-flex;align-items:center;justify-content:center;
      width:32px;height:32px;border-radius:50%;background:",ACCENT_PRIMARY,";
      color:",BG_CARD,";font-weight:700;font-size:14px;flex-shrink:0;}
    .step-status-badge{font-size:11px;font-weight:600;padding:3px 10px;border-radius:12px;
      letter-spacing:.5px;text-transform:uppercase;}
    .step-status-badge.status-complete{background:rgba(45,122,61,.12);color:",STATUS_SUCCESS,";
      border:1px solid rgba(45,122,61,.35);}
    .step-status-badge.status-pending{background:rgba(200,127,10,.12);color:",STATUS_WARNING,";
      border:1px solid rgba(200,127,10,.35);}
    .file-meta-card{background:",BG_PRIMARY,";border:1px solid ",BORDER_COLOR,";
      border-radius:8px;padding:14px 16px;margin-top:10px;}
    .file-meta-row{display:flex;justify-content:space-between;align-items:baseline;
      padding:5px 0;border-bottom:1px solid ",BORDER_COLOR,";font-size:12.5px;}
    .file-meta-row:last-child{border-bottom:none;}
    .file-meta-label{color:",TEXT_MUTED,";font-weight:500;}
    .file-meta-value{color:",TEXT_PRIMARY,";font-weight:600;text-align:right;max-width:65%;word-break:break-all;}
    .file-meta-value.warn{color:",STATUS_WARNING,";}
    .proyecto-data-table{width:100%;font-size:13px;}
    .proyecto-data-table td:first-child{color:",TEXT_MUTED,";font-weight:500;width:42%;
      padding:5px 8px 5px 0;vertical-align:top;}
    .proyecto-data-table td:last-child{color:",TEXT_PRIMARY,";font-weight:600;}
    .cta-block{background:linear-gradient(135deg,",BG_CARD,",",BG_DARK,");
      border:2px solid ",ACCENT_PRIMARY,";border-radius:12px;padding:28px 24px;
      text-align:center;margin-top:8px;}
    .cta-block h4{color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:6px;}
    .cta-block p{color:",TEXT_MUTED,";font-size:13.5px;margin-bottom:20px;}
    .btn-cambiar{background:transparent;border:1px solid ",BORDER_COLOR,";color:",TEXT_MUTED,";
      font-size:12px;padding:4px 12px;border-radius:5px;cursor:pointer;margin-top:8px;}
    .btn-cambiar:hover{border-color:",ACCENT_PRIMARY,";color:",ACCENT_PRIMARY,";}
    .csf-scenario-card{border:2px solid ",BORDER_COLOR,";border-radius:8px;padding:8px 6px;
      text-align:center;cursor:pointer;transition:all .2s;background:",BG_CARD,";
      user-select:none;height:100%;}
    .csf-scenario-card:hover{border-color:",ACCENT_PRIMARY,";background:",BG_DARK,";}
    .csf-scenario-card.csf-selected{border-color:",ACCENT_PRIMARY,";
      background:rgba(45,122,61,.08);box-shadow:0 0 0 3px rgba(45,122,61,.18);}
    .csf-scenario-title{font-weight:700;font-size:12px;color:",TEXT_PRIMARY,";margin:2px 0 1px;}
    .csf-scenario-desc{font-size:10px;color:",TEXT_MUTED,";line-height:1.2;}
    #ui_dem_res_cards,#ui_cn_equidist_cards,#ui_dem_win_min_cards,
    #ui_dem_win_mean_cards,#ui_chm_res_cards,#ui_chm_subcirc_cards{
      margin:0 !important;padding:0 !important;display:block;}
    .csf-custom-badge{font-size:10px;font-weight:600;color:",STATUS_WARNING,";
      background:rgba(200,127,10,.1);border:1px solid rgba(200,127,10,.3);
      border-radius:10px;padding:3px 10px;display:inline-block;}
    .knob-container{display:flex;flex-direction:column;align-items:center;gap:6px;padding:10px 0;}
    .knob-title{font-size:11px;font-weight:700;text-transform:uppercase;
      letter-spacing:1px;color:",TEXT_MUTED,";margin-bottom:2px;}
    .knob-svg{cursor:grab;filter:drop-shadow(0 2px 4px rgba(0,0,0,.15));}
    .knob-svg:active{cursor:grabbing;}
    .knob-needle{transition:transform .45s cubic-bezier(.34,1.56,.64,1);
      transform-origin:45px 45px;}
    .knob-levels{display:flex;justify-content:space-between;width:90px;
      font-size:10px;font-weight:600;color:",TEXT_MUTED,";margin-top:2px;}
    .knob-level{text-align:center;width:22px;}
    .knob-level.active{color:",ACCENT_PRIMARY,";}
    .knob-status{font-size:18px;font-weight:800;color:",ACCENT_PRIMARY,";
      letter-spacing:.5px;margin-top:2px;}
    .knob-speed{font-size:11px;color:",TEXT_MUTED,";margin-top:1px;}
    .knob-sep{width:100%;border:none;border-top:1px solid ",BORDER_COLOR,";margin:14px 0;}
    .density-card{border:2px solid ",BORDER_COLOR,";border-radius:8px;padding:7px 5px;
      text-align:center;cursor:pointer;transition:all .2s;background:",BG_CARD,";
      user-select:none;height:100%;color:",TEXT_MUTED,";}
    .density-card:hover{border-color:",ACCENT_PRIMARY,";background:",BG_DARK,";}
    .density-card.density-selected{border-color:",ACCENT_PRIMARY,";
      background:rgba(45,122,61,.08);box-shadow:0 0 0 3px rgba(45,122,61,.18);
      color:",ACCENT_PRIMARY,";}
    .density-dots-area{height:28px;display:flex;align-items:center;justify-content:center;
      margin-bottom:3px;}
    .density-card-title{font-weight:700;font-size:12px;color:",TEXT_PRIMARY,";margin-bottom:1px;}
    .density-card-desc{font-size:10px;color:",TEXT_MUTED,";line-height:1.3;}
    #lang_select{font-size:0.82rem;padding:2px 6px;border-radius:4px;
      background:",BG_CARD," !important;border-color:",BORDER_COLOR," !important;
      color:",TEXT_PRIMARY," !important;width:80px;}
  "))),
  tags$script(HTML("
    function tabSetEnabled(tabId, enabled) {
      var el = document.querySelector('.nav-link[data-value=\"' + tabId + '\"]');
      if (!el) return;
      if (enabled) { el.classList.remove('tab-locked'); }
      else         { el.classList.add('tab-locked');    }
    }
    Shiny.addCustomMessageHandler('tabEnable',  function(id) { tabSetEnabled(id, true);  });
    Shiny.addCustomMessageHandler('tabDisable', function(id) { tabSetEnabled(id, false); });
    Shiny.addCustomMessageHandler('btnDisable', function(id) {
      var el = document.getElementById(id); if (el) el.disabled = true;
    });
    Shiny.addCustomMessageHandler('btnEnable', function(id) {
      var el = document.getElementById(id); if (el) el.disabled = false;
    });
    Shiny.addCustomMessageHandler('update_nav_labels', function(labels) {
      var tabs = {
        'tab_config':    labels.config,
        'tab_prepro':    labels.data,
        'tab_modelos':   labels.models,
        'tab_arboles':   labels.trees,
        'tab_exportar':  labels.export,
        'tab_referencias': labels.about
      };
      Object.keys(tabs).forEach(function(id) {
        var link = document.querySelector('.nav-link[data-value=\"' + id + '\"]');
        if (!link) return;
        link.childNodes.forEach(function(node) {
          if (node.nodeType === 3 && node.textContent.trim().length > 0)
            node.textContent = tabs[id];
        });
      });
    });
    Shiny.addCustomMessageHandler('update_ui_text', function(d) {
      Object.keys(d).forEach(function(id) {
        var el = document.getElementById(id);
        if (el) el.textContent = d[id];
      });
    });
    Shiny.addCustomMessageHandler('update_nav_tabs', function(d) {
      Object.keys(d).forEach(function(val) {
        var link = document.querySelector('.nav-link[data-value=\"' + val + '\"]');
        if (!link) return;
        link.childNodes.forEach(function(node) {
          if (node.nodeType === 3 && node.textContent.trim().length > 0)
            node.textContent = d[val];
        });
      });
    });
    $(document).ready(function() {
      ['tab_prepro','tab_modelos', 'tab_arboles', 'tab_exportar'].forEach(function(id) {
        tabSetEnabled(id, false);
      });
      var btnC = document.getElementById('btn_continuar');
      if (btnC) btnC.disabled = true;
    });
  "))
),



  # ── 1: CONFIGURACIÓN ────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","1"),"Configuración"),
    value="tab_config",
    div(class="container-fluid py-4",
      # Banner superior
      div(class="row justify-content-center mb-4",
        div(class="col-lg-7 mx-auto",
          div(class="info-banner text-center",
            tags$h2(id="h2_banner_title", style=paste0("color:",GREEN,";font-weight:800;letter-spacing:-1px;"),
                    "Análisis integral de relevamientos aéreos"),
            tags$p(id="p_banner_subtitle", style=paste0("color:",MUTED,";font-size:15px;margin:4px 0 0;"),
                   "Aplicado a paisajes productivos forestales")
          )
        )
      ),

      # Pipeline centrado
      div(class="row justify-content-center",
        div(class="col-lg-7 mx-auto",

          # ── BLOQUE 0: PROYECTO ─────────────────────────────────────────────
          div(class="pipeline-block block-complete",
            div(class="pipeline-step-header",
              div(class="pipeline-step-title",
                tags$span(class="step-badge", "P"),
                tags$h5(id="h5_project_step", style=paste0("margin:0;color:",TEXT_PRIMARY,";font-weight:700;"),
                        "Proyecto")
              ),
              tags$span(id="span_project_badge", class="step-status-badge status-complete", "✔ Configurado")
            ),
            card(
              card_body(class="p-3",
                uiOutput("cfg_proyecto_html"),
                div(class="mt-3",
                  actionButton("btn_abrir_cfg", "Editar datos del proyecto",
                    class = "btn btn-outline-secondary btn-sm")
                )
              )
            )
          ),

          # ── BLOQUE 1: LAS/LAZ ──────────────────────────────────────────────
          div(class="pipeline-block",
            div(class="pipeline-step-header",
              div(class="pipeline-step-title",
                tags$span(class="step-badge", "1"),
                tags$h5(id="h5_step_las", style=paste0("margin:0;color:",TEXT_PRIMARY,";font-weight:700;"),
                        "Nube de puntos LiDAR o fotogramétrica (LAS/LAZ)")
              ),
              uiOutput("cfg_las_badge")
            ),
            card(
              card_body(class="p-3",
                uiOutput("cfg_las_ui")
              )
            )
          ),

          # ── BLOQUE 2: ÁREA SHP ─────────────────────────────────────────────
          div(class="pipeline-block",
            div(class="pipeline-step-header",
              div(class="pipeline-step-title",
                tags$span(class="step-badge", "2"),
                tags$h5(id="h5_step_shp", style=paste0("margin:0;color:",TEXT_PRIMARY,";font-weight:700;"),
                        "Área de interés para el análisis (SHP)")
              ),
              uiOutput("cfg_shp_badge")
            ),
            card(
              card_body(class="p-3",
                uiOutput("cfg_shp_ui")
              )
            )
          ),

          # ── BLOQUE 3: CARPETA DE SALIDA ────────────────────────────────────
          div(class="pipeline-block",
            div(class="pipeline-step-header",
              div(class="pipeline-step-title",
                tags$span(class="step-badge", "3"),
                tags$h5(id="h5_step_dir", style=paste0("margin:0;color:",TEXT_PRIMARY,";font-weight:700;"),
                        "Carpeta de salida para resultados")
              ),
              uiOutput("cfg_dir_badge")
            ),
            card(
              card_body(class="p-3",
                uiOutput("cfg_dir_ui")
              )
            )
          ),

          # ── BLOQUE FINAL: ACCIÓN ───────────────────────────────────────────
          div(class="cta-block",
            tags$h4(id="h4_cta_title", "Todo listo para continuar"),
            tags$p(id="p_cta_desc", "Revisa la información y continúa con el siguiente paso."),
            actionButton("btn_continuar", "Continuar →",
              class = "btn-run",
              style = "padding:12px 36px;font-size:16px;")
          )

        )  # end col-lg-7
      )  # end row
    )  # end container-fluid
  ),

  # ── 2: CARGA DE DATOS ────────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","2"),"Carga de datos"),
    value="tab_prepro",
    div(class="container-fluid py-3",
      div(class="row g-2",
        # Columna izquierda: Parámetros
        div(class="col-lg-4",
          # Parámetros generales
          card(class="mb-2", card_header(tags$h5(id="h5_subsample_header", class="card-title mb-0","⚙️ Submuestreo de la nube de puntos")),
            card_body(class="p-2",
              # ── Densidad de nube ────────────────────────────────────────
              tags$p(id="p_subsample_desc", style=paste0("color:",TEXT_SECONDARY,";font-size:12px;margin-bottom:3px;"),
                "Elige cuántos puntos por m² se cargarán. Mayor densidad = mayor fidelidad y más tiempo de procesamiento."),
              div(class="mb-1", uiOutput("densidad_ui")),
              # ── Método de submuestreo + Configuración avanzada ──────────
              div(class="row g-2 mb-0",
                div(class="col",
                  div(class="card h-100",
                    div(class="card-header py-1",
                      tags$h6(id="h6_method_header", style="margin:0;font-size:12px;font-weight:700;", "Método de submuestreo")
                    ),
                    div(class="card-body py-1 px-2",
                      radioButtons("decimate_tipo", label=NULL,
                        choices=c("Aleatorio"="aleatorio","Uniforme"="uniforme"),
                        selected="aleatorio", inline=TRUE)
                    )
                  )
                ),
                div(class="col",
                  div(class="card h-100",
                    div(class="card-body d-flex align-items-center justify-content-center p-1",
                      actionButton("btn_densidad_avanzado", "⚙ Configuración avanzada",
                        class="btn-outline-secondary btn-sm w-100")
                    )
                  )
                )
              ),
              tags$p(id="p_method_tip", class="mt-1 mb-0", style=paste0("color:",TEXT_SECONDARY,";font-size:11px;"),
                "Tipos de submuestreo: Aleatorio respeta la distribución original. Uniforme logra una distribución homogénea."),
              # ── Entradas ocultas ─────────────────────────────────────────
              div(style="display:none;",
                numericInput("buffer_m", "", value=10, min=1, max=200),
                numericInput("densidad", "", value=25, min=1, max=500)
              )
            )
          ),
          # Clasificación del suelo
          card(class="mb-2", card_header(tags$h5(id="h5_csf_header", class="card-title mb-0","🏔️ Clasificación del suelo (Algoritmo CSF)")),
            card_body(class="p-2",
              tags$p(id="p_csf_desc", style=paste0("color:",TEXT_SECONDARY,";font-size:12px;margin-bottom:3px;"),
                "Seleccioná el tipo de relieve predominante. El algoritmo CSF modela una malla que se adapta al terreno para separar suelo de vegetación."),

              # Selector de escenario (renderizado desde server)
              uiOutput("csf_scenario_ui"),

              # Badge de configuración personalizada
              uiOutput("csf_custom_badge_ui"),

              # Botón configuración avanzada
              div(class="d-grid mt-1",
                actionButton("btn_csf_avanzado",
                  tagList(tags$span(style="font-size:13px;", "⚙️"), " Configuración avanzada (opcional)"),
                  class = "btn btn-outline-secondary btn-sm")
              ),

              # Inputs CSF ocultos — compatibilidad con btn_preprocesar
              tags$div(style = "display:none;",
                selectInput("csf_rigidness","",choices=c("1"=1,"2"=2,"3"=3),selected=2),
                sliderInput("csf_threshold","",0.05,2,0.5,step=0.05),
                sliderInput("csf_cloth_res","",0.5,5,1,step=0.25),
                checkboxInput("csf_sloop_smooth","",value=FALSE)
              )
            )
          ),
          # Botón ejecutar
          div(class="d-grid",
            actionButton("btn_preprocesar","▶  Ejecutar carga y clasificación",
              class="btn-run w-100", style="padding:10px;font-size:15px;"))
        ),
        
        # Columna derecha: Resultados
        div(class="col-lg-8",
          # Métricas
          div(class="row g-2 mb-3",
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_orig")),
                div(id="lbl_pts_loaded", class="metric-label","Puntos cargados"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_filt")),
                div(id="lbl_pts_filtered", class="metric-label","Filtrados"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_suelo")),
                div(id="lbl_pts_soil", class="metric-label","Suelo"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value",textOutput("met_pts_veg")),
                div(id="lbl_pts_veg", class="metric-label","Vegetación")))
          ),
          
          # Pestañas de visualización
          navset_card_tab(
            nav_panel(value="inner_prepro_log", "📊 Log de procesamiento",
              div(class="log-box", style="height:480px;",
                verbatimTextOutput("log_prepro"))
            ),
            nav_panel(value="inner_prepro_cloud", "☁️ Nube original",
              div(id="tip_cloud_3d", class="tip-box mb-2","🖱️ Girar: clic+arrastrar · Zoom: rueda · Trasladar: clic derecho"),
              plotlyOutput("plot3d_orig",height="480px")
            ),
            nav_panel(value="inner_prepro_classified", "🗺️ Nube clasificada",
              div(id="tip_classified", class="tip-box mb-2","🎨 Colores: suelo vs. vegetación"),
              plotlyOutput("plot3d_clasf",height="480px")
            )
          )
        )
      )
    )
  ),

  # ── 3: MODELOS DIGITALES ─────────────────────────────────────────────────────
  nav_panel(tagList(tags$span(class="stage-badge","3"),"Modelos digitales"),
    value="tab_modelos",
    div(class="container-fluid py-2",
      div(class="row g-2",
        # Columna izquierda: Parámetros
        div(class="col-lg-4",
          # Bloque A: DEM + Curvas
          card(class="mb-0", card_header(class="py-0 px-2", tags$h5(id="h5_dem_header", class="card-title mb-0","🏔️ Modelo digital de elevación (DEM)")),
            card_body(class="p-4",
              # Cards visibles en la pantalla principal
              tags$p(id="p_dem_res_label", class="fw-semibold mb-0", "Resolución del DEM (m)"),
              uiOutput("ui_dem_res_cards"),
              tags$p(id="p_cn_equidist_label", class="fw-semibold mb-0", "Equidistancia de las curvas de nivel (m)"),
              uiOutput("ui_cn_equidist_cards"),
              tags$p(id="p_dem_win_min_label", class="fw-semibold mb-0", "Filtrado de la vegetación baja"),
              uiOutput("ui_dem_win_min_cards"),
              tags$p(id="p_dem_win_mean_label", class="fw-semibold mb-0", "Suavizado del DEM y las curvas de nivel"),
              uiOutput("ui_dem_win_mean_cards"),
              actionButton("btn_dem_avanzado", "⚙️ Configuración avanzada",
                class = "btn btn-outline-secondary w-100 mt-1")
            )
          ),

          # Bloque B: CHM
          card(class="mb-2", card_header(class="py-1 px-2", tags$h5(id="h5_chm_header", class="card-title mb-0","🌿 Modelo de altura de copas (CHM)")),
            card_body(class="p-2",
              # Cards visibles en la pantalla principal
              tags$p(id="p_chm_res_label", class="fw-semibold mb-0", "Resolución del CHM (m)"),
              uiOutput("ui_chm_res_cards"),
              tags$p(id="p_chm_gap_label", class="fw-semibold mb-0", "Relleno de huecos"),
              uiOutput("ui_chm_subcirc_cards"),
              actionButton("btn_chm_avanzado", "⚙️ Configuración avanzada",
                class = "btn btn-outline-secondary w-100 mt-1")
            )
          ),

          # Botón ejecutar
          div(class="d-grid",
            actionButton("btn_modelos","▶  Generar todos los modelos",
              class="btn-run w-100", style="padding:10px;font-size:15px;"))
        ),
        
        # Columna derecha: Visualizaciones
        div(class="col-lg-8",
          navset_card_tab(
            nav_panel(value="inner_mod_dem", "🗺️ DEM",
              div(id="tip_mod_dem", class="tip-box mb-2","Zoom con rueda o selección. Botón 🏠 para restablecer."),
              div(class="row g-2",
                div(class="col-md-6",
                  tags$small(id="lbl_dem_raw", style=paste0("color:",ACCENT_PRIMARY,";font-weight:600;"),"DEM bruto"),
                  plotlyOutput("plot_dem_bruto",height="640px")),
                div(class="col-md-6",
                  tags$small(id="lbl_dem_smooth", style=paste0("color:",ACCENT_PRIMARY,";font-weight:600;"),"DEM suavizado"),
                  plotlyOutput("plot_dem_suav",height="640px"))
              )
            ),
            nav_panel(value="inner_mod_contours", "📏 Curvas nivel",
             div(id="tip_mod_contours", class="tip-box mb-2","Curvas de nivel graficadas sobre el DEM"),
              plotOutput("plot_curvas",height="640px")
            ),
            nav_panel(value="inner_mod_hillshade", "🌓 Relieve sombreado",
              div(id="tip_mod_hillshade", class="tip-box mb-2","Sombreado del relieve según ángulo solar."),
              plotlyOutput("plot_hillshade",height="640px")
            ),
            nav_panel(value="inner_mod_chm", "🌿 CHM — Modelo de altura de copas",
              div(id="tip_mod_chm", class="tip-box mb-2","Gris=suelo · Azul=baja · Verde=media · Amarillo=alta · Rojo=emergentes"),
              plotlyOutput("plot_chm",height="640px")
            ),
            nav_panel(value="inner_mod_profile", "📊 Perfil vertical",
             div(id="tip_mod_profile", class="tip-box mb-2","Distribución vertical de las alturas en la nube de puntos"),
              plotOutput("plot_hist_z",height="640px")
            ),
            nav_panel(value="inner_mod_log", "📝 Log",
              div(class="log-box", style="height:640px;",
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
          card(card_header(tags$h5(id="h5_trees_header", class="card-title","🌲 Detección de ápices")),
            card_body(
              div(id="div_trees_desc", class="tip-box mb-3",
                "El algoritmo busca el punto más alto en ventanas móviles. Cada máximo representa un árbol."),

              tags$p(id="p_lmf_ws_label", class="fw-semibold mb-0", "Diámetro de la ventana de búsqueda (m)"),
              uiOutput("ui_lmf_ws_cards"),

              tags$p(id="p_lmf_hmin_label", class="fw-semibold mb-0 mt-2", "Altura mínima de los árboles (m)"),
              uiOutput("ui_lmf_hmin_cards"),

              tags$p(id="p_canopy_cutoff_label", class="fw-semibold mb-0 mt-2", "Altura de corte para cobertura (m)"),
              uiOutput("ui_canopy_cutoff_cards"),

              # Modal con sliders exactos
              tags$div(class="modal fade", id="modalArbConfig", tabindex="-1",
                `aria-labelledby`="modalArbConfigLabel", `aria-hidden`="true",
                tags$div(class="modal-dialog",
                  tags$div(class="modal-content",
                    tags$div(class="modal-header",
                      tags$h5(class="modal-title", id="modalArbConfigLabel", "Configuración avanzada — Árboles"),
                      tags$button(type="button", class="btn-close", `data-bs-dismiss`="modal", `aria-label`="Cerrar")
                    ),
                    tags$div(class="modal-body",
                      div(class="row g-3",
                        div(class="col-12",
                          tip_label("Diámetro de la ventana (m)", "Diámetro de la ventana de búsqueda, similar al tamaño de la copa de los árboles", label_id="tip_lbl_lmf_ws"),
                          sliderInput("modal_lmf_ws", NULL, min=1, max=10, value=4, step=0.5, ticks=TRUE)
                        ),
                        div(class="col-12",
                          tip_label("Altura mínima de los árboles (m)", "Evita buscar árboles muy bajos. Reduce falsos positivos y agiliza la búsqueda", label_id="tip_lbl_lmf_hmin"),
                          sliderInput("modal_lmf_hmin", NULL, min=1, max=40, value=10, step=1, ticks=TRUE)
                        ),
                        div(class="col-12",
                          tip_label("Altura de corte para cobertura (m)", "Umbral para clasificar áreas cubiertas por copas. Las áreas del CHM por encima de este valor se consideran cubiertas", label_id="tip_lbl_canopy_cutoff"),
                          sliderInput("modal_canopy_cutoff", NULL, min=0, max=20, value=4, step=0.5, ticks=TRUE)
                        )
                      )
                    ),
                    tags$div(class="modal-footer",
                      actionButton("btn_arb_config_aplicar", "Aplicar", class="btn btn-primary"),
                      tags$button(id="btn_arb_modal_close", type="button", class="btn btn-secondary", `data-bs-dismiss`="modal", "Cerrar")
                    )
                  )
                )
              ),
              # Botón para abrir el modal
              tags$button(id="btn_arb_open_modal",
                type="button", class="btn btn-outline-secondary w-100 mt-2",
                `data-bs-toggle`="modal", `data-bs-target`="#modalArbConfig",
                "⚙️ Configuración avanzada")
            )
          ),
          
          # Botón ejecutar
          div(class="d-grid mt-3",
            actionButton("btn_arboles","▶  Detectar árboles",
              class="btn-run w-100", style="padding:12px;font-size:15px;"))
        ),
        
        # Columna derecha: Resultados
        div(class="col-lg-8",
          # Métricas
          div(class="row g-2 mb-3",
            div(class="col-md-3",
             div(class="metric-card",
                div(class="metric-value",textOutput("met_n_arb")),
                div(id="lbl_trees_detected", class="metric-label","🌲 Árboles detectados"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value", textOutput("met_dens_arb")),
                div(id="lbl_trees_density", class="metric-label","árb/ha"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value", style="font-size:20px;", textOutput("met_alt_med")),
                div(id="lbl_trees_avg_height", class="metric-label","Altura media (m)"))),
            div(class="col-md-3",
              div(class="metric-card",
                div(class="metric-value", style="font-size:20px;", textOutput("met_alt_max")),
                div(id="lbl_trees_max_height", class="metric-label","Altura máx (m)")))
          ),

        # Columna derecha: Visualizaciones
          navset_card_tab(
            nav_panel(value="inner_arb_map", "📍 Mapa de ápices",
              div(id="tip_arb_apex", class="tip-box mb-2","Cruces rojas = ápices sobre CHM. Zoom con rueda o selección."),
              plotlyOutput("plot_arb_chm",height="480px")
            ),
            nav_panel(value="inner_arb_dist", "📊 Distribución",
              plotOutput("plot_hist_arb",height="480px"),
              hr(),

            ),
            nav_panel(value="inner_arb_canopy", "🌳 Cobertura de copas",
              div(id="tip_arb_canopy", class="tip-box mb-2","Análisis del área cubierta por las copas usando el CHM y el umbral de altura configurado."),
              plotlyOutput("plot_canopy_coverage",height="480px"),
              hr(),
              div(class="row g-2",
                div(class="col-6",
                  div(class="metric-card",
                    div(class="metric-value", style="font-size:18px;", textOutput("met_canopy_area")),
                    div(id="lbl_canopy_area", class="metric-label","Área copas (ha)"))
                ),
                div(class="col-6",
                  div(class="metric-card",
                    div(class="metric-value", style="font-size:18px;", textOutput("met_canopy_pct")),
                    div(id="lbl_canopy_pct", class="metric-label","% Cubierto"))
                )
              )
            ),
            nav_panel(value="inner_arb_log", "📝 Log",
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
                  tags$h6(id="h6_export_clouds", style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
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
                  tags$h6(id="h6_export_raster", style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
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
                  tags$h6(id="h6_export_vector", style=paste0("color:",ACCENT_PRIMARY,";font-weight:700;margin-bottom:12px;"),
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
          card(card_header(tags$h5(id="h5_export_log", class="card-title","📝 Log de exportación")),
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
          uiOutput("ui_about_section")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    selectInput(
      "lang_select",
      label    = NULL,
      choices  = c("ES" = "es", "EN" = "en", "PT" = "pt"),
      selected = "es",
      width    = "80px"
    )
  ),

  footer = tags$footer(
    style=paste0("position:fixed; bottom:0; left:0; right:0; width:100%; background:",DEEP,"; border-top:1px solid ",BORDER,"; padding:16px 20px; text-align:center; font-size:12px; color:",MUTED,"; z-index:1000; box-shadow:0 -2px 8px rgba(0,0,0,0.1);"),
    tags$div(
      tags$span(id="footer_main_text", "INTA ForestMap — Análisis integral de relevamientos aéreos — Dr. Eduardo Hildt — INTA EEA Montecarlo — "),
      tags$a(href="mailto:hildt.eduardo@inta.gob.ar", "Contacto: hildt.eduardo@inta.gob.ar", style=paste0("color:",TEXTO_PIE,";")),
      paste0(" — Red de Drones INTA — Versión ", NVersion),
      style=paste0("color:",TEXTO_PIE,";")
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# Fin de UI
