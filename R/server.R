# ==============================================================================
# SERVER - Orquestador principal
# ==============================================================================
# Inicializa el estado reactivo y delega cada sección a su módulo:
#   server_helpers.R        — %||%, elegir_archivo/carpeta, csf_presets
#   server_config.R         — E0: configuración de proyecto y archivos
#   server_presets.R        — E1: tarjetas preset (CSF, densidad, DEM, CHM, árboles)
#   server_preprocessing.R  — E2: preproceso LiDAR y resolución de CRS
#   server_models.R         — E3: generación de modelos digitales
#   server_trees.R          — E4: detección de árboles y cobertura de copas
#   server_export.R         — E5: exportación e informe HTML
# ==============================================================================

server <- function(input, output, session) {

  set_lidr_threads(min(max(1L, floor(0.75 * N_CORES_MAX)), N_CORES_MAX - 1L))

  # ── Logging ──────────────────────────────────────────────────────────────────
  ts_log <- function(m) paste(format(Sys.time(), "[%H:%M:%S]"), m)
  ag <- function(campo, m) rv[[campo]] <- c(rv[[campo]], ts_log(m))

  # ── Estado reactivo global ───────────────────────────────────────────────────
  rv <- reactiveValues(
    ruta_las       = NULL,
    ruta_shp       = NULL,
    ruta_dir       = NULL,
    usar_extension_completa = FALSE,
    # Última carpeta conocida — arranca en Home, se actualiza tras cada selección
    ultima_carpeta = path.expand("~"),
    configurado    = FALSE,
    las_raw        = NULL, roi          = NULL,
    las_filtrado   = NULL, las_clasf    = NULL,
    dem            = NULL, dem_suav     = NULL,
    curvas         = NULL, hillshade    = NULL,
    datos_norm     = NULL, chm          = NULL,
    arboles        = NULL,
    # Estadísticas para el informe narrativo
    dens_orig      = NA,   area_ha      = NA,
    dem_min        = NA,   dem_max      = NA,   dem_mean = NA,
    chm_min        = NA,   chm_max      = NA,   chm_mean = NA,
    log_config     = character(0), log_prepro  = character(0),
    log_modelos    = character(0), log_arboles = character(0),
    log_export     = character(0),
    crs_pendiente  = NULL,
    # Configuración del proyecto (editable)
    cfg_autor        = "-",
    cfg_institucion  = "-",
    cfg_email        = "-",
    cfg_destinatario = "-",
    # Metadata de archivos leída del header (sin cargar la nube completa)
    las_meta         = NULL,
    shp_meta         = NULL,
    # Escenario CSF y flag de configuración personalizada
    csf_scenario     = "ondulado",
    csf_custom       = FALSE,
    csf_rigidness    = 2L,
    csf_threshold    = 0.5,
    csf_cloth_res    = 1.0,
    csf_sloop_smooth = FALSE,
    # Nivel de densidad y flag de configuración personalizada
    densidad_nivel   = "medio",
    densidad_custom  = FALSE,
    densidad_val     = 25,
    # Presets modelos DEM
    dem_res_sel      = 1,     dem_res_custom      = FALSE,
    cn_equidist_sel  = 1,     cn_equidist_custom  = FALSE,
    dem_win_min_sel  = 3,     dem_win_min_custom  = FALSE,
    dem_win_mean_sel = 15,    dem_win_mean_custom = FALSE,
    # Presets modelos CHM
    chm_res_sel      = 0.5,   chm_res_custom      = FALSE,
    chm_subcirc_sel  = 0.025, chm_subcirc_custom  = FALSE,
    # Presets árboles
    lmf_ws_sel        = 4,   lmf_ws_custom        = FALSE,
    lmf_hmin_sel      = 10,  lmf_hmin_custom      = FALSE,
    canopy_cutoff_sel = 4,   canopy_cutoff_custom = FALSE
  )

  # ── Idioma ───────────────────────────────────────────────────────────────────
  lang <- reactiveVal("es")

  observeEvent(input$lang_select, {
    l <- input$lang_select
    lang(l)
    session$sendCustomMessage("update_nav_labels", list(
      config  = tr("nav.tab.config",    l),
      data    = tr("nav.tab.data_load", l),
      models  = tr("nav.tab.models",    l),
      trees   = tr("nav.tab.trees",     l),
      export  = tr("nav.tab.export",    l),
      about   = tr("nav.tab.about",     l)
    ))
  }, ignoreInit = TRUE)

  # ── Actualización completa de la UI al cambiar idioma ─────────────────────────
  observeEvent(lang(), {
    l <- lang()

    # Botones de acción
    updateActionButton(session, "btn_continuar",       label = tr("config.pipeline.continue_btn",       l))
    updateActionButton(session, "btn_abrir_cfg",       label = tr("config.pipeline.project_edit",       l))
    updateActionButton(session, "btn_densidad_avanzado", label = tr("data_load.subsample.advanced_btn", l))
    updateActionButton(session, "btn_csf_avanzado",    label = tr("data_load.csf.advanced_btn",      l))
    updateActionButton(session, "btn_preprocesar",     label = tr("data_load.run_btn",                  l))
    updateActionButton(session, "btn_dem_avanzado",    label = tr("models.dem.advanced_btn",             l))
    updateActionButton(session, "btn_chm_avanzado",    label = tr("models.chm.advanced_btn",             l))
    updateActionButton(session, "btn_modelos",         label = tr("models.run_btn",                      l))
    updateActionButton(session, "btn_arb_config_aplicar", label = tr("trees.modal.apply_btn",            l))
    updateActionButton(session, "btn_arboles",         label = tr("trees.run_btn",                       l))
    updateActionButton(session, "btn_exportar",        label = tr("export.export_btn",                   l))
    updateActionButton(session, "btn_informe",         label = tr("export.HTML_btn",                      l))
    updateActionButton(session, "btn_abrir_carpeta",   label = tr("export.open_folder_btn",              l))

    # RadioButtons
    updateRadioButtons(session, "decimate_tipo",
      choiceNames  = list(tr("data_load.subsample.random",  l), tr("data_load.subsample.uniform", l)),
      choiceValues = list("aleatorio", "uniforme"))

    # Textos estáticos via JS
    session$sendCustomMessage("update_ui_text", list(
      # Banner config
      h2_banner_title    = tr("config.banner.title",    l),
      p_banner_subtitle  = tr("config.banner.subtitle", l),
      # Pipeline steps
      h5_project_step    = tr("config.pipeline.project_step",       l),
      span_project_badge = tr("config.pipeline.project_configured", l),
      h5_step_las        = tr("config.pipeline.las_title",          l),
      h5_step_shp        = tr("config.pipeline.shp_title",          l),
      h5_step_dir        = tr("config.pipeline.dir_title",          l),
      # CTA
      h4_cta_title       = tr("config.pipeline.cta_title", l),
      p_cta_desc         = tr("config.pipeline.cta_desc",  l),
      # Tab 2 — Carga de datos
      h5_subsample_header = tr("data_load.subsample.header",      l),
      p_subsample_desc    = tr("data_load.subsample.desc",        l),
      h6_method_header    = tr("data_load.subsample.method_header", l),
      p_method_tip        = tr("data_load.subsample.method_tip",  l),
      h5_csf_header       = tr("data_load.csf.header",            l),
      p_csf_desc          = tr("data_load.csf.desc",              l),
      lbl_pts_loaded      = tr("data_load.metrics.points_loaded", l),
      lbl_pts_filtered    = tr("data_load.metrics.filtered",      l),
      lbl_pts_soil        = tr("data_load.metrics.soil",          l),
      lbl_pts_veg         = tr("data_load.metrics.vegetation",    l),
      tip_cloud_3d        = tr("data_load.tips.cloud_3d",         l),
      tip_classified      = tr("data_load.tips.classified",       l),
      # Tab 3 — Modelos
      h5_dem_header       = tr("models.dem.header",               l),
      p_dem_res_label     = tr("models.dem.resolution_label",     l),
      p_cn_equidist_label = tr("models.dem.contour_label",        l),
      p_dem_win_min_label = tr("models.dem.min_filter_label",     l),
      p_dem_win_mean_label = tr("models.dem.smooth_label",        l),
      h5_chm_header       = tr("models.chm.header",               l),
      p_chm_res_label     = tr("models.chm.resolution_label",     l),
      p_chm_gap_label     = tr("models.chm.gap_fill_label",       l),
      tip_mod_dem         = tr("models.tips.dem",                 l),
      tip_mod_contours    = tr("models.tips.contours",            l),
      tip_mod_hillshade   = tr("models.tips.hillshade",           l),
      tip_mod_chm         = tr("models.tips.chm",                 l),
      tip_mod_profile     = tr("models.tips.profile",             l),
      lbl_dem_raw         = tr("models.sublabels.dem_raw",        l),
      lbl_dem_smooth      = tr("models.sublabels.dem_smooth",     l),
      # Tab 4 — Árboles
      h5_trees_header     = tr("trees.detection.header",          l),
      div_trees_desc      = tr("trees.detection.desc",            l),
      p_lmf_ws_label      = tr("trees.detection.window_label",    l),
      p_lmf_hmin_label    = tr("trees.detection.min_height_label", l),
      p_canopy_cutoff_label = tr("trees.detection.canopy_cutoff_label", l),
      modalArbConfigLabel = tr("trees.modal.title",               l),
      tip_lbl_lmf_ws      = tr("trees.modal.window_tip_label",    l),
      tip_lbl_lmf_hmin    = tr("trees.modal.min_height_tip_label", l),
      tip_lbl_canopy_cutoff = tr("trees.modal.canopy_cutoff_tip_label", l),
      btn_arb_modal_close = tr("trees.modal.close_btn",           l),
      lbl_trees_detected  = tr("trees.metrics.detected",          l),
      lbl_trees_density   = tr("trees.metrics.density",           l),
      lbl_trees_avg_height = tr("trees.metrics.avg_height",       l),
      lbl_trees_max_height = tr("trees.metrics.max_height",       l),
      tip_arb_apex        = tr("trees.tips.apex",                 l),
      tip_arb_canopy      = tr("trees.tips.canopy",               l),
      lbl_canopy_area     = tr("trees.metrics.canopy_area",       l),
      lbl_canopy_pct      = tr("trees.metrics.canopy_pct",        l),
      btn_arb_open_modal  = tr("trees.detection.advanced_btn",    l),
      # Tab 5 — Exportar
      h6_export_clouds    = tr("export.clouds_header",            l),
      h6_export_raster    = tr("export.raster_header",            l),
      h6_export_vector    = tr("export.vector_header",            l),
      h5_export_log       = tr("export.log_header",               l),
      # Footer
      footer_main_text    = tr("footer.main",                     l)
    ))

    # Tabs internos via JS
    session$sendCustomMessage("update_nav_tabs", list(
      inner_prepro_log        = tr("data_load.tabs.log",             l),
      inner_prepro_cloud      = tr("data_load.tabs.cloud_original",  l),
      inner_prepro_classified = tr("data_load.tabs.cloud_classified", l),
      inner_mod_dem           = tr("models.tabs.dem",                l),
      inner_mod_contours      = tr("models.tabs.contours",           l),
      inner_mod_hillshade     = tr("models.tabs.hillshade",          l),
      inner_mod_chm           = tr("models.tabs.chm",                l),
      inner_mod_profile       = tr("models.tabs.profile",            l),
      inner_mod_log           = tr("models.tabs.log",                l),
      inner_arb_map           = tr("trees.tabs.apex_map",            l),
      inner_arb_dist          = tr("trees.tabs.distribution",        l),
      inner_arb_canopy        = tr("trees.tabs.canopy",              l),
      inner_arb_log           = tr("trees.tabs.log",                 l)
    ))
  }, ignoreInit = TRUE)

  # ── Sección "Acerca de" reactiva al idioma ────────────────────────────────────
  output$ui_about_section <- renderUI({
    l <- lang()
    tagList(
      card(card_header(tags$h5(class="card-title", tr("about.references_header", l))),
        card_body(
          div(class="ref-box",
            tags$b(style=paste0("color:", PURPLE, ";"), tr("about.lidr_ref_title", l)), tags$br(),
            tags$p(style="margin:4px 0 0;",
              "Roussel J-R, Auty D, Coops NC, Tompalski P, Goodbody TRH, Meador AS, Bourdon J-F, de Boissieu F, Achim A (2021). ",
              tags$em("lidR: An R package for analysis of Airborne LiDAR Data."),
              " Remote Sensing of Environment, 251, 112061.",
              tags$a(href="https://doi.org/10.1016/j.rse.2020.112061",
                     "doi:10.1016/j.rse.2020.112061", target="_blank",
                     style=paste0("color:", GREEN, ";"))
            ),
            tags$p(style="margin:6px 0 0;",
              "Roussel J-R, Auty D (2023). ",
              tags$em("Airborne LiDAR Data Manipulation and Visualization for Forestry Applications."),
              " R package version 4.x. ",
              tags$a(href="https://cran.r-project.org/package=lidR",
                     "CRAN/lidR", target="_blank",
                     style=paste0("color:", GREEN, ";"))
            )
          ),
          div(class="ref-box",
            tags$b(style=paste0("color:", PURPLE, ";"), tr("about.csf_ref_title", l)), tags$br(),
            tags$p(style="margin:4px 0 0;",
              "Zhang W et al. (2016). An easy-to-use airborne LiDAR data filtering method based on cloth simulation. ",
              tags$em("Remote Sensing"), " 8(6), 501.",
              tags$a(href="https://doi.org/10.3390/rs8060501",
                     "doi:10.3390/rs8060501", target="_blank",
                     style=paste0("color:", GREEN, ";"))
            )
          )
        )
      ),
      tags$br(),
      card(
        card_header(tags$h5(class="card-title", tr("about.tool_header", l))),
        card_body(
          tags$p(tr("about.tool_description", l)),
          tags$p(style=paste0("color:", GREEN, ";font-size:13px;font-weight:600;margin:12px 0 8px;"),
            tr("about.functions_heading", l)),
          div(class="row",
            div(class="col-9",
              tags$ul(style=paste0("color:", MUTED, ";font-size:12px;"),
                tags$li(tr("about.function_1", l)),
                tags$li(tr("about.function_2", l)),
                tags$li(tr("about.function_3", l)),
                tags$li(tr("about.function_4", l)),
                tags$li(tr("about.function_5", l)),
                tags$li(tr("about.function_6", l))
              )
            ),
            div(class="col-3 d-flex align-items-start",
              img(src="assets/logo_INTA.png", alt="INTA Logo", width="120px",
                  style="opacity:0.9; margin-left:10px;")
            )
          ),
          tags$table(class="table table-sm",
            style=paste0("color:", MUTED, ";margin-top:10px;"),
            tags$tr(tags$td(tags$b(tr("about.version_label",  l))), tags$td(NVersion)),
            tags$tr(tags$td(tags$b(tr("about.updated_label",  l))), tags$td(tr("about.updated_value", l))),
            tags$tr(tags$td(tags$b(tr("about.contact_label",  l))), tags$td(tags$a(
              href="mailto:hildt.eduardo@inta.gob.ar",
              "hildt.eduardo@inta.gob.ar",
              style=paste0("color:", GREEN, ";")
            )))
          )
        )
      )
    )
  })

  # ── Registro de módulos ──────────────────────────────────────────────────────
  register_config(input, output, session, rv, ag, lang)
  register_presets(input, output, session, rv, ag, lang)
  register_preprocessing(input, output, session, rv, ag, lang)
  register_models(input, output, session, rv, ag, lang)
  register_trees(input, output, session, rv, ag, lang)
  register_export(input, output, session, rv, ag, lang)

}
