# ===============================
# UI_VRCC.R
# ===============================

project_choices <- sort(unique(as.character(stats::na.omit(vrcc_projects$Project_Type))))
default_project_type <- project_choices
stats_district_choices <- sort(unique(as.character(stats::na.omit(vrcc_projects_sf$District))))
stats_type_choices <- sort(unique(as.character(stats::na.omit(vrcc_projects_sf$Project_Type))))


# ==========================================
# MAIN USER INTERFACE
# ==========================================
ui <- shinydashboard::dashboardPage(
                   shinydashboard::dashboardHeader(
                                   title = tags$div(
                                                    tags$strong("VRCC", style = "font-size: 20px; line-height: 1.2;"),
                                                    tags$div("Volta Regional Coordinating Council",
                                                    style = "font-size: 12px; font-weight: normal; opacity: 0.8;")
                                                  )
                                                   ),
 # ========================================
 # SIDEBAR
 # ========================================
                   
  shinydashboard::dashboardSidebar(
    width = 300,
    
# --------------------------------------
# PROJECT STATISTICS SIDEBAR INFORMATION
# --------------------------------------

    conditionalPanel(
                    condition = "input.main_tabs === 'Project Statistics'",
                    h4("Project Statistics", style = "color: white;"),
                    tags$p("Explore project distribution, implementation status, project types and district performance. 
                           See the location of the projects on the map in the next tab",
                           style = "color: white; font-size: 13px;"),
                    hr(),
                    tags$p("Use the filters in the Project Statistics tab to refine the report and download if you want to",
                           style = "color: white; font-size: 12px;")
                   ),

# --------------------------------------
# PROJECTS MAP FILTERS
# --------------------------------------

    conditionalPanel(
                    condition = "input.main_tabs === 'Projects Map'",
                    selectInput("districts",
                                "Select a District/Municipal",
                                choices = volta_districts, 
                                selected = "Ho Municipal"
                                ),
                    selectInput("type_filter", 
                                "Project Type", 
                                choices = project_choices,
                                selected = default_project_type, 
                                multiple = TRUE
                                ),
                    checkboxGroupInput("status_filter",
                                       "Project Status",
                                       choices = c("Completed", "Planned", "On-going"),
                                       selected = c("Completed", "Planned", "On-going")
                                       ),
      br(), hr(),
                  downloadButton("download_map", 
                                 "Download VR Map for Selected District"),
      br(),
                  tags$div(style = "margin-top: 10px;",
                           tags$p("On an iPhone, use the live interactive map as shown. Downloaded HTML files may not display in the iPhone's Files preview.",
                                  style = "color: white; font-size: 12px;"))
                                ),
# --------------------------------------
# HO MUNICIPAL FILTERS
# --------------------------------------

    conditionalPanel(
                    condition = "input.main_tabs === 'Ho Municipal'",
                    selectInput("hm_type_filter", 
                                "Project Type", 
                                choices = project_choices,
                                selected = default_project_type, 
                                multiple = TRUE
                                ),
        br(), hr(),
                    downloadButton("download_hm_map", 
                                   "Download Ho Municipal Map"),
        br(), br(),
                    tags$p("On an iPhone, use the live interactive map as shown. Downloaded HTML files may not display in the iPhone's Files preview.",
                           style = "color: white; font-size: 12px;")
                   ),
    br(), br(),
# --------------------------------------
# FOOTER
# --------------------------------------
                    tags$div(
                             style = "padding: 20px 15px; color: #B8C9CC; font-size: 12px;",
                             HTML("&copy; 2026 Volta Regional Coordinating Council (VRCC)<br>Creator: D.A Analytics (senyoackuaku@gmail.com); +233550359499")
                            )
                         ),

# ========================================
# DASHBOARD BODY
# ========================================
  shinydashboard::dashboardBody(
    
    # --------------------------------------
    # CUSTOM CSS
    # --------------------------------------
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #F3F7F6 !important; }
        .main-header .logo, .main-header .navbar { background-color: #0B6E75 !important; }
        .main-sidebar, .left-side { background-color: #123B46 !important; }
        .box { border-top-color: #0B6E75 !important; }
        .hm-status-strip {
          background: rgba(255,255,255,0.96); padding: 5px 8px;
          border-radius: 6px; box-shadow: 0 1px 5px rgba(0,0,0,0.25);
          max-width: calc(100vw - 20px);
        }
        .hm-status-strip .shiny-input-container { width: auto !important; margin-bottom: 0; }
        .hm-status-strip .control-label { display: block; font-size: 12px; margin-bottom: 2px; }
        .hm-status-strip .shiny-options-group {
          display: flex; flex-wrap: nowrap; gap: 10px;
          overflow-x: auto; white-space: nowrap;
        }
        .hm-status-strip .radio-inline { margin-left: 0 !important; padding-top: 0; }
        .stats-chart-container {
          background: white; padding: 15px; border-radius: 6px;
          margin-bottom: 15px; min-height: 360px;
        }
        .stats-section-title {
          color: #123B46; font-weight: 600;
          margin-top: 20px; margin-bottom: 15px;
        }
        @media (max-width: 767px) {
          .hm-status-strip { font-size: 12px; }
          .hm-status-strip .shiny-options-group { gap: 8px; }
          .stats-chart-container { min-height: 300px; padding: 10px; }
          .content-wrapper { overflow-x: hidden; }
          .nav-tabs > li > a { padding: 10px 9px; font-size: 12px; }
        }
      "))
    ),
    # --------------------------------------
    # EXISTING MEDIA GALLERY ASSETS
    # --------------------------------------
    media_gallery_assets,
    
    fluidRow(
      shinydashboard::tabBox(
        id = "main_tabs",
        width = 12,
        selected = "Project Statistics",
        
        # ==================================================
        # TAB 1: PROJECT STATISTICS (default opening tab)
        # ==================================================
        tabPanel(
                title = "Project Statistics",
                h3("VRCC - Project Statistics", style = "color: #123B46;"),
                p("Statistical overview of projects across the Volta Region."),
          hr(),
          fluidRow(
                  column(6,
                         selectInput("stats_district", 
                                     "District / Municipality",
                                     choices = c("All" = "All", stats_district_choices),
                                     selected = "All"
                                     )
                         ),
                  column(6,
                         selectInput("stats_type", 
                                     "Project Type",
                                     choices = c("All" = "All", stats_type_choices),
                                     selected = "All"
                                     )
                         )
          ),
          br(),
          fluidRow(
            shinydashboard::valueBoxOutput("stats_total_projects", width = 3),
            shinydashboard::valueBoxOutput("stats_district_count", width = 3),
            shinydashboard::valueBoxOutput("stats_completed", width = 3),
            shinydashboard::valueBoxOutput("stats_completion_rate", width = 3)
          ),
          br(),
          fluidRow(
            column(6, div(class = "stats-chart-container",
                          h4("Projects by District / Municipality", class = "stats-section-title"),
                          plotly::plotlyOutput("stats_district_plot", height = "320px"))),
            column(6, div(class = "stats-chart-container",
                          h4("Projects by Status", class = "stats-section-title"),
                          plotly::plotlyOutput("stats_status_plot", height = "320px")))
          ),
          fluidRow(
            column(6, div(class = "stats-chart-container",
                          h4("Projects by Type", class = "stats-section-title"),
                          plotly::plotlyOutput("stats_type_plot", height = "320px"))),
            column(6, div(class = "stats-chart-container",
                          h4("Project Status by District", class = "stats-section-title"),
                          plotly::plotlyOutput("stats_district_status_plot", height = "320px")))
          ),
          hr(),
          h4("District / Municipality Project Summary", class = "stats-section-title"),
          DT::DTOutput("stats_district_table"),
          br(),
          h4("Project Type by District", class = "stats-section-title"),
          DT::DTOutput("stats_type_table"),
          br(),
          h4("Detailed Project Records", class = "stats-section-title"),
          DT::DTOutput("stats_project_table"),
          br(),
          fluidRow(
            column(6,
                   downloadButton("download_project_statistics", "Download Statistics (Excel)")),
            column(6,
                   downloadButton("download_statistics_html", "Download All Graphs (HTML)"),
                   tags$div(
                     style = "margin-top: 10px;",
                     tags$p(
                       "On an iPhone, use the live interactive graphs as shown. ",
                       "Downloaded HTML files may not display correctly in the iPhone's Files preview. ",
                       "For the best experience, open the downloaded HTML in a compatible web browser or on a computer.",
                       style = "color: #555; font-size: 12px; line-height: 1.5;"
                     )
                   ))
          ),
          br(), br()
        ),
        
        # ==================================
        # TAB 2: PROJECTS MAP
        # ==================================
        tabPanel(
          title = "Projects Map",
          hr(),
          leafletOutput("vr_map", height = "600px"),
          hr(), hr()
        ),
        
        # ==================================
        # TAB 3: HO MUNICIPAL
        # ==================================
        tabPanel(
          title = "Ho Municipal",
          hr(),
          div(
            style = "position: relative; height: 600px;",
            leafletOutput("hm_map", height = "600px"),
            absolutePanel(
              top = 10, right = 10, width = 175, draggable = FALSE,
              style = paste(
                "z-index: 1100;", "background: white;", "padding: 10px 12px;",
                "border-radius: 6px;", "box-shadow: 0 2px 8px rgba(0,0,0,0.25);"
              ),
              div(
                class = "hm-status-strip",
                checkboxGroupInput(
                  "hm_status_filter", "Project Status",
                  choices = c("Completed", "Planned", "On-going"),
                  selected = c("Completed", "Planned", "On-going"),
                  inline = TRUE
                )
              )
            )
          ),
          hr(), hr()
        )
      )
    )
  ),
  skin = "green"
)
