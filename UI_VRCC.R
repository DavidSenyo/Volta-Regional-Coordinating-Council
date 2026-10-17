
# ===============================
# UI_VRCC.R
# ===============================

project_choices <- sort(unique(trimws(as.character(stats::na.omit(vrcc_projects_sf$Project_Type)))))
project_choices <- project_choices[nzchar(project_choices)]
stats_district_choices <- sort(unique(trimws(as.character(stats::na.omit(vrcc_projects_sf$District)))))
stats_district_choices <- stats_district_choices[nzchar(stats_district_choices)]
stats_type_choices <- project_choices
ho_type_choices <- sort(unique(trimws(as.character(stats::na.omit(
  vrcc_projects_sf$Project_Type[trimws(as.character(vrcc_projects_sf$District)) == "Ho Municipal"]
)))))
ho_type_choices <- ho_type_choices[nzchar(ho_type_choices)]

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = shiny::tags$div(
      shiny::tags$strong("VRCC", style="font-size:20px;line-height:1.2;"),
      shiny::tags$div("Volta Regional Coordinating Council",
                      style="font-size:12px;font-weight:normal;opacity:0.8;")
    )
  ),
  shinydashboard::dashboardSidebar(
    width=300,
    shiny::conditionalPanel(
      condition="input.main_tabs === 'Project Statistics'",
      shiny::h4("Project Statistics", style="color:white;"),
      shiny::tags$p("Explore project distribution, implementation status, project types and district performance. See the locations on the map in the next tab.", style="color:white;font-size:13px;"),
      shiny::hr(),
      shiny::tags$p("Use the filters within Project Statistics to refine and download the report.", style="color:white;font-size:12px;")
    ),
    shiny::conditionalPanel(
      condition="input.main_tabs === 'Projects Map'",
      shiny::selectInput("districts", "Select a District/Municipal",
                         choices=volta_districts, selected="Ho Municipal"),
      shiny::selectInput("type_filter", "Project Type", choices=project_choices,
                         selected=project_choices, multiple=TRUE),
      shiny::checkboxGroupInput("status_filter", "Project Status",
                                choices=c("Completed", "Planned", "On-going"),
                                selected=c("Completed", "Planned", "On-going")),
      shiny::br(), shiny::hr(),
      shiny::downloadButton("download_map", "Download VR Map for Selected District"),
      shiny::tags$p("On iPhone, use the live map. Downloaded HTML may not display in the Files preview.", style="color:white;font-size:12px;margin-top:10px;")
    ),
    shiny::conditionalPanel(
      condition="input.main_tabs === 'Ho Municipal Statistics'",
      shiny::h4("Ho Municipal Statistics", style="color:white;"),
      shiny::tags$p("Filter by project type, status, and Town/Village in the statistics tab. See the locations on the map in the next tab", style="color:white;font-size:13px;")
    ),
    shiny::conditionalPanel(
      condition="input.main_tabs === 'Ho Municipal'",
      shiny::selectInput("hm_type_filter", "Project Type", choices=project_choices,
                         selected=project_choices, multiple=TRUE),
      shiny::br(), shiny::hr(),
      shiny::downloadButton("download_hm_map", "Download Ho Municipal Map"),
      shiny::tags$p("On iPhone, use the live map. Downloaded HTML may not display in the Files preview.", style="color:white;font-size:12px;margin-top:10px;")
    ),
    shiny::br(), shiny::br(),
    shiny::tags$div(style="padding:20px 15px;color:#B8C9CC;font-size:12px;",
                    shiny::HTML("&copy; 2026 Volta Regional Coordinating Council (VRCC)<br>Creator: D.A Analytics (senyoackuaku@gmail.com); +233550359499"))
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(shiny::tags$style(shiny::HTML('
      .content-wrapper,.right-side { background-color:#F3F7F6 !important; }
      .main-header .logo,.main-header .navbar { background-color:#0B6E75 !important; }
      .main-sidebar,.left-side { background-color:#123B46 !important; }
      .box { border-top-color:#0B6E75 !important; }
      .hm-status-strip { background:rgba(255,255,255,.96);padding:5px 8px;
        border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.25);
        max-width:calc(100vw - 20px); }
      .hm-status-strip .shiny-input-container { width:auto !important;margin-bottom:0; }
      .hm-status-strip .control-label { display:block;font-size:12px;margin-bottom:2px; }
      .hm-status-strip .shiny-options-group { display:flex;flex-wrap:wrap;gap:6px; }
      .hm-status-strip .radio-inline { margin-left:0 !important;padding-top:0; }
      .stats-chart-container { background:white;padding:15px;border-radius:6px;
        margin-bottom:15px;min-height:360px; }
      .stats-section-title { color:#123B46;font-weight:600;margin-top:20px;margin-bottom:15px; }
      @media(max-width:767px) {
        .hm-status-strip { font-size:12px; }
        .stats-chart-container { min-height:300px;padding:10px; }
        .content-wrapper { overflow-x:hidden; }
        .nav-tabs>li>a { padding:10px 9px;font-size:12px; }
      }
    '))),
    media_gallery_assets,
    shiny::fluidRow(
      shinydashboard::tabBox(
        id="main_tabs", width=12, selected="Project Statistics",
        
        # =======================================================
        # TAB 1: PROJECT STATISTICS
        # =======================================================
        shiny::tabPanel(
          title="Project Statistics",
          shiny::h3("VRCC - Project Statistics", style="color:#123B46;"),
          shiny::p("Statistical overview of projects across the Volta Region."), shiny::hr(),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput("stats_district", "District / Municipality",
                                                choices=c("All",stats_district_choices), selected="All")),
            shiny::column(6, shiny::selectInput("stats_type", "Project Type",
                                                choices=c("All",stats_type_choices), selected="All"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("stats_total_projects",width=3),
            shinydashboard::valueBoxOutput("stats_district_count",width=3),
            shinydashboard::valueBoxOutput("stats_completed",width=3),
            shinydashboard::valueBoxOutput("stats_completion_rate",width=3)
          ), shiny::br(),
          shiny::fluidRow(
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by District / Municipality",class="stats-section-title"),
                                       plotly::plotlyOutput("stats_district_plot",height="320px"))),
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by Status",class="stats-section-title"),
                                       plotly::plotlyOutput("stats_status_plot",height="320px")))
          ),
          shiny::fluidRow(
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by Type",class="stats-section-title"),
                                       plotly::plotlyOutput("stats_type_plot",height="320px"))),
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Project Status by District",class="stats-section-title"),
                                       plotly::plotlyOutput("stats_district_status_plot",height="320px")))
          ),
          shiny::hr(),
          shiny::h4("District / Municipality Project Summary",class="stats-section-title"),
          DT::DTOutput("stats_district_table"), shiny::br(),
          shiny::h4("Project Type by District",class="stats-section-title"),
          DT::DTOutput("stats_type_table"), shiny::br(),
          shiny::h4("Detailed Project Records",class="stats-section-title"),
          DT::DTOutput("stats_project_table"), shiny::br(),
          shiny::fluidRow(
            shiny::column(6,shiny::downloadButton("download_project_statistics","Download Statistics (Excel)")),
            shiny::column(6,shiny::downloadButton("download_statistics_html","Download All Graphs (HTML)"),
                          shiny::tags$p("On iPhone, view graphs in the app. Files preview may not display downloaded HTML correctly; use a compatible browser or computer.",
                                        style="color:#555;font-size:12px;line-height:1.5;margin-top:10px;"))
          ), shiny::br(),shiny::br()
        ),
        
        # =======================================================
        # TAB 2: PROJECTS MAP
        # =======================================================
        shiny::tabPanel(title="Projects Map", shiny::hr(),
                        leaflet::leafletOutput("vr_map",height="600px"),shiny::hr(),shiny::hr()),
        
        # =======================================================
        # TAB 3: HO MUNICIPAL STATISTICS
        # =======================================================
        shiny::tabPanel(
          title="Ho Municipal Statistics",
          shiny::h3("Ho Municipal - Project Statistics",style="color:#123B46;"),
          shiny::p("Project statistics for Ho Municipal. Town/Village currently uses the Location field."),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(4,shiny::selectInput("ho_stats_type","Project Type",
                                               choices=c("All",ho_type_choices),selected="All")),
            shiny::column(4,shiny::selectInput("ho_stats_status","Project Status",
                                               choices=c("All","Completed","On-going","Planned"),selected="All")),
            shiny::column(4,shiny::selectInput("ho_stats_town","Town/Village",
                                               choices=c("All"="All"),selected="All"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("ho_stats_total_projects",width=3),
            shinydashboard::valueBoxOutput("ho_stats_village_count",width=3),
            shinydashboard::valueBoxOutput("ho_stats_completed",width=3),
            shinydashboard::valueBoxOutput("ho_stats_completion_rate",width=3)
          ), shiny::br(),
          shiny::fluidRow(
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by Town/Village",class="stats-section-title"),
                                       plotly::plotlyOutput("ho_stats_village_plot",height="320px"))),
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by Status",class="stats-section-title"),
                                       plotly::plotlyOutput("ho_stats_status_plot",height="320px")))
          ),
          shiny::fluidRow(
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Projects by Type",class="stats-section-title"),
                                       plotly::plotlyOutput("ho_stats_type_plot",height="320px"))),
            shiny::column(6,shiny::div(class="stats-chart-container",
                                       shiny::h4("Project Status by Town/Village",class="stats-section-title"),
                                       plotly::plotlyOutput("ho_stats_village_status_plot",height="320px")))
          ),
          shiny::hr(),
          shiny::h4("Town/Village Project Summary",class="stats-section-title"),
          DT::DTOutput("ho_stats_village_table"),shiny::br(),
          shiny::h4("Project Type by Town/Village",class="stats-section-title"),
          DT::DTOutput("ho_stats_type_table"),shiny::br(),
          shiny::h4("Detailed Ho Municipal Projects",class="stats-section-title"),
          DT::DTOutput("ho_stats_project_table"),shiny::br(),
          shiny::fluidRow(
            shiny::column(6,shiny::downloadButton("download_ho_statistics","Download Statistics (Excel)")),
            shiny::column(6,shiny::downloadButton("download_ho_statistics_html","Download All Graphs (HTML)"),
                          shiny::tags$p("On iPhone, view graphs in the app. Files preview may not display downloaded HTML correctly.",
                                        style="color:#555;font-size:12px;margin-top:10px;"))
          ),shiny::br(),shiny::br()
        ),
        
        # =======================================================
        # TAB 4: HO MUNICIPAL MAP
        # =======================================================
        shiny::tabPanel(
          title="Ho Municipal",shiny::hr(),
          shiny::div(style="position:relative;height:600px;",
                     leaflet::leafletOutput("hm_map",height="600px"),
                     shiny::absolutePanel(top=10,right=10,width=175,draggable=FALSE,
                                          style=paste("z-index:1100;","background:white;","padding:10px 12px;",
                                                      "border-radius:6px;","box-shadow:0 2px 8px rgba(0,0,0,.25);"),
                                          shiny::div(class="hm-status-strip",
                                                     shiny::checkboxGroupInput("hm_status_filter","Project Status",
                                                                               choices=c("Completed","Planned","On-going"),
                                                                               selected=c("Completed","Planned","On-going"),inline=TRUE)
                                          )
                     )
          ),shiny::hr(),shiny::hr()
        )
      )
    )
  ),
  skin="green"
)



