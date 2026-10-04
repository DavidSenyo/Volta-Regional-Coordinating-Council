# ===============================
# UI_VRCC.R
# ===============================

project_choices <- sort(unique(
                              as.character(na.omit(vrcc_projects$Project_Type))
                              ))

default_project_type <- if ("Infrastructure" %in% project_choices) {
                            "Infrastructure"
                              } else {
                               project_choices[1]
                                      }

ui <- dashboardPage(
  
dashboardHeader(
                title = tags$div(
                                tags$strong(
                                            "VRCC",
                                             style = "font-size: 20px; line-height: 1.2;"
                                            ),
              tags$div(
                       "Volta Regional Coordinating Council",
                        style = "font-size: 12px; font-weight: normal; opacity: 0.8;"
                    )
                              )
               ),
  
dashboardSidebar(
    width = 300,
    
    # Filters for Projects Map
conditionalPanel(
      condition = "input.main_tabs === 'Projects Map'",
      
      selectInput(
                  inputId = "districts",
                  label = "Select a District/Municipal",
                  choices = volta_districts,
                  selected = "Ho Municipal"
                  ),
      
      selectInput(
                  inputId = "type_filter",
                  label = "Project Type",
                  choices = project_choices,
                  selected = default_project_type
                 ),
      
      checkboxGroupInput(
                         inputId = "status_filter",
                         label = "Project Status",
                         choices = c("Completed", "Planned", "On-going"),
                         selected = "On-going"
                         ),
      
      br(),
      hr(),
      
      downloadButton(
                     "download_map",
                     "Download VR Map for Selected District"
                    )
                ), br(), br(), 
    
    # Filters for Ho Municipal
conditionalPanel(
                 condition = "input.main_tabs === 'Ho Municipal'",
      
      selectInput(
                 inputId = "hm_type_filter",
                 label = "Project Type",
                 choices = project_choices,
                 selected = default_project_type
                 ),
      
      br(),
      hr(),
      
      downloadButton(
                    "download_hm_map",
                    "Download Ho Municipal Map"
                   )
                 ), br(), br(), 
    
    # Filters for Overview
conditionalPanel(
                 condition = "input.main_tabs === 'overview'",
      
                h4(
                  "Filters for Overview Tab",
                  style = "color: white; padding: 10px 15px 0;"
                  ),
      
      div(
          style = "padding: 0 15px 15px;",
        
        selectInput(
                    inputId = "overview_region",
                    label = "Region",
                    choices = c(
                               "All", sort(unique(na.omit(sample_df$Region)))
                               ),
                    selected = "All"
                   ),
        
        selectInput(
                    inputId = "overview_gender",
                    label = "Gender",
                    choices = c(
                               "All", sort(unique(na.omit(sample_df$Gender)))
                               ),
                    selected = "All"
                   ),
        
        sliderInput(
                    inputId = "overview_age",
                    label = "Age",
                    min = floor(min(sample_df$Age, na.rm = TRUE)),
                    max = ceiling(max(sample_df$Age, na.rm = TRUE)),
                    value = c(
                             floor(min(sample_df$Age, na.rm = TRUE)),
                             ceiling(max(sample_df$Age, na.rm = TRUE))
                              )
                    ),
        
        sliderInput(
                    inputId = "overview_purchases",
                    label = "Previous Purchases",
                    min = floor(min(
                                    sample_df$Previous_Purchases,
                                    na.rm = TRUE
                                    )),
                    max = ceiling(max(
                                     sample_df$Previous_Purchases,
                                     na.rm = TRUE
                                    )),
                    value = c(
                             floor(min(
                                       sample_df$Previous_Purchases,
                                       na.rm = TRUE
                                      )),
                    ceiling(max(
                               sample_df$Previous_Purchases,
                               na.rm = TRUE
                              ))
          )
        )
      )
    ),
    
    tags$div(
      style = paste(
                    "padding: 20px 15px;",
                    "color: #B8C9CC;",
                    "font-size: 12px;"
                    ),
      HTML(
        "&copy; 2026 Volta Regional Coordinating Council (VRCC)<br>
         Creator: D.A Analytics (senyoackuaku@gmail.com); +233550359499"
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #F3F7F6 !important;
        }

        .main-header .logo,
        .main-header .navbar {
          background-color: #0B6E75 !important;
        }

        .main-sidebar, .left-side {
          background-color: #123B46 !important;
        }

        .box {
          border-top-color: #0B6E75 !important;
        }
      "))
    ),
    
    # Existing gallery CSS/JavaScript object
    media_gallery_assets,
    
    fluidRow(
      tabBox(
        id = "main_tabs",
        width = 12,
        
        tabPanel(
          "Projects Map",
          hr(), 
          leafletOutput("vr_map", height = "600px"), hr(), hr()
        ),
        
        tabPanel(
          "Ho Municipal",
          hr(), 
          div(
            style = "position: relative; height: 600px;",
            
            leafletOutput("hm_map", height = "600px"), 
            
            absolutePanel(
              top = 10,
              right = 10,
              width = 175,
              draggable = FALSE,
              style = paste(
                "z-index: 1100;",
                "background: white;",
                "padding: 10px 12px;",
                "border-radius: 6px;",
                "box-shadow: 0 2px 8px rgba(0,0,0,0.25);"
              ),
              
              radioButtons(
                inputId = "hm_status_filter",
                label = "Project Status",
                choices = c(
                  "All",
                  "On-going",
                  "Completed",
                  "Planned"
                ),
                selected = "On-going"
              )
            )
          ), hr(), hr(),
        ),
        
        tabPanel(
          "Overview",
          value = "overview",
          
          fluidRow(
            valueBoxOutput("overview_total_customers", width = 4),
            valueBoxOutput("overview_conversion_rate", width = 4),
            valueBoxOutput("overview_total_spent", width = 4)
          ),
          
          fluidRow(
            column(
              width = 12,
              downloadButton(
                "overview_download_report",
                "Download Report (Overview)"
              ),
              br(),
              br()
            )
          ),
          
          fluidRow(
            box(
              title = "Conversion by Campaign",
              width = 6,
              plotlyOutput(
                "overview_conversion_plot",
                height = "350px"
              )
            ),
            
            box(
              title = "Spend by Region",
              width = 6,
              plotlyOutput(
                "overview_region_spend",
                height = "350px"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Income vs Amount Spent",
              width = 12,
              plotlyOutput(
                "overview_scatter_plot",
                height = "400px"
              )
            )
          )
        )
      )
    )
  ),
  
  skin = "green"
)


