# ===============================
# SERVER_VRCC.R
# ===============================

server <- function(input, output, session) {
  
  # ============================================================
  # Shared map functions
  # ============================================================
  
  getColor <- function(status) {
    dplyr::case_when(
      status == "Completed" ~ "green",
      status == "Planned"   ~ "orange",
      status == "On-going"  ~ "blue",
      TRUE                  ~ "gray"
    )
  }
  
  project_marker_icons <- function(pts) {
    icon_by_type <- c(
      "Infrastructure"      = "home",
      "The Big Push"        = "wrench",
      "24hr Eco Market"     = "flag",
      "Infrastructure-Road" = "road",
      "New Project Type 5"  = "plus",
      "New Project Type 6"  = "leaf"
    )
    
    library_by_type <- c(
      "Infrastructure"      = "glyphicon",
      "The Big Push"        = "glyphicon",
      "24hr Eco Market"     = "fa",
      "Infrastructure-Road" = "glyphicon",
      "New Project Type 5"  = "glyphicon",
      "New Project Type 6"  = "glyphicon"
    )
    
    project_type <- as.character(pts$Project_Type)
    
    icons <- unname(icon_by_type[project_type])
    libraries <- unname(library_by_type[project_type])
    
    icons[is.na(icons)] <- "home"
    libraries[is.na(libraries)] <- "glyphicon"
    
    leaflet::awesomeIcons(
      icon = icons,
      library = libraries,
      iconColor = "white",
      markerColor = getColor(as.character(pts$Status))
    )
  }
  
  # Escape spreadsheet text before placing it in popup HTML.
  popup_text <- function(value) {
    if (length(value) == 0 || is.na(value)) {
      return("—")
    }
    
    htmltools::htmlEscape(as.character(value))
  }
  
  project_popups <- function(pts) {
    vapply(
      seq_len(nrow(pts)),
      function(i) {
        paste0(
          "<b>Project Type:</b> ",
          popup_text(pts$Project_Type[i]),
          "<br><br>",
          
          "<b>Project Name:</b> ",
          popup_text(pts$Project_Name[i]),
          
          "<br><b>Location:</b> ",
          popup_text(pts$Location[i]),
          
          "<br><b>Summary:</b> ",
          popup_text(pts$Summary[i]),
          
          "<br><b>Launched Date:</b> ",
          popup_text(pts$Launched.Date[i]),
          
          "<br><b>Status:</b> ",
          popup_text(pts$Status[i]),
          
          "<br><br>",
          make_photo_gallery(pts$Project_ID[i])
        )
      },
      character(1)
    )
  }
  
  # Both the app and the downloaded HTML call this function.
  build_project_map <- function(district_polygons, projects) {
    req(nrow(district_polygons) > 0)
    
    center <- sf::st_centroid(
      sf::st_union(district_polygons)
    )
    
    coordinates <- sf::st_coordinates(center)
    
    map <- leaflet::leaflet(
      width = "100%",
      height = 600
    ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap
      ) %>%
      leaflet::setView(
        lng = coordinates[1],
        lat = coordinates[2],
        zoom = 10
      ) %>%
      leaflet::addPolygons(
        data = district_polygons,
        fillColor = ~pal(NAME_2),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = ~NAME_2
      )
    
    if (nrow(projects) > 0) {
      map <- map %>%
        leaflet::addAwesomeMarkers(
          data = projects,
          lng = ~longitude,
          lat = ~latitude,
          icon = project_marker_icons(projects),
          popup = project_popups(projects),
          popupOptions = leaflet::popupOptions(
            maxWidth = 340,
            maxHeight = 450
          )
        )
    }
    
    map
  }
  
  # Add the gallery behavior, heading, and styling to a download.
  make_downloadable_map <- function(map, heading_text) {
    map <- htmlwidgets::onRender(
      map,
      vr_media_js
    )
    
    download_css <- paste0(
      "
      html, body {
        margin: 0;
        padding: 0;
      }

      body {
        font-family: Arial, sans-serif;
        background: #f5f5f5;
      }

      .vr-header {
        background: #ace500;
        padding: 15px 20px;
        text-align: center;
        font-size: 1.4rem;
        font-weight: bold;
        color: #1e1e4c;
        border-bottom: 2px solid #1e1e4c;
      }

      .leaflet.html-widget {
        width: 100% !important;
        height: calc(100vh - 65px) !important;
        min-height: 450px;
      }

      .vr-watermark {
        position: fixed;
        bottom: 10px;
        right: 10px;
        background: rgba(255,255,255,0.85);
        padding: 5px 12px;
        border-radius: 6px;
        font-size: 12px;
        font-family: monospace;
        color: #333;
        z-index: 10000;
        pointer-events: none;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2);
      }
      ",
      vr_media_css
    )
    
    map <- htmlwidgets::prependContent(
      map,
      htmltools::tags$style(
        htmltools::HTML(download_css)
      ),
      htmltools::tags$div(
        class = "vr-header",
        heading_text
      )
    )
    
    map <- htmlwidgets::appendContent(
      map,
      htmltools::tags$div(
        class = "vr-watermark",
        "(c) 2026 Volta Region. Creator: D.S.A (for Regional Minister)"
      )
    )
    
    map
  }
  
  # ============================================================
  # TAB 1: Projects Map
  # ============================================================
  
  observeEvent(input$districts, {
    shiny::updateCheckboxGroupInput(
      session,
      inputId = "status_filter",
      selected = "On-going"
    )
  })
  
  filtered_data <- reactive({
    req(input$districts)
    
    volta_data %>%
      dplyr::filter(NAME_2 == input$districts)
  })
  
  filtered_projects <- reactive({
    req(input$districts)
    
    pts <- vrcc_projects_sf %>%
      dplyr::filter(District == input$districts)
    
    if (
      is.null(input$status_filter) ||
      length(input$status_filter) == 0
    ) {
      return(pts[0, ])
    }
    
    pts <- pts %>%
      dplyr::filter(Status %in% input$status_filter)
    
    if (
      is.null(input$type_filter) ||
      length(input$type_filter) == 0
    ) {
      return(pts[0, ])
    }
    
    pts %>%
      dplyr::filter(Project_Type %in% input$type_filter)
  })
  
  output$vr_map <- leaflet::renderLeaflet({
    build_project_map(
      district_polygons = filtered_data(),
      projects = filtered_projects()
    )
  })
  
  output$download_map <- shiny::downloadHandler(
    filename = function() {
      district_file <- paste(input$districts, collapse = "_")
      
      if (!nzchar(district_file)) {
        district_file <- "All_Districts"
      }
      
      district_file <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        district_file
      )
      
      paste0(
        "vrccproj_",
        district_file,
        "_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
    },
    
    content = function(file) {
      df <- filtered_data()
      pts <- filtered_projects()
      
      req(nrow(df) > 0)
      
      district_name <- paste(
        input$districts,
        collapse = ", "
      )
      
      status_text <- if (length(input$status_filter) > 0) {
        paste(input$status_filter, collapse = ", ")
      } else {
        "No status selected"
      }
      
      type_text <- if (length(input$type_filter) > 0) {
        paste(input$type_filter, collapse = ", ")
      } else {
        "No type selected"
      }
      
      heading_text <- paste0(
        "Projects in Volta Region - ",
        district_name,
        " | Status: ",
        status_text,
        " | Type: ",
        type_text
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts
      )
      
      map <- make_downloadable_map(
        map,
        heading_text
      )
      
      htmlwidgets::saveWidget(
        widget = map,
        file = file,
        selfcontained = TRUE,
        title = paste(
          "VRCC Projects -",
          district_name
        )
      )
    },
    
    contentType = "text/html"
  )
  
  # ============================================================
  # TAB 2: Ho Municipal
  # ============================================================
  
  fltd_data <- reactive({
    volta_data %>%
      dplyr::filter(NAME_2 == "Ho Municipal")
  })
  
  fltd_projects <- reactive({
    req(input$hm_status_filter)
    
    pts <- vrcc_projects_sf %>%
      dplyr::filter(District == "Ho Municipal")
    
    if (input$hm_status_filter != "All") {
      pts <- pts %>%
        dplyr::filter(
          trimws(as.character(Status)) ==
            input$hm_status_filter
        )
    }
    
    # Use the Ho Municipal tab's own Project Type control.
    if (
      is.null(input$hm_type_filter) ||
      length(input$hm_type_filter) == 0
    ) {
      return(pts[0, ])
    }
    
    pts %>%
      dplyr::filter(
        Project_Type %in% input$hm_type_filter
      )
  })
  
  output$hm_map <- leaflet::renderLeaflet({
    build_project_map(
      district_polygons = fltd_data(),
      projects = fltd_projects()
    )
  })
  
  output$download_hm_map <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "vrccproj_Ho_Municipal_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
    },
    
    content = function(file) {
      df <- fltd_data()
      pts <- fltd_projects()
      
      req(nrow(df) > 0)
      
      heading_text <- paste0(
        "Projects in Ho Municipal",
        " | Status: ",
        input$hm_status_filter,
        " | Type: ",
        input$hm_type_filter
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts
      )
      
      map <- make_downloadable_map(
        map,
        heading_text
      )
      
      htmlwidgets::saveWidget(
        widget = map,
        file = file,
        selfcontained = TRUE,
        title = "VRCC Projects - Ho Municipal"
      )
    },
    
    contentType = "text/html"
  )
  
  # ============================================================
  # TAB 3: Overview
  # ============================================================
  
  overview_filtered <- reactive({
    req(
      input$overview_region,
      input$overview_gender,
      input$overview_age,
      input$overview_purchases
    )
    
    data <- sample_df %>%
      dplyr::filter(
        !is.na(Age),
        !is.na(Previous_Purchases),
        Age >= input$overview_age[1],
        Age <= input$overview_age[2],
        Previous_Purchases >= input$overview_purchases[1],
        Previous_Purchases <= input$overview_purchases[2]
      )
    
    if (input$overview_region != "All") {
      data <- data %>%
        dplyr::filter(
          Region == input$overview_region
        )
    }
    
    if (input$overview_gender != "All") {
      data <- data %>%
        dplyr::filter(
          Gender == input$overview_gender
        )
    }
    
    data
  })
  
  output$overview_total_customers <-
    shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = format(
          nrow(overview_filtered()),
          big.mark = ","
        ),
        subtitle = "Total Customers",
        icon = shiny::icon("users"),
        color = "navy"
      )
    })
  
  output$overview_conversion_rate <-
    shinydashboard::renderValueBox({
      data <- overview_filtered()
      
      rate <- if (
        nrow(data) == 0 ||
        all(is.na(data$Purchase_Made))
      ) {
        NA_real_
      } else {
        mean(data$Purchase_Made, na.rm = TRUE)
      }
      
      shinydashboard::valueBox(
        value = if (is.na(rate)) {
          "N/A"
        } else {
          paste0(round(rate * 100, 1), "%")
        },
        subtitle = "Conversion Rate",
        icon = shiny::icon("check"),
        color = "yellow"
      )
    })
  
  output$overview_total_spent <-
    shinydashboard::renderValueBox({
      total <- sum(
        overview_filtered()$Amount_Spent,
        na.rm = TRUE
      )
      
      shinydashboard::valueBox(
        value = paste0(
          "$",
          format(round(total), big.mark = ",")
        ),
        subtitle = "Revenue",
        icon = shiny::icon("dollar-sign"),
        color = "purple"
      )
    })
  
  output$overview_conversion_plot <- plotly::renderPlotly({
    data <- overview_filtered() %>%
      dplyr::filter(
        !is.na(Campaign_Exposure),
        !is.na(Purchase_Made)
      ) %>%
      dplyr::group_by(Campaign_Exposure) %>%
      dplyr::summarise(
        Conversion = mean(Purchase_Made),
        .groups = "drop"
      )
    
    shiny::validate(
      shiny::need(
        nrow(data) > 0,
        "No campaign data for these filters."
      )
    )
    
    chart <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Campaign_Exposure,
        y = Conversion,
        fill = Campaign_Exposure
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(
        labels = function(x) {
          paste0(round(x * 100), "%")
        }
      ) +
      ggplot2::labs(
        x = "Campaign",
        y = "Conversion Rate"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none"
      )
    
    plotly::ggplotly(chart)
  })
  
  output$overview_region_spend <- plotly::renderPlotly({
    data <- overview_filtered() %>%
      dplyr::filter(!is.na(Region)) %>%
      dplyr::group_by(Region) %>%
      dplyr::summarise(
        TotalSpent = sum(
          Amount_Spent,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    shiny::validate(
      shiny::need(
        nrow(data) > 0,
        "No regional data for these filters."
      )
    )
    
    chart <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Region,
        y = TotalSpent,
        fill = Region
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::labs(
        x = "Region",
        y = "Total Amount Spent"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none"
      )
    
    plotly::ggplotly(chart)
  })
  
  output$overview_scatter_plot <- plotly::renderPlotly({
    data <- overview_filtered() %>%
      dplyr::filter(
        !is.na(Income),
        !is.na(Amount_Spent)
      )
    
    shiny::validate(
      shiny::need(
        nrow(data) > 0,
        "No income and spending data for these filters."
      )
    )
    
    chart <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Income,
        y = Amount_Spent,
        color = factor(Purchase_Made)
      )
    ) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::labs(
        x = "Income",
        y = "Amount Spent",
        color = "Purchased"
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(chart)
  })
  
  output$overview_download_report <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "overview_report_",
        Sys.Date(),
        ".html"
      )
    },
    
    content = function(file) {
      report_source <- "overview_report.Rmd"
      
      if (!file.exists(report_source)) {
        stop(
          "overview_report.Rmd is missing from the app directory."
        )
      }
      
      rmarkdown::render(
        input = report_source,
        output_file = file,
        params = list(
          data = overview_filtered(),
          name = "David K. Ackuaku"
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    },
    
    contentType = "text/html"
  )
}

