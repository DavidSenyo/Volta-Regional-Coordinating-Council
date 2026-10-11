
# ===============================
# SERVER_VRCC.R
# ===============================

# Make the standalone HTML map fit below its heading on a phone.
make_download_map_mobile <- function(map) {
                                           map$width <- "100%"
                                           map$height <- "100vh"
  
  map <- htmlwidgets::prependContent(
    map,
    htmltools::tags$head(
               htmltools::tags$meta(
                                    name = "viewport",
                                    content = "width=device-width, initial-scale=1"
                                   ),
      htmltools::tags$style(htmltools::HTML("
        html, body {
          width: 100%;
          height: 100%;
          margin: 0;
          padding: 0;
          overflow: hidden;
        }

        .leaflet.html-widget {
          width: 100% !important;
          height: calc(100vh - var(--vr-header-height, 65px)) !important;
          height: calc(100dvh - var(--vr-header-height, 65px)) !important;
          min-height: 0 !important;
        }

        .leaflet-popup-content-wrapper {
          max-width: calc(100vw - 45px);
        }

        .leaflet-popup-content {
          max-width: calc(100vw - 85px);
          overflow-wrap: anywhere;
        }

        .leaflet-popup-content img,
        .leaflet-popup-content video {
          max-width: 100%;
          height: auto;
        }
      "))
    )
  )
  
  map <- htmlwidgets::onRender(
    map,
    "
    function(el, x) {
      var leafletMap = this;

      function resizeMap() {
        var header = document.querySelector('.vr-header');
        var headerHeight = header
          ? Math.ceil(header.getBoundingClientRect().height)
          : 65;

        document.documentElement.style.setProperty(
          '--vr-header-height',
          headerHeight + 'px'
        );

        setTimeout(function() {
          leafletMap.invalidateSize();
        }, 50);
      }

      setTimeout(resizeMap, 300);
      window.addEventListener('resize', resizeMap);
      window.addEventListener('orientationchange', function() {
        setTimeout(resizeMap, 300);
      });
    }
    "
  )
  
  map
}

server <- function(input, output, session) {
  
  # ============================================================
  # SHARED MAP FUNCTIONS
  # ============================================================
  
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
    
    marker_color_by_type <- c(
                              "Infrastructure"      = "blue",
                              "The Big Push"        = "orange",
                              "24hr Eco Market"     = "green",
                              "Infrastructure-Road" = "purple",
                              "New Project Type 5"  = "red",
                              "New Project Type 6"  = "cadetblue"
                             )
    
    project_type <- as.character(pts$Project_Type)
    
    icons <- unname(icon_by_type[project_type])
    libraries <- unname(library_by_type[project_type])
    marker_colors <- unname(marker_color_by_type[project_type])
    
    # Defaults for types not listed above.
    icons[is.na(icons)] <- "home"
    libraries[is.na(libraries)] <- "glyphicon"
    marker_colors[is.na(marker_colors)] <- "gray"
    
    leaflet::awesomeIcons(
      icon = icons,
      library = libraries,
      iconColor = "white",
      markerColor = marker_colors
    )
  }
  
  # Escape spreadsheet text before adding it to popup HTML.
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
  
  # Used by the live maps and both HTML downloads.
  build_project_map <- function(district_polygons, projects) {
    shiny::req(nrow(district_polygons) > 0)
    
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
  
  # Add the media behavior, heading, and watermark to an export.
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
    
    htmlwidgets::appendContent(
      map,
      htmltools::tags$div(
        class = "vr-watermark",
        "(c) 2026 Volta Region. Creator: D.S.A (for Regional Minister)"
      )
    )
  }
  
  # ============================================================
  # TAB 1: PROJECTS MAP
  # ============================================================
  
  filtered_data <- shiny::reactive({
    shiny::req(input$districts)
    
    volta_data %>%
      dplyr::filter(NAME_2 == input$districts)
  })
  
  filtered_projects <- shiny::reactive({
    shiny::req(input$districts)
    
    pts <- vrcc_projects_sf %>%
      dplyr::filter(District == input$districts)
    
    if (is.null(input$status_filter) ||
        length(input$status_filter) == 0 ||
        is.null(input$type_filter) ||
        length(input$type_filter) == 0) {
      return(pts[0, ])
    }
    
    pts %>%
      dplyr::filter(
        Status %in% input$status_filter,
        Project_Type %in% input$type_filter
      )
  })
  
  output$vr_map <- leaflet::renderLeaflet({
    df <- filtered_data()
    pts <- filtered_projects()
    
    shiny::req(nrow(df) > 0)
    
    build_project_map(
      district_polygons = df,
      projects = pts
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
      
      shiny::req(nrow(df) > 0)
      
      district_name <- paste(input$districts, collapse = ", ")
      
      status_text <- if (length(input$status_filter) > 0) {
        paste(input$status_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      type_text <- if (length(input$type_filter) > 0) {
        paste(input$type_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      heading_text <- paste0(
        "Projects in Volta Region - ", district_name,
        " | Status: ", status_text,
        " | Type: ", type_text
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts
      )
      
      export_map <- make_downloadable_map(
        map,
        heading_text
      )
      
      export_map <- make_download_map_mobile(
        export_map
      )
      
      htmlwidgets::saveWidget(
        widget = export_map,
        file = file,
        selfcontained = TRUE,
        title = paste("VRCC Projects -", district_name)
      )
    },
    
    contentType = "text/html"
  )
  
  # ============================================================
  # TAB 2: HO MUNICIPAL
  # ============================================================
  
  fltd_data <- shiny::reactive({
    volta_data %>%
      dplyr::filter(NAME_2 == "Ho Municipal")
  })
  
  fltd_projects <- shiny::reactive({
    pts2 <- vrcc_projects_sf %>%
      dplyr::filter(District == "Ho Municipal")
    
    if (is.null(input$hm_status_filter) ||
        length(input$hm_status_filter) == 0 ||
        is.null(input$hm_type_filter) ||
        length(input$hm_type_filter) == 0) {
      return(pts2[0, ])
    }
    
    pts2 %>%
      dplyr::filter(
        Status %in% input$hm_status_filter,
        Project_Type %in% input$hm_type_filter
      )
  })
  
  output$hm_map <- leaflet::renderLeaflet({
    df <- fltd_data()
    pts2 <- fltd_projects()
    
    shiny::req(nrow(df) > 0)
    
    build_project_map(
      district_polygons = df,
      projects = pts2
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
      pts2 <- fltd_projects()
      
      shiny::req(nrow(df) > 0)
      
      status_text <- if (length(input$hm_status_filter) > 0) {
        paste(input$hm_status_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      type_text <- if (length(input$hm_type_filter) > 0) {
        paste(input$hm_type_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      heading_text <- paste0(
        "Projects in Ho Municipal",
        " | Status: ", status_text,
        " | Type: ", type_text
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts2
      )
      
      export_map <- make_downloadable_map(
        map,
        heading_text
      )
      
      export_map <- make_download_map_mobile(
        export_map
      )
      
      htmlwidgets::saveWidget(
        widget = export_map,
        file = file,
        selfcontained = TRUE,
        title = "VRCC Projects - Ho Municipal"
      )
    },
    
    contentType = "text/html"
  )
}



