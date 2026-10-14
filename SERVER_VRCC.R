# =============================================================
# SERVER_VRCC.R - Complete replacement
# Requires objects from DATAREAD_VRCC.R:
# volta_data, vrcc_projects_sf, pal, make_photo_gallery,
# vr_media_js, vr_media_css
# =============================================================

make_download_map_mobile <- function(map) {
  map$width <- "100%"
  map$height <- "100vh"
  map <- htmlwidgets::prependContent(
    map,
    htmltools::tags$head(
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$style(htmltools::HTML('
        html, body { width:100%; height:100%; margin:0; padding:0; overflow:hidden; }
        .leaflet.html-widget {
          width:100% !important;
          height:calc(100vh - var(--vr-header-height, 65px)) !important;
          height:calc(100dvh - var(--vr-header-height, 65px)) !important;
          min-height:0 !important;
        }
        .leaflet-popup-content-wrapper { max-width:calc(100vw - 45px); }
        .leaflet-popup-content { max-width:calc(100vw - 85px); overflow-wrap:anywhere; }
        .leaflet-popup-content img, .leaflet-popup-content video { max-width:100%; height:auto; }
      '))
    )
  )
  htmlwidgets::onRender(map, '
    function(el, x) {
      var leafletMap = this;
      function resizeMap() {
        var header = document.querySelector(".vr-header");
        var h = header ? Math.ceil(header.getBoundingClientRect().height) : 65;
        document.documentElement.style.setProperty("--vr-header-height", h + "px");
        setTimeout(function() { leafletMap.invalidateSize(); }, 50);
      }
      setTimeout(resizeMap, 300);
      window.addEventListener("resize", resizeMap);
      window.addEventListener("orientationchange", function() { setTimeout(resizeMap, 300); });
    }
  ')
}

server <- function(input, output, session) {
  
  # ==================== SHARED MAP HELPERS ====================
  project_marker_icons <- function(pts) {
    
    icon_by_type <- c(
      "Infrastructure"="home", 
      "The Big Push"="wrench",
      "24hr Eco Market"="flag", 
      "Infrastructure-Road"="road",
      "New Project Type 5"="plus", 
      "New Project Type 6"="leaf"
    )
    library_by_type <- c(
      "Infrastructure"="glyphicon", 
      "The Big Push"="glyphicon",
      "24hr Eco Market"="fa", 
      "Infrastructure-Road"="glyphicon",
      "New Project Type 5"="glyphicon", 
      "New Project Type 6"="glyphicon"
    )
    marker_color_by_type <- c(
      "Infrastructure"="blue",
      "The Big Push"="orange",
      "24hr Eco Market"="green", 
      "Infrastructure-Road"="purple",
      "New Project Type 5"="red",
      "New Project Type 6"="cadetblue"
    )
    
    types <- as.character(pts$Project_Type)
    icons <- unname(icon_by_type[types]); icons[is.na(icons)] <- "home"
    libs <- unname(library_by_type[types]); libs[is.na(libs)] <- "glyphicon"
    colors <- unname(marker_color_by_type[types]); colors[is.na(colors)] <- "gray"
    leaflet::awesomeIcons(icon=icons, library=libs, iconColor="white", markerColor=colors)
  }
  
  
  popup_text <- function(value) {
    if (length(value) == 0L || is.na(value[1])) return("—")
    htmltools::htmlEscape(as.character(value[1]))
  }
  
  project_popups <- function(pts) {
    vapply(seq_len(nrow(pts)), function(i) {
      paste0(
        "<b>Project Type:</b> ", popup_text(pts$Project_Type[i]), "<br><br>",
        "<b>Project Name:</b> ", popup_text(pts$Project_Name[i]),
        "<br><b>Location:</b> ", popup_text(pts$Location[i]),
        "<br><b>Summary:</b> ", popup_text(pts$Summary[i]),
        "<br><b>Launched Date:</b> ", popup_text(pts$Launched.Date[i]),
        "<br><b>Status:</b> ", popup_text(pts$Status[i]),
        "<br><br>", make_photo_gallery(pts$Project_ID[i])
      )
    }, character(1))
  }
  
  build_project_map <- function(district_polygons, projects) {
    shiny::req(nrow(district_polygons) > 0)
    center <- sf::st_centroid(sf::st_union(district_polygons))
    xy <- sf::st_coordinates(center)
    map <- leaflet::leaflet(width="100%", height=600) |>
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
      leaflet::setView(lng=xy[1], lat=xy[2], zoom=10) |>
      leaflet::addPolygons(
        data=district_polygons, fillColor=~pal(NAME_2),
        fillOpacity=0.7, color="black", weight=1, label=~NAME_2
      )
    if (nrow(projects) > 0) {
      map <- map |>
        leaflet::addAwesomeMarkers(
          data=projects, lng=~longitude, lat=~latitude,
          icon=project_marker_icons(projects), popup=project_popups(projects),
          popupOptions=leaflet::popupOptions(maxWidth=340, maxHeight=450)
        )
    }
    map
  }
  
  make_downloadable_map <- function(map, heading_text) {
    map <- htmlwidgets::onRender(map, vr_media_js)
    css <- paste0('
      html,body { margin:0; padding:0; }
      body { font-family:Arial,sans-serif; background:#f5f5f5; }
      .vr-header { background:#ace500; padding:15px 20px; text-align:center;
        font-size:1.4rem; font-weight:bold; color:#1e1e4c;
        border-bottom:2px solid #1e1e4c; }
      .vr-watermark { position:fixed; bottom:10px; right:10px;
        background:rgba(255,255,255,.85); padding:5px 12px;
        border-radius:6px; font-size:12px; font-family:monospace;
        color:#333; z-index:10000; pointer-events:none;
        box-shadow:0 1px 3px rgba(0,0,0,.2); }
    ', vr_media_css)
    map <- htmlwidgets::prependContent(
      map, htmltools::tags$style(htmltools::HTML(css)),
      htmltools::tags$div(class="vr-header", heading_text)
    )
    htmlwidgets::appendContent(
      map, htmltools::tags$div(
        class="vr-watermark",
        "(c) 2026 Volta Regional Coordinating Council. By D.A Analytics (senyoackuaku@gmail.com)"
      )
    )
  }
  
  # ==================== TAB 1: PROJECTS MAP ====================
  filtered_data <- shiny::reactive({
    shiny::req(input$districts)
    dplyr::filter(volta_data, NAME_2 == input$districts)
  })
  filtered_projects <- shiny::reactive({
    shiny::req(input$districts)
    pts <- dplyr::filter(vrcc_projects_sf, District == input$districts)
    if (length(input$status_filter) == 0L || length(input$type_filter) == 0L)
      return(pts[0, ])
    dplyr::filter(pts, Status %in% input$status_filter,
                  Project_Type %in% input$type_filter)
  })
  output$vr_map <- leaflet::renderLeaflet({
    build_project_map(filtered_data(), filtered_projects())
  })
  output$download_map <- shiny::downloadHandler(
    filename=function() {
      district <- paste(input$districts, collapse="_")
      if (!nzchar(district)) district <- "All_Districts"
      district <- gsub("[^A-Za-z0-9_-]+", "_", district)
      paste0("vrccproj_", district, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    },
    content=function(file) {
      df <- filtered_data(); pts <- filtered_projects()
      shiny::req(nrow(df) > 0)
      status <- if (length(input$status_filter)) paste(input$status_filter, collapse=", ") else "None selected"
      type <- if (length(input$type_filter)) paste(input$type_filter, collapse=", ") else "None selected"
      district <- paste(input$districts, collapse=", ")
      heading <- paste0("Projects in Volta Region - ", district,
                        " | Status: ", status, " | Type: ", type)
      export_map <- build_project_map(df, pts) |>
        make_downloadable_map(heading) |>
        make_download_map_mobile()
      htmlwidgets::saveWidget(export_map, file=file, selfcontained=TRUE,
                              title=paste("VRCC Projects -", district))
    },
    contentType="text/html"
  )
  
  # ==================== TAB 2: HO MUNICIPAL ====================
  fltd_data <- shiny::reactive({
    dplyr::filter(volta_data, NAME_2 == "Ho Municipal")
  })
  fltd_projects <- shiny::reactive({
    pts <- dplyr::filter(vrcc_projects_sf, District == "Ho Municipal")
    if (length(input$hm_status_filter) == 0L || length(input$hm_type_filter) == 0L)
      return(pts[0, ])
    dplyr::filter(pts, Status %in% input$hm_status_filter,
                  Project_Type %in% input$hm_type_filter)
  })
  output$hm_map <- leaflet::renderLeaflet({
    build_project_map(fltd_data(), fltd_projects())
  })
  output$download_hm_map <- shiny::downloadHandler(
    filename=function() {
      paste0("vrccproj_Ho_Municipal_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    },
    content=function(file) {
      df <- fltd_data(); pts <- fltd_projects()
      shiny::req(nrow(df) > 0)
      status <- if (length(input$hm_status_filter)) paste(input$hm_status_filter, collapse=", ") else "None selected"
      type <- if (length(input$hm_type_filter)) paste(input$hm_type_filter, collapse=", ") else "None selected"
      heading <- paste0("Projects in Ho Municipal | Status: ", status, " | Type: ", type)
      export_map <- build_project_map(df, pts) |>
        make_downloadable_map(heading) |>
        make_download_map_mobile()
      htmlwidgets::saveWidget(export_map, file=file, selfcontained=TRUE,
                              title="VRCC Projects - Ho Municipal")
    },
    contentType="text/html"
  )
  
  # ==================== TAB 3: STATISTICS ====================
  stats_data <- shiny::reactive({
    df <- vrcc_projects_sf |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        Project_ID=trimws(as.character(Project_ID)),
        District=trimws(as.character(District)),
        Project_Type=trimws(as.character(Project_Type)),
        Status=trimws(as.character(Status))
      ) |>
      dplyr::filter(!is.na(Project_ID), nzchar(Project_ID)) |>
      dplyr::distinct(Project_ID, .keep_all=TRUE)
    if (!is.null(input$stats_district) && input$stats_district != "All")
      df <- dplyr::filter(df, District == input$stats_district)
    if (!is.null(input$stats_type) && input$stats_type != "All")
      df <- dplyr::filter(df, Project_Type == input$stats_type)
    df
  })
  
  district_summary <- shiny::reactive({
    dplyr::count(stats_data(), District, name="Projects", sort=TRUE)
  })
  status_summary <- shiny::reactive({
    dplyr::count(stats_data(), Status, name="Projects")
  })
  type_summary <- shiny::reactive({
    dplyr::count(stats_data(), Project_Type, name="Projects", sort=TRUE)
  })
  district_status_summary <- shiny::reactive({
    dplyr::count(stats_data(), District, Status, name="Projects")
  })
  district_table_data <- shiny::reactive({
    stats_data() |>
      dplyr::group_by(District) |>
      dplyr::summarise(
        Total=dplyr::n(),
        Completed=sum(Status == "Completed", na.rm=TRUE),
        `On-going`=sum(Status == "On-going", na.rm=TRUE),
        Planned=sum(Status == "Planned", na.rm=TRUE),
        `Completion Rate (%)`=round(100 * Completed / Total, 1),
        .groups="drop"
      ) |>
      dplyr::arrange(dplyr::desc(Total))
  })
  type_table_data <- shiny::reactive({
    df <- dplyr::count(stats_data(), District, Project_Type, name="Projects")
    if (nrow(df) == 0L) return(data.frame(District=character()))
    tidyr::pivot_wider(df, names_from=Project_Type,
                       values_from=Projects, values_fill=0)
  })
  
  # Dashboard cards
  output$stats_total_projects <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(nrow(stats_data()), "Total Projects",
                             icon=shiny::icon("folder-open"), color="blue")
  })
  output$stats_district_count <- shinydashboard::renderValueBox({
    df <- stats_data()
    districts <- df$District[!is.na(df$District) & nzchar(df$District)]
    shinydashboard::valueBox(dplyr::n_distinct(districts), "Districts Covered",
                             icon=shiny::icon("map-marker"), color="aqua")
  })
  output$stats_completed <- shinydashboard::renderValueBox({
    completed <- sum(stats_data()$Status == "Completed", na.rm=TRUE)
    shinydashboard::valueBox(completed, "Completed Projects",
                             icon=shiny::icon("check-circle"), color="green")
  })
  output$stats_completion_rate <- shinydashboard::renderValueBox({
    df <- stats_data()
    rate <- if (nrow(df)) 100 * sum(df$Status == "Completed", na.rm=TRUE) / nrow(df) else 0
    shinydashboard::valueBox(paste0(round(rate, 1), "%"), "Completion Rate",
                             icon=shiny::icon("percent"), color="yellow")
  })
  
  # Shared chart creators: used for live display and HTML export.
  district_plot <- function(df) {
    if (!nrow(df)) return(plotly::plot_ly() |> plotly::layout(title="No district data"))
    plotly::plot_ly(df, x=~Projects, y=~reorder(District, Projects),
                    type="bar", orientation="h", text=~Projects,
                    textposition="auto", showlegend=FALSE) |>
      plotly::layout(xaxis=list(title="Number of Projects"),
                     yaxis=list(title=""), margin=list(l=145))
  }
  status_plot <- function(df) {
    if (!nrow(df)) return(plotly::plot_ly() |> plotly::layout(title="No status data"))
    plotly::plot_ly(df, labels=~Status, values=~Projects, type="pie",
                    textinfo="label+percent", hoverinfo="label+value+percent",
                    showlegend=FALSE)
  }
  type_plot <- function(df) {
    if (!nrow(df)) return(plotly::plot_ly() |> plotly::layout(title="No project type data"))
    plotly::plot_ly(df, x=~Projects, y=~reorder(Project_Type, Projects),
                    type="bar", orientation="h", text=~Projects,
                    textposition="auto", showlegend=FALSE) |>
      plotly::layout(xaxis=list(title="Number of Projects"),
                     yaxis=list(title=""), margin=list(l=145))
  }
  district_status_plot <- function(df) {
    if (!nrow(df)) return(plotly::plot_ly() |> plotly::layout(title="No district status data"))
    plotly::plot_ly(df, x=~District, y=~Projects, color=~Status, type="bar") |>
      plotly::layout(barmode="stack", xaxis=list(title="", tickangle=-45),
                     yaxis=list(title="Number of Projects"))
  }
  
  output$stats_district_plot <- plotly::renderPlotly({ district_plot(district_summary()) })
  output$stats_status_plot <- plotly::renderPlotly({ status_plot(status_summary()) })
  output$stats_type_plot <- plotly::renderPlotly({ type_plot(type_summary()) })
  output$stats_district_status_plot <- plotly::renderPlotly({
    district_status_plot(district_status_summary())
  })
  
  output$stats_district_table <- DT::renderDT({
    DT::datatable(district_table_data(), rownames=FALSE, filter="top",
                  options=list(pageLength=10, scrollX=TRUE))
  })
  output$stats_type_table <- DT::renderDT({
    DT::datatable(type_table_data(), rownames=FALSE,
                  options=list(pageLength=10, scrollX=TRUE))
  })
  detailed_projects <- shiny::reactive({
    df <- stats_data()
    cols <- intersect(c("Project_ID", "District", "Project_Name", "Project_Type",
                        "Status", "Location", "Launched.Date"), names(df))
    df[, cols, drop=FALSE]
  })
  output$stats_project_table <- DT::renderDT({
    DT::datatable(detailed_projects(), rownames=FALSE, filter="top",
                  options=list(pageLength=10, scrollX=TRUE))
  })
  
  # ==================== EXCEL DOWNLOAD (.xlsx) ====================
  output$download_project_statistics <- shiny::downloadHandler(
    filename=function() paste0("VRCC_Project_Statistics_", Sys.Date(), ".xlsx"),
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content=function(file) {
      if (!requireNamespace("openxlsx", quietly=TRUE))
        stop("Package 'openxlsx' is missing. Run install.packages('openxlsx').")
      wb <- openxlsx::createWorkbook()
      sheets <- list(
        "District Summary"=district_table_data(),
        "Project Types"=type_table_data(),
        "Project Status"=status_summary(),
        "Detailed Projects"=detailed_projects()
      )
      heading_style <- openxlsx::createStyle(
        fgFill="#0B6E75", fontColour="#FFFFFF", textDecoration="bold"
      )
      for (sheet in names(sheets)) {
        dat <- as.data.frame(sheets[[sheet]])
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, dat)
        if (ncol(dat) > 0L) {
          openxlsx::addStyle(wb, sheet, heading_style,
                             rows=1, cols=seq_len(ncol(dat)), gridExpand=TRUE)
          openxlsx::setColWidths(wb, sheet, cols=seq_len(ncol(dat)), widths="auto")
          openxlsx::freezePane(wb, sheet, firstRow=TRUE)
        }
      }
      openxlsx::saveWorkbook(wb, file=file, overwrite=TRUE)
      if (!file.exists(file) || file.info(file)$size == 0)
        stop("Excel workbook was not created successfully.")
    }
  )
  
  # ==================== INTERACTIVE HTML DOWNLOAD ====================
  # Four FULL-SIZE, INDEPENDENT self-contained Plotly charts.
  # Each chart has its own hover, zoom, pan, legend and toolbar.
  output$download_statistics_html <- shiny::downloadHandler(
    filename = function() {
      paste0("VRCC_Project_Graphs_", Sys.Date(), ".html")
    },
    contentType = "text/html",
    content = function(file) {
      df <- stats_data()
      if (nrow(df) == 0L) {
        stop("No project records match the selected statistics filters.")
      }
      
      total <- nrow(df)
      completed <- sum(df$Status == "Completed", na.rm = TRUE)
      districts <- df$District[!is.na(df$District) & nzchar(df$District)]
      district_count <- dplyr::n_distinct(districts)
      rate <- round(100 * completed / total, 1)
      district_label <- if (is.null(input$stats_district)) "All" else input$stats_district
      type_label <- if (is.null(input$stats_type)) "All" else input$stats_type
      
      # IMPORTANT: do not use subplot() here. Each chart is an independent
      # widget, sized to be readable on both desktop and mobile browsers.
      p1 <- district_plot(district_summary())
      p2 <- status_plot(status_summary())
      p3 <- type_plot(type_summary())
      p4 <- district_status_plot(district_status_summary())
      
      # Each Plotly chart is saved as its OWN self-contained HTML document.
      # An iframe srcdoc embeds that document without losing widget initialization.
      # This avoids blank graphs caused by htmltools::save_html() not initializing
      # multiple independent htmlwidgets embedded in an arbitrary tagList.
      chart_document <- function(widget, height) {
        widget <- plotly::layout(
          widget, autosize = TRUE, height = height,
          font = list(size = 13)
        )
        widget <- plotly::config(
          widget, responsive = TRUE, displayModeBar = TRUE,
          displaylogo = FALSE, scrollZoom = TRUE,
          toImageButtonOptions = list(format = "png", scale = 2)
        )
        widget$width <- "100%"
        widget$height <- paste0(height, "px")
        temp_chart <- tempfile(fileext = ".html")
        on.exit(unlink(temp_chart), add = TRUE)
        htmlwidgets::saveWidget(
          widget, file = temp_chart, selfcontained = TRUE,
          title = "VRCC Interactive Chart", background = "white"
        )
        paste(readLines(temp_chart, warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
      }
      
      make_chart_card <- function(title, widget, height = 460) {
        chart_html <- chart_document(widget, height)
        htmltools::tags$section(
          class = "graph-card",
          htmltools::tags$h2(title),
          htmltools::tags$iframe(
            title = title,
            srcdoc = chart_html,
            loading = "eager",
            style = paste0(
              "display:block;width:100%;height:", height + 25,
              "px;border:0;background:white;"
            )
          )
        )
      }
      
      # Use htmltools to retain four independent htmlwidgets, including
      # each widget's JavaScript dependencies. Then embed all dependencies.
      page <- htmltools::tagList(
        htmltools::tags$head(
          htmltools::tags$meta(charset = "utf-8"),
          htmltools::tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
          ),
          htmltools::tags$style(htmltools::HTML('
            * { box-sizing: border-box; }
            body { margin: 0; background: #F3F7F6; color: #17353B;
                   font-family: Arial, Helvetica, sans-serif; }
            .report-header { background: #0B6E75; color: white;
                             padding: 24px 18px; text-align: center; }
            .report-header h1 { margin: 0 0 8px; font-size: 26px; }
            .report-header p { margin: 0; }
            .report-content { max-width: 1120px; margin: 0 auto; padding: 16px; }
            .filter-line { margin: 8px 0 16px; overflow-wrap: anywhere; }
            .metrics { display: grid; grid-template-columns: repeat(4, 1fr);
                       gap: 12px; margin: 16px 0 22px; }
            .metric { background: white; border-radius: 8px; padding: 16px;
                      box-shadow: 0 1px 5px rgba(0,0,0,.09); }
            .metric strong { display: block; font-size: 25px; color: #0B6E75; }
            .metric span { display: block; font-size: 13px; margin-top: 5px; }
            .graph-card { background: white; border-radius: 9px;
                          padding: 16px 12px; margin: 18px 0;
                          box-shadow: 0 1px 6px rgba(0,0,0,.1); }
            .graph-card h2 { margin: 6px 10px 10px; font-size: 19px; }
            .graph-wrap { width: 100%; min-width: 0; overflow: hidden; }
            .graph-wrap .html-widget { width: 100% !important; }
            .report-footer { text-align: center; color: #556;
                             padding: 22px; font-size: 12px; }
            @media (max-width: 700px) {
              .report-header h1 { font-size: 20px; }
              .metrics { grid-template-columns: repeat(2, 1fr); }
              .metric strong { font-size: 21px; }
              .report-content { padding: 8px; }
              .graph-card { padding: 10px 4px; }
              .graph-card h2 { font-size: 16px; }
            }
          '))
        ),
        htmltools::tags$header(
          class = "report-header",
          htmltools::tags$h1("VRCC - Project Statistics"),
          htmltools::tags$p("Volta Regional Coordinating Council")
        ),
        htmltools::tags$main(
          class = "report-content",
          htmltools::tags$p(
            class = "filter-line",
            htmltools::tags$strong("District: "), district_label,
            " | ", htmltools::tags$strong("Project Type: "), type_label
          ),
          htmltools::tags$div(
            class = "metrics",
            htmltools::tags$div(class = "metric",
                                htmltools::tags$strong(total), htmltools::tags$span("Total Projects")),
            htmltools::tags$div(class = "metric",
                                htmltools::tags$strong(district_count), htmltools::tags$span("Districts Covered")),
            htmltools::tags$div(class = "metric",
                                htmltools::tags$strong(completed), htmltools::tags$span("Completed Projects")),
            htmltools::tags$div(class = "metric",
                                htmltools::tags$strong(paste0(rate, "%")), htmltools::tags$span("Completion Rate"))
          ),
          make_chart_card("1. Projects by District / Municipality", p1,
                          height = max(440, min(1000, 35 * nrow(district_summary()) + 180))),
          make_chart_card("2. Projects by Status", p2, height = 460),
          make_chart_card("3. Projects by Type", p3,
                          height = max(440, min(850, 40 * nrow(type_summary()) + 160))),
          make_chart_card("4. Project Status by District", p4, height = 540)
        ),
        htmltools::tags$footer(
          class = "report-footer",
          "(c) 2026 Volta Regional Coordinating Council (VRCC) Creator: D.A Analytics (senyoackuaku@gmail.com)"
        )
      )
      
      # All four iframe srcdoc values already contain self-contained Plotly
      # documents. No separate dependencies or Pandoc conversion are needed.
      htmltools::save_html(
        htmltools::browsable(page),
        file = file,
        background = "white"
      )
      if (!file.exists(file) || file.info(file)$size == 0L) {
        stop("The interactive HTML report could not be created.")
      }
    }
  )
  
} # END server



