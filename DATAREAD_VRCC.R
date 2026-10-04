# ===============================
# DATAREAD_VRCC.R
# ===============================

# Required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
library(tidyverse)
library(DT)
library(plotly)
library(gtsummary)
library(gt)
library(broom)
library(knitr)
library(lubridate)
library(forecast)
library(cluster)
library(kableExtra)


# ===============================
# Geographic data
# ===============================

load("ghanashapefile")

ghana_shapefile <- sf::st_make_valid(ghana_shapefile)
ghana_shapefile <- sf::st_transform(ghana_shapefile, 4326)

volta_data <- ghana_shapefile %>%
  dplyr::filter(NAME_1 == "Volta")

volta_districts <- sort(unique(volta_data$NAME_2))



pal <- leaflet::colorFactor(
                            palette = grDevices::hcl.colors(
                                                           length(volta_districts),
                                                           palette = "Dark 3"
                                                           ),
                            domain = volta_districts
                            )


# ===============================
# Read the two Excel sheets
# ===============================

vrcc_projects <- readxl::read_excel(
                                    "vrcc.xlsx",
                                    sheet = "projects"
                                  )

vrcc_projects$Status <- trimws(vrcc_projects$Status)

vrcc_pictures <- readxl::read_excel(
                                    "vrcc.xlsx",
                                    sheet = "media"
                                  )

vrcc_pictures$Media_type <- trimws(vrcc_pictures$Media_type)

required_project_columns <- c(
  "Project_ID",
  "Project_Type",
  "Project_Name",
  "District",
  "longitude",
  "latitude"
)

missing_project_columns <- setdiff(
  required_project_columns,
  names(vrcc_projects)
)

if (length(missing_project_columns) > 0) {
  stop(
    "Missing Sheet1 columns: ",
    paste(missing_project_columns, collapse = ", ")
  )
}

required_media_columns <- c(
  "Project_ID",
  "Picture_URL",
  "Display_Order"
)

missing_media_columns <- setdiff(
  required_media_columns,
  names(vrcc_pictures)
)

if (length(missing_media_columns) > 0) {
  stop(
    "Missing Sheet2 columns: ",
    paste(missing_media_columns, collapse = ", ")
  )
}

# Caption and Media_Type can be blank or absent until you add more media.
if (!"Caption" %in% names(vrcc_pictures)) {
  vrcc_pictures$Caption <- NA_character_
}

if (!"Media_Type" %in% names(vrcc_pictures)) {
  vrcc_pictures$Media_Type <- NA_character_
}


# ===============================
# Clean and order Sheet2 media
# ===============================

# Save Excel row order before sorting. This resolves ties in Display_Order.
vrcc_pictures$.excel_row <- seq_len(nrow(vrcc_pictures))

order_text <- trimws(
  as.character(vrcc_pictures$Display_Order)
)

order_text[is.na(order_text) | order_text == ""] <- NA_character_

order_number <- suppressWarnings(
  as.numeric(order_text)
)

invalid_order <- !is.na(order_text) & is.na(order_number)

if (any(invalid_order)) {
  stop(
    "Sheet2 has non-numeric Display_Order values in Excel rows: ",
    paste(vrcc_pictures$.excel_row[invalid_order] + 1, collapse = ", ")
  )
}

vrcc_pictures$Display_Order <- order_number

vrcc_pictures <- vrcc_pictures %>%
  dplyr::mutate(
    Project_ID = trimws(as.character(Project_ID)),
    Picture_URL = trimws(as.character(Picture_URL)),
    Caption = as.character(Caption),
    Media_Type = as.character(Media_Type)
  ) %>%
  dplyr::filter(
    !is.na(Project_ID),
    nzchar(Project_ID),
    !is.na(Picture_URL),
    nzchar(Picture_URL)
  ) %>%
  dplyr::arrange(
    Project_ID,
    is.na(Display_Order),
    Display_Order,
    .excel_row
  )


# ===============================
# Clean Sheet1 projects
# ===============================

vrcc_projects <- vrcc_projects %>%
  dplyr::mutate(
    Project_ID = trimws(as.character(Project_ID)),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude))
  )

# Your workbook currently calls this column "Launched Date".
# Keep it and provide the dotted name used by older server code.
if (
  "Launched Date" %in% names(vrcc_projects) &&
  !"Launched.Date" %in% names(vrcc_projects)
) {
  vrcc_projects$Launched.Date <- as.character(
    vrcc_projects[["Launched Date"]]
  )
}


# ===============================
# Optional list of media links
# ===============================
# Keep photo_html for any older popup code that still refers to it.
# The interactive gallery below uses vrcc_pictures directly.

picture_links <- vrcc_pictures %>%
  dplyr::group_by(Project_ID) %>%
  dplyr::mutate(
    link_number = dplyr::row_number(),
    link_label = dplyr::if_else(
      is.na(Caption) | !nzchar(trimws(Caption)),
      paste("View media", link_number),
      paste(Caption, link_number)
    )
  ) %>%
  dplyr::summarise(
    photo_html = paste0(
      '<a href="',
      htmltools::htmlEscape(Picture_URL),
      '" target="_blank" rel="noopener noreferrer">',
      htmltools::htmlEscape(link_label),
      "</a>",
      collapse = "<br>"
    ),
    .groups = "drop"
  )

vrcc_projects <- vrcc_projects %>%
  dplyr::select(-dplyr::any_of("photo_html")) %>%
  dplyr::left_join(
    picture_links,
    by = "Project_ID"
  ) %>%
  dplyr::mutate(
    photo_html = dplyr::if_else(
      is.na(photo_html),
      "No pictures available",
      photo_html
    )
  )


# ===============================
# Spatial project data
# ===============================

vrcc_projects <- vrcc_projects %>%
  dplyr::filter(
    !is.na(longitude),
    !is.na(latitude),
    is.finite(longitude),
    is.finite(latitude)
  )

vrcc_projects_sf <- sf::st_as_sf(
  vrcc_projects,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)


# ===============================
# Popup gallery
# ===============================

make_photo_gallery <- function(project_id) {
  
  if (length(project_id) != 1 || is.na(project_id)) {
    return("")
  }
  
  # vrcc_pictures is already sorted by Project_ID and Display_Order.
  # Subsetting it preserves that order.
  media <- vrcc_pictures[
    vrcc_pictures$Project_ID == as.character(project_id),
    ,
    drop = FALSE
  ]
  
  if (nrow(media) == 0) {
    return("<b>Project media:</b> None available")
  }
  
  types <- rep("image", nrow(media))
  
  entered_types <- tolower(trimws(media$Media_Type))
  types[!is.na(entered_types) & entered_types == "video"] <- "video"
  
  video_urls <- grepl(
    "/video/upload/|\\.(mp4|webm|ogg)($|\\?)",
    media$Picture_URL,
    ignore.case = TRUE
  )
  
  types[video_urls] <- "video"
  
  captions <- media$Caption
  
  missing_caption <- is.na(captions) |
    !nzchar(trimws(captions))
  
  captions[missing_caption] <- ifelse(
    types[missing_caption] == "video",
    "Video",
    "Picture"
  )
  
  slides <- vapply(
    seq_len(nrow(media)),
    function(i) {
      
      url <- htmltools::htmlEscape(media$Picture_URL[i])
      caption <- htmltools::htmlEscape(captions[i])
      
      content <- if (types[i] == "video") {
        
        video_type <- if (
          grepl("\\.webm($|\\?)", media$Picture_URL[i], ignore.case = TRUE)
        ) {
          "video/webm"
        } else if (
          grepl("\\.ogg($|\\?)", media$Picture_URL[i], ignore.case = TRUE)
        ) {
          "video/ogg"
        } else {
          "video/mp4"
        }
        
        paste0(
          '<video controls playsinline preload="none">',
          '<source src="', url, '" type="', video_type, '">',
          'Your browser cannot play this video.',
          '</video>',
          '<br><a href="', url,
          '" target="_blank" rel="noopener noreferrer">',
          caption, ' — Open video</a>'
        )
        
      } else {
        
        paste0(
          '<a href="', url,
          '" target="_blank" rel="noopener noreferrer">',
          '<img src="', url,
          '" alt="', caption,
          '" loading="lazy">',
          '</a>',
          '<br><a href="', url,
          '" target="_blank" rel="noopener noreferrer">',
          caption, ' — View full picture</a>'
        )
        
      }
      
      paste0(
        '<div class="vr-media-slide" hidden>',
        content,
        '</div>'
      )
    },
    character(1)
  )
  
  paste0(
    '<div class="vr-media-gallery">',
    '<b>Project media:</b><br>',
    
    '<button type="button" class="vr-media-open">',
    'View pictures and videos (', nrow(media), ')',
    '</button>',
    
    '<div class="vr-media-viewer" hidden>',
    
    paste(slides, collapse = ""),
    
    '<div class="vr-media-controls">',
    '<button type="button" class="vr-media-prev">Previous</button>',
    '<span class="vr-media-count"></span>',
    '<button type="button" class="vr-media-next">Next</button>',
    '</div>',
    
    '<button type="button" class="vr-media-close">',
    'Hide pictures and videos',
    '</button>',
    
    '</div>',
    '</div>'
  )
}


# ===============================
# Gallery CSS
# ===============================

vr_media_css <- "
  .vr-media-gallery {
    width: 280px;
    max-width: 100%;
    margin-top: 6px;
  }

  .vr-media-viewer[hidden],
  .vr-media-slide[hidden],
  .vr-media-open[hidden] {
    display: none !important;
  }

  .vr-media-viewer {
    max-height: 390px;
    overflow-y: auto;
  }

  .vr-media-slide img,
  .vr-media-slide video {
    width: 100%;
    height: auto;
    border-radius: 6px;
  }

  .vr-media-slide a {
    display: inline-block;
    margin-top: 5px;
  }

  .vr-media-open,
  .vr-media-close {
    background: none;
    border: 0;
    padding: 5px 0;
    color: #1676a2;
    text-decoration: underline;
    cursor: pointer;
    font-weight: bold;
  }

  .vr-media-controls {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 8px;
    margin-top: 10px;
  }

  .vr-media-controls button {
    padding: 5px 8px;
    cursor: pointer;
  }

  .vr-media-controls button:disabled {
    cursor: default;
    opacity: 0.45;
  }
"


# ===============================
# Gallery JavaScript
# ===============================

vr_media_click_js <- r"---(
  if (!window.vrMediaGalleryInstalled) {
    window.vrMediaGalleryInstalled = true;

    document.addEventListener("click", function(event) {
      if (!(event.target instanceof Element)) return;

      const button = event.target.closest(
        ".vr-media-gallery button"
      );

      if (!button) return;

      const gallery = button.closest(".vr-media-gallery");
      const viewer = gallery.querySelector(".vr-media-viewer");
      const opener = gallery.querySelector(".vr-media-open");
      const slides = Array.from(
        gallery.querySelectorAll(".vr-media-slide")
      );
      const counter = gallery.querySelector(".vr-media-count");
      const previous = gallery.querySelector(".vr-media-prev");
      const next = gallery.querySelector(".vr-media-next");

      if (slides.length === 0) return;

      function showSlide(index) {
        slides.forEach(function(slide, i) {
          slide.hidden = i !== index;

          if (i !== index) {
            slide.querySelectorAll("video").forEach(function(video) {
              video.pause();
            });
          }
        });

        gallery.dataset.index = String(index);
        counter.textContent =
          (index + 1) + " / " + slides.length;

        previous.disabled = index === 0;
        next.disabled = index === slides.length - 1;
      }

      event.preventDefault();
      event.stopPropagation();

      if (button.classList.contains("vr-media-open")) {
        opener.hidden = true;
        viewer.hidden = false;
        showSlide(0);

      } else if (
        button.classList.contains("vr-media-prev")
      ) {
        const index = Number(gallery.dataset.index || 0);
        showSlide(Math.max(0, index - 1));

      } else if (
        button.classList.contains("vr-media-next")
      ) {
        const index = Number(gallery.dataset.index || 0);
        showSlide(Math.min(slides.length - 1, index + 1));

      } else if (
        button.classList.contains("vr-media-close")
      ) {
        slides.forEach(function(slide) {
          slide.querySelectorAll("video").forEach(function(video) {
            video.pause();
          });
        });

        viewer.hidden = true;
        opener.hidden = false;
      }
    });
  }
)---"

# Used by UI_VRCC.R for the map inside the Shiny app.
media_gallery_assets <- htmltools::tagList(
  htmltools::tags$style(
    htmltools::HTML(vr_media_css)
  ),
  htmltools::tags$script(
    htmltools::HTML(vr_media_click_js)
  )
)

# Used by htmlwidgets::onRender() in the downloaded map code.
vr_media_js <- paste0(
  "function(el, x) {\n",
  vr_media_click_js,
  "\n}"
)


# ===============================
# Project Type choices
# ===============================

project_types <- sort(unique(
  as.character(stats::na.omit(vrcc_projects$Project_Type))
))


# ===============================
# Overview tab data
# ===============================

sample_df <- readr::read_csv(
  "marketing_campaign_data.csv",
  show_col_types = FALSE
)

overview_columns <- c(
  "Region",
  "Gender",
  "Age",
  "Previous_Purchases",
  "Campaign_Exposure",
  "Purchase_Made",
  "Amount_Spent",
  "Income"
)

missing_columns <- setdiff(
  overview_columns,
  names(sample_df)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing Overview data columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

sample_df <- sample_df %>%
  dplyr::mutate(
    Purchase_Made = as.numeric(Purchase_Made),
    Age = as.numeric(Age),
    Previous_Purchases = as.numeric(Previous_Purchases),
    Amount_Spent = as.numeric(Amount_Spent),
    Income = as.numeric(Income)
  )

