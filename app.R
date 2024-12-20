#
# Dies ist eine Shiny web Applikation. Du kannst durch drücken des Button "Run App" loslegen
#


# Setup ####
## Set maximum upload size to 5GB ####
options(shiny.maxRequestSize = 5000 * 1024^2)
## R-Pakete laden ####
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(base64enc)

## Definiere das Kategoriensystem ####
categories <- list(
  "Blau" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = c("Mattenrand", "Mattenzentrum", "Mattenecke"),
      "Kampfauslage" = c("LL", "RL", "LR", "RR"),
      "Wertungsstand" = c("Führung", "Rückstand", "Gleichstand"),
      "Art der Kontaktaufnahme" = c("Überfallartig", "Offensiv", "Defensiv", "Neutral")
    ),
    "Kumi kata" = list(
      "Griffsystem" = c("Klassisch", "Cross Grip", "Single Sleeve", "Single Collar"),
      "Distanz" = c("Halbdistanz", "Normal", "Infight"),
      "Kampfauslage" = c("LL", "RL", "LR", "RR"),
      "Wertungsstand" = c("Führung", "Rückstand", "Gleichstand")
    ),
    "Angriff Stand" = list(
      "Art des Angriffs" = c("Direktangriff", "Gegenangriff", "Kombination", "Finte"),
      "Nage waza" = c("Seoi nage", "Uchi mata", "Taiotoshi"),
      "Wertung" = c("No Score", "Kinza", "Waza ari", "Ippon", "Shido")
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion"),
      "Kontaktaufnahme" = c("Vorderseite", "Rückseite", "Seite SX", "Seite DX", "Herab", "Direkt")
    ),
    "Angriff Boden" = list(
      "Ne waza" = c("Sankaku", "Kesa gatame", "Mune gatame"),
      "Griffbeginn" = c("Arm", "Nacken", "Achselhöhle")
    )
  ),
  "Weiss" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = c("Mattenrand", "Mattenzentrum", "Mattenecke"),
      "Kampfauslage" = c("LL", "RL", "LR", "RR"),
      "Wertungsstand" = c("Führung", "Rückstand", "Gleichstand"),
      "Art der Kontaktaufnahme" = c("Überfallartig", "Offensiv", "Defensiv", "Neutral")
    ),
    "Kumi kata" = list(
      "Griffsystem" = c("Klassisch", "Cross Grip", "Single Sleeve", "Single Collar"),
      "Distanz" = c("Halbdistanz", "Normal", "Infight"),
      "Kampfauslage" = c("LL", "RL", "LR", "RR"),
      "Wertungsstand" = c("Führung", "Rückstand", "Gleichstand")
    ),
    "Angriff Stand" = list(
      "Art des Angriffs" = c("Direktangriff", "Gegenangriff", "Kombination", "Finte"),
      "Nage waza" = c("Seoi nage", "Uchi mata", "Taiotoshi"),
      "Wertung" = c("No Score", "Kinza", "Waza ari", "Ippon", "Shido")
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion"),
      "Kontaktaufnahme" = c("Vorderseite", "Rückseite", "Seite SX", "Seite DX", "Herab", "Direkt")
    ),
    "Angriff Boden" = list(
      "Ne waza" = c("Sankaku", "Kesa gatame", "Mune gatame"),
      "Griffbeginn" = c("Arm", "Nacken", "Achselhöhle")
    )
  )
)


js_code <- "
shinyjs.getVideoTime = function() {
  var video = document.getElementById('video');
  if (video) {
    Shiny.setInputValue('video_time', video.currentTime);
  }
}

shinyjs.jumpToTime = function(params) {
  var video = document.getElementById('video');
  if (video) {
    video.currentTime = params;
    video.play();
  }
}

// Add video error handling
shinyjs.initializeVideo = function() {
  var video = document.getElementById('video');
  if (video) {
    video.addEventListener('error', function(e) {
      console.error('Video Error:', e);
      Shiny.setInputValue('video_error_details', {
        code: video.error.code,
        message: video.error.message
      });
    });
  }
}
"


# User Interface für Shiny app ####

ui <- page_navbar(
  title = "Behaviour 1.0",
  theme = bs_theme(version = 5),
  header = tags$head(
    tags$link(href = "https://unpkg.com/video.js/dist/video-js.min.css", rel = "stylesheet"),
    tags$script(src = "https://unpkg.com/video.js/dist/video.min.js"),
    tags$style(HTML("
    .video-container {
      position: relative;
      width: 100%;
      padding-top: 56.25%; /* 16:9 Aspect Ratio */
    }
    .video-container video {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
    .video-js {
      width: 100% !important;
      height: 100% !important;
      position: absolute;
      top: 0;
      left: 0;
    }
    .video-js .vjs-tech {
      position: absolute !important;
    }
  "))
  ),
  ## Sidebar ####
  sidebar = sidebar(
    width = "30%",
    position = "right",
    open = FALSE,
    useShinyjs(),
    extendShinyjs(text = js_code, functions = c("getVideoTime", "jumpToTime", "initializeVideo"))
  ),
  ## Analyse Seite ####
  nav_panel(
    "Analyse",
    # Erste Zeile mit Video und Event Tagging
    fluidRow(
      column(6,
             card(
               card_header("Video"),
               div(
                 style = "display: flex; justify-content: space-between; align-items: center;",
                 fileInput("video_upload", "Video hochladen",
                           accept = c("video/mp4", "video/webm", "video/ogg", "video/quicktime"),
                           multiple = FALSE),
                 selectInput("video_select", "Video auswählen", 
                             choices = NULL, 
                             width = "200px")
               ),
               uiOutput("video_container"),
               verbatimTextOutput("video_debug_info")  # For debugging
             )
      ),
      column(6,  
             card(
               card_header("Event Tagging"),
               fluidRow(
                 column(6,
                        radioButtons("rolle", "Rolle",
                                     choices = c("Blau", "Weiss"),
                                     selected = "Blau")
                 ),
                 column(6,
                        div(class = "radio-inline"),
                        selectInput("main_category", "Phase:",
                                    choices = NULL)
                 )
               ),
               uiOutput("criteria_ui"),
               actionButton("add_tag", "Event taggen", class = "btn-primary")
             )
      )
    ),
    # Zweite Zeile mit der Tabelle über volle Breite
    fluidRow(
      column(12,
             card(
               card_header("Getaggte Events"),
               div(
                 class = "event-list",
                 DTOutput("event_list")
               ),
               div(
                 style = "display: flex; justify-content: center; gap: 10px;",
                 downloadButton("download_tags", "Events exportieren"),
                 actionButton("clear_all", "Alle löschen", class = "btn-danger")
               )
             )
      )
    )
  ),
  
  nav_panel(
    "Auswertung",
    card(
      card_header("Auswertung"),
      p("Dieser Bereich wird später für die Auswertung verwendet.")
    )
  )
)

# Server für Shiny App ####
  server <- function(input, output, session) {
  
  # Reactive Werte #
  tagged_events <- reactiveVal(data.frame(
    Zeit = numeric(),
    Zeit_Format = numeric(),
    Rolle = character(),
    Kampfphase = character(),
    stringsAsFactors = FALSE
  ))
  
  # Zeit in FPS #
  format_time <- function(seconds) {
    total_frames <- round(seconds * 30)
    return(total_frames)
  }
  
  # Daten Management von CSV ####
  saveData <- function(data) {
    write.csv(data, "data.csv", row.names = FALSE)
  }
  
  loadData <- function() {
    if (file.exists("data.csv")) {
      tagged_events(read.csv("data.csv"))
    }
  }
  
  # Video Path Management ####
  saveVideoPath <- function(path) {
    if (!is.null(path) && is.character(path) && length(path) == 1) {
      path <- trimws(path)
      writeLines(path, "video_path.txt")
    }
  }
  
  loadVideoPath <- function() {
    if (!file.exists("video_path.txt")) return(NULL)
    tryCatch({
      lines <- readLines("video_path.txt", warn = FALSE)
      if (length(lines) > 0) {
        path <- trimws(lines[1])
        if (nzchar(path) && file.exists(path)) {
          return(normalizePath(path, winslash = "/"))
        }
      }
      return(NULL)
    }, error = function(e) {
      warning("Error reading video path: ", e$message)
      return(NULL)
    })
  }
  
  # Dateninitialisierung #
  loadData()
  
  ## Observers ####
  observe({
    req(input$rolle)
    updateSelectInput(session, "main_category",
                      choices = names(categories[[input$rolle]]),
                      selected = names(categories[[input$rolle]])[1])
  })
  
  # Autosave Funktion des Taggings
  observe({
    invalidateLater(10000, session)
    saveData(tagged_events())
  })
  
  # Event Handling ####
  observeEvent(input$add_tag, {
    req(input$main_category)
    time <- input$currentTime %||% 0
    
    sub_cats <- categories[[input$rolle]][[input$main_category]]
    
    # Erstelle eine Liste to store Kriteriumvariable
    criteria_values <- list()
    
    # Sammeln aller Kriterien
    for (sub_cat in names(sub_cats)) {
      input_name <- paste0("criteria_", make.names(sub_cat))
      criteria_values[[sub_cat]] <- input[[input_name]]
    }
    
    # Erstellt ein neues Event als eine Zeile
    new_event <- data.frame(
      Zeit = time,
      Zeit_Format = format_time(time),
      Rolle = input$rolle,
      Kampfphase = input$main_category,
      stringsAsFactors = FALSE
    )
    
    # Hinzufügen von Kriterien zum Datensatz
    for (name in names(criteria_values)) {
      new_event[[name]] <- criteria_values[[name]]
    }
    
    # Combine with existing events
    current_events <- tagged_events()
    if (nrow(current_events) == 0) {
      tagged_events(new_event)
    } else {
      # Make sure column names match
      missing_cols <- setdiff(names(new_event), names(current_events))
      for (col in missing_cols) {
        current_events[[col]] <- NA
      }
      missing_cols <- setdiff(names(current_events), names(new_event))
      for (col in missing_cols) {
        new_event[[col]] <- NA
      }
      
      # Combine the data frames
      tagged_events(rbind(current_events, new_event))
    }
    
    # Save the updated data
    saveData(tagged_events())
  })
  
  observeEvent(input$jump_to_row, {
    events <- tagged_events()
    if (nrow(events) > 0) {
      time <- events$Zeit[input$jump_to_row + 1]
      js$jumpToTime(time)
    }
  })
  
  # Outputs ####
  
  ## Video Handling des Players/Container ####
  output$video_container <- renderUI({
    req(input$video_upload)
    
    if (!dir.exists("www/videos")) {
      dir.create("www/videos", recursive = TRUE)
    }
    
    file_ext <- tolower(tools::file_ext(input$video_upload$name))
    supported_formats <- c("mp4", "webm", "ogg", "mov")
    
    if (!file_ext %in% supported_formats) {
      showNotification(
        "Nicht unterstütztes Videoformat. Bitte verwenden Sie MP4, WebM, OGG oder MOV.",
        type = "error"
      )
      return(NULL)
    }
    
    video_filename <- paste0("video_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", file_ext)
    video_path <- file.path("www/videos", video_filename)
    web_path <- file.path("videos", video_filename)
    
    tryCatch({
      file.copy(input$video_upload$datapath, video_path, overwrite = TRUE)
      
      tagList(
        div(
          class = "video-container",
          tags$video(
            id = "video",
            class = "video-js vjs-default-skin vjs-big-play-centered",
            controls = TRUE,
            preload = "auto",
            `data-setup` = '{"fluid": true, "responsive": true}',
            tags$source(
              src = web_path,
              type = paste0("video/", file_ext)
            )
          )
        ),
        tags$script(HTML("
        if (typeof player !== 'undefined') {
          player.dispose();
        }
        var player = videojs('video', {
  controls: true,
  fluid: true,
  responsive: true,
  aspectRatio: '16:9',
  playbackRates: [0.5, 1, 1.5, 2],
  controlBar: {
    children: [
      'playToggle',
      {
        name: 'volumePanel',
        inline: false,
      },
      'currentTimeDisplay',
      'timeDivider',
      'durationDisplay',
      'progressControl',
      'playbackRateMenuButton',
      'fullscreenToggle'
    ]
  }
}, function() {
  // Player ist bereit
  this.volume(1); // Setze Standardlautstärke
});

// Stelle sicher, dass die Steuerelemente aktiviert sind
player.controls(true);
        
        player.on('timeupdate', function() {
          Shiny.setInputValue('currentTime', this.currentTime());
        });
      "))
      )
    }, error = function(e) {
      showNotification(
        paste("Fehler beim Laden des Videos:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })

  
  # Error handling
  observeEvent(input$video_error, {
    showNotification(
      HTML(
        "Fehler beim Laden des Videos.<br>
        Mögliche Lösungen:<br>
        - Überprüfen Sie das Videoformat (MP4, WebM, OGG, MOV)<br>
        - Stellen Sie sicher, dass der Video-Codec H.264 ist<br>
        - Versuchen Sie, das Video in ein anderes Format zu konvertieren"
      ),
      type = "error",
      duration = 10
    )
  })
  
  # Video Liste aus Ordner laden
  list_videos <- reactive({
    if (dir.exists("www/videos")) {
      videos <- list.files("www/videos", pattern = "\\.mp4$|\\.webm$|\\.ogg$|\\.mov$", full.names = TRUE)
      basename(videos)
    } else {
      character(0)
    }
  })
  
  # Update Video Dropdown
  observe({
    videos <- list_videos()
    updateSelectInput(session, "video_select", 
                      choices = c("Bitte auswählen" = "", videos))
  })
  
  # Handle Video Auswahl
  observeEvent(input$video_select, {
  req(input$video_select)
  if (input$video_select != "") {
    video_path <- file.path("www/videos", input$video_select)
    if (file.exists(video_path)) {
      file_ext <- tools::file_ext(video_path)
      output$video_container <- renderUI({
        div(
          class = "video-container",
          tags$video(
            id = "video",
            class = "video-js vjs-default-skin vjs-big-play-centered",
            controls = TRUE,
            preload = "auto",
            `data-setup` = '{"fluid": true, "responsive": true}',
            tags$source(
              src = file.path("videos", basename(video_path)),
              type = paste0("video/", file_ext)
            )
          )
        )
      })
      saveVideoPath(video_path)
    }
  }
})
  
  
  ### Kriterien UI ####
  output$criteria_ui <- renderUI({
    req(input$main_category, input$rolle)
    main_cat <- input$main_category
    sub_cats <- categories[[input$rolle]][[main_cat]]
    
    div(
      class = "criteria-container",
      lapply(names(sub_cats), function(sub_cat) {
        div(
          class = "criteria-group",
          radioButtons(
            inputId = paste0("criteria_", make.names(sub_cat)),
            label = sub_cat,
            choices = sub_cats[[sub_cat]]
          )
        )
      })
    )
  })
  
  ### Eventliste als Tabelle ####
  output$event_list <- renderDT({
    events <- tagged_events()
    if (nrow(events) > 0) {
      # Add a delete button column
      events$Delete <- paste('<button class="btn btn-danger btn-sm delete-btn" data-row="', 
                             1:nrow(events), 
                             '"><i class="fa fa-trash"></i></button>')
      
      datatable(
        events,
        selection = 'single',
        escape = FALSE,  # Important for rendering HTML buttons
        options = list(
          pageLength = 5,
          order = list(list(0, 'desc')),
          columnDefs = list(
            list(targets = ncol(events) - 1, orderable = FALSE)  # Make delete column non-sortable
          )
        ),
        callback = DT::JS("
        table.on('click', 'td:first-child', function() {
          var data = table.row($(this).parents('tr')).data();
          Shiny.setInputValue('jump_to_row', this._DT_CellIndex.row);
        });
        
        // Add delete button handler
        table.on('click', '.delete-btn', function() {
          var rowNum = $(this).data('row');
          Shiny.setInputValue('delete_row', rowNum);
        });
      ")
      )
    }
  })
  
  #### Zeilen in Tabelle entfernen ####
  observeEvent(input$delete_row, {
    current_data <- tagged_events()
    row_to_delete <- as.numeric(input$delete_row)
    
    if (!is.na(row_to_delete) && nrow(current_data) >= row_to_delete) {
      # Remove the selected row
      current_data <- current_data[-row_to_delete, , drop = FALSE]
      tagged_events(current_data)
      
      # Save the updated data
      saveData(current_data)
      
      # Show notification
      showNotification("Event wurde gelöscht", type = "message")
    }
  })
  
  ### Download CSV ####
  output$download_tags <- downloadHandler(
    filename = function() {
      paste0("kampfanalyse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      export_data <- tagged_events()
      export_data <- export_data[order(export_data$Zeit), ]
      write.csv2(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Initialisierung des Videos beim Start
  observe({
    saved_path <- loadVideoPath()
    if (!is.null(saved_path) && file.exists(saved_path)) {
      output$video_container <- renderUI({
        file_ext <- tools::file_ext(saved_path)
        tags$div(
          tags$video(
            id = "video",
            controls = TRUE,
            width = "100%",
            preload = "metadata",
            style = "max-width: 100%;",
            tags$source(
              src = file.path(saved_path),
              type = paste0("video/", file_ext)
            ),
            "Ihr Browser unterstützt das Video-Element nicht.",
            onloadeddata = "console.log('Video loaded successfully');",
            onTimeUpdate = "Shiny.setInputValue('currentTime', this.currentTime);",
            onerror = sprintf("
            console.error('Video loading error');
            Shiny.setInputValue('video_error', {
              message: 'Error loading video',
              path: '%s'
            });
          ", saved_path)
          )
        )
      })
    }
  })
  
  observeEvent(input$clear_all, {
    showModal(modalDialog(
      title = "Bestätigung",
      "Möchten Sie wirklich alle Events löschen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_clear", "Ja, alle löschen", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    tagged_events(data.frame(
      Zeit = numeric(),
      Zeit_Format = numeric(),
      Rolle = character(),
      Kampfphase = character(),
      stringsAsFactors = FALSE
    ))
    saveData(tagged_events())
    removeModal()
    showNotification("Alle Events wurden gelöscht", type = "message")
  })
  
}

# Applikation ausführen ####

shinyApp(ui = ui, server = server)

