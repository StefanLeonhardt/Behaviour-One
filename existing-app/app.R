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
  "Tori" = list(
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
  "Uke" = list(
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
  title = "Behavior 1.0",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    width = "30%",
    position = "right",
    title = useShinyjs(),
    extendShinyjs(text = js_code, functions = c("getVideoTime", "jumpToTime", "initializeVideo"))
  ),
  collapsible = nav_panel(
    title = "Analyse",
    fluidRow(
                                                                                                                                                                                                          column(6,
                                                                                                                                                                                                                 card(
                                                                                                                                                                                                                   card_header("Video"),
                                                                                                                                                                                                                   fileInput("video_upload", "Video hochladen",
                                                                                                                                                                                                                             accept = c("video/mp4", "video/webm", "video/ogg", "video/quicktime"),
                                                                                                                                                                                                                             multiple = FALSE),
                                                                                                                                                                                                                   uiOutput("video_container"),
                                                                                                                                                                                                                   verbatimTextOutput("video_debug_info")  # For debugging
                                                                                                                                                                                                                 ),
                                                                                                                                                                                                                 # Moved the events card here, below the video
                                                                                                                                                                                                                 card(
                                                                                                                                                                                                                   card_header("Getaggte Events"),
                                                                                                                                                                                                                   div(
                                                                                                                                                                                                                     class = "event-list",
                                                                                                                                                                                                                     DTOutput("event_list")
                                                                                                                                                                                                                   ),
                                                                                                                                                                                                                   downloadButton("download_tags", "Events exportieren")
                                                                                                                                                                                                                 )
                                                                                                                                                                                                          ),
                                                                                                                                                                                                          column(6,  # Event Tagging System
                                                                                                                                                                                                                 card(
                                                                                                                                                                                                                   card_header("Event Tagging"),
                                                                                                                                                                                                                   fluidRow(
                                                                                                                                                                                                                     column(6,
                                                                                                                                                                                                                            radioButtons("rolle", "Rolle",
                                                                                                                                                                                                                                         choices = c("Tori", "Uke"),
                                                                                                                                                                                                                                         selected = "Tori")
                                                                                                                                                                                                                     ),
                                                                                                                                                                                                                     column(6,
                                                                                                                                                                                                                            selectInput("main_category", "Kampfphasen",
                                                                                                                                                                                                                                        choices = NULL)
                                                                                                                                                                                                                     )
                                                                                                                                                                                                                   ),
                                                                                                                                                                                                                   uiOutput("criteria_ui"),
                                                                                                                                                                                                                   textInput("note", "Notiz (optional)"),
                                                                                                                                                                                                                   actionButton("add_tag", "Event taggen", class = "btn-primary")
                                                                                                                                                                                                                 )
                                                                                                                                                                                                          )
                                                                                                                                                                                                        )
  ),
  nav_panel(
    title = "Auswertung",
    card(
      card_header("Auswertung"),
      p("Dieser Bereich wird später für die Auswertung verwendet.")
    )
  )
)

# Server für Shiny App ####

server <- function(input, output, session) {
  # Reactive Values ####
  tagged_events <- reactiveVal(data.frame(
    Zeit = numeric(),
    Zeit_Format = character(),
    Rolle = character(),
    Kampfphase = character(),
    stringsAsFactors = FALSE
  ))
  
  # Helper Functions ####
  format_time <- function(seconds) {
    total_frames <- round(seconds * 30)
    return(total_frames)
  }
  
  # Data Management Functions ####
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
  
  # Initialize Data ####
  loadData()
  
  # Observers ####
  observe({
    req(input$rolle)
    updateSelectInput(session, "main_category",
                      choices = names(categories[[input$rolle]]),
                      selected = names(categories[[input$rolle]])[1])
  })
  
  # Autosave
  observe({
    invalidateLater(10000, session)
    saveData(tagged_events())
  })
  
  # Event Handlers ####
  observeEvent(input$add_tag, {
    req(input$main_category)
    time <- input$currentTime %||% 0
    
    sub_cats <- categories[[input$rolle]][[input$main_category]]
    criteria_values <- sapply(names(sub_cats), function(sub_cat) {
      input[[paste0("criteria_", make.names(sub_cat))]]
    })
    names(criteria_values) <- names(sub_cats)
    
    new_event <- data.frame(
      Zeit = time,
      Zeit_Format = format_time(time),
      Rolle = input$rolle,
      Kampfphase = input$main_category,
      t(criteria_values),
      Notiz = input$note,
      stringsAsFactors = FALSE
    )
    
    current_events <- tagged_events()
    current_events <- if (nrow(current_events) == 0) new_event else rbind(current_events, new_event)
    tagged_events(current_events)
    
    updateTextInput(session, "note", value = "")
    saveData(current_events)
  })
  
  observeEvent(input$jump_to_row, {
    events <- tagged_events()
    if (nrow(events) > 0) {
      time <- events$Zeit[input$jump_to_row + 1]
      js$jumpToTime(time)
    }
  })
  
  # Video handling verbessert
  output$video_container <- renderUI({
    req(input$video_upload)
    
    # Debug-Ausgabe
    print(paste("Original filename:", input$video_upload$name))
    print(paste("Data path:", input$video_upload$datapath))
    
    if (!dir.exists("Video")) {
      dir.create("Video")
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
    
    # Verwenden Sie einen relativen Pfad für das Web
    video_filename <- paste0("video_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", file_ext)
    video_path <- file.path("Video", video_filename)
    web_path <- file.path("Video", video_filename)
    
    tryCatch({
      file.copy(input$video_upload$datapath, video_path, overwrite = TRUE)
      
      tags$div(
        tags$video(
          id = "Video",
          controls = TRUE,
          width = "100%",
          preload = "metadata",
          style = "max-width: 100%;",
          tags$source(
            src = web_path,
            type = paste0("video/", file_ext)
          ),
          "Ihr Browser unterstützt das Video-Element nicht."
        ),
        tags$div(
          id = "video_debug",
          style = "margin-top: 10px; font-size: 12px; color: #666;",
          paste("Format:", file_ext, "| Pfad:", web_path)
        )
      )
    }, error = function(e) {
      showNotification(
        paste("Fehler beim Laden des Videos:", e$message),
        type = "error",
        duration = NULL
      )
      print(paste("Error details:", e$message))
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
  
  # Outputs ####
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
  
  output$event_list <- renderDT({
    events <- tagged_events()
    if (nrow(events) > 0) {
      datatable(
        events,
        selection = 'single',
        options = list(
          pageLength = 5,
          order = list(list(0, 'desc'))
        ),
        callback = DT::JS("
          table.on('click', 'td:first-child', function() {
            var data = table.row($(this).parents('tr')).data();
            Shiny.setInputValue('jump_to_row', this._DT_CellIndex.row);
          });
        ")
      )
    }
  })
  
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
}

# Applikation ausführen ####

shinyApp(ui = ui, server = server)

