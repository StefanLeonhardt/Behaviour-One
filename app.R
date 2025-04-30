# Dies ist eine Shiny web Applikation zum Tagging von Judowettkämpfen #

# Setup ####
## Set maximum upload Größe auf 5GB pro Video ####
options(shiny.maxRequestSize = 5000 * 1024^2)
## R-Pakete laden ####
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(base64enc)

## Definiere das Kategoriensystem mit Inputtypen ####
categories <- list(
  "Blau" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = list(
        type = "select",
        choices = c("Mattenrand", "Mattenzentrum", "Mattenecke")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("LL", "RL", "LR", "RR")
      ),
      "Wertungsstand" = list(
        type = "select",
        choices = c("Führung", "Rückstand", "Gleichstand")
      ),
      "Art der Kontaktaufnahme" = list(
        type = "select",
        choices = c("Überfallartig", "Offensiv", "Defensiv", "Neutral")
      )
    ),
    "Kumi kata" = list(
      "Griffsystem" = list(
        type = "select",
        choices = c("Klassisch", "Cross Grip", "Single Sleeve", "Single Collar")
      ),
      "Distanz" = list(
        type = "radio",
        choices = c("Halbdistanz", "Normal", "Infight")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("LL", "RL", "LR", "RR")
      ),
      "Wertungsstand" = list(
        type = "select",
        choices = c("Führung", "Rückstand", "Gleichstand")
      )
    ),
    "Angriff Stand" = list(
      "Art des Angriffs" = list(
        type = "select",
        choices = c("Direktangriff", "Gegenangriff", "Kombination", "Finte")
      ),
      "Nage waza" = list(
        type = "selectize",
        choices = c("Seoi-nage", "Uchi-mata", "Taiotoshi")
      ),
      "Wertung" = list(
        type = "radio",
        choices = c("No Score", "Kinza", "Yuko", "Waza-ari", "Ippon", "Shido")
      )
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = list(
        type = "select",
        choices = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion")
      ),
      "Kontaktaufnahme" = list(
        type = "radio",
        choices = c("Vorderseite", "Rückseite", "Seite SX", "Seite DX", "Herab", "Direkt")
      )
    ),
    "Angriff Boden" = list(
      "Ne waza" = list(
        type = "selectize",
        choices = c("Sankaku", "Kesa gatame", "Mune gatame")
      ),
      "Griffbeginn" = list(
        type = "radio",
        choices = c("Arm", "Nacken", "Achselhöhle")
      )
    )
  ),
  "Weiss" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = list(
        type = "select",
        choices = c("Mattenrand", "Mattenzentrum", "Mattenecke")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("LL", "RL", "LR", "RR")
      ),
      "Wertungsstand" = list(
        type = "select",
        choices = c("Führung", "Rückstand", "Gleichstand")
      ),
      "Art der Kontaktaufnahme" = list(
        type = "select",
        choices = c("Überfallartig", "Offensiv", "Defensiv", "Neutral")
      )
    ),
    "Kumi kata" = list(
      "Griffsystem" = list(
        type = "select",
        choices = c("Klassisch", "Cross Grip", "Single Sleeve", "Single Collar")
      ),
      "Distanz" = list(
        type = "radio",
        choices = c("Halbdistanz", "Normal", "Infight")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("LL", "RL", "LR", "RR")
      ),
      "Wertungsstand" = list(
        type = "select",
        choices = c("Führung", "Rückstand", "Gleichstand")
      )
    ),
    "Angriff Stand" = list(
      "Art des Angriffs" = list(
        type = "select",
        choices = c("Direktangriff", "Gegenangriff", "Kombination", "Finte")
      ),
      "Nage waza" = list(
        type = "selectize",
        choices = c("Seoi-nage", "Uchi-mata", "Tai-otoshi")
      ),
      "Wertung" = list(
        type = "radio",
        choices = c("No Score", "Kinza", "Yuko", "Waza ari", "Ippon", "Shido")
      )
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = list(
        type = "select",
        choices = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion")
      ),
      "Kontaktaufnahme" = list(
        type = "radio",
        choices = c("Vorderseite", "Rückseite", "Seite SX", "Seite DX", "Herab", "Direkt")
      )
    ),
    "Angriff Boden" = list(
      "Ne waza" = list(
        type = "selectize",
        choices = c("Sankaku", "Kesa gatame", "Mune gatame")
      ),
      "Griffbeginn" = list(
        type = "radio",
        choices = c("Arm", "Nacken", "Achselhöhle")
      )
    )
  )
)

# User Interface für Shiny app ####
ui <- page_navbar(
  title = "Behaviour One",
  theme = bs_theme(version = 5),
  header = tags$head(
      # Lade Font Awesome für Icons
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
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
      /* Hover-Effekt für klickbare Zeilen */
      .clickable-row {
        cursor: pointer;
      }
      .clickable-row:hover {
        background-color: #f0f8ff !important;
      }
      /* Zeit-Wert formatieren */
      .time-display {
        font-family: monospace;
        color: #0066cc;
        text-decoration: underline;
        cursor: pointer;
      }
      /* Lösch-Button formatieren */
      .delete-btn {
        color: white;
        cursor: pointer;
        background-color: #dc3545; /* Optional: Ein Rot für den Hintergrund */
        border: none;
        padding: 5px 10px;
        border-radius: 4px;
      }
      .delete-btn:hover {
        color: white;
        background-color: #bd2130; /* Ein dunkleres Rot für den Hover-Zustand */
      }
      /* Edit-Button formatieren */
      .edit-btn {
        color: white;
        cursor: pointer;
        background-color: #28a745; /* Grün für den Hintergrund */
        border: none;
        padding: 5px 10px;
        border-radius: 4px;
        margin-right: 5px;
      }
      .edit-btn:hover {
        color: white;
        background-color: #218838; /* Ein dunkleres Grün für den Hover-Zustand */
      }
      /* Styling für Edit-Modus */
      .edit-mode {
        border: 2px solid #28a745 !important;
        background-color: #f0fff0 !important;
      }
      .action-btn-container {
        display: flex;
        justify-content: space-between;
        margin-top: 10px;
      }
      /* Autosave Notification Styles */
      .autosave-notification {
        position: fixed;
        bottom: 20px;
        right: 20px;
        padding: 10px 20px;
        background-color: #4CAF50;
        color: white;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        display: none;
        z-index: 1000;
      }
      /* Sidebar buttons Styling */
      .sidebar-buttons {
        display: flex;
        flex-direction: column;
        gap: 10px;
        margin-top: 15px;
      }
      .sidebar-buttons .btn {
        width: 100%;
        margin-bottom: 5px;
      }
      .sidebar-buttons .shiny-input-container {
        width: 100%;
      }
    ")),
  
  tags$script(HTML("
    // Globales Objekt zum Speichern der Video-URLs
    var videoStorage = {};
    
    $(document).ready(function() {
      // Speed control für Video
      Shiny.addCustomMessageHandler('updateSpeed', function(message) {
        var video = document.getElementById('videoPlayer');
        if (video) {
          video.playbackRate = message.speed;
        }
      });
      
      // Zu einer bestimmten Zeit springen
      Shiny.addCustomMessageHandler('seekToTime', function(message) {
        var video = document.getElementById('videoPlayer');
        if (video) {
          video.currentTime = message.time;
        }
      });
      
      // Video File Upload Handler
      $('#videoFile').on('change', function(e) {
        var file = e.target.files[0];
        if (file) {
          var objectURL = URL.createObjectURL(file);
          $('#videoPlayer').attr('src', objectURL);
          
          // Speichere Video URL mit Dateinamen als Schlüssel
          videoStorage[file.name] = objectURL;
          
          // Sende Liste der verfügbaren Videos an Shiny
          var videoNamesList = Object.keys(videoStorage);
          Shiny.setInputValue('videoNames', videoNamesList);
          
          // Setze den aktuellen Videowert (für die Dropdown-Liste)
          Shiny.setInputValue('currentVideoName', file.name);
        }
      });
      
      // Wenn ein Video aus der Dropdown-Liste ausgewählt wird
      Shiny.addCustomMessageHandler('loadSelectedVideo', function(message) {
        var videoName = message.name;
        if (videoStorage[videoName]) {
          $('#videoPlayer').attr('src', videoStorage[videoName]);
        }
      });
      
      // Direkt beim Tagging die aktuelle Videozeit senden
      $('#add_tag').on('click', function() {
        var video = document.getElementById('videoPlayer');
        if (video) {
          Shiny.setInputValue('tagCurrentTime', video.currentTime);
        }
      });
  
      // Delegierte Event-Handler für Delete- und Edit-Buttons
      $(document).on('click', '.delete-btn', function(e) {
        e.stopPropagation(); // Verhindert, dass das Event zum Table-Row-Click bubbled
        var timeValue = $(this).data('time');
        Shiny.setInputValue('delete_row', timeValue);
      });

      $(document).on('click', '.edit-btn', function(e) {
        e.stopPropagation(); // Verhindert, dass das Event zum Table-Row-Click bubbled
        var timeValue = $(this).data('time');
        Shiny.setInputValue('edit_row', timeValue);
      });

      // Aktuelle Videozeit kontinuierlich anzeigen
      setInterval(function() {
        var video = document.getElementById('videoPlayer');
        if (video) {
          Shiny.setInputValue('currentVideoTime', video.currentTime);
        }
      }, 500); // alle 500ms aktualisieren
      
      // Autosave-Benachrichtigung anzeigen
      Shiny.addCustomMessageHandler('showAutosaveNotification', function(message) {
        // Erstelle Benachrichtigung, falls noch nicht vorhanden
        if ($('#autosaveNotification').length === 0) {
          $('body').append('<div id=\"autosaveNotification\" class=\"autosave-notification\"><i class=\"fas fa-save\"></i> ' + message.text + '</div>');
        }
        
      // Zeige Benachrichtigung
        $('#autosaveNotification').text(message.text).fadeIn().delay(2000).fadeOut();
      });
    });
  "))
    ), 
  ## Sidebar ####
  sidebar = sidebar(
    width = "20%",
    position = "right",
    open = FALSE,
    title = useShinyjs(),
    # Füge neue Datenmanagement-Sektion hinzu,
    div(
        style = "padding: 15px 0;",
        h4("Datenmanagement"),
        div(
          class = "sidebar-buttons",
          downloadButton("download_tags", "Events exportieren"),
          downloadButton("download_special_format", "T-Daten exportieren"),
          downloadButton("save_project", "Projekt speichern"),
          fileInput("load_project", "Projekt laden", accept = ".rds"),
          uiOutput("restore_autosave"),
          hr(),
          actionButton("clear_all", "Alles löschen", class = "btn-danger", style = "width: 100%;")
        )
      )
  ),
  collapsible = TRUE,
  
  ## Analyse Seite ####,
  nav_panel(
      "Analyse",
      # Erste Zeile mit Video und Event Tagging
      fluidRow(
        column(6,
               card(height = "100%",
                 card_header("Video"),
                 fluidRow(
                   column(6,
                          tags$input(
                            id = "videoFile",
                            type = "file",
                            accept = "video/mp4,video/webm,video/ogg",
                            class = "form-control mb-3"
                          )
                   ),
                   column(6,
                          uiOutput("videoSelectionDropdown")
                   )
                 ),
                 tags$video(
                   id = "videoPlayer",
                   width = "100%",
                   controls = TRUE,
                   src = ""
                 ),
                 fluidRow(
                   column(6,
                          sliderInput("videoSpeed", "Geschwindigkeit", 
                                     min = 0.25, max = 2, value = 1, step = 0.25)
                   ),
                   column(6,
                          textOutput("currentTimeDisplay")
                   )
                 )
               )
        ),
        column(6,  
               card(height = "100%",
                 card_header("Event Tagging"),
                 fluidRow(
                   column(6,
                          radioButtons("rolle", "Judoka",
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
  # Reaktive Werte für die Anwendung
rv <- reactiveValues(
  events = data.frame(
    Zeit = numeric(0),
    FPS = numeric(0),
    Rolle = character(0),
    Phase = character(0),
    stringsAsFactors = FALSE
  ),
  current_video = NULL,
  editing = FALSE,      # Flag für Bearbeitungsmodus
  edit_index = NULL     # Index der zu bearbeitenden Zeile
)
  
  # Zeit in FPS #
  formatTime <- function(seconds) {
    total_frames <- round(seconds * 30)
    return(total_frames)
  }
  
  # Aktualisiere die Videogeschwindigkeit, wenn sich der Slider ändert
  observeEvent(input$videoSpeed, {
    session$sendCustomMessage("updateSpeed", list(speed = input$videoSpeed))
  })
  
  # Aktualisiere die Anzeige der aktuellen Videozeit
  output$currentTimeDisplay <- renderText({
    req(input$currentVideoTime)
    paste("Zeit:", formatTime(input$currentVideoTime))
  })
  
  # Dropdown für die Videoauswahl aktualisieren
  output$videoSelectionDropdown <- renderUI({
    req(input$videoNames)
    selectInput("selectedVideo", "Geladene Videos:", 
                choices = input$videoNames,
                selected = input$currentVideoName)
  })
  
  # Video wechseln, wenn ein anderes aus der Dropdown-Liste ausgewählt wird
  observeEvent(input$selectedVideo, {
    req(input$selectedVideo)
    if (input$selectedVideo != "") {
      session$sendCustomMessage("loadSelectedVideo", list(name = input$selectedVideo))
      rv$current_video <- input$selectedVideo
    }
  })
  
  # Update main category choices based on selected role
  observe({
    role <- input$rolle
    main_cats <- names(categories[[role]])
    updateSelectInput(session, "main_category", choices = main_cats)
  })
  
# Dynamisches UI für die Kriterien basierend auf der ausgewählten Hauptkategorie erstellen
output$criteria_ui <- renderUI({
  req(input$rolle, input$main_category)
  
  role <- input$rolle
  main_cat <- input$main_category
  
  if (is.null(categories[[role]][[main_cat]])) {
    return(NULL)
  }
  
  criteria_list <- categories[[role]][[main_cat]]
  
  # Erstelle UI-Elemente für jedes Kriterium
  criteria_uis <- lapply(names(criteria_list), function(criterion_name) {
    criterion_info <- criteria_list[[criterion_name]]
    input_id <- paste0("criterion_", gsub(" ", "_", criterion_name))
    
    # Je nach definiertem Typ das passende UI-Element erstellen
    if (criterion_info$type == "select") {
      selectInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    } else if (criterion_info$type == "selectize") {
      selectizeInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1],
        options = list(
          placeholder = paste("Wähle", criterion_name),
          create = TRUE,
          createOnBlur = TRUE
        )
      )
    } else if (criterion_info$type == "radio") {
      radioButtons(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1],
        inline = TRUE
      )
    } else if (criterion_info$type == "checkbox") {
      checkboxGroupInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    } else {
      # Fallback auf select
      selectInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    }
  })
  
  # Gibt alle UI-Elemente zurück
  do.call(tagList, criteria_uis)
})
  
# Event hinzufügen oder aktualisieren, wenn der Button geklickt wird
observeEvent(input$add_tag, {
  req(input$rolle, input$main_category)
  
  # Sammle alle Kriterien-Werte
  role <- input$rolle
  main_cat <- input$main_category
  
  # Zeit vom Video oder aus bestehendem Event
  time_point <- if (rv$editing && !is.null(rv$edit_index)) {
    rv$events$Zeit[rv$edit_index]
  } else {
    req(input$tagCurrentTime)
    input$tagCurrentTime
  }
  
  time_formatted <- formatTime(time_point)
  
  # Hole die Kriterien für diese Kategorie
  criteria_names <- names(categories[[role]][[main_cat]])
  
  # Erstelle eine Liste der gewählten Werte
  selected_values <- list()
  for (criterion in criteria_names) {
    input_id <- paste0("criterion_", gsub(" ", "_", criterion))
    if (!is.null(input[[input_id]])) {
      selected_values[[criterion]] <- input[[input_id]]
    }
  }
  
  # Erstelle einen neuen Eintrag für die Datentabelle
  new_row <- data.frame(
    Zeit = time_point,
    FPS = time_formatted,
    Rolle = role,
    Phase = main_cat,
    stringsAsFactors = FALSE
  )
  
  # Füge ausgewählte Kriterien hinzu
  for (criterion in names(selected_values)) {
    # Wenn es sich um einen Vektor handelt (z.B. bei Checkboxen), verbinde die Werte
    if (length(selected_values[[criterion]]) > 1) {
      new_row[[criterion]] <- paste(selected_values[[criterion]], collapse = ", ")
    } else {
      new_row[[criterion]] <- selected_values[[criterion]]
    }
  }
  
  # Im Bearbeitungsmodus: ersetze die existierende Zeile
  if (rv$editing && !is.null(rv$edit_index)) {
    # Stelle sicher, dass alle Spalten in beiden Dataframes vorhanden sind
    for (col in names(new_row)) {
      if (!col %in% names(rv$events)) {
        rv$events[[col]] <- NA
      }
    }
    for (col in names(rv$events)) {
      if (!col %in% names(new_row)) {
        new_row[[col]] <- NA
      }
    }
    
    # Ersetze die Zeile
    rv$events[rv$edit_index, names(new_row)] <- new_row
    
    # Bearbeitungsmodus zurücksetzen
    rv$editing <- FALSE
    rv$edit_index <- NULL
    
    # Button-Erscheinungsbild zurücksetzen
    shinyjs::removeClass(selector = "#add_tag", class = "btn-success")
    shinyjs::html("add_tag", "Event taggen")
    
    showNotification("Event wurde aktualisiert", type = "message")
  } else {
    # Im normalen Modus: füge eine neue Zeile hinzu
    if (nrow(rv$events) == 0) {
      rv$events <- new_row
    } else {
      # Stelle sicher, dass alle Spalten in beiden Dataframes vorhanden sind
      for (col in names(new_row)) {
        if (!col %in% names(rv$events)) {
          rv$events[[col]] <- NA
        }
      }
      for (col in names(rv$events)) {
        if (!col %in% names(new_row)) {
          new_row[[col]] <- NA
        }
      }
      
      rv$events <- rbind(rv$events, new_row)
    }
  }
  
  # Sortiere Events nach Zeit
  rv$events <- rv$events[order(rv$events$Zeit), ]
})
  
# Anzeige der getaggten Events in einer Tabelle
output$event_list <- renderDT({
  req(rv$events)
  
  if (nrow(rv$events) == 0) {
    return(NULL)
  }
  
  # Formatiere Tabelle für Anzeige
  display_df <- rv$events

  # Runde die Zeit-Spalte auf eine Dezimalstelle
  display_df$Zeit <- round(display_df$Zeit * 10) / 10
  
  # Füge Spalten für den Bearbeiten- und Löschbutton hinzu
  display_df$Event <- sapply(display_df$Zeit, function(time) {
  sprintf('<button class="btn btn-sm btn-success edit-btn" data-time="%s"><i class="fa fa-edit"></i> Bearbeiten</button><button class="btn btn-sm btn-danger delete-btn" data-time="%s"><i class="fa fa-trash"></i> Löschen</button>', time, time)
  })
  
  # Datatable mit angepasstem JavaScript
  datatable(
    display_df,
    options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50),
      dom = 'lftip',
      rowCallback = JS("
      function(row, data) {
        $(row).addClass('clickable-row');
        $(row).on('click', function(e) {
      // Nur wenn NICHT auf einen der Buttons geklickt wurde
      if (!$(e.target).hasClass('delete-btn') && !$(e.target).closest('.delete-btn').length &&
          !$(e.target).hasClass('edit-btn') && !$(e.target).closest('.edit-btn').length) {
        Shiny.setInputValue('selected_time', data[0]);
      }
    });
    
    // Format time value as clickable
    $('td:eq(1)', row).addClass('time-display');
  }
")
    ),
    selection = 'none', # Ändere zu 'none', da wir unsere eigene Klick-Logik haben
    rownames = FALSE,
    escape = FALSE
  )
})
  
  # Zum ausgewählten Zeitpunkt im Video springen
  observeEvent(input$selected_time, {
    req(input$selected_time)
    session$sendCustomMessage("seekToTime", list(time = as.numeric(input$selected_time)))
  })
  
# Einzelne Zeile löschen
observeEvent(input$delete_row, {
  req(input$delete_row)
  time_to_delete <- as.numeric(input$delete_row)
  
  # Finde den Index der zu löschenden Zeile - präziserer Vergleich
  # Benutze which.min() um den genauesten Treffer zu finden
  differences <- abs(rv$events$Zeit - time_to_delete)
  if(length(differences) > 0 && min(differences) < 0.1) { # Toleranz von 0.1 Sekunden
    row_index <- which.min(differences)
    
    # Lösche die Zeile
    rv$events <- rv$events[-row_index, , drop = FALSE]
    
    # Melde Erfolg
    showNotification("Event wurde gelöscht", type = "message")
  } else {
    # Melde Fehler wenn keine passende Zeit gefunden wurde
    showNotification("Konnte kein passendes Event finden", type = "error")
  }
})

# Bearbeitungsmodus aktivieren
observeEvent(input$edit_row, {
  req(input$edit_row)
  time_to_edit <- as.numeric(input$edit_row)
  
  # Finde den Index der zu bearbeitenden Zeile
  differences <- abs(rv$events$Zeit - time_to_edit)
  if(length(differences) > 0 && min(differences) < 0.1) { # Toleranz von 0.1 Sekunden
    row_index <- which.min(differences)
    
    # Setze Bearbeitungsmodus
    rv$editing <- TRUE
    rv$edit_index <- row_index
    
    # Event Daten laden
    event_data <- rv$events[row_index, ]
    
    # Debug-Ausgabe
    print(paste("Bearbeite Event an Zeit:", event_data$Zeit))
    print(event_data)
    
    # Rolle und Hauptkategorie aktualisieren
    updateRadioButtons(session, "rolle", selected = event_data$Rolle)
    updateSelectInput(session, "main_category", selected = event_data$Phase)
    
    # Verzögerung erhöhen und überprüfen, ob die Hauptkategorie korrekt gesetzt wurde
    shinyjs::delay(500, {
      if (input$main_category != event_data$Phase) {
        updateSelectInput(session, "main_category", selected = event_data$Phase)
      }
      
      # Nochmals verzögert die Kriterien aktualisieren
      shinyjs::delay(300, {
        # Alle Kriterien für diese Kategorie durchlaufen
        criteria_names <- names(categories[[event_data$Rolle]][[event_data$Phase]])
        for (criterion in criteria_names) {
          input_id <- paste0("criterion_", gsub(" ", "_", criterion))
          
          # Wenn der Wert in den Event-Daten existiert
          if (criterion %in% names(event_data) && !is.na(event_data[[criterion]])) {
            # Feld-Typ bestimmen
            criterion_type <- categories[[event_data$Rolle]][[event_data$Phase]][[criterion]]$type
            
            print(paste("Aktualisiere Feld:", input_id, "mit Wert:", event_data[[criterion]]))
            
            if (criterion_type == "select" || criterion_type == "selectize") {
              updateSelectInput(session, input_id, selected = event_data[[criterion]])
            } else if (criterion_type == "radio") {
              updateRadioButtons(session, input_id, selected = event_data[[criterion]])
            } else if (criterion_type == "checkbox") {
              # Bei Checkbox die Werte teilen
              selected_values <- unlist(strsplit(event_data[[criterion]], ", "))
              updateCheckboxGroupInput(session, input_id, selected = selected_values)
            }
          }
        }
        
        # Button-Erscheinungsbild ändern
        shinyjs::addClass(selector = "#add_tag", class = "btn-success")
        shinyjs::html("add_tag", "Event aktualisieren")
      })
    })
    
    # Zum Zeitpunkt im Video springen
    session$sendCustomMessage("seekToTime", list(time = event_data$Zeit))
    
    # Melde Erfolg
    showNotification("Event wird bearbeitet. Aktualisieren Sie die Werte und klicken Sie auf 'Event aktualisieren'.", 
                     type = "message", duration = 5)
  }
})  

  # Alle Events löschen
  observeEvent(input$clear_all, {
    showModal(modalDialog(
      title = "Events löschen",
      "Möchtest du wirklich alle getaggten Events löschen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_clear", "Löschen", class = "btn-danger")
      )
    ))
  })
  
  # Bestätigung zum Löschen aller Events
  observeEvent(input$confirm_clear, {
    rv$events <- data.frame(
      Zeit = numeric(0),
      FPS = numeric(0),  # Korrigiert von "ZeitFormatiert" zu "FPS"
      Rolle = character(0),
      Phase = character(0),
      stringsAsFactors = FALSE
    )
    removeModal()
  })
  
  # Download der Events als CSV-Datei
  output$download_tags <- downloadHandler(
    filename = function() {
      paste0("judo_events_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      write.csv(rv$events, file, row.names = FALSE)
    }
  )

  # Download der Events im speziellen Format für T-Daten
output$download_special_format <- downloadHandler(
  filename = function() {
    paste0("judo_t_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  },
  content = function(file) {
    req(rv$events)
    if(nrow(rv$events) == 0) {
      return(NULL)
    }
    
    # Erstelle die Formatierung wie gewünscht
    result <- character(0)
    
    # Füge die Headerzeile hinzu
    result <- c(result, "TIME\tEVENT")
    
    # Sortiere die Events nach FPS (Frames)
    events_sorted <- rv$events[order(rv$events$FPS), ]
    
    # Wenn es mindestens ein Event gibt, füge eine Zeile mit ":" ein Frame vor dem ersten Event hinzu
    if (nrow(events_sorted) > 0) {
      first_frame <- events_sorted$FPS[1]
      start_frame <- first_frame - 1
      result <- c(result, paste0(start_frame, "\t:"))
    }
    
    # Verarbeite alle Events
    for (i in 1:nrow(events_sorted)) {
      event <- events_sorted[i, ]
      time_value <- event$FPS
      
      # Sammle alle nicht-NA Werte für den Event-String
      event_values <- c()
      
      # Beginne mit der Rolle (Blau oder Weiss)
      event_values <- c(event_values, event$Rolle)
      
      # Füge alle weiteren nicht-NA Werte hinzu
      for (col in names(event)) {
        # Überspringe Standardspalten und NA-Werte
        if (col %in% c("Zeit", "FPS", "Rolle", "Phase", "Event") || is.na(event[[col]])) {
          next
        }
        event_values <- c(event_values, event[[col]])
      }
      
      # Verbinde alle Werte mit Komma
      event_str <- paste(event_values, collapse = ",")
      
      # Füge die Zeile zum Ergebnis hinzu
      result <- c(result, paste0(time_value, "\t", event_str))
    }
    
    # Füge eine abschließende Zeile mit "&" ein Frame nach dem letzten Event hinzu
    if (nrow(events_sorted) > 0) {
      last_frame <- events_sorted$FPS[nrow(events_sorted)]
      end_frame <- last_frame + 1
      result <- c(result, paste0(end_frame, "\t&"))
    }
    
    # Schreibe das Ergebnis in die Datei
    writeLines(result, file)
  }
)
  # Projektdaten speichern
output$save_project <- downloadHandler(
  filename = function() {
    paste0("judo_project_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
  },
  content = function(file) {
    # Erstelle ein Projektobjekt mit allen relevanten Daten
    project_data <- list(
      events = rv$events,
      current_video = rv$current_video,
      timestamp = Sys.time()
    )
    saveRDS(project_data, file)
  }
)

# Projektdaten laden
observeEvent(input$load_project, {
  req(input$load_project)
  
  # Projektdaten laden
  project_data <- readRDS(input$load_project$datapath)
  
  # Daten wiederherstellen
  rv$events <- project_data$events
  rv$current_video <- project_data$current_video
  
  # Hinweis anzeigen
  showNotification("Projekt erfolgreich geladen", type = "message")
  
  # Video wiederherstellen (wenn möglich)
  if (!is.null(project_data$current_video)) {
    updateSelectInput(session, "selectedVideo", selected = project_data$current_video)
  }
})

# Automatisches Speichern einrichten
autoSaveTimer <- reactiveTimer(60000)  # alle 60 Sekunden

observe({
  autoSaveTimer()
  
  # Nur speichern, wenn Events vorhanden sind
  if (nrow(rv$events) > 0) {
    # Projektdaten erstellen
    project_data <- list(
      events = rv$events,
      current_video = rv$current_video,
      timestamp = Sys.time()
    )
    
    # In einer temporären Datei speichern
    temp_file <- file.path(tempdir(), "judo_autosave.rds")
    saveRDS(project_data, temp_file)
    
    # Benachrichtigung anzeigen über Custom JS
    session$sendCustomMessage("showAutosaveNotification", 
                              list(text = "Automatisch gespeichert"))
  }
})

# Button für Wiederherstellung hinzufügen (sichtbar in der UI)
output$restore_autosave <- renderUI({
  temp_file <- file.path(tempdir(), "judo_autosave.rds")
  if (file.exists(temp_file)) {
    actionButton("restore_autosave_btn", "Letzte automatische Sicherung wiederherstellen")
  }
})

# Handler für Wiederherstellungs-Button
observeEvent(input$restore_autosave_btn, {
  temp_file <- file.path(tempdir(), "judo_autosave.rds")
  
  if (file.exists(temp_file)) {
    project_data <- readRDS(temp_file)
    rv$events <- project_data$events
    rv$current_video <- project_data$current_video
    showNotification("Automatische Sicherung wiederhergestellt", type = "message")
    
    # Video wiederherstellen (wenn möglich)
    if (!is.null(project_data$current_video)) {
      updateSelectInput(session, "selectedVideo", selected = project_data$current_video)
    }
  } else {
    showNotification("Keine automatische Sicherung gefunden", type = "error")
  }
})
}

# App starten
shinyApp(ui, server)
