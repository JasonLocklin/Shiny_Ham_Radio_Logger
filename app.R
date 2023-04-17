# Shiny Radio Logbook
# This Shiny application provides a simple interface for general-purpose
# Ham Radio logging.
#
# Features:
# * Very Simple input of QSO information
# * Auto-saves to ADIF after every contact or edit.
# * Editing QSOs (Edit, and then clear to delete)
# * Export to an ADIF file
# * Runs in a web browser
#
# Does not support CAT or contests


# Load required libraries
library(shiny)
library(DT)

# List of modes available
modeList <- c("CW", "SSB", "AM", "FM", "RTTY", "PSK31", "FT8", "FT4", "JT65", "JT9")

# Build the user interface
ui <- fluidPage(
  titlePanel("Shiny Radio Logbook"),
  tags$head(tags$script(HTML('$(document).on("click", ".edit", function() {Shiny.onInputChange("edit_id", this.id);});'))),
  tags$head(tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")),
  sidebarLayout(
    sidebarPanel(
      textInput("callsign", "Callsign", ""),
      dateInput("date", "Date (UTC)", as.Date(as.POSIXct(Sys.time(), tz = "UTC"))),
      textInput("time", "Time (UTC)", format(Sys.time(), "%H:%M", tz = "UTC")),
      numericInput("frequency", "Frequency (MHz)", ""),
      selectInput("mode", "Mode",
                  choices = modeList,
                  selected = "CW"),
      uiOutput("rst_sent_ui"),
      uiOutput("rst_received_ui"),
      conditionalPanel("true", textAreaInput("notes", "Notes", "", width = "100%", height = "50px")),
      actionButton("add_log", label = "", icon = icon("plus-circle", class = "fa-2x"), class = "btn btn-primary"),
      actionButton("clear_fields", label = "", icon = icon("times-circle", class = "fa-2x"), class = "btn btn-secondary"),
      br(),
      br(),
      downloadButton("download_log", "Download Log as ADIF"),
      downloadLink("auto_save_log", "", style = "display:none")
    ),
    mainPanel(
      DTOutput("log_table")
    )
  )
)

# Server Backend
server <- function(input, output, session) {
  log <- reactiveVal()

  # Load log from disk, if missing, start a blank one
  # Note: to start fresh, delete the "log_data.RDS" file
  # This is an R data format file and can only be opened with R.
  # Export to ADIF with the button.
  observe({
    if (file.exists("log_data.RDS")) {
      log_data <- readRDS("log_data.RDS")
      log(log_data)
    } else {
      log(data.frame(Date = character(),
                     Time = character(),
                     Callsign = character(),
                     Frequency = character(),
                     Mode = character(),
                     RST_Sent = character(),
                     RST_Received = character(),
                     Notes = character(),
                     stringsAsFactors = FALSE))
    }
  })

  # Render table of QSOs at right
  output$log_table <- DT::renderDataTable({
    log_data <- log()
    if (nrow(log_data) > 0) {
      log_data$Edit <- sapply(1:nrow(log_data), function(i) sprintf('<i class="fa fa-pencil-square-o edit" id="%s"></i>', i))
    }

    datatable(log_data,
              escape = FALSE, rownames = FALSE, filter = "bottom",
              ) %>%
          formatStyle(columns = ncol(log_data), cursor = "pointer", textDecoration = "none", textAlign = "center")
  })

  # RST input for different modes is different
  output$rst_sent_ui <- renderUI({
    default_value <- switch(input$mode, "CW" = "599", "SSB" = "59", "")
    default_name <- switch(input$mode, "CW" = "RST Sent", "SSB" = "RS Sent", "Report Sent")
    textInput("rst_sent", default_name, default_value)
  })
  output$rst_received_ui <- renderUI({
    default_value <- switch(input$mode, "CW" = "599", "SSB" = "59", "")
    default_name <- switch(input$mode, "CW" = "RST Sent", "SSB" = "RS Sent", "Report Sent")
    textInput("rst_received", default_name, default_value)
  })

  # Add QSO to log button
  observeEvent(input$add_log, {
    new_entry <- data.frame(Date = as.Date(input$date), Time = input$time, Callsign = input$callsign, Frequency = input$frequency, Mode = input$mode, RST_Sent = input$rst_sent, RST_Received = input$rst_received, Notes = input$notes, stringsAsFactors = FALSE)
    log(rbind(log(), new_entry))
    saveRDS(log(), "log_data.RDS")
    session$sendCustomMessage("download", list(id = "auto_save_log", filename = "log_data.adif"))
  })

  # Clear fields in left pane (Leave mode alone)
  observeEvent(c(input$add_log, input$clear_fields), {
    updateTextInput(session, "callsign", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateTextInput(session, "time", value = format(Sys.time(), "%H:%M"))
    updateNumericInput(session, "frequency", value = "")
    updateTextInput(session, "rst_sent", value = "")
    updateTextInput(session, "rst_received", value = "")
    updateTextAreaInput(session, "notes", value = "")
  })


  # Handle Edit button in log table
  observeEvent(input$edit_id, {
    edit_id <- as.numeric(input$edit_id)

    if (!is.null(edit_id) && !is.na(edit_id)) {
      edit_row <- log()[edit_id, ]

      updateTextInput(session, "callsign", value = edit_row$Callsign)
      updateDateInput(session, "date", value = as.character(edit_row$Date))
      updateTextInput(session, "time", value = format(as.POSIXct(edit_row$Time, format = "%H:%M"), "%H:%M"))
      updateNumericInput(session, "frequency", value = edit_row$Frequency)
      updateSelectInput(session, "mode", selected = edit_row$Mode)
      updateTextInput(session, "rst_sent", value = edit_row$RST_Sent)
      updateTextInput(session, "rst_received", value = edit_row$RST_Received)
      updateTextAreaInput(session, "notes", value = edit_row$Notes)

      log_data <- log()
      log_data <- log_data[-edit_id, ]
      log(log_data)
      saveRDS(log_data, "log_data.RDS")
    }
  })

  # Download ADIF data
  output$download_log <- downloadHandler(
    filename = function() {
      "log_data.adif"
    },
    content = function(file) {
      writeLines(paste("  <", log(), ">", sep = ""), file)
    }
  )

}

shinyApp(ui = ui, server = server)