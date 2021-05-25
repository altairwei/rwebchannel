library(shiny)
library(rwebchannel)
library(websocket)

# Define UI ----
ui <- fluidPage(
  titlePanel("RWebchannel Shiny Example"),
  mainPanel(
    verbatimTextOutput("display"),
      textInput("message", "Message to send:"),
      actionButton("send", "Send")
  )
)

connect_to_server <- function(rv) {
  base_url <- "ws://localhost:12345"
  cat(paste0("Connecting to WebSocket server at ", base_url, ".\n"))

  socket <- WebSocket$new(base_url, autoConnect = FALSE)

  socket$onClose(function(event) {
    cat("web channel closed\n\n")
  })

  socket$onError(function(event) {
    cat(paste0("web channel error:", event$message, "\n"))
  })

  socket$onOpen(function(event) {
    cat("WebSocket connected, setting up QWebChannel.\n")
    QWebChannel$new(socket, function(channel) {
      rv$core <- channel$objects[["core"]]
    })
  })

  socket$connect()
}

# Define server logic ----
server <- function(input, output, session) {
  rv <- reactiveValues(
    core = NULL,
    send = NULL,
    messages = "",
    new_msg = NULL
  )

  connect_to_server(rv)

  # Init
  observeEvent(rv$core, {
    rv$send <- function(text) {
      msg <- paste("Sent message:", text, "\n")
      rv$messages <- c(rv$messages, msg)
      rv$core$receiveText(text)
    }

    rv$core$sendText$connect(function(msg) {
      rv$new_msg <- paste("Received message:", msg, "\n")
    })

  })

  observeEvent(rv$new_msg, {
    rv$messages <- c(rv$messages, rv$new_msg)
  })

  observeEvent(input$send, {
    rv$send(input$message)
    updateTextInput(session, "message", value = "")
  })

  output$display <- renderText(rv$messages)
}

# Run the app ----
app <- shinyApp(ui = ui, server = server)

shiny::runApp(app)