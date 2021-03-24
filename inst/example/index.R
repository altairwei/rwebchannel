library(rwebchannel)
library(websocket)

baseUrl <- "ws://localhost:12345"
cat(paste0("Connecting to WebSocket server at ", baseUrl, ".\n\n"))

socket <- WebSocket$new(baseUrl, autoConnect = FALSE)

socket$onClose(function(event) {
  cat("web channel closed\n\n")
})

socket$onError(function(event) {
  cat(paste0("web channel error:", event$message, "\n\n"))
})

socket$onOpen(function(event) {
  cat("WebSocket connected, setting up QWebChannel.\n\n")
  QWebChannel$new(socket, function(channel) {
    assign("core", channel$objects[["core"]], envir = .GlobalEnv)
    assign("send", function(text) {
      cat("Sent message: ", text, "\n")
      core$receiveText(text)
    }, envir = .GlobalEnv)
    core$sendText$connect(function(message) {
      cat("Received message: ", message, "\n")
    })
  })
})

socket$connect()


start <- function() {
  while(TRUE) {
    cat("Enter: ")
    input <- readLines("stdin", 1)
    if (input == "exit")
      break;
    send(input)
    later::run_now(3L)
  }
}

while(TRUE) {
  later::run_now(1L)
}