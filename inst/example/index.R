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
    assign("core", channel$objects["core"], envir = .GlobalEnv)
  })
})

socket$connect()

