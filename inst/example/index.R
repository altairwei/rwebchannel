library(rwebchannel)
library(websocket)

baseUrl <- "ws://localhost:8848"
cat(paste0("Connecting to WebSocket server at ", baseUrl, ".\n"))

socket <- WebSocket$new(baseUrl, autoConnect = FALSE)

socket$onClose(function(event) {
  cat("web channel closed\n")
})

socket$onError(function(event) {
  cat(paste0("web channel error:", event$message, "\n"))
})

socket$onOpen(function(event) {
  cat("WebSocket connected, setting up QWebChannel.\n")
  QWebChannel$new(socket, function(channel) {
    cat("Hello World.\n")
  })
})

socket$connect()

