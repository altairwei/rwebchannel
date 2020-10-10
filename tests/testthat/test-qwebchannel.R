library(websocket)

test_that("Try to build connection", {
  socket <- WebSocket$new("ws://localhost:8848", autoConnect = FALSE)

  socket$onClose(function(event) {
    cat("web channel closed\n")
  })

  socket$onError(function(event) {
    msg = paste("Client failed to connect:", event$message, "\n")
    cat(msg)
  })

  socket$onOpen(function(event) {
    QWebChannel$new(socket, function(channel) {
      cat("Hello World.\n")
    })
  })

  socket$connect()
})
