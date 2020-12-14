library(websocket)

# test_that("Try to build connection", {
#   socket <- WebSocket$new("ws://localhost:8848", autoConnect = FALSE)

#   socket$onClose(function(event) {
#     cat("web channel closed\n")
#   })

#   socket$onError(function(event) {
#     msg = paste("Client failed to connect:", event$message, "\n")
#     cat(msg)
#   })

#   socket$onOpen(function(event) {
#     QWebChannel$new(socket, function(channel) {
#       cat("Hello World.\n")
#     })
#   })

#   socket$connect()
# })


FakeTransport <- R6::R6Class("FakeTransport", list(
  initialize = function() {
    self$send = mockery::mock()
    self$onMessage = mockery::mock()
  }
), lock_objects = FALSE)


test_that("Test exec", {
  trans <- FakeTransport$new()
  channel <- QWebChannel$new(trans)

  expect_called(trans$send, 1)
  expect_called(trans$onMessage, 1)

  channel$exec(list(type = QWebChannelMessageTypes[['idle']]))
  expect_args(trans$send, 2, rjson::toJSON(list(type = QWebChannelMessageTypes[['idle']])))

  channel$exec(list(type = QWebChannelMessageTypes[['init']]), function() {})
  expect_args(trans$send, 3, rjson::toJSON(
    list(
      type = QWebChannelMessageTypes[['init']],
      id = 2L
    )
  ))

  channel$exec(list(type = QWebChannelMessageTypes[['debug']]), function() {})
  expect_args(trans$send, 4, rjson::toJSON(
    list(
      type = QWebChannelMessageTypes[['debug']],
      id = 3L
    )
  ))


  channel$exec(list(type = QWebChannelMessageTypes[['debug']], id = 3L))
  expect_called(trans$send, 5)
  expect_warning(
    channel$exec(list(type = QWebChannelMessageTypes[['debug']], id = 3L), function() {}),
    "Cannot exec message with property id: "
  )
})
