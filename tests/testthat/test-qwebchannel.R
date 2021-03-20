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


test_that("Test send", {
  trans <- FakeTransport$new()
  channel <- QWebChannel$new(trans)
  expect_called(trans$send, 1)

  channel$send(list(A = iconv("这是中文", from = "UTF-8", to = "gbk")))
  expect_args(trans$send, 2, rjson::toJSON(
    list(A = iconv("这是中文", from = "UTF-8", to = "gbk"))
  ))
})


obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "methods": [
    ["deleteLater", 3],
    ["deleteLater()", 3],
    ["DocumentsFromSQLWhere", 5],
    ["DocumentsFromSQLWhere()", 5]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]
  ]
}', simplify = FALSE)

test_that("Test handleSignal", {
  signal_data = rjson::fromJSON('
{
  "args": [
    "hello world"
  ],
  "object": "Database",
  "signal": 2,
  "type": 1
}', simplify = FALSE)

  trans <- FakeTransport$new()
  webChannel <- QWebChannel$new(trans)
  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  qobj$tagModified$connect(func1)

  webChannel$handleSignal(signal_data)
  expect_args(func1, 1, "hello world")
})


test_that("Test handleResponse", {
  resp_data = rjson::fromJSON('
{
  "data": "Hello World",
  "id":  2,
  "type":  10
}', simplify = FALSE)

  trans <- FakeTransport$new()
  webChannel <- QWebChannel$new(trans)
  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  # exec id should be 2
  webChannel$exec(list(A = "data"), func1)
  expect_equal(webChannel$execCallbacks[["2"]], func1)
  expect_equal(webChannel$execId, 3L)

  webChannel$handleResponse(resp_data)
  expect_null(webChannel$execCallbacks[["2"]])
  expect_args(func1, 1, "Hello World")

  # Check order of response
  func2 <- mockery::mock()
  func3 <- mockery::mock()
  webChannel$exec(list(A = "first exec"), func2)
  expect_equal(webChannel$execCallbacks[["3"]], func2)
  webChannel$exec(list(A = "second exec"), func3)
  expect_equal(webChannel$execCallbacks[["4"]], func3)

  webChannel$handleResponse(list(
    data = "second exec resp",
    id = 4,
    type = 10
  ))
  expect_null(webChannel$execCallbacks[["4"]])
  expect_args(func3, 1, "second exec resp")

  webChannel$handleResponse(list(
    data = "first exec resp",
    id = 3,
    type = 10
  ))
  expect_null(webChannel$execCallbacks[["3"]])
  expect_args(func2, 1, "first exec resp")
})

test_that("Test complex handleResponse", {
  resp_data = rjson::fromJSON('
{
  "data": [
    {
      "__QObject*__": true,
      "data": {
        "properties": [
          [0, "Title", [1, 4], "Hello World"],
          [1, "GUID", [1, 5], "5e0f57b0-d53c-4002-8bdc-57884ccfb7b9"]
        ],
        "signals": [
          ["tagCreated", 0],
          ["tagModified", 1],
          ["styleCreated", 2],
          ["documentCreated", 3],
          ["destroyed", 6]
        ]
      },
      "id": "{2e34b2d4-8804-4ab7-bf45-7cc452f5f6d5}"
    },
    {
      "__QObject*__": true,
      "data": {
        "properties": [
          [0, "Title", [1, 4], "Hahahahaha"],
          [1, "GUID", [1, 5], "b97eee40-8d04-4788-a6d1-b93deb9a0801"]
        ],
        "signals": [
          ["tagCreated", 0],
          ["tagModified", 1],
          ["styleCreated", 2],
          ["documentCreated", 3],
          ["destroyed", 6]
        ]
      },
      "id": "{2f84de23-98df-4fba-93d4-5a323ce6addf}"
    }
  ],
  "id":  2,
  "type":  10
}', simplify = FALSE)

  trans <- FakeTransport$new()
  webChannel <- QWebChannel$new(trans)
  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  qobj$DocumentsFromSQLWhere("DOCUMENT_LOCATION like '/My Notes/'", func1)

  webChannel$handleResponse(resp_data)
  expect_null(webChannel$execCallbacks[["2"]])
  expect_called(func1, 1)
  expect_equal(mock_args(func1)[[1]][[1]][[1]]$Title, "Hello World")
  expect_equal(mock_args(func1)[[1]][[1]][[2]]$Title, "Hahahahaha")
})

test_that("Test handlePropertyUpdate", {
  prop_data = rjson::fromJSON('
{
  "data": [
    {
      "object": "Database",
      "properties": {
        "0": "New Title",
        "1": "bd5c726c-120a-4d61-a4e9-d82fd07821a3"
      },
      "signals": {
        "5": ["New Title"],
        "6": ["bd5c726c-120a-4d61-a4e9-d82fd07821a3"]
      }
    }
  ],
  "type": 2
}', simplify = FALSE)

  trans <- FakeTransport$new()
  webChannel <- QWebChannel$new(trans)
  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  func2 <- mockery::mock()
  qobj$TitleChanged$connect(func1)
  qobj$GUIDChanged$connect(func2)

  webChannel$handlePropertyUpdate(prop_data)

  # Check updated property
  expect_equal(qobj$Title, "New Title")
  expect_equal(qobj$GUID, "bd5c726c-120a-4d61-a4e9-d82fd07821a3")

  # Check signal callback
  expect_called(func1, 1)
  expect_equal(mock_args(func1)[[1]][[1]], "New Title")
  expect_called(func2, 1)
  expect_equal(mock_args(func2)[[1]][[1]], "bd5c726c-120a-4d61-a4e9-d82fd07821a3")

  # Wrong case
  expect_warning(
    webChannel$handlePropertyUpdate(list(
      data = list(
        list(object = "WrongObj")
      ),
      type = 2
    )),
    "Unhandled property update: WrongObj"
  )
})
