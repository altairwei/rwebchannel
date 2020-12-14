library(rjson)
library(mockery)
library(testthat)

FakeQWebChannel <- R6::R6Class(
  "FakeQWebChannel",
  lock_objects = FALSE,
  list(
    objects = list(),
    execCallbacks = list(),
    transport = NULL,
    execId = 1L
  )
)

object_data = '
{
  "methods": [
    ["deleteLater", 3],
    ["deleteLater()", 3],
    ["CreateWizObject", 11],
    ["CreateWizObject(QString)", 11],
    ["SetSavingDocument", 12],
    ["SetSavingDocument(bool)", 12],
    ["ProcessClipboardBeforePaste", 13],
    ["ProcessClipboardBeforePaste(QVariantMap)", 13],
    ["Locale", 14],
    ["Locale()", 14],
    ["GetGroupDatabase", 15],
    ["GetGroupDatabase(QString)", 15],
    ["TranslateString", 16],
    ["TranslateString(QString)", 16],
    ["OpenURLInDefaultBrowser", 17],
    ["OpenURLInDefaultBrowser(QString)", 17],
    ["GetToken", 18],
    ["GetToken(QString)", 18],
    ["SetDialogResult", 19],
    ["SetDialogResult(int)", 19],
    ["AppStoreIAP", 20],
    ["AppStoreIAP()", 20],
    ["copyLink", 21],
    ["copyLink(QString)", 21],
    ["onClickedImage", 22],
    ["onClickedImage(QString,QString)", 22]
  ],
  "properties": [
    [0, "objectName", [1, 2], ""],
    [5, "DatabaseManager", [1, 8],
      {
        "__QObject*__": true,
        "data": {
          "methods": [
            ["deleteLater", 3],
            ["deleteLater()", 3],
            ["Database", 5],
            ["Database()", 5],
            ["CreateDocument", 6],
            ["CreateDocument(QString,QString,QString,QStringList,QString)", 6],
            ["GetGroupDatabase", 7],
            ["GetGroupDatabase(QString)", 7]
          ],
          "properties": [
            [0, "objectName", [1, 2], ""]
          ],
          "signals": [
            ["destroyed", 0],
            ["destroyed(QObject*)", 0],
            ["destroyed()", 1]
          ]
        },
        "id": "{9c3d34a0-e08d-48db-ac17-2c23bae193a9}"
      }
    ]
  ],
  "signals": [
    ["destroyed", 0],
    ["destroyed(QObject*)", 0],
    ["destroyed()", 1]
  ]
}
'

test_that("Test addMethod", {

  obj_data = rjson::fromJSON('
{
  "methods": [
    ["deleteLater", 3],
    ["deleteLater()", 3],
    ["SetSavingDocument", 12],
    ["SetSavingDocument(bool)", 12],
    ["GetToken", 18],
    ["GetToken(QString)", 18],
    ["copyLink", 21],
    ["copyLink(QString)", 21]
  ]
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  stub(QObject$new, 'bindGetterSetter', '')
  stub(QObject$new, 'addSignal', '')
  qobj <- QObject$new("WizExplorerApp", obj_data, webChannel)

  expect_type(qobj$deleteLater, "closure")
  expect_type(qobj[["deleteLater()"]], "closure")
  expect_type(qobj$SetSavingDocument, "closure")
  expect_type(qobj[["SetSavingDocument(bool)"]], "closure")
  expect_type(qobj$GetToken, "closure")
  expect_type(qobj[["GetToken(QString)"]], "closure")
  expect_type(qobj$copyLink, "closure")
  expect_type(qobj[["copyLink(QString)"]], "closure")

  qobj$deleteLater()
  expect_called(webChannel$exec, 1)
  expect_equal(mock_args(webChannel$exec)[[1]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 3,
    args = list()
  ))

  qobj$SetSavingDocument(FALSE)
  expect_called(webChannel$exec, 2)
  expect_equal(mock_args(webChannel$exec)[[2]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 12,
    args = list(FALSE)
  ))

  qobj$GetToken("test")
  expect_called(webChannel$exec, 3)
  expect_equal(mock_args(webChannel$exec)[[3]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 18,
    args = list("test")
  ))

  qobj$copyLink("https://www.test.cn")
  expect_called(webChannel$exec, 4)
  expect_equal(mock_args(webChannel$exec)[[4]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 21,
    args = list("https://www.test.cn")
  ))

  # Do not send callback as argument.
  qobj$GetToken("test_callback", function(x) x)
  expect_called(webChannel$exec, 5)
  expect_equal(mock_args(webChannel$exec)[[5]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 18,
    args = list("test_callback")
  ))

  # Send QObject
  obj_to_send <- QObject$new("ObjToSend", list(), webChannel)
  obj_to_send2 <- QObject$new("ObjToSend2", list(), webChannel)
  qobj$GetToken("test_qobj", obj_to_send, obj_to_send2, function(x) x)
  expect_called(webChannel$exec, 6)
  expect_equal(mock_args(webChannel$exec)[[6]][[1]], list(
    type = QWebChannelMessageTypes["invokeMethod"],
    object = "WizExplorerApp",
    method = 18,
    args = list("test_qobj", list(id = "ObjToSend"), list(id = "ObjToSend2"))
  ))
})


test_that("Test bindGetterSetter", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 2], "Hello World"],
    [1, "GUID", [1, 2], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"],
    [2, "Database", [1, 5], {"__QObject*__": true, "data": {}, "id": "{81b0ce6f-a09f-4dcf-bd04-70b332760b33}"}],
    [3, "Tag", [1, 2], null]
  ]
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  stub(QObject$new, 'addMethod', '')
  stub(QObject$new, 'addSignal', '', depth = 2)


  expect_warning(
    qobj <- QObject$new("WizDocument", obj_data, webChannel),
    "Undefined initial value for property")
  expect_equal(qobj$Title, "Hello World")
  expect_equal(qobj$GUID, "7bb5a78d-2f21-44ad-ad50-2f7e52437133")
  expect_true(is.na(qobj$Tag))

  qobj$Title <- "Haha"
  expect_equal(qobj$Title, "Haha")
  expect_called(webChannel$exec, 1)
  expect_equal(mock_args(webChannel$exec)[[1]][[1]], list(
    type = QWebChannelMessageTypes["setProperty"],
    object = "WizDocument",
    property = 0,
    value = "Haha"
  ))

  qobj$GUID <- "882fb189-6390-420e-8a30-be44fc24a577"
  expect_equal(qobj$Title, "Haha")
  expect_called(webChannel$exec, 2)
  expect_equal(mock_args(webChannel$exec)[[2]][[1]], list(
    type = QWebChannelMessageTypes["setProperty"],
    object = "WizDocument",
    property = 1,
    value = "882fb189-6390-420e-8a30-be44fc24a577"
  ))

  obj_to_send <- QObject$new("ObjToSend", list(), webChannel)
  qobj$Database <- obj_to_send
  expect_called(webChannel$exec, 3)
  expect_equal(mock_args(webChannel$exec)[[3]][[1]], list(
    type = QWebChannelMessageTypes["setProperty"],
    object = "WizDocument",
    property = 2,
    value = list(id = "ObjToSend")
  ))

  # expect warings
  expect_warning(qobj$Title <- NULL, "called with null value!")
  expect_equal(qobj$Title, "Haha")
})


test_that("Test addSignal", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 4], "Hello World"],
    [1, "GUID", [1, 5], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "signals": [
    ["tagCreated", 0],
    ["tagModified", 1],
    ["styleCreated", 2],
    ["documentCreated", 3],
    ["destroyed", 6]
  ]
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)

  expect_true(is.list(qobj$TitleChanged))
  expect_true(is.list(qobj$GUIDChanged))
  expect_true(is.list(qobj$tagCreated))
  expect_true(is.list(qobj$tagModified))
  expect_true(is.list(qobj$styleCreated))
  expect_true(is.list(qobj$documentCreated))

  connect_callback <- mockery::mock()

  qobj$tagCreated$connect(connect_callback)
  expect_called(webChannel$exec, 1)
  expect_equal(mock_args(webChannel$exec)[[1]][[1]], list(
    type = QWebChannelMessageTypes["connectToSignal"],
    object = "Database",
    signal = 0
  ))

  qobj$tagModified$connect(connect_callback)
  expect_called(webChannel$exec, 2)
  expect_equal(mock_args(webChannel$exec)[[2]][[1]], list(
    type = QWebChannelMessageTypes["connectToSignal"],
    object = "Database",
    signal = 1
  ))

  qobj$styleCreated$connect(connect_callback)
  expect_called(webChannel$exec, 3)
  expect_equal(mock_args(webChannel$exec)[[3]][[1]], list(
    type = QWebChannelMessageTypes["connectToSignal"],
    object = "Database",
    signal = 2
  ))

  qobj$documentCreated$connect(connect_callback)
  expect_called(webChannel$exec, 4)
  expect_equal(mock_args(webChannel$exec)[[4]][[1]], list(
    type = QWebChannelMessageTypes["connectToSignal"],
    object = "Database",
    signal = 3
  ))

  qobj$TitleChanged$connect(connect_callback)
  expect_called(webChannel$exec, 4)
  qobj$GUIDChanged$connect(connect_callback)
  expect_called(webChannel$exec, 4)
  qobj$destroyed$connect(connect_callback)
  expect_called(webChannel$exec, 4)

  expect_error(
    qobj$documentCreated$connect(list()),
    "Bad callback given to connect to signal"
  )

  disconnect_callback <- mockery::mock()
  disconnect_callback2 <- mockery::mock()

  qobj$tagCreated$disconnect(connect_callback)
  expect_called(webChannel$exec, 5)
  expect_equal(mock_args(webChannel$exec)[[5]][[1]], list(
    type = QWebChannelMessageTypes["disconnectFromSignal"],
    object = "Database",
    signal = 0
  ))

  qobj$tagCreated$connect(disconnect_callback)
  expect_called(webChannel$exec, 6)
  qobj$tagCreated$disconnect(disconnect_callback)
  expect_called(webChannel$exec, 7)
  expect_equal(mock_args(webChannel$exec)[[7]][[1]], list(
    type = QWebChannelMessageTypes["disconnectFromSignal"],
    object = "Database",
    signal = 0
  ))

  qobj$tagCreated$connect(disconnect_callback)
  qobj$tagCreated$connect(disconnect_callback2)
  expect_called(webChannel$exec, 9)
  qobj$tagCreated$disconnect(disconnect_callback)
  expect_called(webChannel$exec, 9)
  qobj$tagCreated$disconnect(disconnect_callback2)
  expect_called(webChannel$exec, 10)
  expect_equal(mock_args(webChannel$exec)[[10]][[1]], list(
    type = QWebChannelMessageTypes["disconnectFromSignal"],
    object = "Database",
    signal = 0
  ))

  qobj$TitleChanged$disconnect(connect_callback)
  expect_called(webChannel$exec, 10)

  expect_error(
    qobj$documentCreated$disconnect(list()),
    "Bad callback given to disconnect to signal"
  )

  expect_error(
    qobj$documentCreated$disconnect(disconnect_callback),
    "Cannot find connection of signal"
  )
})


test_that("Test invokeSignalCallbacks", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]

  ]
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)

  obj_private <- environment(qobj$unwrapQObject)$private

  expect_error(
    obj_private$invokeSignalCallbacks(1 + 1, c("A", "B", "C")),
    "Arguments are not wrapped as a list for signal tagCreated"
  )

  func1 <- mockery::mock()
  func2 <- mockery::mock()
  func3 <- mockery::mock()
  qobj$tagCreated$connect(func1)
  qobj$tagCreated$connect(func2)
  qobj$tagCreated$connect(func3)

  obj_private$invokeSignalCallbacks(1 + 1, list(tag_data = list("A", "B", "C")))
  expect_args(func1, 1, list(tag_data = list("A", "B", "C")))
  expect_args(func2, 1, list(tag_data = list("A", "B", "C")))
  expect_args(func3, 1, list(tag_data = list("A", "B", "C")))

  obj_private$invokeSignalCallbacks(1 + 1, list(tag_data = list("Hello World")))
  expect_args(func1, 2, list(tag_data = list("Hello World")))
  expect_args(func2, 2, list(tag_data = list("Hello World")))
  expect_args(func3, 2, list(tag_data = list("Hello World")))

  qobj$tagCreated$disconnect(func2)
  qobj$tagCreated$disconnect(func3)
  obj_private$invokeSignalCallbacks(1 + 1, list(tag_data = list("Hello World")))
  expect_called(func1, 3)
  expect_called(func2, 2)
  expect_called(func3, 2)

  qobj$tagCreated$disconnect(func1)
  obj_private$invokeSignalCallbacks(1 + 1, list(tag_data = list("Hello World")))
  expect_called(func1, 3)

  qobj$tagCreated$connect(func1)
  obj_private$invokeSignalCallbacks(1 + 1, list(list("Hello World")))
  expect_args(func1, 4, list("Hello World"))
})


test_that("Test propertyUpdate", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]

  ]
}', simplify = FALSE)

  # Notify signal for `Title` property is automatically generated
  #   and it's notify signal index is 5
  resp_data = rjson::fromJSON('
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
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  func2 <- mockery::mock()
  qobj$TitleChanged$connect(func1)
  qobj$GUIDChanged$connect(func2)

  qobj$propertyUpdate(resp_data[["signals"]], resp_data[["properties"]])

  # Check updated property
  expect_equal(qobj$Title, "New Title")
  expect_equal(qobj$GUID, "bd5c726c-120a-4d61-a4e9-d82fd07821a3")

  # Check signal callback
  expect_called(func1, 1)
  expect_equal(mock_args(func1)[[1]][[1]], "New Title")
  expect_called(func2, 1)
  expect_equal(mock_args(func2)[[1]][[1]], "bd5c726c-120a-4d61-a4e9-d82fd07821a3")
})


test_that("Test unwrapQObject", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]

  ]
}', simplify = FALSE)

  # These objects are usually unnamed and unregistered.
  tmp_data = rjson::fromJSON(
'{
  "__QObject*__": true,
  "data":{
    "methods":[
      ["deleteLater", 3],
      ["deleteLater()", 3],
      ["CurrentDocument", 5],
      ["CurrentDocument()", 5]
    ],
    "properties":[
      [0, "objectName", [1, 2], ""],
      [1, "Location", [1, 3], "US"]
    ],
    "signals":[
      ["destroyed", 0],
      ["destroyed(QObject*)", 0],
      ["destroyed()", 1]
    ]
  },
  "id":"{b85f0bb9-0a9e-44bd-9d16-9fcfa84d6fa6}"
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)

  # Unknown data
  wrong_data1 = list(1, 2, 3, 4)
  expect_null(qobj$unwrapQObject(NULL))
  expect_equal(qobj$unwrapQObject(wrong_data1), wrong_data1)
  wrong_data2 = list(A = 1, B = 2, C = 3, D = 4)
  expect_equal(qobj$unwrapQObject(wrong_data2), wrong_data2)
  wrong_data3 = list(id = "{6b6dff54-4de8-4183-b1cf-37f8e1209790}")
  expect_equal(qobj$unwrapQObject(wrong_data3), wrong_data3)

  # Simple wrong data
  expect_equal(qobj$unwrapQObject("Hello World"), "Hello World")
  expect_equal(qobj$unwrapQObject(124), 124)
  expect_equal(qobj$unwrapQObject(TRUE), TRUE)

  # Single QObject
  new_obj <- qobj$unwrapQObject(tmp_data)
  expect_equal(new_obj$Location, "US")
  expect_true(is.function(new_obj$CurrentDocument))
  # Object already unwrapped
  expect_equal(
    qobj$unwrapQObject(
      list(
        id = "{b85f0bb9-0a9e-44bd-9d16-9fcfa84d6fa6}",
        `__QObject*__` = TRUE
      )
    ),
    new_obj
  )
  expect_message(
    qobj$unwrapQObject(
      list(
        id = "{e0a9f0ee-7930-4943-9084-81c83eda54cc}",
        `__QObject*__` = TRUE
      )
    ),
    "Cannot unwrap unknown QObject"
  )

  # Test destroyed signal callback
  obj_private <- environment(new_obj$unwrapQObject)$private
  expect_equal(length(obj_private$objectSignals[[1]]), 1)
  tmp_data4 <- tmp_data
  tmp_data4[["id"]] <- "{bc30d9ee-213c-40e1-8a12-6ae7408c682e}"
  new_obj2 <- qobj$unwrapQObject(tmp_data4)
  obj_private <- environment(new_obj2$unwrapQObject)$private
  expect_equal(length(obj_private$objectSignals[[1]]), 1)
  obj_private$objectSignals[[1]][[1]]()
  expect_null(new_obj2$id)
  expect_null(new_obj2$objectName)
  expect_null(new_obj2$Location)
  expect_null(new_obj2$destroyed)
  expect_null(new_obj2$deleteLater)
  expect_null(new_obj2$CurrentDocument)
  expect_null(obj_private$webChannel)
  expect_null(obj_private$objectSignals)
  expect_null(obj_private$objectSignalData)
  expect_null(obj_private$propertyCache)

  # list of QObject
  tmp_data2 <- tmp_data
  tmp_data2[["id"]] <- "{b1fe966e-89b2-4727-b032-b33fae0453e6}"
  tmp_data3 <- tmp_data
  tmp_data3[["id"]] <- "{1fa16570-2c41-459a-88cd-1380f91bc196}"
  obj_data_list <- list(tmp_data, tmp_data2, tmp_data3)
  obj_list <- qobj$unwrapQObject(obj_data_list)
  expect_equal(length(obj_list), 3)
  expect_equal(obj_list[[1]]$id, "{b85f0bb9-0a9e-44bd-9d16-9fcfa84d6fa6}")
  expect_equal(obj_list[[2]]$id, "{b1fe966e-89b2-4727-b032-b33fae0453e6}")
  expect_equal(obj_list[[3]]$id, "{1fa16570-2c41-459a-88cd-1380f91bc196}")
})


test_that("Test unwrapProperties", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"],
    [2, "RootFolder", [1, 7],
      {
        "__QObject*__": true,
        "data":{
          "properties":[
            [0, "objectName", [1, 3], ""],
            [1, "Name", [1, 4], "My Notes"]
          ],
          "signals":[
            ["destroyed", 0],
            ["destroyed(QObject*)", 0],
            ["destroyed()", 1],
            ["htmlModified", 2]
          ]
        },
        "id":"{b85f0bb9-0a9e-44bd-9d16-9fcfa84d6fa6}"
      }
    ]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]
  ]
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)
  qobj$unwrapProperties()

  expect_equal(qobj$Title, "Hello World")
  expect_equal(qobj$GUID, "7bb5a78d-2f21-44ad-ad50-2f7e52437133")

  expect_equal(qobj$RootFolder$Name, "My Notes")
  expect_true(is.function(qobj$RootFolder$htmlModified$connect))

})

test_that("Test signalEmitted", {
  obj_data = rjson::fromJSON('
{
  "properties": [
    [0, "Title", [1, 5], "Hello World"],
    [1, "GUID", [1, 6], "7bb5a78d-2f21-44ad-ad50-2f7e52437133"]
  ],
  "signals": [
    ["destroyed", 0],
    ["tagCreated", 1],
    ["tagModified", 2],
    ["styleCreated", 3],
    ["documentCreated", 4]
  ]
}', simplify = FALSE)

  signal_data = rjson::fromJSON('
{
  "args": [
    "hello world"
  ],
  "object": "Database",
  "signal": 2,
  "type": 1
}', simplify = FALSE)

  webChannel <- FakeQWebChannel$new()
  webChannel$exec <- mockery::mock()

  qobj <- QObject$new("Database", obj_data, webChannel)

  func1 <- mockery::mock()
  qobj$tagModified$connect(func1)

  qobj$signalEmitted(signal_data$signal, signal_data$args)
  expect_args(func1, 1, "hello world")

  qobj$signalEmitted(signal_data$signal, list(A = "a"))
  expect_args(func1, 2, list(A = "a"))
})
