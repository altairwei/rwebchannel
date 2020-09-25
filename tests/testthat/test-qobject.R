library(rjson)
library(mockery)
library(testthat)

FakeQWebChannel <-
  R6::R6Class(
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
}')

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
}')

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
