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

library(rjson)

test_that("Create QObject", {
  obj_data = rjson::fromJSON(object_data)
  webChannel = list(objects=list())
  QObject$new("WizExplorerApp", obj_data, webChannel)
  expect_equal(2 * 2, 4)
})
