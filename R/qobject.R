#' QObject is used to represent C++ Qt objects
#'
#' @export
QObject <- R6::R6Class("QObject",
  private = list(
    #' @field id The name of object.
    id = NULL,
    webChannel = NULL,

    #' @field objectSignals List of callbacks that get invoked upon signal emission
    objectSignals = list(),

    #' @field propertyCache Cache of all properties, updated when a notify signal is emitted
    propertyCache = list(),

    addSignal = function(signalData, isPropertyNotifySignal) {

    },

    invokeSignalCallbacks = function(signalName, signalArgs) {

    },

    addMethod = function(methodData) {
      methodName <-  methodData[[0]]
      methodIdx <-  methodData[[1]]
      self$set("public", methodName, function(...) {
        args_to_send <- list()
        arguments <- list(...)
        callback <- NULL
        for (argument in arguments) {
          if (class(argument) == "function") {
            callback <- argument
          } else if (
            "QObject" %in% class(argument) &&
            !is.null(private$webChannel$objects[[argument[["__id__"]]]])) {
            args_to_send <- append(args_to_send, list(id = argument[["__id__"]]))
          } else {
            args_to_send <- append(args_to_send, argument)
          }
        }
        private$webChannel$exec(list(
          type = QWebChannelMessageTypes["invokeMethod"],
          object = private$id,
          method = methodIdx,
          args = args_to_send
        ), function(response) {
          if (!is.null(response)) {
            result <- self$unwrapQObject(response);
            if (!is.null(callback)) {
              callback(result);
            }
          }
        })
      })
    },

    bindGetterSetter = function(propertyInfo) {

    }

  ),
  public = list(
    initialize = function(name, data, webChannel) {
      private$id <- name
      private$webChannel <- webChannel
      webChannel$objects[[name]] = self

      for (m in data[["methods"]]) {
        private$addMethod(m)
      }

      for (p in data[["properties"]]) {
        private$bindGetterSetter(p)
      }

      for (s in data[["signals"]]) {
        private$addSignal(s, FALSE);
      }

      for (name in data[["enums"]]) {
        self[[name]] = data[["enums"]][[name]];
      }
    },

    unwrapQObject = function(response = NULL) {
      # support list of objects

      if (is.null(response) || is.null(response[["__QObject*__"]])
            || is.null(response[["id"]])) {
        return(response)
      }

      objectId = response[["id"]];
      if (!is.null(private$webChannel$objects[[objectId]]))
          return(private$webChannel$objects[[objectId]])

      if (is.null(response$data)) {
          message(paste0("Cannot unwrap unknown QObject ", objectId, " without data."))
          return()
      }


    },

    unwrapProperties = function() {

    },

    propertyUpdate = function(signals, propertyMap) {

    },

    signalEmitted = function(signalName, signalArgs) {

    }
  )
)
