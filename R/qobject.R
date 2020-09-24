#' QObject is used to represent C++ Qt objects
#'
#' @export
QObject <- R6::R6Class("QObject", lock_objects = FALSE,
  private = list(
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
      methodName <-  methodData[[1L]]
      methodIdx <-  methodData[[2L]]
      self[[methodName]] <- function(...) {
        args_to_send <- list()
        arguments <- list(...)
        callback <- NULL
        for (argument in arguments) {
          if (is.function(argument)) {
            callback <- argument
          } else if (
            "QObject" %in% class(argument) &&
            !is.null(private$webChannel$objects[[argument$id]])) {
            args_to_send[[length(args_to_send) + 1]] <- list(id = argument$id)
          } else {
            args_to_send[[length(args_to_send) + 1]] <- argument
          }
        }
        private$webChannel$exec(list(
          type = QWebChannelMessageTypes["invokeMethod"],
          object = self$id,
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
      }
    },

    bindGetterSetter = function(propertyInfo) {

    }

  ),
  public = list(
    #' @field id The name of object.
    id = NULL,

    initialize = function(name, data, webChannel) {
      self$id <- name
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
