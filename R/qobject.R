#' QObject is used to represent C++ Qt objects
#'
#' @export
QObject <- R6::R6Class("QObject", lock_objects = FALSE,
  private = list(
    webChannel = NULL,

    #' @field objectSignals List of callbacks that get invoked upon signal emission
    objectSignals = list(),
    objectSignalData = list(),

    #' @field propertyCache Cache of all properties, updated when a notify signal is emitted
    propertyCache = list(),

    addSignal = function(signalData, isPropertyNotifySignal) {
      signalName <- signalData[[1L]]
      signalIndex <- as.character(signalData[[2L]] + 1L)

      private$objectSignalData[[signalIndex]] <- signalData

      self[[signalName]] <- list(
        connect = function(callback) {
          if (!is.function(callback)) {
            stop(paste0("Bad callback given to connect to signal ", signalName))
          }

          if (! signalIndex %in% names(private$objectSignals)) {
            private$objectSignals[[signalIndex]] <- list()
          }
          private$objectSignals[[signalIndex]] <- append(private$objectSignals[[signalIndex]], callback)

          if (!isPropertyNotifySignal && signalName != "destroyed") {
            # only required for "pure" signals, handled separately for properties in propertyUpdate
            # also note that we always get notified about the destroyed signal
            private$webChannel$exec(list(
              type = QWebChannelMessageTypes[["connectToSignal"]],
              object = self$id,
              signal = as.integer(signalIndex) - 1L
            ))
          }
        },
        disconnect = function(callback) {
          if (!is.function(callback)) {
            stop(paste0("Bad callback given to disconnect to signal ", signalName))
          }

          if (! signalIndex %in% names(private$objectSignals)) {
            private$objectSignals[[signalIndex]] <- list()
          }

          idx <- index_of(private$objectSignals[[signalIndex]], callback)
          if (idx == -1) {
            stop(paste0("Cannot find connection of signal ", signalName))
          }
          private$objectSignals[[signalIndex]][[idx]] <- NULL
          if (!isPropertyNotifySignal && length(private$objectSignals[[signalIndex]]) == 0) {
            # only required for "pure" signals, handled separately for properties in propertyUpdate
            private$webChannel$exec(list(
              type = QWebChannelMessageTypes[["disconnectFromSignal"]],
              object = self$id,
              signal = as.integer(signalIndex) - 1L
            ))
          }
        }
      )
    },

    # Invokes all callbacks for the given signalname. Also works for property notify callbacks.
    invokeSignalCallbacks = function(signalName, signalArgs) {
      # FIXME: private$objectSignals 应该当作字典用，所以 signalName 应该是字符串才对！
      # JavaScript 中，Object 下标运算会自动将数字转换出字符串！
      signalName <- as.character(signalName)
      if (!is.list(signalArgs)) {
        stop(paste0("Arguments are not wrapped as a list for signal ", private$objectSignalData[[signalName]][[1L]]))
      }
      # signalName means signalIndex
      # TODO: 测试超出下标范围的信号
      connections <-  private$objectSignals[[signalName]]
      if (length(connections) != 0) {
        for (callback in connections) {
          #FIXME: In JavaScript the apply() method calls a function with a given this value, 
          # and arguments provided as an array (or an array-like object).
          if (is.list(signalArgs) && is.null(names(signalArgs))) {
            # Behave like an array
            do.call(callback, signalArgs)
          } else {
            # Just an ordinary object
            callback(signalArgs)
          }
          
        }
      }
    },

    addMethod = function(methodData) {
      methodName <- methodData[[1L]]
      methodIdx <- methodData[[2L]]
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
          type = QWebChannelMessageTypes[["invokeMethod"]],
          object = self$id,
          method = methodIdx,
          args = args_to_send
        ), function(response) {
          if (!is.null(response)) {
            result <- self$unwrapQObject(response)
            if (!is.null(callback)) {
              tryCatch(callback(result),
                error = function(e) {
                  cat(sprintf(
                    "Error occurred when handling callback of %s\n", methodName))
                  cat(.makeMessage(e))
                }
              )
            }
          }
        })
      }
    },

    bindGetterSetter = function(propertyInfo) {
      propertyIndex <- propertyInfo[[1L]] + 1L # 1-based indexing
      propertyName <- propertyInfo[[2L]]
      notifySignalData <- propertyInfo[[3L]]
      propertyInitValue <- propertyInfo[[4L]]

      if (is.null(propertyInitValue)) {
        warning(paste0("Undefined initial value for property \"", propertyName, "\" in object ", self$id))
        propertyInitValue <- NA
      }
      private$propertyCache[[propertyIndex]] <- propertyInitValue

      if (!is.null(notifySignalData) && length(notifySignalData) > 0) {
        if (notifySignalData[[1L]] == 1) {
          # signal name is optimized away, reconstruct the actual name
          notifySignalData[[1L]] <- paste0(propertyName, "Changed")
        }
        private$addSignal(notifySignalData, TRUE)
      }

      makeActiveBinding(propertyName, function(value) {
        if (missing(value)) {
          # Get property
          propertyValue <- private$propertyCache[[propertyIndex]]
          if (is.null(propertyValue)) {
            # This shouldn't happen
            warning(paste0("Undefined value in property cache for property \"", propertyName, "\" in object ", self$id))
          }

          return(propertyValue)

        } else {
          # Set property
          if (is.null(value)) {
            warning(paste0("Property setter for ", propertyName, " called with null value!"))
            return()
          }
          private$propertyCache[[propertyIndex]] <- value
          valueToSend <- value
          if ( "QObject" %in% class(valueToSend) && !is.null(private$webChannel$objects[[valueToSend$id]])) {
            valueToSend <- list(id = valueToSend$id)
          }
          private$webChannel$exec(list(
              type = QWebChannelMessageTypes[["setProperty"]],
              object = self$id,
              property = propertyIndex - 1L, # Convert to 0-based indexing
              value = valueToSend
          ))
        }
      }, env = self)
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

    unwrapQObject = function(response) {
      # support list of objects, unnamed list can be uesed as array
      if (is.list(response) && is.null(names(response))) {
        ret = list()
        for (r in response) {
          if (!is.list(r))
              return(response)
          ret[[length(ret) + 1]] <- self$unwrapQObject(r)
        }
        return(ret)
      }

      if (is.null(response) || !is.list(response)
            || is.null(response[["__QObject*__"]])
            || is.null(response[["id"]])) {
        return(response)
      }

      objectId = response[["id"]]
      if (!is.null(private$webChannel$objects[[objectId]]))
          return(private$webChannel$objects[[objectId]])

      if (is.null(response$data)) {
          message(paste0("Cannot unwrap unknown QObject ", objectId, " without data."))
          return()
      }

      qObject = QObject$new(objectId, response$data, private$webChannel)
      qObject$destroyed$connect(function() {
        if (identical(private$webChannel$objects[[objectId]], qObject)) {
          #browser()
          # 为什么下面这条语句会出错？
          private$webChannel$objects[[objectId]] <- NULL
          # reset the now deleted QObject to an empty object
          enclos_env <- environment(qObject$unwrapProperties)
          rm(list = ls(envir = enclos_env$private), envir = enclos_env$private)
          rm(list = ls(envir = enclos_env$self), envir = enclos_env$self)
          rm(list = ls(envir = enclos_env), envir = enclos_env)
        }
      })
      # here we are already initialized, and thus must directly unwrap the properties
      qObject$unwrapProperties()
      return(qObject)
    },

    unwrapProperties = function() {
      for (propertyIdx in seq_along(private$propertyCache)) {
        #browser()
        private$propertyCache[[propertyIdx]] <- self$unwrapQObject(private$propertyCache[[propertyIdx]])
      }
    },

    propertyUpdate = function(signals, propertyMap) {
      stopifnot(is.list(propertyMap))

      # update property cache
      for (propertyIndex in names(propertyMap)) {
        # The propertyIndex is in the form of strings as a key
        propertyValue <- propertyMap[[propertyIndex]]
        # Convert propertyIndex to 1-based indexing
        private$propertyCache[[strtoi(propertyIndex) + 1L]] <- propertyValue
      }

      for (signalName in names(signals)) {
        # Invoke all callbacks, as signalEmitted() does not. This ensures the
        # property cache is updated before the callbacks are invoked.

        # Convert signalName to 1-based indexing
        private$invokeSignalCallbacks(as.character(strtoi(signalName) + 1L), signals[[signalName]])
      }
    },

    signalEmitted = function(signalName, signalArgs) {
      private$invokeSignalCallbacks(as.character(strtoi(signalName) + 1L), self$unwrapQObject(signalArgs))
    }
  )
)
