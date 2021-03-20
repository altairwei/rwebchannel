QWebChannelMessageTypes = c(
  signal = 1L,
  propertyUpdate = 2L,
  init = 3L,
  idle = 4L,
  debug = 5L,
  invokeMethod = 6L,
  connectToSignal = 7L,
  disconnectFromSignal = 8L,
  setProperty = 9L,
  response = 10L
)


#' QWebChannel is used to communicate with QWebChannel C++ Client
#'
#' @export
QWebChannel <- R6::R6Class("QWebChannel", list(

  #' @field transport A R6 object used to transport message.
  transport = NULL,

  #' @field execCallbacks List of callbacks which should be executed after receiving data
  execCallbacks = list(),

  #' @field execId Identity number of QObject method excuting.
  execId = 1L,

  #' @field objects List of QObject which caching object data of C++ side.
  objects = list(),

  #' @description Create a new QWebChannel object
  #'
  #' @param transport A R6 object with \code{send} method
  #' @param initCallback Function to excute after building connection
  initialize = function(transport, initCallback = NULL) {
    # Make sure transport is a R6 object, and have a send method
    stopifnot("R6" %in% class(transport), is.function(transport$send))

    self$transport <- transport

    # Define message handler
    self$transport$onMessage(function(msg) {
      tryCatch(
        {
          data = msg$data
          if (is.character(data)) {
            data = rjson::fromJSON(data, simplify = FALSE)
          }
          # Dispath to different message handler
          if (data$type == QWebChannelMessageTypes[['signal']])
            {
              self$handleSignal(data)
            }
          else
          if (data$type == QWebChannelMessageTypes[['response']])
            {
              self$handleResponse(data)
            }
          else
          if (data$type == QWebChannelMessageTypes[['propertyUpdate']])
            {
              self$handlePropertyUpdate(data)
            }
          else
            {
              warning("invalid message received: ", data)
            }
        },
        error = function(e) {
          debugfile <- tempfile(fileext = ".json")
          cat(rjson::toJSON(data, indent=2), file = debugfile)
          cat(sprintf("Error occured when handling the message: %s ...\n", substr(msg$data, 1, 40)))
          cat(.makeMessage(e))
          cat(sprintf("Message data haved been saved to %s \n", debugfile))
        }
      )

    })

    # Initialize QWebChannel and build connection to C++ side
    self$exec(list(type = QWebChannelMessageTypes[['init']]), function(data) {
      for (objectName in names(data)) {
        object <- QObject$new(objectName, data[[objectName]], self)
      }

      # now unwrap properties, which might reference other registered objects
      for (objectName in names(self$objects)) {
        self$objects[[objectName]]$unwrapProperties()
      }

      if (!is.null(initCallback)) {
        initCallback(self)
      }

      self$exec(list(type = QWebChannelMessageTypes[['idle']]))
    })
  },

  #' @description Record and send message
  #'
  #' @param data Message data to send
  #' @param callback Function to excute after receiving returned information
  exec = function(data, callback = NULL) {
    if (is.null(callback)) {
      # if no callback is given, send directly
      self$send(data);
      return()
    }

    if (self$execId == .Machine$integer.max) {
      # Reset
      self$execId = 1L;
    }

    if (!is.null(data[['id']])) {
      warning(paste0("Cannot exec message with property id: ", rjson::toJSON(data)))
      return()
    }

    data[['id']] = self$execId
    self$execId <- self$execId + 1L
    self$execCallbacks[[ as.character(data$id) ]] = callback
    self$send(data)
  },

  #' @description Send data to C++ client
  #'
  #' @param data A string or object which can be serialized to JSON format
  send = function(data) {
    if (typeof(data) != "character") {
      data = rjson::toJSON(data)
    }
    self$transport$send(data)
  },

  #' Send debug message
  #'
  #' @param message Data to send.
  debug = function(message) {
    self$send(list(
      type = QWebChannelMessageTypes[["debug"]],
      data = message
    ));
  },

  handleSignal = function(message) {
    object <- self$objects[[message$object]]
    if (!is.null(object)) {
      object$signalEmitted(message$signal, message$args)
    } else {
      warning(paste0("Unhandled signal: ", message$object, "::", message$signal))
    }
  },

  #' Handle response type message
  #'
  #' @param message Message to handle
  handleResponse = function(message) {
    if (is.null(message[['id']])) {
      warning("Invalid response message received: ", rjson::toJSON(message))
      return()
    }

    self$execCallbacks[[ as.character(message$id) ]](message[['data']])
    self$execCallbacks[[ as.character(message$id) ]] <- NULL
  },

  handlePropertyUpdate = function(message) {
    for (data in message$data) {
      object <- self$objects[[data$object]]
      if (!is.null(object)) {
        object$propertyUpdate(data$signals, data$properties)
      } else {
        warning(paste0("Unhandled property update: ", data$object))
      }
    }
    self$exec(list(type = QWebChannelMessageTypes[["idle"]]))
  }
))
