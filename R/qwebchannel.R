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
      data = msg$data
      if (typeof(data) == "character") {
        data = rjson::fromJSON(data, simplify = FALSE)
      }
      # Dispath to different message handler
      if (data$type == QWebChannelMessageTypes['signal'])
        {
          message("'signal' message received: ", data)
        }
      else
      if (data$type == QWebChannelMessageTypes['response'])
        {
          self$handleResponse(data)
        }
      else
      if (data$type == QWebChannelMessageTypes['propertyUpdate'])
        {
          message("'propertyUpdate' message received: ", data)
        }
      else
        {
          warning("invalid message received: ", data)
        }
    })

    # Initialize QWebChannel and build connection to C++ side
    self$exec(list(type = QWebChannelMessageTypes[['init']]), function(data) {

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
    self$execCallbacks[[data[['id']]]] = callback
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
      type = QWebChannelMessageTypes['debug'],
      data = message
    ));
  },

  #' Handle response type message
  #'
  #' @param message Message to handle
  handleResponse = function(message) {
    if (is.null(message[['id']])) {
      warning("Invalid response message received: ", rjson::toJSON(message))
      return()
    }

    self$execCallbacks[[ message[['id']] ]](message[['data']])
    self$execCallbacks[[ message[['id']] ]] <- NULL
  }
))
