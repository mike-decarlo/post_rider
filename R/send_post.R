#' @title Send Post
#'
#' @description Send a message to a Slack channel using R and webhooks.
#' @param .letter a string, default: \code{"Hello world!"}; should be enclosed
#'   in quotes and contain the user's message
#' @param .rider a string, default: \code{NULL}; should be enclosed in quotes.
#'   If left as NULL will fill in as \code{Sys.info()[["user"]]}
#' @param .channel a string, default: \code{NULL}; should be enclosed in quotes
#'   and contain the name of a valid environment variable containing a channel
#'   webhook string the user/.rider has access to
#' @param .webhook a string, default: \code{NULL}; recommend storing webhook
#'   as a user environment variable or some other secure manner and extracting
#'   as a character string to this argument
#' @keywords slack message send post
#' @examples
#' send_post(.letter = "foo", .channel = "bar")
#' @importFrom stringr str_c
#' @importFrom jsonlite validate
#' @importFrom httr POST
#' @importFrom RCurl curlUnescape
#' @export
send_post <- function(.letter = "Hello world!", .rider = NULL
                 , .channel = NULL, .webhook = NULL) {
  .post <- stringr::str_c(
    '{"text":"'
    , if (is.null(.rider)) {
      Sys.info()[["user"]]
    } else {
      .rider
    }
    , ": "
    , .letter
    , '"}'
  )
  if (!jsonlite::validate(.post)) {
    stop("Not valid JSON text.")
  } else {
    .post <- httr::POST(
      url = if (!is.null(.webhook)) {
        .webhook
      } else if (!is.null(.channel)) {
        Sys.getenv(.channel)
      } else {
        stop("A channel environment variable or webhook must be supplied.")
      }
      , body = RCurl::curlUnescape(.post)
    )
    .post
  }
}
