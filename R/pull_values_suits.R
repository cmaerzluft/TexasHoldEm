#' Pull Values
#'
#' @param x A card of the form ### where the one hundreds digit represents the suit and the tens and ones digits
#'          represent the cards value
#'
#' @note Since it depends on how the card deck is defined, this function won't be exported
#'
#' @return x %% 100 which gives the tens and ones digits, i.e. the card value
#' @export
pull_value <- function(x) {
  UseMethod("pull_value")
}
#' @export
pull_value.default <- function(x) { as.integer(x %% 100) }
#' @export
pull_value.data.frame <- function(x) { vapply(x, FUN = pull_value, FUN.VALUE = rep.int(0L, nrow(x))) }
#' @export
pull_value.tbl_df <- function(x) {
  x %>%
    mutate(
      across(.fns = pull_value)
    )
}
#' @export
pull_value.data.table <- function(x) {
  y <- copy(x)
  cols <- colnames(y)
  y[ , (cols) := lapply(.SD, pull_value), .SDcols = cols]
}

#' Pull Suits
#'
#' @param x A card of the form ### where the one hundreds digit represents the suit and the tens and ones digits
#'          represent the cards value
#'
#' @note Since it depends on how the card deck is defined, this function won't be exported
#'
#' @return x %/% 100 which gives the 100 hundreds digit value, i.e. the suit
#' @export
pull_suits <- function(x) {
  UseMethod("pull_suits")
}
#' @export
pull_suits.default <- function(x) { as.integer(x %/% 100) }
#' @export
pull_suits.data.frame <- function(x) { vapply(x, FUN = pull_suits, FUN.VALUE = rep.int(0L, nrow(x))) }
#' @export
pull_suits.tbl_df <- function(x) {
  x %>%
    mutate(
      across(.fns = pull_suits)
    )
}
#' @export
pull_suits.data.table <- function(x) {
  y <- copy(x)
  cols <- colnames(y)
  y[ , (cols) := lapply(.SD, pull_suits), .SDcols = cols]
}

