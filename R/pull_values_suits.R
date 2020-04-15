#' Pull Values
#'
#' @param x A card of the form ### where the one hundreds digit represents the suit and the tens and ones digits
#'          represent the cards value
#'
#' @return x %% 100 which gives the tens and ones digits, i.e. the card value
#' @export
pull_value <- Vectorize(function(x) x %% 100, "x")

#' Pull Suits
#'
#' @param x A card of the form ### where the one hundreds digit represents the suit and the tens and ones digits
#'          represent the cards value
#'
#' @return x %/% 100 which gives the 100 hundreds digit value, i.e. the suit
#' @export
pull_suits <- Vectorize(function(x) x %/% 100, "x")
