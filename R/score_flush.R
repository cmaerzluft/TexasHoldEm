#' Score Flush
#'
#' @param suit The suit that makes up the flush
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_flush <- function(suit_card, suits, values) {
  # Turn cards with the wrong suit to impossible values
  values[suits != matrix(rep.int(suit_card, 7), ncol = 7)] <- 0

  # Sort cards so highest value is on the left and lowest value is on the right
  values <- matrix(values[order(row(values), -values, method = "radix")], ncol = ncol(values), byrow = TRUE)[, 1:5]

  # Add hand rank value to value of cards
  score <- 6*10^10 + values %*% 10^(seq(from = 8, to = 0, by = -2))

  return(score)
}
