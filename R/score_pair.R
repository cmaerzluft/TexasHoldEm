#' Score Pair
#'
#' @param pair The value of the card that makes up the pair
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_pair <- function(pair_card, values) {
  # Turn pair cards to impossible values
  values[values == matrix(rep.int(pair_card, 7), ncol = 7)] <- 0

  # Sort cards so highest value is on the left and lowest value is on the right
  values <- matrix(values[order(row(values), -values, method = "radix")], ncol = ncol(values), byrow = TRUE)[, 1:3]

  # Add hand rank value to value of cards
  score <- 2*10^10 + pair_card*10^8 + values %*% 10^(seq(from = 6, to = 2, by = -2))

  return(score)
}
