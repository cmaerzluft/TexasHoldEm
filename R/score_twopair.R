#' Score Two Pair
#'
#' @param twoone The value of the highest valued pair
#' @param twotwo The value of the lowest valued pair
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_twopair <- function(pair_cards, values) {
  # Remove pairs to get kicker
  values[values == matrix(rep.int(pair_cards[, 1], 7), ncol = 7)] <- 0
  values[values == matrix(rep.int(pair_cards[, 2], 7), ncol = 7)] <- 0
  kicker <- rowMaxs(values)

  # Add hand rank value to value of cards
  score <- 3*10^10 + pair_cards %*% 10^(c(8, 6)) + kicker*10^4

  return(score)
}
