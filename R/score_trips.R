#' Score Three of a kind
#'
#' @param three The value the cards that makes up the three of a kind
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_trips <- function(trip_card, values) {
  # Remove three-of-a-kinds from hands to get top kicker
  values[values == matrix(rep.int(trip_card, 7), ncol = 7)] <- 0
  kicker1 <- rowMaxs(values)

  # Remove top kicker to get second kicker
  values[values == matrix(rep.int(kicker1, 7), ncol = 7)] <- 0
  kicker2 <- rowMaxs(values)

  # Add hand rank value to three-of-a-kind card value and kicker values
  score <- 4*10^10 + trip_card*10^8 + kicker1*10^6 + kicker2*10^4

  return(score)
}
