#' Score Four of a Kind
#'
#' @param quad_value The card value of the four of a kind
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_quad <- function(quad_card, values) {
  # Remove four-of-a-kinds from hands to get kicker
  values[values == matrix(rep.int(quad_card, 7), ncol = 7)] <- 0
  kicker <- rowMaxs(values)

  # Add hand rank value to four-of-a-kind card value and kicker value
  score <- 8*10^10 + quad_card*10^8 + kicker*10^6

  return(score)
}
