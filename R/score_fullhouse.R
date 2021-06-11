#' Score Full House
#'
#' @param three the card value that makes up the three of a kind within a full house
#' @param two the card value that makes up the pair within a full house
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_fullhouse <- function(trip_card, pair_card) {
  # Add hand rank value to three-of-a-kind card value and pair card value
  score <- 7*10^10 + trip_card*10^8 + pair_card*10^6

  return(score)
}
