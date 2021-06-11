#' Score Straight
#'
#' @param straight_top The value of the high end of the straight
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_straight <- function(top_card) {
  # Add hand rank value to top card value and kicker value
  #   (rest of cards don't add any additional value for scoring purposes)
  score <- 5*10^10 + top_card*10^8

  return(score)
}
