#' Score Straight Flush
#'
#' @param high_card The top value of the straight for a straight flush
#' @param flush_suit The suit of the flush for a straight flush
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_straightflush <- function(top_card) {
  # Add hand rank value to top card value and kicker value
  #   (rest of cards don't add any additional value for scoring purposes)
  score <- 9*10^10 + top_card*10^8

  return(score)
}
