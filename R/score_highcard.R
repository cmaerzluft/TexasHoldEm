#' Score High Card
#'
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_highcard <- function(values) {
  # Sort cards so highest value is on the left and lowest value is on the right
  values <- matrix(values[order(row(values), -values, method = "radix")], ncol = ncol(values), byrow = TRUE)[, 1:5]

  # Add hand rank value to value of cards
  score <- 1*10^10 + values %*% 10^(seq(from = 8, to = 0, by = -2))

  return(score)
}
