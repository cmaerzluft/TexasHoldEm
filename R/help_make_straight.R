#' Help Make Straight
#'
#' @param sorted_cards Cards that help make a straight
#'
#' @return The index of the cards that are best given the size of the window
#' @export
help_make_straight <- function(sorted_cards, stage, k) {
  # Initialize useful information
  make_straight <- c()
  needs <- 5 - k

  # Find cards that fill gaps or add to end
  for (i2 in 1:((length(sorted_cards) + 1) - k)) {
    iter_cards <- i2:(i2 + k - 1)
    missing_cards <- sum(diff(sorted_cards[iter_cards]) - 1)
    if (missing_cards <= (5 - k)) {
      low_end <- max(1, min(sorted_cards[iter_cards]) - needs, max(sorted_cards[iter_cards]) - 4)
      upp_end <- min(min(sorted_cards[iter_cards]) + 4, max(sorted_cards[iter_cards]) + needs, 14)
      helps <- setdiff(low_end:upp_end, sorted_cards)
      make_straight <- c(make_straight, helps)
    }
  }

  # Split cards into different groups of potential straights (only necessary when Ace can be used two different ways
  #   after the flop)
  my_straight <- sort(c(sorted_cards, unique(make_straight)))
  straights <- cumsum(c(1, diff(my_straight) != 1))
  chk <- rle(straights)
  consec_init <- split(my_straight, straights)

  # Check if the card would actually be useful in making a straight (specifically worried about cards that would be
  #   useless to making a straight even if you got it e.g. if you have a flop with 6, 8, 9, 10, 12, getting a 5 won't
  #   help you because you still need a 7 and if you got that 7 your straight would be 6-10 regardless of whether you
  #   get the 5 or not)
  tmp_hand <- do.call(c, lapply(consec_init, function(x, og_cards) {
    check_cards <- x[!x %in% og_cards & x < max(x) - 4]
    if (length(check_cards) > 0) {
      can_use <- sapply(check_cards, function(y, x, og_cards) {
        if (stage != "turn") {
          !(find_straight_top(matrix(x[x < (y + 5)], nrow = 1)) + 1) %in% og_cards ||
            !(find_straight_top(matrix(x[x < min(og_cards, y) + 5], nrow = 1)) + 1) %in% og_cards
        } else {
          is.na(find_straight_top(matrix(x[x != y & x %in% og_cards], nrow = 1)))
        }
      }, x = x, og_cards = og_cards)
      remove_cards <- check_cards[!can_use]
      if (length(remove_cards) > 0) { x <- x[!x %in% remove_cards] }
    }
    return(x)
  }, og_cards = sorted_cards))

  # Reduce cards to ones we don't have
  make_straight <- intersect(tmp_hand, make_straight)

  # Return Results
  return(make_straight)
}
