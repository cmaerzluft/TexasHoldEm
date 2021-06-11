#' Outs to make a Straight Flush
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Straight Flush or a vector of cards that complete a Straight Flush. If
#' a data.frame is input, the output is the same length as number of rows.
#' @export
outs_sf <- function(cards, stage, output = "counts") {
  # Info needed for calculation
  must_have_n <- if_else(stage == "flop", 3, 4)
  cards_left <- 5 - must_have_n
  suits <- pull_suits(cards)
  value <- pull_value(cards)

  # Handle only one hand being passed
  if (is.null(dim(value))) {
    iters <- 1
  } else {
    iters <- nrow(value)
  }

  # Initialize outputs
  sf_outs <- rep.int(0, times = iters)
  sf_suit <- rep.int(0, times = iters)

  # Iterate over all hands
  for (i1 in seq_along(sf_outs)) {
    # Handle one hand vs a lot of hands
    if (iters == 1) {
      freq <- sort(table(as.numeric(suits)), decreasing = TRUE)
    } else {
      freq <- sort(table(as.numeric(suits[i1, ])), decreasing = TRUE)
    }
    # Find hands with enough cards of same suit that a straight flush is possible given cards left to be dealt
    sf_suit[i1] <- if_else(max(freq) >= must_have_n, as.integer(names(freq[1])), NA_integer_)

    # If enough similarly suited cards exist, check for whether they can be used for a straight
    if (!is.na(sf_suit[i1])) {
      # Straight Flushes have same outs as a Straight but instead of 4 suits only 1 suit works as an out
      # Handle one hand vs a lot of hands
      if (iters == 1) {
        sf_outs[i1] <- outs_st(cards = cards[suits == sf_suit[i1]], stage = stage)/4L
      } else {
        sf_outs[i1] <- outs_st(cards = cards[i1, suits[i1, ] == sf_suit[i1]], stage = stage)/4L
      }
    }
  }

  # Return Results
  # If we want to return more stuff, the suit and whether the draw is inside or outside can be un-commented in code
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(sf_outs)
}
