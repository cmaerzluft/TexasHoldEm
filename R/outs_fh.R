#' Outs to make a Full House
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Full House or a vector of cards that complete a Full House. If a
#' data.frame is input, the output is the same length as number of rows.
#' @export
outs_fh <- function(cards, stage, output = "counts") {
  # Count duplicate cards
  bests <- help_make_duplicates(cards)

  # Count up the outs
  if (stage == "turn") {
    # At the turn, need at least two pair
    fh_outs <- if_else(
      # Three of a Kind
      bests$count1 == 3 & bests$count2 < 2, 9L, if_else(
        # Two Pair (4) or Three Pairs (6)
        bests$count1 == 2 & bests$count2 == 2, 4L + (bests$count3 == 2)*2L,
        # Full House or not possible
        0L
      ))
  } else {
    # At the flop, need at least one pair
    fh_outs <- if_else(
      # Three of a Kind
      bests$count1 == 3 & bests$count2 < 2, 46L, if_else(
        # Two Pair (7) or Pair (11)
        bests$count1 == 2, 11L - as.integer(bests$count2 == 2)*4L,
        # Full House or not possible
        0L
      ))
  }

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(fh_outs)
}
