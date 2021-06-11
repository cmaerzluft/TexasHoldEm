#' Outs to make a Two Pair
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Two Pair or a vector of cards that complete a Two Pair. If a data.frame
#' is input, the output is the same length as number of rows.
#' @export
outs_tp <- function(cards, stage, output = "counts") {
  # Count duplicate cards
  bests <- help_make_duplicates(cards)

  # Count up the outs
  if (stage == "turn") {
    tp_outs <- if_else(
      # A Two Pair (3 * number of cards on board that are better than worst pair)
      bests$count1 == 2 & bests$count2 == 2 & bests$count3 < 2,
      ((bests$card3 > bests$card2) + (bests$card4 > bests$card2))*3L, if_else(
        # A Pair (3* all remaining non-paired cards)
        bests$count1 == 2 & bests$count2 < 2, 4L*3L,
        # All other hands are better or not possible
        0L
      ))
  } else {
    tp_outs <- if_else(
      # A Two Pair ((14 - small pair - 1)*4 - (kicker > small pair))
      bests$count1 == 2 & bests$count2 == 2,
      as.integer(14 - bests$card2 - 1)*4L - (bests$card3 > bests$card2)*1L, if_else(
        # A Pair ((all remaining non-paired cards) - the 3 individual cards we already have)
        bests$count1 == 2 & bests$count2 < 2, 12L*4L - 3L, if_else(
          # A High Card
          bests$count1 == 1, 52L - 5L,
          # Two Pair or better become better hands than Three of a Kind, or not possible
          0L
        )))
  }

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(tp_outs)
}
