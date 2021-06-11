#' Outs to make a Three of a Kind
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Three of a Kind or a vector of cards that complete a Three of a Kind.
#' If a data.frame is input, the output is the same length as number of rows.
#' @export
outs_tk <- function(cards, stage, output = "counts") {
  # Count duplicate cards
  bests <- help_make_duplicates(cards)

  # Count up the outs
  tk_outs <- if_else(
    # A Pair that isn't a two pair
    bests$count1 == 2 & bests$count2 < 2, 2L, if_else(
      # No Pairs on the flop
      stage == "flop" & bests$count1 == 1, 15L,
      # Two Pair or better become better hands than Three of a Kind, or not possible
      0L
    ))

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(tk_outs)
}
