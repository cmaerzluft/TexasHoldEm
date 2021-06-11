#' Outs to make a Pair
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Pair or a vector of cards that complete a Pair. If a data.frame is
#' input, the output is the same length as number of rows.
#' @export
outs_pa <- function(cards, stage, output = "counts") {
  # Count duplicate cards
  bests <- help_make_duplicates(cards)

  # Count up the outs
  pa_outs <- if_else(
    # No Pair after the flop (can make a pair with any card not already on board)
    bests$count1 == 1 & stage == "flop", 47L, if_else(
      # No Pair after the turn (can only make pair with cards on board partners)
      bests$count1 == 1 & stage == "turn", 18L,
      # Already have a pair or greater
      0L
    ))

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(pa_outs)
}
