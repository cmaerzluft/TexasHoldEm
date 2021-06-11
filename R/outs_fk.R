#' Outs to make a Four of a Kind
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Four of a Kind or a vector of cards that complete a Four of a Kind. If
#' a data.frame is input, the output is the same length as number of rows.
#' @export
outs_fk <- function(cards, stage, output = "counts") {
  # Count duplicate cards
  bests <- help_make_duplicates(cards)

  # Count up the outs
  if (stage == "turn") {
    # At the turn, need at least one three of a kind
    fk_outs <- if_else(
      # Three of a Kind (1), Full House (1), and 2 Three of a Kinds (1 + 1)
      bests$count1 == 3, 1L + as.integer(bests$count2 == 3),
      # Four of a Kind, or not possible
      0L
    )
  } else {
    # At the flop, need at least one pair
    fk_outs <- if_else(
      # Pair (2), Two Pair (2 + 2), Three of a Kind (1), Full House (1 + 2), and Four of a Kind (0)
      bests$count1 >= 2, as.integer(4 - bests$count1) + (bests$count2 == 2)*2L,
        # Not possible
        0L
      )

  }

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(fk_outs)
}
