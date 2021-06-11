#' Outs to make a Flush
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Flush or a vector of cards that complete a Flush. If a data.frame is
#' input, the output is the same length as number of rows.
#' @export
outs_fl <- function(cards, stage, output = "counts") {
  # Info needed for calculation
  suits <- pull_suits(cards)
  value <- pull_value(cards)

  # Handle only one hand being passed
  if (is.null(dim(value))) {
    iters <- 1
  } else {
    iters <- nrow(value)
  }

  # Initialize outputs
  best_count <- rep.int(0, times = iters)
  one_st_out <- rep.int(0, times = iters)
  lowest_val <- rep.int(0, times = iters)
  is_straight <- rep.int(FALSE, times = iters)

  # Find top card for a flush
  for (i1 in seq_along(best_count)) {
    # Count cards in hand
    # Handle one hand vs a lot of hands
    if (iters == 1) {
      freq <- table(as.numeric(suits))
      grab_suit <- freq[which.max(freq)]
      grab_card <- suits == names(grab_suit)
      best_count[i1] <- grab_suit
      one_st_out[i1] <- as.integer(outs_st(value[grab_card], stage = stage)/4 == 1)
      lowest_val[i1] <- sort(value[grab_card], decreasing = TRUE)[min(sum(grab_card), 5)]
      is_straight[i1] <- !is.na(find_straight_top(value[grab_card]))
    } else {
      freq <- table(as.numeric(suits[i1, ]))
      grab_suit <- freq[which.max(freq)]
      grab_card <- suits[i1, ] == names(grab_suit)
      best_count[i1] <- grab_suit
      one_st_out[i1] <- as.integer(outs_st(value[i1, grab_card], stage = stage)/4)
      lowest_val[i1] <- sort(value[i1, grab_card], decreasing = TRUE)[min(sum(grab_card), 5)]
      is_straight[i1] <- !is.na(find_straight_top(value[i1, grab_card]))
    }
  }

  # Count outs
  fl_outs <- if_else(
    # Has Flush outs are cards that can improve overall flush (i.e. improve the worst kicker)
    best_count >= 5 & !is_straight, as.integer(14 - lowest_val - 4 - one_st_out), if_else(
      # Has enough suited cards that flush is possible
      ((stage == "flop" & best_count >= 3) | (stage == "turn" & best_count >= 4)) & !is_straight,
      13L - as.integer(best_count + one_st_out),
      # Not possible
      0L
    ))

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(fl_outs)
}
