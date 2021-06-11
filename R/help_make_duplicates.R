#' Help Make Duplicates
#'
#' @param sorted_cards Cards that help make a straight
#'
#' @return The index of the cards that are best given the size of the window
#' @export
help_make_duplicates <- function(cards, stage) {
  # Get Card values
  value <- pull_value(cards)

  # Handle only one hand being passed
  if (is.null(dim(value))) {
    iters <- 1
  } else {
    iters <- nrow(value)
  }

  # Initialize outputs
  empty_col <- rep.int(-1, times = iters)
  make_duplicates <- data.frame(
    card1 = empty_col,
    card2 = empty_col,
    card3 = empty_col,
    card4 = empty_col,
    count1 = empty_col,
    count2 = empty_col,
    count3 = empty_col,
    count4 = empty_col
  )

  # Find the three most repeated cards in hand
  for (i1 in seq_along(empty_col)) {
    # Handle one hand vs a lot of hands
    if (iters == 1) {
      freq <- table(as.numeric(value))
    } else {
      freq <- table(as.numeric(value[i1, ]))
    }
    freq <- freq[order(freq, as.numeric(names(freq)), decreasing = TRUE)]

    # Store cards
    make_duplicates[i1, ] <- c(as.numeric(names(freq[1:4])), freq[1:4])
  }

  # Return Results
  return(make_duplicates)
}
