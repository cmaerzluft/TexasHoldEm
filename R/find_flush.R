#' Find Flush Suit
#'
#' @param df The data frame with all hands that you want to search for a flush in
#'
#' @return The suit value (1 - 4) that makes up the flush. If no flush exists, returns NA
#' @export
find_flush <- function(suits, n_hands = 0) {
  UseMethod("find_flush")
}

#' @export
find_flush.default <- function(suits, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(suits) }

  # Initialize output vector
  out <- rep.int(0, n_hands)

  # Track which suit (if any) has enough cards to make a flush
  out[rowSums(suits == 1) >= 5] <- 1
  out[rowSums(suits == 2) >= 5] <- 2
  out[rowSums(suits == 3) >= 5] <- 3
  out[rowSums(suits == 4) >= 5] <- 4

  return(out)
}

#' @export
find_flush.tbl_df <- function(suits) {
  # Track which suit (if any) has enough cards to make a flush
  out <- suits %>%
    mutate(
      out = if_else(
        rowSums(select(., everything()) == 1) >= 5, 1, if_else(
          rowSums(select(., everything()) == 2) >= 5, 2, if_else(
            rowSums(select(., everything()) == 3) >= 5, 3, if_else(
              rowSums(select(., everything()) == 4) >= 5, 4, 0
            ))))
    ) %>%
    pull(out)

  return(out)
}

#' @export
find_flush.data.table <- function(suits, n_hands = 0) {
  # Track which suit (if any) has enough cards to make a flush
  out <- suits[, (
    fcase(
      rowSums(suits == 1) >= 5, 1,
      rowSums(suits == 2) >= 5, 2,
      rowSums(suits == 3) >= 5, 3,
      rowSums(suits == 4) >= 5, 4,
      default = 0
    )
  )]

  return(out)
}
