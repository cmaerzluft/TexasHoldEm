#' Find Flush Suit
#'
#' @param df The data frame with all hands that you want to search for a flush in
#'
#' @return The suit value (1 - 4) that makes up the flush. If no flush exists, returns NA
#' @export
find_flush_suit <- function(df) {
  if (nrow(df) == 0) return()

  out <- vector(mode = "integer", length = nrow(df))
  for (i1 in seq_along(out)) {
    freq <- sort(table(as.numeric(df[i1, ])), decreasing = TRUE)

    out[i1] <- ifelse(max(freq) >= 5, as.integer(names(freq[1])), NA_integer_)
  }

  return(out)
}

#' Find Top of a Straight
#'
#' @param df The data frame with all hands that you want to search for a straight in
#'
#' @return The card value (2 - 14) that defines the upper limit of a straight. If no straight exists, returns NA
#' @export
find_straight_top <- function(df) {
  if (nrow(df) == 0) return()

  out <- vector(mode = "integer", length = nrow(df))
  for (i1 in seq_along(out)) {
    straights <- cumsum(c(1, diff(sort(unique(df[i1, ]))) != 1))
    chk <- rle(straights)
    if (any(chk$lengths >= 5)) {
      consec_init <- split(sort(unique(df[i1, ])), straights)
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      out[i1] <- max(consec_init[[longest]])
    } else if (any(df[i1, ] %in% 14) & any(chk$lengths == 4)) {
      df[i1, ][df[i1, ] == 14] <- 1
      out[i1] <- find_straight_top(df[i1, , drop = FALSE])
    } else {
      out[i1] <- NA_integer_
    }
  }

  return(out)
}

#' Find Four of a Kind Card
#'
#' @param df The data frame with all hands that you want to search for a quad in
#'
#' @return The card value (2 - 14) that makes a four of a kind. If no four of a kind exists, returns NA
#' @export
find_quads <- function(df) {
  if (nrow(df) == 0) return()

  out <- vector(mode = "integer", length = nrow(df))
  for (i1 in seq_along(out)) {
    freq <- sort(table(as.numeric(df[i1, ])), decreasing = TRUE)

    out[i1] <- ifelse(any(freq == 4), max(as.integer(names(freq)[freq == 4])), NA_integer_)
  }

  return(out)
}

#' Find Trips or Pairs
#'
#' @description If
#'
#' @param df The data frame with all hands that you want to search for a quad in
#' @param find Type to find. 1 looks for the highest three of a kind, 2 looks for a second three of a kind or the
#'              highest pair, 3 looks for the second highest pair
#' @param top_tripple a vector of length nrow(df) that tells the function which 3 of a kind has already been found.
#'                     Required when find > 1.
#' @param second_dup a vector of length nrow(df) that tells the function what was the second three of a kind value or
#'                    the highest pair already found. Required when find > 2
#'
#' @return The card value (2 - 14) that makes the highest remaining trip or pair being searched for. If none exist,
#'          returns NA
#' @export
find_tripsNpairs <- function(df, find = 1, top_tripple, second_dup) {
  if (nrow(df) == 0) return()

  out <- vector(mode = "integer", length = nrow(df))
  for (i1 in seq_along(out)) {
    freq <- sort(table(as.numeric(df[i1, ])), decreasing = TRUE)

    if (find == 1) {
      PickOut <- freq == 3
    } else if (find == 2) {
      PickOut <- freq >= 2 &
        names(freq) != max(top_tripple[i1], -1, na.rm = TRUE)
    } else if (find == 3) {
      PickOut <- freq >= 2 &
        names(freq) != max(top_tripple[i1], -1, na.rm = TRUE) &
        names(freq) != max(second_dup[i1], -1, na.rm = TRUE)
    }
    out[i1] <- ifelse(any(PickOut), max(as.integer(names(freq)[PickOut])), NA_integer_)
  }

  return(out)
}
