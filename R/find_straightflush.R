#' Find Straight Flush High Card
#'
#' @param suits card suit code
#' @param values card value code
#' @param fl_suit suit of flush
#' @param st_top top card of straight
#' @param n_hands number of hands
#'
#' @return
#' @export
find_straightflush <- function(suits, values, fl_suit = NULL, st_top = NULL, n_hands = 0) {
  UseMethod("find_straightflush")
}

#' @export
find_straightflush.default <- function(suits, values, fl_suit = NULL, st_top = NULL, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(suits) }

  # Initialize output vector
  out <- rep.int(0, n_hands)

  # Use Vector of suits and top straight card if provided, calculate if not
  if (is.null(fl_suit)) { fl_suit <- find_flush(suits, n_hands = n_hands) }
  if (is.null(st_top)) { st_top <- find_straight(values, n_hands = n_hands) }

  # Which hands have the potential to be a straight flush
  potential <- fl_suit != 0 & st_top != 0

  # If any hands have potential
  if (any(potential)) {
    # Subset data to only potential straight flush hands
    suits <- suits[potential, , drop = FALSE]
    values <- values[potential, , drop = FALSE]
    fl_suit <- fl_suit[potential]

    # Turn cards with the wrong suit to impossible values
    values[suits != matrix(rep.int(fl_suit, 7), ncol = 7)] <- 16

    # Save High Card in Straight Flushes
    out[potential] <- find_straight(values, n_hands = nrow(values))
  }

  return(out)
}

#' @export
find_straightflush.tbl_df <- function(suits, values, fl_suit = NULL, st_top = NULL, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(suits) }

  # Initialize output vector
  out <- rep.int(0, n_hands)

  # Use Vector of suits and top straight card if provided, calculate if not
  if (is.null(fl_suit)) { fl_suit <- card_suits %>% find_flush() }
  if (is.null(st_top)) { st_top <- card_value %>% find_straight() }

  # Which hands have the potential to be a straight flush
  potential <- fl_suit != 0 & st_top != 0

  # If any hands have potential
  if (any(potential)) {
    # Subset data to only potential straight flush hands
    values <- values %>%
      tibble::rowid_to_column() %>%
      filter(potential)
    idx <- values %>% pull(rowid)
    suits <- suits %>% filter(potential)
    fl_suit <- fl_suit[potential]

    # Save High Card in Straight Flushes
    out[idx] <- values %>%
      select(-rowid) %>%
      mutate(
        across(
          # Turn cards with the wrong suit to impossible values
          .fns = function(x, suit_fn = suits, suit_fl = fl_suit) if_else(suit_fn[[cur_column()]] == suit_fl, x, 16L)
        )) %>%
      find_straight()
  }

  return(out)
}

#' @export
find_straightflush.data.table <- function(suits, values, fl_suit = NULL, st_top = NULL, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(suits) }

  # Initialize output vector
  out <- rep.int(0, n_hands)

  # Use Vector of suits and top straight card if provided, calculate if not
  if (is.null(fl_suit)) { fl_suit <- find_flush(suits, n_hands = n_hands) }
  if (is.null(st_top)) { st_top <- find_straight(values, n_hands = n_hands) }

  # Which hands have the potential to be a straight flush
  potential <- fl_suit != 0 & st_top != 0

  # If any hands have potential
  if (any(potential)) {
    # Subset data to only potential straight flush hands
    suits <- copy(suits[potential, ])
    values <- copy(values[potential, ])
    fl_suit <- fl_suit[potential]

    # Turn cards with the wrong suit to impossible values
    vars <- seq_along(names(values))
    values[, (vars) := lapply(vars, FUN = function(x, va, su, fs) {
      va[[x]][su[[x]] != fs] <- 16
      va[[x]]
    }, va = values, su = suits, fs = fl_suit)]

    # Save High Card in Straight Flushes
    out[potential] <- find_straight(values, n_hands = nrow(values))
  }

  return(out)
}
