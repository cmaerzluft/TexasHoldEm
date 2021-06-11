#' Find Straight High Card
#'
#' @param df The data frame with all hands that you want to search for a straight in
#'
#' @return The card value (2 - 14) that defines the upper limit of a straight. If no straight exists, returns NA
#' @export
find_straight <- function(values, n_hands = 0) {
  UseMethod("find_straight")
}
#' @export
find_straight.default <- function(values, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(values) }

  # Initialize output vector
  out <- rep.int(0, n_hands)

  # Find rows with Ace's (so they can be used as 1s for a 5 high straight if possible)
  aces <- rowMaxs(matrix(as.numeric(values == 14), ncol = 7)) == 1

  # Convert data.frame to matrix to take advantage of its vector like properties
  values <- as.matrix(values)

  # Add low Ace into matrix
  values <- cbind(values, as.numeric(aces))

  # Sort each row by card value
  values <- matrix(values[order(row(values), values, method = "radix")], ncol = ncol(values), byrow = TRUE)

  # Remove duplicates
  values[values[, 2] == values[, 3], 2] <- 18
  values[values[, 3] == values[, 4], 3] <- 20
  values[values[, 4] == values[, 5], 4] <- 22
  values[values[, 5] == values[, 6], 5] <- 24
  values[values[, 6] == values[, 7], 6] <- 26
  values[values[, 7] == values[, 8], 7] <- 28

  # Resort to get duplicates out of way
  values <- matrix(values[order(row(values), values, method = "radix")], ncol = ncol(values), byrow = TRUE)

  # Flag consecutive cards (this transposes the matrix as well)
  straights <- colCumsums(rbind(rep.int(1, times = n_hands), matrix(!diff(t(values)) %in% 1, ncol = n_hands)))

  # Add a row of 0s to force a split between lines
  straights <- rbind(straights, rep.int(0, times = n_hands))

  # Count consecutive cards
  chk <- rle(as.numeric(straights))

  # Determine which rows (now columns) have enough consecutive cards to make a straight
  row_ends <- which(chk$values == 0)
  full_straights <- which(chk$lengths >= 5)
  straight_cols <- findInterval(full_straights, row_ends) + 1

  # If we have any straights
  if (length(straight_cols) > 0) {
    # Determine which columns (now rows) are apart of the straight
    straight_row_ids <- matrix(rep(chk$values[full_straights], 8), ncol = length(full_straights), byrow = TRUE)
    straight_cells <- straights[-9, straight_cols] == straight_row_ids

    # Set cards not used to make a straight to 0
    straights <- values[straight_cols, ]
    straights[!t(straight_cells)] <- 0

    # Find highest remaining card (5 - Ace) and store it
    straight_tops <- do.call(pmax, as.data.frame(straights))
    out[straight_cols] <- straight_tops
  }

  return(out)
}
#' @export
find_straight.tbl_df <- function(values, n_hands = 0) {
  # Find rows with Ace's (so they can be used as 1s for a 5 high straight if possible)
  aces <- do.call(pmax, values) == 14

  # Find Straights
  values <- values %>%
    # Arrange cards by value (treat ace as both a 1 or 14) and remove duplicate values
    bind_cols(aces = as.numeric(aces)) %>%
    tibble::rowid_to_column() %>%
    pivot_longer(-rowid) %>%
    arrange(rowid, value) %>%
    group_by(rowid) %>%
    mutate(
      value = if_else(value == lag(value, default = -1), row_number()*2 + 10, value)
    ) %>%
    arrange(rowid, value) %>%

    # Set cards not part of 5 consecutive card values to 0
    mutate(
      straights = cumsum(if_else(row_number() == 1, 1, as.numeric((value - lag(value)) != 1)))
    ) %>%
    group_by(straights, .add = TRUE) %>%
    mutate(
      lengths = n()
    ) %>%
    group_by(rowid) %>%
    mutate(
      value = if_else(lengths != 5, 0, value)
    ) %>%

    # Return hands to normal format values format
    select(-straights, -lengths) %>%
    pivot_wider(
      id_cols = c(rowid, name),
      names_from = name,
      values_from = value
    ) %>% ungroup() %>%
    select(contains("card"))

  # Find highest remaining card (5 - Ace) and store it
  out <- do.call(pmax, values)

  return(out)
}

#' @export
find_straight.data.table <- function(values, n_hands = 0) {
  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(values) }

  # Initialize output vector
  out <- copy(values)
  cols <- as.character(1:ncol(values))

  # Find rows with Ace's (so they can be used as 1s for a 5 high straight if possible)
  aces <- do.call(pmax, out) == 14

  # Add low Ace into matrix
  out[, ace := as.numeric(aces)]

  # Sort each row by card value
  out <- out[, {
    tmp <- as.matrix(.SD)
    fin <- matrix(tmp[order(row(tmp), as.matrix(tmp))], ncol = ncol(out), byrow = TRUE)

    list(as.data.frame(fin))
  }, .SDcols = colnames(out)]

  # Remove duplicates
  out[, `:=`(
    V2 = ifelse(V2 == V3, 18, V2),
    V3 = ifelse(V3 == V4, 20, V3),
    V4 = ifelse(V4 == V5, 22, V4),
    V5 = ifelse(V5 == V6, 24, V5),
    V6 = ifelse(V6 == V7, 26, V6),
    V7 = ifelse(V7 == V8, 28, V7)
  )]

  # Resort to get duplicates out of way
  out <- out[, {
    tmp <- as.matrix(.SD)
    fin <- matrix(tmp[order(row(tmp), as.matrix(tmp))], ncol = ncol(out), byrow = TRUE)

    list(as.data.frame(fin))
  }, .SDcols = colnames(out)]

  # Pull highest card used in a straight
  out <- out[, {
    straight_top <- rep.int(0, n_hands)
    # Flag consecutive cards
    tmp <- cbind(rep.int(1, n_hands), (.SD[, -1, with = FALSE] - .SD[, -ncol(.SD), with = FALSE]) != 1)
    tmp <- rowCumsums(tmp)

    # Add a columns of 0s to force a split between lines
    tmp <- cbind(tmp, rep.int(0, n_hands))

    # Count consecutive cards
    chk <- rle(as.numeric(t(tmp)))

    # Determine which rows (now columns) have enough consecutive cards to make a straight
    row_ends <- which(chk$values == 0)
    full_straights <- which(chk$lengths >= 5)
    straight_rows <- findInterval(full_straights, row_ends) + 1

    # If we have any straights
    if (length(straight_rows) > 0) {
      # Determine which columns (now rows) are apart of the straight
      straight_row_ids <- matrix(rep(chk$values[full_straights], 8), nrow = length(full_straights), byrow = FALSE)
      straight_cells <- tmp[straight_rows, -9] == straight_row_ids

      # Set cards not used to make a straight to 0
      tmp <- as.matrix(.SD[straight_rows, ])
      tmp[!straight_cells] <- 0

      # Find highest remaining card (5 - Ace) and store it
      straight_tops <- do.call(pmax, as.data.frame(tmp))
      straight_top[straight_rows] <- straight_tops
    }

    list(straight_top = straight_top)
  }]
  out <- out[[1]]

  return(out)
}
