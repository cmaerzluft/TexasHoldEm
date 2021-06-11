#' Find Duplicates
#'
#' @param df The data frame with all hands that you want to search for a straight in
#'
#' @return The card value (2 - 14) that defines the upper limit of a straight. If no straight exists, returns NA
#' @export
find_duplicates <- function(values, type, n_hands) {
  UseMethod("find_duplicates")
}

#' @export
find_duplicates.default <- function(values, type = c("all", "four", "three", "two"), n_hands = 0) {
  # Input handling
  type <- match.arg(type, c("all", "four", "three", "two"))

  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(values) }

  # Empty vector
  empty_vec <- rep.int(0, n_hands)

  # Count cards in hand
  # Row Tabulate. Include:
  #   1 so column corresponds to card value
  #   15 so any row without four will return an impossible card value
  value_counts <- rowTabulates(values, values = 1:15)

  # Look for Four-of-a-Kind
  if (type %in% c("all", "four")) {
    out_four <- max.col(value_counts == 4, ties.method = "last")
    out_four[out_four == 15] <- 0
  }

  # Look for Three-of-a-Kind
  if (type %in% c("all", "three")) {
    # Find highest card value with three of a kind
    trips <- value_counts == 3
    out_three1 <- max.col(trips, ties.method = "last")
    out_three1[out_three1 == 15] <- 0

    # Find lowest card value with three of a kind
    out_three2 <- max.col(trips, ties.method = "first")
    out_three2[out_three2 == 1 | out_three2 == out_three1] <- 0
  }

  # Look for Pair
  if (type %in% c("all", "two")) {
    # Find highest card value with pair
    dups <- value_counts == 2
    out_two1 <- max.col(dups, ties.method = "last")
    out_two1[out_two1 == 15] <- 0

    # Remove bottom pair from three pair hands
    threepairs <- which(rowSums(dups) == 3)
    if (length(threepairs) > 0) {
      bot_pair <- max.col(dups[threepairs, ], ties.method = "first")
      dups[cbind(threepairs, bot_pair)] <- FALSE
    }
    # Find second highest card value with pair
    out_two2 <- max.col(dups, ties.method = "first")
    out_two2[out_two2 == 1 | out_two2 == out_two1] <- 0
  }

  # Join output
  out <- switch(
    type,
    all = cbind(out_four, out_three1, out_three2, out_two1, out_two2),
    four = out_four,
    three = cbind(out_three1, out_three2),
    two = cbind(out_two1, out_two2)
  )

  return(out)
}

#' @export
find_duplicates.tbl_df <- function(values, type = c("all", "four", "three", "two")) {
  # Input handling
  type <- match.arg(type, c("all", "four", "three", "two"))

  # Build Counting functions
  fns <- do.call(quos, parse_exprs(sprintf("rowSums(across(.fns = function(x) x == %s))", 1:15)))
  names(fns) <- 1:15

  # Count appearances of card values. Include:
  #   1 so column corresponds to card value
  #   15 so any row without four will return an impossible card value
  value_counts <- values %>%
    mutate(!!!fns) %>%
    select(matches("^[0-9]"))

  # Look for Four-of-a-Kind
  if (type %in% c("all", "four")) {
    out_four <- value_counts %>%
      mutate(across(.fns = function(x) x == 4)) %>%
      max.col(ties.method = "last") %>%
      if_else(. == 15, 0L, .)
  }

  # Look for Three-of-a-Kind
  if (type %in% c("all", "three")) {
    out_threes <- value_counts %>%
      mutate(across(.fns = function(x) x == 3)) %>%
      bind_cols(
        # Highest value
        out_three1 = max.col(., ties.method = "last"),
        # Lowest value
        out_three2 = max.col(., ties.method = "first")
      ) %>%
      select(out_three1:out_three2) %>%
      mutate(
        out_three1 = if_else(out_three1 == 15, 0L, out_three1),
        out_three2 = if_else(out_three2 == 1 | out_three1 == out_three2, 0L, out_three2)
      )
  }

  # Look for Pair
  if (type %in% c("all", "two")) {
    dups <- value_counts %>%
      mutate(across(.fns = function(x) x == 2))
    threepairs <- which(rowSums(dups) == 3)
    if (length(threepairs) > 0) {
      bot_pair <- dups %>% slice(threepairs) %>% max.col(ties.method = "first")
      dups <- data.frame(dups)
      dups[cbind(threepairs, bot_pair)] <- FALSE
    }
    out_twos <- dups %>%
      bind_cols(
        # Highest value
        out_two1 = max.col(., ties.method = "last"),
        # Lowest value
        out_two2 = max.col(., ties.method = "first")
      ) %>%
      select(out_two1:out_two2) %>%
      mutate(
        out_two1 = if_else(out_two1 == 15, 0L, out_two1),
        out_two2 = if_else(out_two2 == 1 | out_two1 == out_two2, 0L, out_two2)
      )
  }

  # Join output
  out <- switch(
    type,
    all = bind_cols(out_four = out_four, out_threes, out_twos),
    four = out_four,
    three = out_threes,
    two = out_twos
  )

  return(out)
}

#' @export
find_duplicates.data.table <- function(values, type = c("all", "four", "three", "two"), n_hands = 0) {
  # Input handling
  type <- match.arg(type, c("all", "four", "three", "two"))

  # Calculate n_hands if not already calculated
  if (n_hands == 0) { n_hands <- nrow(values) }

  # Count cards in hand
  out <- copy(values)
  cols <- as.character(1:15)

  # Row Tabulate. Include:
  #   1 so column corresponds to card value
  #   15 so any row without four will return an impossible card value
  out[, (cols) := (
    as.data.frame(rowTabulates(as.matrix(.SD), values = 1:15))
  ), .SDcols = colnames(values)]
  out[, (colnames(values)) := NULL]

  # Look for Four-of-a-Kind
  if (type %in% c("all", "four")) {
    out[, out_four := max.col(
      as.matrix(.SD == 4), ties.method = "last"
    ), .SDcols = cols][out_four == 15, out_four := 0]
  }

  # Look for Three-of-a-Kind
  if (type %in% c("all", "three")) {
    out[, `:=`(
      # Find highest card value with three of a kind
      out_three1 = max.col(as.matrix(.SD == 3), ties.method = "last"),
      # Find lowest card value with three of a kind
      out_three2 = max.col(as.matrix(.SD == 3), ties.method = "first")
    ), .SDcols = cols]

    # Set rows with no 3 of a kind to 0
    out[out_three1 == 15, out_three1 := 0]
    # Set rows with no 3 of a kind or just one 3 of a kind to 0
    out[out_three2 == 1 | out_three2 == out_three1, out_three2 := 0]
  }

  # Look for Pair
  if (type %in% c("all", "two")) {
    out <- cbind(out, out[, {
      # Find all pairs
      dups <- as.matrix(.SD == 2)

      # Find highest card value with pair
      out_two1 <- max.col(dups, ties.method = "last")

      # Remove highest pair
      dups[cbind(1:n_hands, out_two1)] <- FALSE

      # Find second highest card value with pair
      out_two2 <- max.col(dups, ties.method = "last")

      # Return columns
      list(out_two1 = out_two1, out_two2 = out_two2)
    }, .SDcols = cols])

    # Set rows with no pair to 0
    out[out_two1 == 15, out_two1 := 0]
    # Set rows with low pair to 0
    out[out_two2 == 15, out_two2 := 0]

  }

  # Remove non-needed columns
  out[, (cols) := NULL]

  return(out)
}
