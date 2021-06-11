#' Outs to make a Straight
#'
#' @param cards Cards from hand, including pocket cards and all community cards
#' @param stage What part of deal was just performed, "flop" or "stage"?
#' @param output IN PROGRESS. Should function return number of outs ("counts") or the cards themselves ("cards")
#'
#' @return Either counts of cards that can make a Straight or a vector of cards that complete a Straight. If a data.frame
#' is input, the output is the same length as number of rows.
#' @export
outs_st <- function(cards, stage, output = "counts") {
  # Info needed for calculation
  must_have_n <- if_else(stage == "flop", 3, 4)
  cards_left <- 5 - must_have_n
  value <- pull_value(cards)

  # Handle only one hand being passed
  if (is.null(dim(value))) {
    iters <- 1
  } else {
    iters <- nrow(value)
  }

  # Initialize outputs
  st_outs <- rep.int(0, times = iters)

  # Iterate over all hands
  for (i1 in seq_along(st_outs)) {
    # Handle one hand vs a lot of hands (mostly used for outs_sf call)
    if (iters == 1) {
      ordered_values <- sort(unique(value), decreasing = FALSE)
    } else {
      ordered_values <- sort(unique(value[i1, ]), decreasing = FALSE)
    }
    # Determine which cards are close enough to other cards to be part of a straight
    straights <- cumsum(c(1, diff(ordered_values) > (cards_left + 1)))
    chk <- rle(straights)
    if (any(chk$lengths >= must_have_n)) {
      consec_init <- split(ordered_values, straights)
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      use_cards <- consec_init[[longest]]
    } else {
      use_cards <- c()
    }

    # Track how many cards are useful
    n_need <- max(5 - length(use_cards), 0)
    cards_range <- if (n_need != 5) {
      diff(range(use_cards))
    } else {
      15
    }

    # Check Ace-low for better results (need at least a 2, 3, or 4 as well)
    if (14 %in% ordered_values & any(2:4 %in% ordered_values)) {
      acelo_cards <- ordered_values
      acelo_cards[acelo_cards == 14] <- 1
      acelo_cards <- sort(acelo_cards, decreasing = FALSE)
      straights <- cumsum(c(1, diff(acelo_cards) > (cards_left + 1)))
      chk <- rle(straights)
      if (any(chk$lengths >= must_have_n)) {
        consec_init <- split(acelo_cards, straights)
        lengths <- lapply(consec_init, length)
        longest <- which.max(lengths)
        use_cards_acelo <- consec_init[[longest]]
      } else {
        use_cards_acelo <- c()
      }

      # Update card needs
      n_need_acelo <- max(5 - length(use_cards_acelo), 0)
      cards_range_acelo <- if (n_need_acelo != 5) {
        diff(range(use_cards_acelo))
      } else {
        15
      }

      # If Ace-lo is useful, use it and the cards with it
      if (n_need_acelo <= cards_left && cards_range_acelo < 5) {
        # If Ace-hi is also useful, use all the cards, otherwise just the Ace-lo cards
        if (n_need <= cards_left && cards_range < 5) {
          use_cards <- unique(c(use_cards_acelo, use_cards))
          n_need <- max(5 - length(use_cards), 0)
          cards_range <- if (n_need != 5) {
            diff(range(use_cards))
          } else {
            15
          }
        } else {
          use_cards <- use_cards_acelo
          n_need <- n_need_acelo
          cards_range <- cards_range_acelo
        }
      } # n_need_acelo <= cards_left && cards_range_acelo < 5
    } # 14 %in% ordered_values & any(2:4 %in% ordered_values)

    # Calculate which cards can be used to make a potential straight
    make_straight_fin <- c()
    # When 6 cards are available (stage == "turn" | Ace == 1 & Ace == 14), see how many ways we can get a straight
    if (length(use_cards) > 5) {
      make_straight <- help_make_straight(sorted_cards = use_cards, stage = stage, k = 5)
      make_straight_fin <- c(make_straight_fin, make_straight)
    }

    # See if four cards can be used for a straight and pick the best
    if (length(use_cards) > 3) {
      make_straight <- help_make_straight(sorted_cards = use_cards, stage = stage, k = 4)
      make_straight_fin <- c(make_straight_fin, make_straight)
    }

    # See if three cards can be used (only useful at flop) and pick the best
    if (length(use_cards) > 2 && stage == "flop") {
      make_straight <- help_make_straight(sorted_cards = use_cards, stage = stage, k = 3)
      make_straight_fin <- c(make_straight_fin, make_straight)
    }

    # Count up the outs
    make_straight_fin <- unique(make_straight_fin)
    if (length(make_straight_fin) > 0) {
      st_outs[i1] <- as.integer(length(make_straight_fin)*4)
    } else if (length(use_cards) >= 5 && !is.na(find_straight_top(use_cards))) {
      # Case when we have an Ace high straight
      st_outs[i1] <- 0L
    }
  }

  # Return Results
  # If we want to return more stuff
  #  Could potentially return actual card outs (e.g. 304 and 307) in order to combined with other hand outs to get a
  #   total outs list/count
  return(st_outs)
}
