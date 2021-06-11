#' Deal the hands for a series of Texas Hold'em games
#'
#' @description  A function for simulating a deal to a set number of players in a set number of rounds.
#'
#' @param empty_table An empty data.frame/tibble/data.table with a row for each hand and player. Most easily created using
#'          the deal_cards function to ensure the format and class is correct.
#' @param cards a vector with every possible card code stored in card_cd. If you are playing with one deck this will have
#'                52 rows and card_cd should uniquely identify cards. To play with multiple decks just concatenate multiple
#'                decks.
#' @param n_hands The number of hands to produce
#' @param n_players The number of players in each hand
#'
#' @return a data frame/tibble/data.table with the original empty_table input plus unique pocket cards for each row and
#'           community cards that are the same for everyone.
new_hand <- function(empty_table, cards, n_hands, n_players) {
  UseMethod("new_hand")
}

# Using Base R formulation
new_hand.data.frame <- function(empty_table, cards, n_hands, n_players) {
  # Initialize info used multiple times
  playersx2 <- n_players*2

  # Samples all cards needed
  dealt_cards <- replicate(n_hands, sample(cards, (playersx2 + 5)), simplify = TRUE)

  # Pull pocket cards
  pocket_cards <- matrix(dealt_cards[1:playersx2, ], ncol = 2, byrow = TRUE)
  empty_table[, c("pocket_card1", "pocket_card2")] <- pocket_cards

  # Pull community cards
  community_cards <- c("flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card")
  dealt_cards <- t(dealt_cards[(playersx2 + 1):(playersx2 + 5), ])
  empty_table[, community_cards] <- dealt_cards[rep(1:n_hands, each = n_players), ]

  # Return results
  return(empty_table)
}

# Using tidyverse formulation
new_hand.tbl_df <- function(empty_table, cards, n_hands, n_players) {
  # Initialize info used multiple times
  playersx2 <- n_players*2

  # Samples all cards needed
  dealt_cards <- replicate(n_hands, sample(cards, (playersx2 + 5)), simplify = TRUE)

  # Pull pocket cards
  pocket_cards <- matrix(dealt_cards[1:playersx2, ], ncol = 2, byrow = TRUE)
  colnames(pocket_cards) <- c("pocket_card1", "pocket_card2")
  pocket_cards <- as_tibble(pocket_cards) %>%
    mutate(
      player_id = rep.int(1:n_players, times = n_hands),
      hand_id = rep(1:n_hands, each = n_players)
      ) %>%
    nest(data = !hand_id)
  empty_table$player <- pocket_cards$data

  # Pull community cards
  community_cards <- c("flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card")
  dealt_cards <- t(dealt_cards[(playersx2 + 1):(playersx2 + 5), ])
  empty_table[, community_cards] <- dealt_cards

  return(empty_table)
}

# Using data.table formulation
new_hand.data.table <- function(empty_table, cards, n_hands, n_players) {
  # Initialize info used multiple times
  playersx2 <- n_players*2

  # Samples all cards needed
  dealt_cards <- replicate(n_hands, sample(cards, (playersx2 + 5)), simplify = TRUE)

  # Pull pocket cards
  pocket_cards <- as.data.table(matrix(dealt_cards[1:playersx2, ], ncol = 2, byrow = TRUE))
  set(empty_table, j = c("pocket_card1", "pocket_card2"), value = pocket_cards)

  # Pull community cards
  community_cards <- c("flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card")
  dealt_cards <- t(dealt_cards[(playersx2 + 1):(playersx2 + 5), ])
  dealt_cards <- dealt_cards[rep(1:n_hands, each = n_players), ]
  if (is.null(dim(dealt_cards)) || nrow(dealt_cards) == 1) {
    values <- as.list(dealt_cards)
  } else {
    values <- as.data.table(dealt_cards)
  }
  set(empty_table, j = community_cards, value = values)

  return(empty_table)
}
