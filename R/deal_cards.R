#' Create a new set of Texas Hold'em hands
#'
#' @description Simulated a hand of Texas Hold'em for a desired number of players
#'
#' @param n_players numeric(). The number of players you wish to have in the game. Must be >= 1.
#' @param n_hands numeric() The number of hands you wish to simulate. Must be >= 1.
#' @param player_ids Optional. A vector of IDs for each player you wish to be in the game.
#' @param method character(). What R method should be used for this game: "base" (R), "tidy" (tidyverse), "dt (data.table)
#'
#' @return A completely dealt hand of Texas Hold'em in a data.frame/tibble/data.table form.
#'
#' @examples
#' deal_cards(n_players = 2, n_hands = 10, method = "base")
#' deal_cards(n_players = 10, n_hands = 2, method = "tidy")
#' deal_cards(n_players = 2, n_hands = 1, method = "dt")
#'
#' @export
deal_cards <- function(
  n_players = 1,
  n_hands = 1,
  player_ids = NULL,
  method = c("base", "tidy", "dt")
) {
  # Pull Method
  method <- match.arg(method, c("base", "tidy", "dt"))

  # Check if we have any players/hands
  stopifnot(
    "Number of players must be greater than 0" = n_players > 0,
    "Number of hands must be greater than 0" = n_hands > 0
  )
  # Use package card codes if not otherwise provided
  cards <- TexasHoldEm::card_deck$card_cd

  # Check Deck size is big enough to handle empty_table size
  if (length(cards) < (n_players*2 + 5 + 4)) {
    stop("Need a larger deck or fewer players")
  }

  # Check Player IDs
  if (!is.null(player_ids)) { # Player IDs given
    # Handle differing number of players and n_players
    if (n_players < length(player_ids)) { # If more players than seats
      warning("Gave more player_ids than available n_players. Selecting a random subset of IDs to join the game.")
      player_ids <- sample(player_ids, n_players)

    } else if (n_players > length(player_ids)) { # If less players than seats
      player_ids <- union(player_ids, 1:n_players)[1:n_players]
    }

  } else { # If no player_ids given
    player_ids <- 1:n_players
  }

  # Create empty data.frame for the players involved in the game
  if (method == "base") { # Use Base R (data.frame/no extra packages)
    empty_col <- rep(NA_integer_, n_hands*n_players)
    players <- data.frame(
      hand_id = rep(1:n_hands, each = n_players),
      flop_card1 = empty_col,
      flop_card2 = empty_col,
      flop_card3 = empty_col,
      turn_card = empty_col,
      river_card = empty_col,
      player_id = rep(player_ids, n_hands),
      pocket_card1 = empty_col,
      pocket_card2 = empty_col
    )
    players <- new_TexasHoldEm_base(players)
  } else if (method == "tidy") { # Use tidyverse (tibble/dplyr, tidyr, etc.)
    empty_hands_col <- rep(NA_integer_, n_hands)
    empty_playe_col <- rep(NA_integer_, n_players)
    players <- tibble(
      hand_id = 1:n_hands,
      flop_card1 = empty_hands_col,
      flop_card2 = empty_hands_col,
      flop_card3 = empty_hands_col,
      turn_card = empty_hands_col,
      river_card = empty_hands_col,
      player = list(tibble(
        player_id = player_ids,
        pocket_card1 = empty_playe_col,
        pocket_card2 = empty_playe_col
      ))
    )
    players <- new_TexasHoldEm_tidy(players)
  } else if (method == "dt") { # Use data.table (data.table/data.table)
    empty_col <- rep(NA_integer_, n_hands*n_players)
    players <- data.table(
      hand_id = rep(1:n_hands, each = n_players),
      flop_card1 = empty_col,
      flop_card2 = empty_col,
      flop_card3 = empty_col,
      turn_card = empty_col,
      river_card = empty_col,
      player_id = rep(player_ids, n_hands),
      pocket_card1 = empty_col,
      pocket_card2 = empty_col
    )
    players <- new_TexasHoldEm_dt(players)
  }

  # Deal cards
  final_table <- new_hand(empty_table = players, cards, n_hands, n_players)

  return(final_table)
}
