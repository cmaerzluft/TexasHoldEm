#' Create a new hand of Texas Hold'em
#'
#' @description This is designed so that multiple games can be done in the same object.
#'
#' @param cards A vector with all the cards available to deal in the new hand
#' @param chairs The number of players you wish to have in the game
#' @param player_ids A vector of IDs for each player you wish to be in the game
#' @param hand_id An ID for the hand being played
#' @param auto_fill If the number of chairs available is greater than length(player_ids), should we auto-generate new
#'                   players to fill all the chairs
#'
#' @note Things we need to do/consider still:
#'        1) build a function that can join multiple rounds together (rbind method)
#'        2) Should this be turned into an explicit new class?
#'        3) Do we need to make a more auto-generated unique player_id - the idea being that if we want to simulate
#'            players in a tournament, we want to make sure they all have unique IDs. Though, we could create all the
#'            IDs and feed it to the function ourselves.
#'        4) Should we include a game_id as well? We could just do that ourselves
#'
#' @return a list with two empty data.frames. players which has one row for n_players and holds each players pocket cards
#'          and community which has places for the 5 community cards of the game.
#' @export
new_hand <- function(cards, chairs = NULL, player_ids = NULL, hand_id = NULL, auto_fill = TRUE) {
  # Check inputs
  if (is.null(chairs) & is.null(player_ids)) { # No inputs given
    stop("Must input at least one value between chairs and player_ids")

  } else if (!is.null(chairs) & !is.null(player_ids)) { # Both inputs given
    # Handle differing number of players and chairs
    if (chairs < length(player_ids)) { # Less chairs than players
      warning("Gave more player_ids than available chairs. Selecting a random subset of IDs to join the game.")
      player_ids <- sample(player_ids, chairs)

    } else if (chairs > length(player_ids)) { # If more chairs than players
      if (auto_fill) { # Add new players to fill empty chairs
        player_ids <- union(player_ids, 1:chairs)[1:chairs]
      } # else player_ids is player_ids
    }

  } else if (!is.null(chairs) & is.null(player_ids)) { # If no player_ids given
    player_ids <- 1:chairs
  } # else player_ids is player_ids

  if (is.null(hand_id)) {
    hand_id <- 1
  }

  # Create empty data.frame for the players involved in the game
  players <- data.frame(
    hand_id = hand_id,
    player_id = player_ids,
    pocket_card1 = NA_integer_,
    pocket_card2 = NA_integer_,
    flop_card1 = NA_integer_,
    flop_card2 = NA_integer_,
    flop_card3 = NA_integer_,
    turn_card = NA_integer_,
    river_card = NA_integer_
  )

  # Deal cards
  final_table <- deal_TexasHoldEm(cards = cards, empty_table = players)

  return(final_table)
}
