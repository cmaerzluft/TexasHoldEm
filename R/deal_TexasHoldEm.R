#' Deal a hand of Texas Hold'em
#'
#' @description  A function for simulating a deal to the number of players in the round. This mimics regulation Texas
#'   Hold'em exactly, including burning cards and dealing in order. Since we are using the closest thing to random a
#'   computer can possibly get, those things shouldn't really matter that much but I want to include them anyways. At
#'   realistic deck sizes, they don't cost much.
#'
#' @param cards a vector with every possible card code stored in card_cd. If you are playing with one deck this will have
#'                52 rows and card_cd should uniquely identify cards. To play with multiple decks just concatenate multiple
#'                decks.
#' @param empty_table a data frame with a row for each player in the hand. Though not required I include a hand ID and a
#'                player ID for later use. The hand ID is the same for all players and the player ID is unique to each player.
#'
#' @return a data frame with the original empty_table input plus unique pocket cards for each row and community cards that
#'           are the same for everyone.
#'
#' @note Things to try exploring in the future with respect to this code:
#'         1) Does burning a card influence outcomes? (remove burn code)
#'         2) Does dealing one card to each player before dealing a second card influence outcomes (switch byrow to FALSE)
#'        In the future this function would not be exported.
#'
#' @export
deal_TexasHoldEm <- function(cards, empty_table) {
  n_players <- nrow(empty_table)

  # Check Deck size is big enough to handle empty_table size
  if (length(cards) < (n_players*2 + 5 + 4)) {
    stop("deal_TexasHoldEm: Need a larger deck or fewer players")
  }

  # Burn one card
  burn <- sample(length(cards), 1)
  remaining_cards <- cards[-burn]

  # Deal everyone a single card before going back and giving everyone else a second card
  cards_dealt_in <- sample(length(remaining_cards), n_players*2, replace = FALSE)
  empty_table[, 3:4] <- matrix(remaining_cards[cards_dealt_in], byrow = FALSE, nrow = n_players, ncol = 2)
  remaining_cards <- remaining_cards[-cards_dealt_in]

  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]

  # The Flop
  cards_dealt_in <- sample(length(remaining_cards), 3, replace = FALSE)
  empty_table[, 5:7] <- matrix(remaining_cards[cards_dealt_in], byrow = TRUE, nrow = n_players, ncol = 3)
  remaining_cards <- remaining_cards[-cards_dealt_in]

  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]

  # The Turn
  cards_dealt_in <- sample(length(remaining_cards), 1)
  empty_table[, 8] <- remaining_cards[cards_dealt_in]
  remaining_cards <- remaining_cards[-cards_dealt_in]

  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]

  # The River
  cards_dealt_in <- sample(length(remaining_cards), 1)
  empty_table[, 9] <- remaining_cards[cards_dealt_in]
  # For saving the final deck if we want to analyze it later
  # remaining_cards <- remaining_cards[-cards_dealt_in]
  # empty_table$deck <- remaining_cards

  return(empty_table)
}
