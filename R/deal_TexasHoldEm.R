#########################################################################################################################
#                                                                                                                       #
#  Title: Deal Texas Hold'em
#  Author: Christopher Maerzluft
#  Description: A function for simulating a deal to the number of players at a table
#  Last Edit: 10/08/18
#                                                                                                                       #
#########################################################################################################################
deal_TexasHoldEm <- function(round, full_deck) {
  # Deal 2 random cards to each player
  cards_dealt_in <- sample(nrow(full_deck), nrow(round)*2, replace = FALSE)
  pocket_cards <- data.frame(matrix(full_deck$hand_cd[cards_dealt_in], byrow = FALSE, nrow = nrow(round), ncol = 2))
  colnames(pocket_cards) <- c("pocket_card1", "pocket_card2")
  round <- cbind(round, pocket_cards)
  # Remove dealt cards from deck
  final.deck <- full_deck[-cards_dealt_in, ]
  
  # Just as in regulation Hold'em we will remove or "burn" on card from the deck before continuing on. Just as dealing in 
  #   a certain order shouldn't matter when the cards are truly random, the burns shouldn't matter either but will still
  #   be included for completeness.
  # Burn one card
  burn <- sample(nrow(final.deck), 1)
  final.deck <- final.deck[-burn, ]
  # The Flop
  cards_dealt_in <- sample(nrow(final.deck), 3, replace = FALSE)
  pocket_cards <- data.frame(matrix(final.deck$hand_cd[cards_dealt_in], byrow = TRUE, nrow = nrow(round), ncol = 3))
  colnames(pocket_cards) <- c("flop_card1", "flop_card2", "flop_card3")
  round <- cbind(round, pocket_cards)
  # Remove dealt cards from deck
  final.deck <- final.deck[-cards_dealt_in, ]
  
  # Burn one card
  burn <- sample(nrow(final.deck), 1)
  final.deck <- final.deck[-burn, ]
  # The Turn
  cards_dealt_in <- sample(nrow(final.deck), 1, replace = FALSE)
  pocket_cards <- data.frame(matrix(final.deck$hand_cd[cards_dealt_in], byrow = TRUE, nrow = nrow(round), ncol = 1))
  colnames(pocket_cards) <- c("turn_card")
  round <- cbind(round, pocket_cards)
  # Remove dealt cards from deck
  final.deck <- final.deck[-cards_dealt_in, ]
  
  # Burn one card
  burn <- sample(nrow(final.deck), 1)
  final.deck <- final.deck[-burn, ]
  # The River
  cards_dealt_in <- sample(nrow(final.deck), 1, replace = FALSE)
  pocket_cards <- data.frame(matrix(final.deck$hand_cd[cards_dealt_in], byrow = TRUE, nrow = nrow(round), ncol = 1))
  colnames(pocket_cards) <- c("river_card")
  round <- cbind(round, pocket_cards)
  # Remove dealt cards from deck - don't need this one since we are done dealing but if we decide to return deck as well
  #   in the future this will need to be here
  final.deck <- final.deck[-cards_dealt_in, ]
  
  return(round)
}

#########################################################################################################################