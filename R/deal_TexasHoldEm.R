#########################################################################################################################
#                                                                                                                       #
#  Title: Deal a round of Texas Hold'em
#  Author: Chris Maerzluft
#  Last Edit: 7/19/2019
#                                                                                                                       #
#########################################################################################################################
# Details ###############################################################################################################
# A function for simulating a deal to the number of players in the round. This mimics regulation Texas Hold'em exactly,
#   including burning cards and dealing in order. Since we are using the closest thing to random a computer can possibly
#   get, those things shouldn't really matter that much but I want to include them anyways. At realistic deck sizes, they
#   don't cost much
# 
# Inputs ################################################################################################################
# round       a data frame with a row for each player in the hand. Though not required I include a hand ID and a player 
#               ID for later use. The hand ID is the same for all players and the player ID is unique to each player.
# full_deck   a data frame with every possible card code stored in card_cd. If you are playing with one deck this will 
#               have 52 rows and card_cd should uniquely identify cards. To play with multiple decks just rbind a full
#               deck with itself until you have the correct number of decks.
#               deck size must be >= # of players*2 (pocket cards) + 5 (community cards) + 4 (burned cards)
# 
# Outputs ###############################################################################################################
# round       the data frame that was inputted with 7 additional columns. The first 2 are their pocket cards and should
#               be unique amongst both pocket_card1 and pocket_card2 as well as across all players. The next 5 are the
#               community cards (3 flop cards, 1 turn, and 1 river). These will be the same for all players and should
#               not also appear as pocket cards
# 
# Begin Function ########################################################################################################
deal_TexasHoldEm <- function(round, cards) {
  # Check Deck size is big enough to handle round size
  if (length(cards) < (nrow(round)*2 + 5 + 4)) {
    stop("deal_TexasHoldEm: Need a larger deck or fewer players")
  }
  
  # Burn one card
  burn <- sample(length(cards), 1)
  remaining_cards <- cards[-burn]
  
  # Deal everyone a single card before going back and giving everyone else a second card
  cards_dealt_in <- sample(length(remaining_cards), nrow(round)*2, replace = FALSE)
  pocket_cards <- data.frame(matrix(remaining_cards[cards_dealt_in], byrow = FALSE, nrow = nrow(round), ncol = 2))
  colnames(pocket_cards) <- c("pocket_card1", "pocket_card2")
  round <- cbind(round, pocket_cards)
  # Remove dealt cards from deck
  remaining_cards <- remaining_cards[-cards_dealt_in]
  
  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]
  
  # The Flop
  cards_dealt_in <- sample(length(remaining_cards), 3, replace = FALSE)
  flop_cards <- data.frame(matrix(remaining_cards[cards_dealt_in], byrow = TRUE, nrow = nrow(round), ncol = 3))
  colnames(flop_cards) <- c("flop_card1", "flop_card2", "flop_card3")
  round <- cbind(round, flop_cards)
  # Remove dealt cards from deck
  remaining_cards <- remaining_cards[-cards_dealt_in]
  
  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]
  
  # The Turn
  cards_dealt_in <- sample(length(remaining_cards), 1)
  round$turn_card <- remaining_cards[cards_dealt_in]
  # Remove dealt cards from deck
  remaining_cards <- remaining_cards[-cards_dealt_in]
  
  # Burn one card
  burn <- sample(length(remaining_cards), 1)
  remaining_cards <- remaining_cards[-burn]
  
  # The River
  cards_dealt_in <- sample(length(remaining_cards), 1)
  round$river_card <- remaining_cards[cards_dealt_in]
  # Not run: # Save the final deck in case we want to analyze it later
  # final.deck <- final.deck[-cards_dealt_in, ]
  
  # Convert cards to factors for speed
  # round$pocket_card1 <- factor(round$pocket_card1, levels = levels(final.deck$card_cd))
  # round$pocket_card2 <- factor(round$pocket_card2, levels = levels(final.deck$card_cd))
  # round$flop_card1 <- factor(round$flop_card1, levels = levels(final.deck$card_cd))
  # round$flop_card2 <- factor(round$flop_card2, levels = levels(final.deck$card_cd))
  # round$flop_card3 <- factor(round$flop_card3, levels = levels(final.deck$card_cd))
  # round$turn_card <- factor(round$turn_card, levels = levels(final.deck$card_cd))
  # round$river_card <- factor(round$river_card, levels = levels(final.deck$card_cd))
  return(round)
}
#########################################################################################################################
# Test ##################################################################################################################
# # Test the code
# n.decks <- 1
# # Define a deck of cards
# values <- c("Ace", 2:10, "Jack", "Queen", "King")
# suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
# deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
# colnames(deck) <- c("Value", "Suit")
# deck$card_pt <- c(14, 2:10, 11:13)
# deck$card_cd <- paste(substr(deck$Suit, 1, 1), sprintf("%02d", deck$card_pt), sep = "")
# # Define the final deck based on n.decks
# cards <- do.call("rbind", replicate(n.decks, deck, simplify = FALSE))
# 
# # Test 1
# n.players <- 1
# round <- data.frame(iter = x, player_id = 1:n.players)
# test <- deal_TexasHoldEm(round, cards)
# 
# # Test 2
# n.players <- 21
# round <- data.frame(iter = x, player_id = 1:n.players)
# test <- deal_TexasHoldEm(round, cards)
# # Should be 47 (21*2 + 5 community cards)
# length(unique(do.call(c, test[,3:ncol(test)])))
# 
# # Test 3
# n.players <- 22
# round <- data.frame(iter = x, player_id = 1:n.players)
# # Should fail
# deal_TexasHoldEm(round, cards)

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################