#########################################################################################################################
#                                                                                                                       #
#  Title: Simulate Hold'Em
#  Author: Christopher Maerzluft
#  Description: Simulates all the games of Texas Hold'em that we desire
#  Last Edit:
#                                                                                                                       #
#########################################################################################################################
# Prepare Environment ###################################################################################################
rm(list = ls())
gc()
library(knitr)
source("R/deal_TexasHoldEm.R")
source("R/score_TexasHoldEm.R")
set.seed(12345)

#########################################################################################################################
# Define the game #######################################################################################################
n.players <- 8
n.decks <- 1 # if this isn't one the code gets more complicated
iters <- 100000
# Define a deck of cards
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(deck) <- c("Value", "Suit")
deck$value_pt <- c(14, 2:9, 10:13)
deck$hand_cd <- paste(substr(deck$Suit, 1, 1), sprintf("%02d", deck$value_pt), sep = "")
# Define the final deck based on n.decks
full_deck <- do.call("rbind", replicate(n.decks, deck, simplify = FALSE))

# Simulate the games ####################################################################################################
games <- do.call(rbind, lapply(1:iters, function(x) {
  print(x)
  round <- data.frame(iter = x, player_id = 1:n.players)
  round <- deal_TexasHoldEm(round = round, full_deck = full_deck)
  round <- score_TexasHoldEm(round = round)
  round$hand_place <- rank(-round$final_hand_points)
  round$winner <- round$hand_place == min(round$hand_place)
  round
}))

save(games, file = paste("Output/", "simulated_", iters, "_games.Rdata", sep = ""))
