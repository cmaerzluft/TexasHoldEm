#########################################################################################################################
#                                                                                                                       #
#  Title: Simulate Hold'Em
#  Author: Christopher Maerzluft
#  Description: Simulates all the games of Texas Hold'em that we desire
#  Last Edit: 3/05/19
#                                                                                                                       #
#########################################################################################################################
# Prepare Environment ###################################################################################################
rm(list = ls())
gc()
options(scipen = 999)
library(knitr)
source("R/deal_TexasHoldEm.R")
source("R/score_TexasHoldEm.R")
set.seed(12345)

#########################################################################################################################
# Define the game #######################################################################################################
min_players <- 2 # can vary from 2-10
max_players <- 10 # can vary from 2-10
n.decks <- 1 # if this isn't one the code gets more complicated
iters <- 1000000
# Define a deck of cards
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(deck) <- c("Value", "Suit")
deck$value_pt <- c(14, 2:10, 11:13)
deck$hand_cd <- paste(substr(deck$Suit, 1, 1), sprintf("%02d", deck$value_pt), sep = "")
# Define the final deck based on n.decks
full_deck <- do.call("rbind", replicate(n.decks, deck, simplify = FALSE))

#########################################################################################################################
# Simulate the games ####################################################################################################
for (i1 in min_players:max_players) {
  n.players <<- i1
  games <- do.call(rbind, lapply(1:iters, function(x) {
    if (x%%1000 == 0) { print(x%/%1000) }
    round <- data.frame(iter = x, player_id = 1:n.players)
    round <- deal_TexasHoldEm(round = round, full_deck = full_deck)
    round <- score_TexasHoldEm(round = round)
    round$hand_place <- rank(-round$final_hand_points, ties.method = "min")
    round$winner <- round$hand_place == min(round$hand_place)
    round
  }))
  
  #########################################################################################################################
  # Save Resuls ###########################################################################################################
  iters.chr <- gsub("000$", "k", iters)
  iters.chr <- gsub("000", "k", iters.chr)
  iters.chr <- gsub("kk$", "M", iters.chr)
  filename <- paste("Data/", "sim", iters.chr, "games", "_", n.players, "players", ".Rdata", sep = "")
  save(games, file = filename)
}