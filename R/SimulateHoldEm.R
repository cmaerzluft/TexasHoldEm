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
# library(knitr)
library(dplyr)
library(microbenchmark)
source("R/deal_TexasHoldEm.R")
source("R/score_TexasHoldEm.R")
source("R/score_TexasHoldEm_v2.R")
set.seed(12345)

#########################################################################################################################
# Define the game #######################################################################################################
min_players <- 2 # can vary from 2-10
max_players <- 10 # can vary from 2-10
iters <- 1
# Define a deck of cards
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
full_deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(full_deck) <- c("Value", "Suit")
full_deck$card_pt <- c(14, 2:10, 11:13)
full_deck$card_cd <- paste(substr(full_deck$Suit, 1, 1), sprintf("%02d", full_deck$card_pt), sep = "")
full_deck$card_cd <- factor(full_deck$card_cd, levels = unique(full_deck$card_cd))

# Use integers for efficient data size and speed
values <- c(14, 2:10, 11:13)
suits <- c(1:4)
full_deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(full_deck) <- c("Value", "Suit")
full_deck$card_cd <- as.integer(paste(full_deck$Suit, sprintf("%02d", full_deck$Value), sep = ""))

#########################################################################################################################
# Simulate the games ####################################################################################################
for (i1 in min_players:max_players) {
  print(i1)
  n.players <- i1

  games <- do.call(rbind, lapply(1:iters, function(x, n.players) {
    if (x%%1000 == 0) { print(x%/%1000) }
    round <- data.frame(iter = x, player_id = 1:n.players)
    round <- deal_TexasHoldEm(round = round, cards = full_deck$card_cd)
  }, n.players = n.players))
  games <- score_TexasHoldEm2(games = games)
  games <- games %>% group_by(iter) %>%
    mutate(
      hand_place = rank(desc(final_hand_points), ties.method = "min"),
      winner = hand_place == min(hand_place)
    )
  
  #########################################################################################################################
  # Save Resuls ###########################################################################################################
  iters.chr <- gsub("000$", "k", iters)
  iters.chr <- gsub("000", "k", iters.chr)
  iters.chr <- gsub("kk$", "M", iters.chr)
  filename <- paste("Data/", "sim", iters.chr, "games", "_", n.players, "players", ".Rdata", sep = "")
  save(games, file = filename)
  rm(games)
}
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################