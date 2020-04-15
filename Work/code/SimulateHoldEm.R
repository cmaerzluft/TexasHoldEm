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
devtools::load_all()
# Vectorize are hand creation function(s)
# hand_vec <- Vectorize(new_hand, vectorize.args = "hand_id")
hand_vec <- Vectorize(new_hand, vectorize.args = "hand_id", SIMPLIFY = FALSE)
set.seed(12345)

#########################################################################################################################
# Define the game #######################################################################################################
min_players <- 8 # can vary from 2-10
max_players <- 10 # can vary from 2-10
iters <- 100000

# Define a deck of cards
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
full_deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(full_deck) <- c("Value", "Suit")
full_deck$card_pt <- c(14, 2:10, 11:13)
full_deck$card_cd <- as.integer(sprintf("%s%02d", as.numeric(factor(full_deck$Suit)), full_deck$card_pt))

#########################################################################################################################
# Simulate the games ####################################################################################################
start_time <- proc.time()
for (i1 in min_players:max_players) {
  it_start_time <- proc.time()
  print(i1)

  # Deal Texas Hold'em
  hands <- hand_vec(cards = full_deck$card_cd, chairs = i1, hand_id = 1:iters)
  it_hands_dealth <- proc.time()
  print("Hands Dealt")
  print(it_hands_dealth - it_start_time)
  games <- do.call(rbind, hands)

  print("Score Function")
  games <- score_TexasHoldEm(games = games, verbose = TRUE)
  # games2 <- score_TexasHoldEm_fn2(games = games, verbose = TRUE)
  it_hands_scored <- proc.time()
  print("Hands Scored")
  print(it_hands_scored - it_hands_dealth)
  games <- games %>% group_by(hand_id) %>%
    mutate(
      hand_place = rank(desc(final_hand_points), ties.method = "min"),
      winner = hand_place == min(hand_place)
    )

  #########################################################################################################################
  # Save Resuls ###########################################################################################################
  print("Save Results")
  iters.chr <- gsub("000$", "k", iters)
  iters.chr <- gsub("000", "k", iters.chr)
  iters.chr <- gsub("kk$", "M", iters.chr)
  filename <- paste("data-raw/Simulated/", "sim", iters.chr, "games", "_", i1, "players", ".Rdata", sep = "")
  save(games, file = filename)
  rm(games)
  print("Iteration Time")
  print(proc.time() - it_start_time)
}
print("Complete Time")
print(proc.time() - start_time)
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
