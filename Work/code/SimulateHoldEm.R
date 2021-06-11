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
devtools::document()
devtools::load_all()
set.seed(12345)
bad_data <- NULL

#########################################################################################################################
# Define the game #######################################################################################################
min_players <- 1 # can vary from 2-10
max_players <- 10 # can vary from 2-10
iters <- 1

library(matrixStats)
library(dplyr)
library(tidyr)
library(rlang)
library(data.table)

# CM NOTE: Need to write UNIT TESTS for score_cards
# CM NOTE: Get an Error - Error in if (n_hands == 0) { : argument is of length zero but seemingly random
#   UNIT TESTS should help with this
# CM NOTE: Maybe should create a function that converts card codes into proper format (map someones personal definition
#   to 102, 103, ..., 414 for example). There could be other package specific identities that could be controlled by the
#   TexasHoldEm class
# CM NOTE: Need to write score function with Bit methodology:
#   https://www.codeproject.com/Articles/569271/A-Poker-hand-analyzer-in-JavaScript-using-bit-math
#   Need to find vectorized bit functions in R though (or make them?)
# CM NOTE: tidy method score_games is really slow... surely we can speed it up
# CM NOTE: Idea for simulation. We should be able to create a imabc style simulation model. Use real data to calibrate
#   towards a number of bluffs (probability of betting despite bad hand), etc.
games <- deal_cards(n_players = min_players, n_hands = iters, method = "base")
games <- score_games(games)
#
# #########################################################################################################################
# # Simulate the games ####################################################################################################
# start_time <- proc.time()
# for (i1 in min_players:max_players) {
#   it_start_time <- proc.time()
#   print(i1)
#
#   # Deal Texas Hold'em
#   hands <- deal_cards(n_players = i1, n_hands = iters)
#   it_hands_dealth <- proc.time()
#   print("Hands Dealt")
#   print(it_hands_dealth - it_start_time)
#   games <- do.call(rbind, hands)
#
#   print("Score Function")
#   games <- score_TexasHoldEm(games = games, verbose = TRUE)
#   # games2 <- score_TexasHoldEm_fn2(games = games, verbose = TRUE)
#   it_hands_scored <- proc.time()
#   print("Hands Scored")
#   print(it_hands_scored - it_hands_dealth)
#   games <- games %>% group_by(hand_id) %>%
#     mutate(
#       hand_place = rank(desc(final_hand_points), ties.method = "min"),
#       winner = hand_place == min(hand_place)
#     )
#
#   #########################################################################################################################
#   # Save Resuls ###########################################################################################################
#   print("Save Results")
#   iters.chr <- gsub("000$", "k", iters)
#   iters.chr <- gsub("000", "k", iters.chr)
#   iters.chr <- gsub("kk$", "M", iters.chr)
#   filename <- paste("data-raw/Simulated/", "sim", iters.chr, "games", "_", i1, "players", ".Rdata", sep = "")
#   save(games, file = filename)
#   rm(games)
#   print("Iteration Time")
#   print(proc.time() - it_start_time)
# }
# print("Complete Time")
# print(proc.time() - start_time)

# CM NOTE: Timing elements
# set.seed(12345)
# min_players <- 10 # can vary from 2-10
# max_players <- 10 # can vary from 2-10
# iters <- 10000
# deals_bench <- bench::mark(
#   check = FALSE, iterations = 30,
#   base = { games_base <- deal_cards(n_players = min_players, n_hands = iters, method = "base") },
#   tidy = { games_tidy <- deal_cards(n_players = min_players, n_hands = iters, method = "tidy") },
#   dtab = { games_dtab <- deal_cards(n_players = min_players, n_hands = iters, method = "dt") }
# )
# # # A tibble: 3 x 13
# # expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# # <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
# # base         97.8ms  101.5ms      9.85    59.7MB    163.      2    33      203ms
# # tidy           93ms     93ms     10.8     36.1MB    333.      1    31       93ms
# # dtab         72.8ms   75.1ms     13.4     46.4MB     60.1     6    27      449ms
# score_bench <- bench::mark(
#   check = FALSE, iterations = 30,
#   base = { games_base <- score_games(games_base) },
#   tidy = { games_tidy <- score_games(games_tidy) },
#   dtab = { games_dtab <- score_games(games_dtab) }
# )
# # # A tibble: 3 x 13
# # expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# # <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
# # base       677.87ms 821.58ms    1.20    470.61MB     8.18    30   204        25s
# # tidy         24.42s   26.07s    0.0382    1.16GB     4.82    30  3780      13.1m
# # dtab          1.03s    1.25s    0.807   798.59MB     9.28    30   345      37.2s

set.seed(12345)
deal_timing_results <- list()
score_timing_results <- list()
for (players_i1 in c(2, 10)) {
  for (hands_i1 in c(1, seq(0, 500, 100)[-1])) {
    run_name <- sprintf("Players%s_Hands%s", players_i1, hands_i1)
    print(run_name)
    deal_timing_results[[run_name]] <- bench::mark(
      check = FALSE, min_iterations = 1,
      base = { base <- deal_cards(n_players = players_i1, n_hands = hands_i1, method = "base") }#,
      # tidy = { tidy <- deal_cards(n_players = players_i1, n_hands = hands_i1, method = "tidy") },
      # dtab = { dtab <- deal_cards(n_players = players_i1, n_hands = hands_i1, method = "dt") }
    ) %>%
      mutate(
        expression = names(expression),
        hands = hands_i1,
        players = players_i1
        ) %>%
      select(expression, players, hands, min, median, `itr/sec`, mem_alloc, n_itr, total_time)

    score_timing_results[[run_name]] <- bench::mark(
      check = FALSE, min_iterations = 1,
      base = { score_games(base) }#,
      # tidy = { score_games(tidy) },
      # dtab = { score_games(dtab) }
    ) %>%
      mutate(
        expression = names(expression),
        hands = hands_i1,
        players = players_i1
      ) %>%
      select(expression, players, hands, min, median, `itr/sec`, mem_alloc, n_itr, total_time)
  }
}
deal_timing_results <- do.call(rbind, deal_timing_results)
score_timing_results <- do.call(rbind, score_timing_results)
saveRDS(deal_timing_results, "data-raw/deal_cards_timings.rds")
saveRDS(score_timing_results, "data-raw/score_timing_results.rds")
# Timing Plot IDEAS:
# deal_cards_timings <- readRDS("data-raw/deal_cards_timings.rds")
# library(ggplot2)
# ggplot() +
#   geom_line(data = deal_cards_timings %>% filter(players == 2), aes(x = hands, y = median, linetype = method), color = "blue") +
#   geom_line(data = deal_cards_timings %>% filter(players == 10), aes(x = hands, y = median, linetype = method), color = "red")
# ggplot() +
#   geom_line(data = deal_cards_timings %>% filter(players == 2), aes(x = hands, y = `itr/sec`, linetype = method), color = "blue") +
#   geom_line(data = deal_cards_timings %>% filter(players == 10), aes(x = hands, y = `itr/sec`, linetype = method), color = "red")
# ggplot() +
#   geom_line(data = deal_cards_timings %>% filter(players == 2), aes(x = hands, y = mem_alloc, linetype = method), color = "blue") +
#   geom_line(data = deal_cards_timings %>% filter(players == 10), aes(x = hands, y = mem_alloc, linetype = method), color = "red")
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
