#########################################################################################################################
#                                                                                                                       #
#  Title: Basic Texas Hold'em
#  Author: Chris Maerzluft
#  Last Edit: August 11, 2019
#                                                                                                                       #
#########################################################################################################################
# Description ###########################################################################################################
#
# Summarises the basic facts about Texas Hold'em. That is, if games were to be fully played out by every player what are
#   the best hands or cards. I use simulated data to study this. In order to bring in gambling techniques I need other
#   data. Some sources for that include:
#     https://poker.stackexchange.com/questions/881/publicly-available-poker-stats
#     http://poker.cs.ualberta.ca/irc_poker_database.html
#     http://web.archive.org/web/20110205042259/http://www.outflopped.com/questions/286/obfuscated-datamined-hand-histories
#
# Setup R ###############################################################################################################
# Clean environment
rm(list = ls(all = TRUE))
gc()

# Libraries Needed
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(wesanderson)
options(scipen = 999)

#########################################################################################################################
# Plot Options ##########################################################################################################
plt_background <- "#252732"
plt_wordscolor <- "white"
plt_colorscale <- wes_palette("Zissou1", 100, type = "continuous")
plt_colordiscr5 <- wes_palette("Zissou1", 5, type = "discrete")
plt_colordiscr6 <- c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e")

plt_theme <- theme(
  plot.background = element_rect(fill = plt_background),
  plot.title = element_text(hjust = 0.5, size = 24, colour = plt_wordscolor),
  plot.subtitle = element_text(hjust = 0.5, size = 18, colour = plt_wordscolor),
  plot.caption = element_text(hjust = 0, size = 12, colour = plt_wordscolor),

  legend.text = element_text(colour = plt_wordscolor),
  legend.title = element_text(colour = plt_wordscolor),
  legend.background = element_rect(fill = plt_background),
  legend.key = element_rect(fill = plt_background, color = plt_background),

  panel.background = element_blank(),
  panel.grid = element_blank(),

  axis.title = element_blank(),
  axis.text = element_text(size = 16, colour = plt_wordscolor),
  axis.ticks = element_blank(),
  axis.line = element_blank()
)

#########################################################################################################################
# Load Data #############################################################################################################
datasize <- 5
units <- "k"
files <- grep(sprintf("sim%s%sgames", datasize, units), list.files("data-raw/TexasHoldEm"), value = TRUE)
# Make sure 10 is moved to last spot
files <- files[order(nchar(files), files)]
games_list <- list()
for (i1 in files) {
  print(i1)
  load(sprintf("data-raw/TexasHoldEm/%s", i1))
  name <- gsub(sprintf("sim%s%sgames_", datasize, units), "", i1)
  name <- gsub("players.Rdata", "", name)
  games_list[[sprintf("players%s", name)]] <- games
}

#########################################################################################################################
# Data handling #########################################################################################################
hand_lvls <- c("Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair",
               "Pair", "High Card")
games_list <- lapply(games_list, function(x) {
  # x$flop_hand_type <- factor(x$flop_hand_type, levels = hand_lvls)
  # x$turn_hand_type <- factor(x$turn_hand_type, levels = hand_lvls)
  x$final_hand_type <- factor(x$final_hand_type, levels = hand_lvls)
  return(x)
})

#########################################################################################################################
# Table 1 ###############################################################################################################
t1 <- prop.table(table(games_list[[7]]$final_hand_type))
t1 <- sprintf("%0.4f%%", t1*100)

#########################################################################################################################
# Save Results
save(t1, file = "Work/results/2020-01-TexasHoldem-Analysis-pt1.Rdata")

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
