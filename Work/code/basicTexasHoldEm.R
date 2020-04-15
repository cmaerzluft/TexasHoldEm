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
files <- grep(sprintf("sim%s%sgames", datasize, units), list.files("data-raw/Simulated"), value = TRUE)
# Make sure 10 is moved to last spot
files <- files[order(nchar(files), files)]
games_list <- list()
for (i1 in files) {
  print(i1)
  load(sprintf("data-raw/Simulated/%s", i1))
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
# Basic Probabilities ###################################################################################################
# Looking at the end result
# What hands win?
hand_win_tables <- lapply(games_list, function(x) {
  # What are chances a hand wins given you have it
  win_given_hand <- as.data.frame(prop.table(table(x$final_hand_type, x$winner), margin = 1))
  colnames(win_given_hand) <- c("Hand", "Win", "Percentage")
  win_given_hand$Comparison <- "Given Hand"
  # What are chances a hand wins ever
  win_overall <- as.data.frame(prop.table(table(x$final_hand_type, x$winner), margin = 2))
  colnames(win_overall) <- c("Hand", "Win", "Percentage")
  win_overall$Comparison <- "Overall"
  win_pct <- rbind(win_given_hand, win_overall)
  win_pct <- win_pct[win_pct$Win == TRUE, ]
  win_pct$Percentage[is.na(win_pct$Percentage)] <- 0
  win_pct$Comparison <- factor(win_pct$Comparison, levels = c("Overall", "Given Hand"))

  return(win_pct)
})
# Plot
hand_win_plots <- lapply(seq_along(hand_win_tables), function(x, df_list, name) {
  title <- paste(gsub("players", "", name[x]), "Players", sep = " ")
  p <- ggplot(data = df_list[[x]]) +
    geom_bar(aes(x = Hand, y = Percentage, fill = Comparison), stat = "identity", position = "dodge") +
    geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    # scale_fill_manual("How often does a hand win:", values = plt_colordiscr5[c(3, 1)]) +
    scale_fill_manual("How often does a hand win:", values = plt_colordiscr6[c(2, 5)]) +
    ggtitle(title) + plt_theme + theme(legend.position = "bottom")

  return(p)
}, df_list = hand_win_tables, name = names(hand_win_tables))
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

# What wins, when a good hand loses
# Why does given a flush have a smaller pct of winning than given a straight?
#   hypothesis, when a flush wins, another flush loses more often than a straight losing to a straight
# Does this control for the fact that the straight has more hands that can beat it anyways?
beaters <- function(gList, hand) {
  table_sizes <- as.numeric(gsub("players", "", names(gList)))
  tab <- do.call(rbind, lapply(seq_along(gList), function(x, df_list, n_players) {
    df <- df_list[[x]]
    loses <- df[df$hand_id %in% df$hand_id[df$winner == FALSE & df$final_hand_type == hand], ]
    dta <- as.data.frame(prop.table(table(loses$final_hand_type, loses$winner), margin = 2))
    colnames(dta) <- c("Hand", "Win", "Percentage")
    dta <- dta[dta$Win == TRUE & dta$Percentage > 0, ]
    dta$Players <- factor(n_players[x], levels = 2:10)
    return(dta)
  }, df_list = gList, n_players = table_sizes))
  return(tab)
}
fullhouse_loses <- beaters(games_list, "Full House")
fullhouse_loses$Hand <- droplevels(fullhouse_loses$Hand)
fullhouse_loses <- complete(fullhouse_loses, Hand, Players)
lose_fullhouse <- ggplot(data = fullhouse_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
  scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  ggtitle("When a Full House Loses") + plt_theme

flush_loses <- beaters(games_list, "Flush")
flush_loses$Hand <- droplevels(flush_loses$Hand)
flush_loses <- complete(flush_loses, Hand, Players)
lose_flush <- ggplot(data = flush_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
  scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  ggtitle("When a Flush Loses") + plt_theme

straight_loses <- beaters(games_list, "Straight")
straight_loses$Hand <- droplevels(straight_loses$Hand)
straight_loses <- complete(straight_loses, Hand, Players)
lose_straight <- ggplot(data = straight_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
  scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  ggtitle("When a Straight Loses") + plt_theme

kind3_loses <- beaters(games_list, "Three of a Kind")
kind3_loses$Hand <- droplevels(kind3_loses$Hand)
kind3_loses <- complete(kind3_loses, Hand, Players)
lose_3kind <- ggplot(data = kind3_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
  scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  ggtitle("When a Three of a Kind Loses") + plt_theme
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

# How often do you get two starting cards and how often do you win given two starting cards
pocket_information <- do.call(rbind, lapply(games_list, function(x) {
  # title <- "Pocket Cards Chances of Winning"
  x$pocket_card1 <- x$pocket_card1 %% 100
  x$pocket_card2 <- x$pocket_card2 %% 100
  x$Win <- ifelse(x$winner == TRUE, "Winner", "Loser")
  tmp <- x %>% group_by(pocket_card1, pocket_card2, suited_pocket, Win) %>%
    summarise(N = n()) %>%
    spread(Win, N, fill = 0) %>%
    arrange(pocket_card1, pocket_card2)
  tmp <- left_join(tmp, tmp, by = c("pocket_card1" = "pocket_card2", "pocket_card2" = "pocket_card1", "suited_pocket"))
  tmp$Winner <- ifelse(tmp$pocket_card1 == tmp$pocket_card2, tmp$Winner.x, tmp$Winner.x + tmp$Winner.y)
  tmp$Loser <- ifelse(tmp$pocket_card1 == tmp$pocket_card2, tmp$Loser.x, tmp$Loser.x + tmp$Loser.y)
  tmp <- tmp[, c("pocket_card1", "pocket_card2", "suited_pocket", "Winner", "Loser")]
  tmp$pct_get <- (tmp$Winner + tmp$Loser)/sum(tmp[, c("Winner", "Loser")])
  tmp$pct_win <- tmp$Winner/rowSums(tmp[, c("Winner", "Loser")])
  # Pairs
  tmp1 <- tmp[tmp$pocket_card1 == tmp$pocket_card2, ]
  # Unique Suited Cards
  tmp2 <- tmp[tmp$suited_pocket & tmp$pocket_card1 != tmp$pocket_card2, ]
  tmp2 <- tmp2[tmp2$pocket_card1 <= tmp2$pocket_card2, ]
  # Unique Non-suited Cards
  tmp3 <- tmp[!tmp$suited_pocket & tmp$pocket_card1 != tmp$pocket_card2, ]
  tmp3 <- tmp3[tmp3$pocket_card1 > tmp3$pocket_card2, ]
  tmp.new <- rbind(tmp1, tmp2, tmp3)

  # Control card values
  tmp.new$pocket_card1 <- gsub(11, "Ja", tmp.new$pocket_card1)
  tmp.new$pocket_card1 <- gsub(12, "Qu", tmp.new$pocket_card1)
  tmp.new$pocket_card1 <- gsub(13, "Ki", tmp.new$pocket_card1)
  tmp.new$pocket_card1 <- gsub(14, "Ac", tmp.new$pocket_card1)
  tmp.new$pocket_card2 <- gsub(11, "Ja", tmp.new$pocket_card2)
  tmp.new$pocket_card2 <- gsub(12, "Qu", tmp.new$pocket_card2)
  tmp.new$pocket_card2 <- gsub(13, "Ki", tmp.new$pocket_card2)
  tmp.new$pocket_card2 <- gsub(14, "Ac", tmp.new$pocket_card2)

  # Number of players
  tmp.new$Players <- max(table(x$hand_id[1:20])) # So we don't rely on player_id and don't have to table everything

  # Return
  tmp.new
}))
# Normalized versions of win percentages
pocket_information <- pocket_information %>%
  group_by(pocket_card1, pocket_card2) %>% mutate(
    min_win_pct = round(min(pct_win)*100, 0),
    max_win_pct = round(max(pct_win)*100, 0),
    avg_win_pct = round(mean(pct_win)*100, 0)
  ) %>%
  group_by(Players) %>% mutate(
    norm_win_pct = (min(pct_win) - pct_win)/(min(pct_win) - max(pct_win))
  ) %>% ungroup()
cards <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Ja", "Qu", "Ki", "Ac")
pocket_information$pocket_card1 <- factor(pocket_information$pocket_card1, levels = cards)
pocket_information$pocket_card2 <- factor(pocket_information$pocket_card2, levels = cards)

# Chance of getting all combinations of pocket cards
pocket_get_likeli <- lapply(unique(pocket_information$Players), function(x, df) {
  title <- "Chance of Getting Pocket Cards"
  df <- df[df$Players == x, ]
  df$Label <- round(df$pct_get*100, 2)
  df$Label[df$pct_get <= quantile(df$pct_get, .5)] <- ""

  # Plot
  ggplot(data = df, aes(x = pocket_card1, y = pocket_card2)) +
    geom_tile(aes(fill = suited_pocket)) +
    geom_point(aes(color = pct_get, size = pct_get)) +
    geom_text(aes(x = pocket_card1, y = pocket_card2, label = Label), fontface = "bold", size = 7, color = "black") +
    scale_x_discrete(position = "top", limits = rev(levels(df$pocket_card1))) +
    scale_size_continuous("Probability of\nGetting", guide = "legend", range = c(1, 20), labels = percent) +
    scale_color_gradientn("Probability of\nGetting", guide = "legend", colours = plt_colorscale, labels = percent) +
    scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = plt_background), breaks = "FALSE", labels = "No") +
    labs(title = title, subtitle = paste(x, "Players", sep = " "), x = "", y = '') +
    plt_theme
}, df = pocket_information)
# pocket_get_likeli
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

# Chance of winning by number of players
pocket_win_likeli <- lapply(unique(pocket_information$Players), function(x, df) {
  title <- "Chance of Winning with Pocket Cards"
  df <- df[df$Players == x, ]
  df$Label <- round(df$pct_win*100, 0)
  df$Label[df$pct_win <= quantile(df$pct_win, .75)] <- ""

  # Plot
  ggplot(data = df, aes(x = pocket_card1, y = pocket_card2)) +
    geom_tile(aes(fill = suited_pocket)) +
    geom_point(aes(color = pct_win, size = pct_win)) +
    geom_text(aes(x = pocket_card1, y = pocket_card2, label = Label), fontface = "bold", size = 7, color = "black") +
    scale_x_discrete(position = "top", limits = rev(levels(df$pocket_card1))) +
    scale_size_continuous("Probability of\nWinning", guide = "legend", range = c(1, 20), limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2), labels = percent) +
    scale_color_gradientn("Probability of\nWinning", guide = "legend", colours = plt_colorscale, limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2), labels = percent) +
    scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = plt_background), breaks = "FALSE", labels = "No") +
    labs(title = title, subtitle = paste(x, "Players", sep = " "), x = "", y = "") +
    plt_theme
}, df = pocket_information)
# pocket_win_likeli
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

title <- "Chance of Winning with Pocket Cards"
win_likeli_all <- ggplot(data = pocket_information, aes(x = pocket_card1, y = pocket_card2)) +
  geom_tile(aes(fill = suited_pocket)) +
  geom_point(aes(colour = pct_win, size = pct_win)) +
  scale_x_discrete(position = "top", limits = rev(levels(pocket_information$pocket_card1))) +
  scale_size_continuous("Probability of\nWinning", guide = "legend", range = c(1, 20), breaks = seq(0, 1, 0.2), labels = percent) +
  scale_color_gradientn("Probability of\nWinning", guide = "legend", colours = plt_colorscale, breaks = seq(0, 1, 0.2), labels = percent) +
  scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = "#252732"), breaks = "FALSE", labels = "No") +
  labs(title = title, subtitle = "2 - 10 Players", x = "", y = "",
       caption = "The rings represents the probability that a person with the corresponding pocket cards will ultimately win the hand for a given number of players.\nLogically the smallest circle will be when there are more players (10 in this case) and the outer ring will be for the fewest number of players (2 in this case).") +
  plt_theme
win_likeli_all
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

# Relative Hand strength
# CM NOTE: I don't think I quite like this one because you will only see the increase in relative hand strength and not
#   the decreases (because the first point will be on top of the second point and if it is a decrease the top point will
#   cover up the second point)
# pocket_information$Label <- ""
# GetsLabel <- pocket_information$avg_win_pct > quantile(pocket_information$avg_win_pct, 0.75) & pocket_information$Players == 2
# pocket_information$Label[GetsLabel] <- paste(pocket_information$min_win_pct[GetsLabel], pocket_information$max_win_pct[GetsLabel], sep = "-")
# lab_function <- function(x) {
#   ifelse(x == 0.05, "Weakest",
#          ifelse(x == max(x), "Strongest", ""))
# }
# title <- "Relative Strength of each pair"
# ggplot(data = pocket_information, aes(x = pocket_card1, y = pocket_card2)) +
#   geom_tile(aes(fill = suited_pocket)) +
#   geom_point(aes(colour = norm_win_pct, size = norm_win_pct))+#, shape = 21, show.legend = FALSE) +
#   geom_text(aes(x = pocket_card1, y = pocket_card2, label = Label), fontface = "bold", size = 4, color = "black") +
#   scale_x_discrete(position = "top", limits = rev(levels(pocket_information$pocket_card1))) +
#   scale_size_continuous("Relative Hand\nStrength", guide = "legend", range = c(1, 20), breaks = c(0.05, 0.5, 1), labels = lab_function) +
#   scale_color_gradientn("Relative Hand\nStrength", guide = "legend", colours = plot_colors, breaks = c(0.05, 0.5, 1), labels = lab_function) +
#   scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = "#000000"), breaks = "FALSE", labels = "No", guide = guide_legend(label.hjust = -0.5)) +
#   labs(title = title, subtitle = "2 - 10 Players", x = "", y = "",
#        caption = "The rings represents the relative strength that a pair of pocket cards has. If there are differences between the relative strengths when there are differing number of players, rings will appear.") +
#   theme(panel.background = element_blank(),
#         plot.background = element_rect(fill = "black"),
#         legend.text = element_text(colour = "white"),
#         # legend.position = "bottom",
#         legend.title = element_text(colour = "white"),
#         legend.background = element_rect(fill = "black"),
#         legend.key = element_rect(fill = "black", color = "black"),
#         axis.text = element_text(size = 16, colour = "white"),
#         panel.grid.major = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 24, colour = "white"),
#         plot.subtitle = element_text(hjust = 0.5, size = 18, colour = "white"),
#         plot.caption = element_text(hjust = 0, size = 12, colour = "white"))
# ggsave("Work/TestPlot.png", dpi = 1200, height = 10, width = 15)

# Overall flow of hands from pocket cards to winning or losing
# CM NOTE: I deal would be Sankey Diagram but I haven't been able to implement this as cleaning as I would like. The
#   best method was plotly but it does not give as much control as I would like. ggplot is possible but it would take
#   alot of work to really do properly.
#   https://www.r-bloggers.com/generating-sankey-diagrams-from-rcharts/
#   https://stackoverflow.com/questions/50395027/beautifying-sankey-alluvial-visualization-using-r
#   https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# # Sankey Diagram
# # Handle data
# x <- games_list[[9]]
# x$Card1 <- gsub("[A-z]", "", x$pocket_card1)
# x$Card2 <- gsub("[A-z]", "", x$pocket_card2)
# tmp <- as.data.frame(t(apply(x[, c("Card1", "Card2")], MARGIN = 1, FUN = sort)))
# colnames(tmp) <- c("Card1", "Card2")
# tmp$Pocket <- paste(tmp$Card1, tmp$Card2, sep = ", ")
# tmp <- cbind(tmp, x[, c("suited_pocket", "flop_hand_type", "turn_hand_type", "final_hand_type", "winner")])
# tmp$suited_pocket <- ifelse(tmp$suited_pocket, "Suited", "Off-suit")
# tmp$winner <- ifelse(tmp$winner, "Won", "Lost")
# tmp$winner <- factor(tmp$winner, levels = c("Won", "Lost"))
# # Create tables of transition
# first_tran <- as.data.frame(table(tmp$Pocket, tmp$flop_hand_type), stringsAsFactors = FALSE)
# colnames(first_tran) <- c("source", "target", "value")
# first_tran$source <- gsub("11", "Ja", first_tran$source)
# first_tran$source <- gsub("12", "Qu", first_tran$source)
# first_tran$source <- gsub("13", "Ki", first_tran$source)
# first_tran$source <- gsub("14", "Ac", first_tran$source)
# first_tran$source <- factor(first_tran$source)
# first_tran$target <- factor(first_tran$target, levels = hand_lvls)
# # fourth column is post turn hand type
# #   should just be a table of flop+hand_type by turn_hand_type
# third_tran <- as.data.frame(table(tmp$flop_hand_type, tmp$turn_hand_type), stringsAsFactors = FALSE)
# colnames(third_tran) <- c("source", "target", "value")
# third_tran$source <- factor(third_tran$source, levels = hand_lvls)
# third_tran$target <- factor(third_tran$target, levels = hand_lvls)
# # fifth column is post river hand type
# #   should just be a table of turn_hand_type by final_hand_type
# fourth_tran <- as.data.frame(table(tmp$turn_hand_type, tmp$final_hand_type), stringsAsFactors = FALSE)
# colnames(fourth_tran) <- c("source", "target", "value")
# fourth_tran$source <- factor(fourth_tran$source, levels = hand_lvls)
# fourth_tran$target <- factor(fourth_tran$target, levels = hand_lvls)
# # sixth column is winner/loser
# #   should just be a table of final_hand_type by winner
# fifth_tran <- as.data.frame(table(tmp$final_hand_type, tmp$winner), stringsAsFactors = FALSE)
# colnames(fifth_tran) <- c("source", "target", "value")
# fifth_tran$source <- factor(fifth_tran$source, levels = hand_lvls)
# fifth_tran$target <- factor(fifth_tran$target, levels = c("Won", "Lost"))
# nodes <- c(
#   levels(first_tran$source), # Node 0
#   levels(first_tran$target),
#   levels(third_tran$target),
#   levels(fourth_tran$target),
#   levels(fifth_tran$target)
# )
# first_tran$source <- as.numeric(first_tran$source) - 1
# first_tran$target <- as.numeric(first_tran$target) + max(first_tran$source)
# third_tran$source <- as.numeric(third_tran$source) + max(first_tran$source)
# third_tran$target <- as.numeric(third_tran$target) + max(third_tran$source)
# fourth_tran$source <- as.numeric(fourth_tran$source) + max(third_tran$source)
# fourth_tran$target <- as.numeric(fourth_tran$target) + max(fourth_tran$source)
# fifth_tran$source <- as.numeric(fifth_tran$source) + max(fourth_tran$source)
# fifth_tran$target <- as.numeric(fifth_tran$target) + max(fifth_tran$source)
# links <- rbind(first_tran, third_tran, fourth_tran, fifth_tran)
# library(plotly)
# library(RColorBrewer)
# cols <- brewer.pal(12, "Set3")
# p <- plot_ly(
#   type = "sankey",
#   orientation = "h",
#   arrangement = "perpendicular",
#   domain = list(
#     x =  c(0,1),
#     y =  c(0,1)
#   ),
#   node = list(
#     label = nodes,
#     color = c(rep(cols[1], nrow(first_tran)/length(hand_lvls)), rep(cols[4:12], 3), cols[2:3]),
#     pad = 10000,
#     thickness = 20,
#     line = list(
#       color = "black",
#       width = 0.5
#     )
#   ),
#
#   link = list(
#     source = links$source,
#     target = links$target,
#     value =  links$value
#   )
# ) %>%
#   layout(
#     title = "Hand Flow",
#     font = list(
#       size = 10
#     )
#   )

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
