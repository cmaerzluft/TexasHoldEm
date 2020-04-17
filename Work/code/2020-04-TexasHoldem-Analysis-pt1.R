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
# Approx size of plots in blog: 861 x 612
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
library(grid)
library(gridExtra)
library(cowplot)
options(scipen = 999)

#########################################################################################################################
# Load Data #############################################################################################################
n_players_index <- 8 - 1
datasize <- 1
units <- "M"
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
# Plot Options ##########################################################################################################
plt_background <- "#272935"
plt_wordscolor <- "white"
plt_colorscale <- wes_palette("Zissou1", 100, type = "continuous")
plt_colordiscr5 <- wes_palette("Zissou1", 5, type = "discrete")
plt_colordiscr6 <- c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e")

plt_theme <- theme(
  plot.background = element_rect(fill = plt_background, colour = plt_background),
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
hand_distributions_8players <- prop.table(table(games_list[[n_players_index]]$final_hand_type))
hand_distributions_8players <- sprintf("%0.4f%%", hand_distributions_8players*100)

#########################################################################################################################
# Plot 1 and 2 ##########################################################################################################
# Data calculation
hand_win_tables <- lapply(games_list, function(x) {
  # What are chances a hand wins given you have it
  win_given_hand <- as.data.frame(prop.table(table(x$final_hand_type, x$winner), margin = 1))
  colnames(win_given_hand) <- c("Hand", "Win", "Percentage")
  win_given_hand$Comparison <- "Conditional"
  # What are chances a hand wins ever
  win_overall <- as.data.frame(prop.table(table(x$final_hand_type, x$winner), margin = 2))
  colnames(win_overall) <- c("Hand", "Win", "Percentage")
  win_overall$Comparison <- "Overall"
  win_pct <- rbind(win_given_hand, win_overall)
  win_pct <- win_pct[win_pct$Win == TRUE, ]
  win_pct$Percentage[is.na(win_pct$Percentage)] <- 0
  win_pct$Comparison <- factor(win_pct$Comparison, levels = c("Overall", "Conditional"))

  return(win_pct)
})
# Plot
hand_win_plots <- lapply(seq_along(hand_win_tables), function(x, df_list, name) {
  tmp_dta <- df_list[[x]]
  # title <- paste(gsub("players", "", name[x]), "Players", sep = " ")
  # p <- ggplot(data = tmp_dta) +
  #   geom_bar(aes(x = Hand, y = Percentage, fill = Comparison), stat = "identity", position = "dodge") +
  #   geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
  #   scale_y_continuous(labels = percent, limits = c(0, 1), position = "right") +
  #   scale_x_discrete(limits = rev(levels(tmp_dta$Hand))) +
  #   # scale_fill_manual("How often does a hand win:", values = plt_colordiscr5[c(3, 1)]) +
  #   scale_fill_manual("How often does a hand win?", values = plt_colordiscr6[c(2, 5)]) +
  #   plt_theme + theme(
  #     legend.position = "top",
  #     legend.text = element_text(size = 16),
  #     legend.title = element_text(size = 16)
  #   ) + coord_flip() #+ ggtitle(title)

  tmp_dta$Labels <- levels(tmp_dta$Hand)
  tmp_dta$Labels <- gsub("Straight Flush", "Straight\nFlush", tmp_dta$Labels)
  tmp_dta$Labels <- gsub("Four of a Kind", "Four of a\nKind", tmp_dta$Labels)
  tmp_dta$Labels <- gsub("Three of a Kind", "Three of a\nKind", tmp_dta$Labels)

  g.mid <- ggplot(tmp_dta) +
    geom_text(aes(x = 1, y = Hand, label = Labels), color = plt_wordscolor) +
    scale_y_discrete(limits = rev(levels(tmp_dta$Hand)), expand = c(0.082, 0.082)) +
    labs(title = "", x = NULL, y = NULL) +
    theme(
      plot.background = element_rect(fill = plt_background, colour = plt_background),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(color = NA),
      axis.ticks.x = element_line(color = NA),
      plot.margin = margin(t = 5, b = 5)
    )

  g1 <- ggplot(data = tmp_dta %>% filter(Comparison == "Overall")) +
    geom_bar(aes(x = Hand, y = Percentage), stat = "identity", fill = plt_colordiscr6[2]) +
    geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
    scale_y_reverse(labels = percent) +
    scale_x_discrete(limits = rev(levels(tmp_dta$Hand))) +
    labs(title = "Overall") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(l = 15, r = 5, t = 5, b = 5)
    ) +
    plt_theme +
    coord_flip()

  g2 <- ggplot(data = tmp_dta %>% filter(Comparison == "Conditional")) +
    geom_bar(aes(x = Hand, y = Percentage), stat = "identity", fill = plt_colordiscr6[5]) +
    geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
    scale_y_continuous(labels = percent) +
    scale_x_discrete(limits = rev(levels(tmp_dta$Hand))) +
    labs(title = "Conditional", x = NULL) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(l = 5, r = 15, t = 5, b = 5)
    ) +
    plt_theme +
    coord_flip()

  title <- ggdraw() +
    draw_label(
      paste(gsub("players", "", name[x]), "Players", sep = " "),
      size = 24,
      color = plt_wordscolor,
      fontface = "bold"
    ) +
    draw_line(
      c(0, 1), c(0.000000001, 0.000000001),
      color = plt_wordscolor,
      size = 2
    ) + theme(
      plot.background = element_rect(fill = plt_background, colour = plt_background)
    )

  plots <- plot_grid(g1, g.mid, g2, nrow = 1, rel_widths = c(45/100, 15/100, 45/100))
  p <- plot_grid(title, plots, ncol = 1, rel_heights = c(0.1, 1))

  return(p)
}, df_list = hand_win_tables, name = names(hand_win_tables))
win_distribution_2players <- hand_win_plots[[1]]
win_distribution_10players <- hand_win_plots[[9]]

#########################################################################################################################
# Plots 3 - 5 ###########################################################################################################
# Data Calculation
# beaters <- function(gList, hand) {
#   table_sizes <- as.numeric(gsub("players", "", names(gList)))
#   tab <- do.call(rbind, lapply(seq_along(gList), function(x, df_list, n_players) {
#     df <- df_list[[x]]
#     loses <- df[df$hand_id %in% df$hand_id[df$winner == FALSE & df$final_hand_type == hand], ]
#     dta <- as.data.frame(prop.table(table(loses$final_hand_type, loses$winner), margin = 2))
#     colnames(dta) <- c("Hand", "Win", "Percentage")
#     dta <- dta[dta$Win == TRUE & dta$Percentage > 0, ]
#     dta$Players <- factor(n_players[x], levels = 2:10)
#     return(dta)
#   }, df_list = gList, n_players = table_sizes))
#   return(tab)
# }
# # Plots
# # fullhouse_loses <- beaters(games_list, "Full House")
# # fullhouse_loses$Hand <- droplevels(fullhouse_loses$Hand)
# # fullhouse_loses <- complete(fullhouse_loses, Hand, Players)
# # lose_fullhouse <- ggplot(data = fullhouse_loses) +
# #   geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
# #   geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
# #   scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
# #   scale_y_continuous(labels = percent, limits = c(0, 1)) +
# #   ggtitle("When a Full House Loses") + plt_theme
# flush_loses <- beaters(games_list, "Flush")
# flush_loses$Hand <- droplevels(flush_loses$Hand)
# flush_loses <- complete(flush_loses, Hand, Players)
# lose_flush <- ggplot(data = flush_loses) +
#   geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
#   scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
#   scale_y_continuous(labels = percent, limits = c(0, 1)) +
#   ggtitle("When a Flush Loses") + plt_theme
# straight_loses <- beaters(games_list, "Straight")
# straight_loses$Hand <- droplevels(straight_loses$Hand)
# straight_loses <- complete(straight_loses, Hand, Players)
# lose_straight <- ggplot(data = straight_loses) +
#   geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
#   scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
#   scale_y_continuous(labels = percent, limits = c(0, 1)) +
#   ggtitle("When a Straight Loses") + plt_theme
# kind3_loses <- beaters(games_list, "Three of a Kind")
# kind3_loses$Hand <- droplevels(kind3_loses$Hand)
# kind3_loses <- complete(kind3_loses, Hand, Players)
# lose_3kind <- ggplot(data = kind3_loses) +
#   geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
#   geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed", color = plt_wordscolor) +
#   scale_fill_manual("Winning Hand", values = plt_colordiscr6) +
#   scale_y_continuous(labels = percent, limits = c(0, 1)) +
#   ggtitle("When a Three of a Kind Loses") + plt_theme

#########################################################################################################################
# Plots 6 - 8 ###########################################################################################################
# Data Calculation
# pocket_information <- do.call(rbind, lapply(games_list, function(x) {
#   # title <- "Pocket Cards Chances of Winning"
#   x$pocket_card1 <- x$pocket_card1 %% 100
#   x$pocket_card2 <- x$pocket_card2 %% 100
#   x$Win <- ifelse(x$winner == TRUE, "Winner", "Loser")
#   tmp <- x %>% group_by(pocket_card1, pocket_card2, suited_pocket, Win) %>%
#     summarise(N = n()) %>%
#     spread(Win, N, fill = 0) %>%
#     arrange(pocket_card1, pocket_card2)
#   tmp <- left_join(tmp, tmp, by = c("pocket_card1" = "pocket_card2", "pocket_card2" = "pocket_card1", "suited_pocket"))
#   tmp$Winner <- ifelse(tmp$pocket_card1 == tmp$pocket_card2, tmp$Winner.x, tmp$Winner.x + tmp$Winner.y)
#   tmp$Loser <- ifelse(tmp$pocket_card1 == tmp$pocket_card2, tmp$Loser.x, tmp$Loser.x + tmp$Loser.y)
#   tmp <- tmp[, c("pocket_card1", "pocket_card2", "suited_pocket", "Winner", "Loser")]
#   tmp$pct_get <- (tmp$Winner + tmp$Loser)/sum(tmp[, c("Winner", "Loser")])
#   tmp$pct_win <- tmp$Winner/rowSums(tmp[, c("Winner", "Loser")])
#   # Pairs
#   tmp1 <- tmp[tmp$pocket_card1 == tmp$pocket_card2, ]
#   # Unique Suited Cards
#   tmp2 <- tmp[tmp$suited_pocket & tmp$pocket_card1 != tmp$pocket_card2, ]
#   tmp2 <- tmp2[tmp2$pocket_card1 <= tmp2$pocket_card2, ]
#   # Unique Non-suited Cards
#   tmp3 <- tmp[!tmp$suited_pocket & tmp$pocket_card1 != tmp$pocket_card2, ]
#   tmp3 <- tmp3[tmp3$pocket_card1 > tmp3$pocket_card2, ]
#   tmp.new <- rbind(tmp1, tmp2, tmp3)
#
#   # Control card values
#   tmp.new$pocket_card1 <- gsub(11, "Ja", tmp.new$pocket_card1)
#   tmp.new$pocket_card1 <- gsub(12, "Qu", tmp.new$pocket_card1)
#   tmp.new$pocket_card1 <- gsub(13, "Ki", tmp.new$pocket_card1)
#   tmp.new$pocket_card1 <- gsub(14, "Ac", tmp.new$pocket_card1)
#   tmp.new$pocket_card2 <- gsub(11, "Ja", tmp.new$pocket_card2)
#   tmp.new$pocket_card2 <- gsub(12, "Qu", tmp.new$pocket_card2)
#   tmp.new$pocket_card2 <- gsub(13, "Ki", tmp.new$pocket_card2)
#   tmp.new$pocket_card2 <- gsub(14, "Ac", tmp.new$pocket_card2)
#
#   # Number of players
#   tmp.new$Players <- max(table(x$hand_id[1:20])) # So we don't rely on player_id and don't have to table everything
#
#   # Return
#   tmp.new
# }))
# # Normalized versions of win percentages
# pocket_information <- pocket_information %>%
#   group_by(pocket_card1, pocket_card2) %>% mutate(
#     min_win_pct = round(min(pct_win)*100, 0),
#     max_win_pct = round(max(pct_win)*100, 0),
#     avg_win_pct = round(mean(pct_win)*100, 0)
#   ) %>%
#   group_by(Players) %>% mutate(
#     norm_win_pct = (min(pct_win) - pct_win)/(min(pct_win) - max(pct_win))
#   ) %>% ungroup()
# cards <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Ja", "Qu", "Ki", "Ac")
# pocket_information$pocket_card1 <- factor(pocket_information$pocket_card1, levels = cards)
# pocket_information$pocket_card2 <- factor(pocket_information$pocket_card2, levels = cards)
# # Plots
# pocket_win_likeli <- lapply(unique(pocket_information$Players), function(x, df) {
#   title <- "Chance of Winning with Pocket Cards"
#   df <- df[df$Players == x, ]
#   df$Label <- round(df$pct_win*100, 0)
#   df$Label[df$pct_win <= quantile(df$pct_win, .75)] <- ""
#
#   ggplot(data = df, aes(x = pocket_card1, y = pocket_card2)) +
#     geom_tile(aes(fill = suited_pocket)) +
#     geom_point(aes(color = pct_win, size = pct_win)) +
#     geom_text(aes(x = pocket_card1, y = pocket_card2, label = Label), fontface = "bold", size = 7, color = "black") +
#     scale_x_discrete(position = "top", limits = rev(levels(df$pocket_card1))) +
#     scale_size_continuous("Probability of\nWinning", guide = "legend", range = c(1, 20), limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2), labels = percent) +
#     scale_color_gradientn("Probability of\nWinning", guide = "legend", colours = plt_colorscale, limits = c(0.2, 1), breaks = seq(0.2, 1, 0.2), labels = percent) +
#     scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = plt_background), breaks = "FALSE", labels = "No") +
#     labs(title = title, subtitle = paste(x, "Players", sep = " "), x = "", y = "") +
#     plt_theme
# }, df = pocket_information)
#
# title <- "Chance of Winning with Pocket Cards"
# win_likeli_all <- ggplot(data = pocket_information, aes(x = pocket_card1, y = pocket_card2)) +
#   geom_tile(aes(fill = suited_pocket)) +
#   geom_point(aes(colour = pct_win, size = pct_win)) +
#   scale_x_discrete(position = "top", limits = rev(levels(pocket_information$pocket_card1))) +
#   scale_size_continuous("Probability of\nWinning", guide = "legend", range = c(1, 20), breaks = seq(0, 1, 0.2), labels = percent) +
#   scale_color_gradientn("Probability of\nWinning", guide = "legend", colours = plt_colorscale, breaks = seq(0, 1, 0.2), labels = percent) +
#   scale_fill_manual("Are Cards Suited?", values = c("FALSE" = "#707070", "TRUE" = "#252732"), breaks = "FALSE", labels = "No") +
#   labs(title = title, subtitle = "2 - 10 Players", x = "", y = "",
#        caption = "The rings represents the probability that a person with the corresponding pocket cards will ultimately win the hand for a given number of players.\nLogically the smallest circle will be when there are more players (10 in this case) and the outer ring will be for the fewest number of players (2 in this case).") +
#   plt_theme
# win_likeli_all

#########################################################################################################################
# Save Results
save(hand_distributions_8players, win_distribution_2players, win_distribution_10players,
     file = "Work/results/2020-01-TexasHoldem-Analysis-pt1.Rdata")

print("END")
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
