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
# Load Data #############################################################################################################
datasize <- 100
units <- "k"
min_p <- 2
max_p <- 10
games_list <- list()
for (i1 in min_p:max_p) {
  print(sprintf("%s%s games with %s players", datasize, units, i1))
  load(sprintf("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim%s%sgames_%splayers.Rdata", datasize, units, i1))
  games_list[[sprintf("players%s", i1)]] <- games
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
  win_given_hand$Comparison <- "Given you have it"
  # What are chances a hand wins ever
  win_overall <- as.data.frame(prop.table(table(x$final_hand_type, x$winner), margin = 2))
  colnames(win_overall) <- c("Hand", "Win", "Percentage")
  win_overall$Comparison <- "Overall"
  win_pct <- rbind(win_given_hand, win_overall)
  win_pct <- win_pct[win_pct$Win == TRUE, ]
  
  return(win_pct)
})
# Plot
n.players <- min_p
hand_win_plots <- lapply(hand_win_tables, function(x) {
  title <- paste(n.players, "Players", sep = " ")
  n.players <<- n.players + 1
  p <- ggplot(data = x) + 
    geom_bar(aes(x = Hand, y = Percentage, fill = Comparison), stat = "identity", position = "dodge") +
    geom_hline(yintercept = c(1, .75, .5, .25), linetype = "dashed") +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_fill_discrete("How often does a hand win:") +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      panel.background = element_blank()
    )
  
  return(p)
})

# What wins, when a good hand loses
# Why does given a flush have a smaller pct of winning than given a straight?
#   hypothesis, when a flush wins, another flush loses more often than a straight losing to a straight
# Does this control for the fact that the straight has more hands that can beat it anyways?
beaters <- function(gList, hand, minP) {
  n.players <- minP
  tab <- do.call(rbind, lapply(gList, function(x) {
    loses <- x[x$iter %in% x$iter[x$winner == FALSE & x$final_hand_type == hand], ]
    dta <- as.data.frame(prop.table(table(loses$final_hand_type, loses$winner), margin = 2))
    colnames(dta) <- c("Hand", "Win", "Percentage")
    dta <- dta[dta$Win == TRUE & dta$Percentage > 0, ]
    dta$Players <- n.players
    n.players <<- n.players + 1
    
    return(dta)
  }))
  
  return(tab)
}
fullhouse_loses <- beaters(games_list, "Full House", min_p)
flush_loses <- beaters(games_list, "Flush", min_p)
straightwins_loses <- beaters(games_list, "Straight", min_p)
kind3_loses <- beaters(games_list, "Three of a Kind", min_p)
lose_fullhouse <- ggplot(data = fullhouse_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  ggtitle("When a Full House Loses") + scale_x_continuous(breaks = seq(min_p, max_p, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))
lose_flush <- ggplot(data = flush_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  ggtitle("When a Flush Loses") + scale_x_continuous(breaks = seq(min_p, max_p, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))
lose_straight <- ggplot(data = straightwins_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  ggtitle("When a Straight Loses") + scale_x_continuous(breaks = seq(min_p, max_p, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))
lose_3kind <- ggplot(data = kind3_loses) +
  geom_bar(aes(x = Players, y = Percentage, fill = Hand), stat = "identity", position = "dodge") +
  ggtitle("When a Three of a Kind Loses") + scale_x_continuous(breaks = seq(min_p, max_p, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))

# How often do you get two cards to start
n.players <- min_p
pocket_strength <- lapply(games_list, function(x) {
  title <- paste("Chance of Getting Pocket Cards", n.players, "Players", sep = " ")
  # title <- "Pocket Cards Chances of Winning"
  n.players <<- n.players + 1
  x$Pocket1 <- gsub("[A-z]", "", x$pocket_card1)
  x$Pocket1 <- gsub("11", "Ja", x$Pocket1)
  x$Pocket1 <- gsub("12", "Qu", x$Pocket1)
  x$Pocket1 <- gsub("13", "Ki", x$Pocket1)
  x$Pocket1 <- gsub("14", "Ac", x$Pocket1)
  x$Pocket2 <- gsub("[A-z]", "", x$pocket_card2)
  x$Pocket2 <- gsub("11", "Ja", x$Pocket2)
  x$Pocket2 <- gsub("12", "Qu", x$Pocket2)
  x$Pocket2 <- gsub("13", "Ki", x$Pocket2)
  x$Pocket2 <- gsub("14", "Ac", x$Pocket2)
  x$Win <- ifelse(x$winner == TRUE, "Winner", "Loser")
  tmp <- x %>% group_by(Pocket1, Pocket2, suited_pocket, Win) %>%
    summarise(N = n()) %>%
    spread(Win, N, fill = 0) %>%
    arrange(Pocket1, Pocket2)
  tmp <- left_join(tmp, tmp, by = c("Pocket1" = "Pocket2", "Pocket2" = "Pocket1", "suited_pocket"))
  tmp$Winner <- ifelse(tmp$Pocket1 == tmp$Pocket2, tmp$Winner.x, tmp$Winner.x + tmp$Winner.y)
  tmp$Loser <- ifelse(tmp$Pocket1 == tmp$Pocket2, tmp$Loser.x, tmp$Loser.x + tmp$Loser.y)
  tmp <- tmp[, c("Pocket1", "Pocket2", "suited_pocket", "Winner", "Loser")]
  cards <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu", "Ki", "Ac")
  tmp$Pocket1 <- factor(tmp$Pocket1, levels = cards)
  tmp$Pocket2 <- factor(tmp$Pocket2, levels = cards)
  tmp$Percentage <- (tmp$Winner + tmp$Loser)/sum(tmp[, c("Winner", "Loser")])
  tmp1 <- tmp[tmp$Pocket1 == tmp$Pocket2, ]
  tmp2 <- tmp[tmp$suited_pocket, ]
  tmp2 <- tmp2[tmp2$Pocket1 == "02" | 
                 (tmp2$Pocket1 == "03" & !tmp2$Pocket2 %in% c("02")) |
                 (tmp2$Pocket1 == "04" & !tmp2$Pocket2 %in% c("02", "03")) |
                 (tmp2$Pocket1 == "05" & !tmp2$Pocket2 %in% c("02", "03", "04")) |
                 (tmp2$Pocket1 == "06" & !tmp2$Pocket2 %in% c("02", "03", "04", "05")) |
                 (tmp2$Pocket1 == "07" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06")) |
                 (tmp2$Pocket1 == "08" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07")) |
                 (tmp2$Pocket1 == "09" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08")) |
                 (tmp2$Pocket1 == "10" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09")) |
                 (tmp2$Pocket1 == "Ja" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10")) |
                 (tmp2$Pocket1 == "Qu" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja")) |
                 (tmp2$Pocket1 == "Ki" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu")), ]
  tmp3 <- tmp[!tmp$suited_pocket & tmp$Pocket1 != tmp$Pocket2, ]
  tmp3 <- tmp3[tmp3$Pocket2 == "02" | 
                 (tmp3$Pocket2 == "03" & !tmp3$Pocket1 %in% c("02")) |
                 (tmp3$Pocket2 == "04" & !tmp3$Pocket1 %in% c("02", "03")) |
                 (tmp3$Pocket2 == "05" & !tmp3$Pocket1 %in% c("02", "03", "04")) |
                 (tmp3$Pocket2 == "06" & !tmp3$Pocket1 %in% c("02", "03", "04", "05")) |
                 (tmp3$Pocket2 == "07" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06")) |
                 (tmp3$Pocket2 == "08" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07")) |
                 (tmp3$Pocket2 == "09" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08")) |
                 (tmp3$Pocket2 == "10" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09")) |
                 (tmp3$Pocket2 == "Ja" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10")) |
                 (tmp3$Pocket2 == "Qu" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja")) |
                 (tmp3$Pocket2 == "Ki" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu")), ]
  tmp.new <- rbind(tmp1, tmp2, tmp3)
  tmp.new$Label <- round(tmp.new$Percentage*100, 2)
  # tmp.new$Label[tmp.new$Percentage <= quantile(tmp.new$Percentage, .5)] <- ""
  
  # Create polygon for Suited/Off-suit coloring
  tri <- data.frame(group = c("Off-suit", "Off-suit", "Off-suit", "Off-suit", "Off-suit", "Suited", "Suited", "Suited"), 
                    polygon.x = c(13.5, 13.5,    1,  0.5, 0.5, 13.5,    1, 13.5), 
                    polygon.y = c( 0.5,    1, 13.5, 13.5, 0.5,    1, 13.5, 13.5))
  
  # Plot
  ggplot(data = tmp.new, aes(x = Pocket1, y = Pocket2)) +
    geom_point(aes(color = Percentage, size = Percentage)) +
    geom_polygon(data = tri, aes(x = polygon.x, y = polygon.y, group = group, fill = group)) +
    geom_point(aes(color = Percentage, size = Percentage)) +
    geom_text(aes(x = Pocket1, y = Pocket2, label = Label), fontface = "bold", size = 7, color = "black") +
    scale_x_discrete(position = "top", limits = rev(levels(tmp$Pocket1))) +
    scale_size_continuous("Probability of\nGetting", range = c(1, 20), labels = percent) +
    scale_color_gradientn("Probability of\nGetting", guide = "legend", colours = wes_palette("Zissou1", 100, type = "continuous"), labels = percent) +
    scale_fill_manual("Are Cards Suited?", values = c("#707070", "#000000")) +
    xlab("") + ylab("") + ggtitle(title) +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = "black"),
          legend.text = element_text(colour = "white"),
          # legend.position = "bottom",
          legend.title = element_text(colour = "white"),
          legend.background = element_rect(fill = "black"),
          legend.key = element_rect(fill = "black", color = "black"),
          axis.text = element_text(size = 16, colour = "white"),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 24, colour = "white"))
})

# How often do you win given two starting cards
cards_list <- list()
i1 <- 0
n.players <- min_p
pocket_strength <- lapply(games_list, function(x) {
  title <- paste("Pocket Cards Chances of Winning\n", n.players, "Players", sep = " ")
  # title <- "Pocket Cards Chances of Winning"
  n.players <<- n.players + 1
  x$Pocket1 <- gsub("[A-z]", "", x$pocket_card1)
  x$Pocket1 <- gsub("11", "Ja", x$Pocket1)
  x$Pocket1 <- gsub("12", "Qu", x$Pocket1)
  x$Pocket1 <- gsub("13", "Ki", x$Pocket1)
  x$Pocket1 <- gsub("14", "Ac", x$Pocket1)
  x$Pocket2 <- gsub("[A-z]", "", x$pocket_card2)
  x$Pocket2 <- gsub("11", "Ja", x$Pocket2)
  x$Pocket2 <- gsub("12", "Qu", x$Pocket2)
  x$Pocket2 <- gsub("13", "Ki", x$Pocket2)
  x$Pocket2 <- gsub("14", "Ac", x$Pocket2)
  x$Win <- ifelse(x$winner == TRUE, "Winner", "Loser")
  tmp <- x %>% group_by(Pocket1, Pocket2, suited_pocket, Win) %>%
    summarise(N = n()) %>%
    spread(Win, N, fill = 0) %>%
    arrange(Pocket1, Pocket2)
  tmp <- left_join(tmp, tmp, by = c("Pocket1" = "Pocket2", "Pocket2" = "Pocket1", "suited_pocket"))
  tmp$Winner <- ifelse(tmp$Pocket1 == tmp$Pocket2, tmp$Winner.x, tmp$Winner.x + tmp$Winner.y)
  tmp$Loser <- ifelse(tmp$Pocket1 == tmp$Pocket2, tmp$Loser.x, tmp$Loser.x + tmp$Loser.y)
  tmp <- tmp[, c("Pocket1", "Pocket2", "suited_pocket", "Winner", "Loser")]
  cards <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu", "Ki", "Ac")
  tmp$Pocket1 <- factor(tmp$Pocket1, levels = cards)
  tmp$Pocket2 <- factor(tmp$Pocket2, levels = cards)
  tmp$Percentage <- tmp$Winner/rowSums(tmp[, c("Winner", "Loser")])
  tmp1 <- tmp[tmp$Pocket1 == tmp$Pocket2, ]
  tmp2 <- tmp[tmp$suited_pocket, ]
  tmp2 <- tmp2[tmp2$Pocket1 == "02" | 
                 (tmp2$Pocket1 == "03" & !tmp2$Pocket2 %in% c("02")) |
                 (tmp2$Pocket1 == "04" & !tmp2$Pocket2 %in% c("02", "03")) |
                 (tmp2$Pocket1 == "05" & !tmp2$Pocket2 %in% c("02", "03", "04")) |
                 (tmp2$Pocket1 == "06" & !tmp2$Pocket2 %in% c("02", "03", "04", "05")) |
                 (tmp2$Pocket1 == "07" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06")) |
                 (tmp2$Pocket1 == "08" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07")) |
                 (tmp2$Pocket1 == "09" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08")) |
                 (tmp2$Pocket1 == "10" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09")) |
                 (tmp2$Pocket1 == "Ja" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10")) |
                 (tmp2$Pocket1 == "Qu" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja")) |
                 (tmp2$Pocket1 == "Ki" & !tmp2$Pocket2 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu")), ]
  tmp3 <- tmp[!tmp$suited_pocket & tmp$Pocket1 != tmp$Pocket2, ]
  tmp3 <- tmp3[tmp3$Pocket2 == "02" | 
                 (tmp3$Pocket2 == "03" & !tmp3$Pocket1 %in% c("02")) |
                 (tmp3$Pocket2 == "04" & !tmp3$Pocket1 %in% c("02", "03")) |
                 (tmp3$Pocket2 == "05" & !tmp3$Pocket1 %in% c("02", "03", "04")) |
                 (tmp3$Pocket2 == "06" & !tmp3$Pocket1 %in% c("02", "03", "04", "05")) |
                 (tmp3$Pocket2 == "07" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06")) |
                 (tmp3$Pocket2 == "08" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07")) |
                 (tmp3$Pocket2 == "09" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08")) |
                 (tmp3$Pocket2 == "10" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09")) |
                 (tmp3$Pocket2 == "Ja" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10")) |
                 (tmp3$Pocket2 == "Qu" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja")) |
                 (tmp3$Pocket2 == "Ki" & !tmp3$Pocket1 %in% c("02", "03", "04", "05", "06", "07", "08", "09", "10", "Ja", "Qu")), ]
  tmp.new <- rbind(tmp1, tmp2, tmp3)
  tmp.new$Label <- round(tmp.new$Percentage*100, 0)
  tmp.new$Label[tmp.new$Percentage <= quantile(tmp.new$Percentage, .5)] <- "" # quantile(tmp.new$Percentage, .9)
  i1 <<- i1 + 1
  cards_list[[i1]] <<- tmp.new
  
  # Create polygon for Suited/Off-suit coloring
  tri <- data.frame(group = c("Off-suit", "Off-suit", "Off-suit", "Off-suit", "Off-suit", "Suited", "Suited", "Suited"), 
                    polygon.x = c(13.5, 13.5,    1,  0.5, 0.5, 13.5,    1, 13.5), 
                    polygon.y = c( 0.5,    1, 13.5, 13.5, 0.5,    1, 13.5, 13.5))
  
  # Plot
  ggplot(data = tmp.new, aes(x = Pocket1, y = Pocket2)) +
    geom_point(aes(color = Percentage, size = Percentage)) +
    geom_polygon(data = tri, aes(x = polygon.x, y = polygon.y, group = group, fill = group)) +
    geom_point(aes(color = Percentage, size = Percentage)) +
    geom_text(aes(x = Pocket1, y = Pocket2, label = Label), fontface = "bold", size = 7, color = "black") +
    scale_x_discrete(position = "top", limits = rev(levels(tmp$Pocket1))) +
    scale_size_continuous("Probability of\nWinning", range = c(1, 20), labels = percent) +
    scale_color_gradientn("Probability of\nWinning", guide = "legend", colours = wes_palette("Zissou1", 100, type = "continuous"), labels = percent) +
    scale_fill_manual("Are Cards Suited?", values = c("#707070", "#000000")) +
    xlab("") + ylab("") + ggtitle(title) +
    theme(panel.background = element_blank(),
          plot.background = element_rect(fill = "black"),
          legend.text = element_text(colour = "white"),
          # legend.position = "bottom",
          legend.title = element_text(colour = "white"),
          legend.background = element_rect(fill = "black"),
          legend.key = element_rect(fill = "black", color = "black"),
          axis.text = element_text(size = 16, colour = "white"),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 24, colour = "white"))
})

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