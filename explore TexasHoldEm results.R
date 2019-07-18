# https://poker.stackexchange.com/questions/881/publicly-available-poker-stats
# http://poker.cs.ualberta.ca/irc_poker_database.html
# http://web.archive.org/web/20110205042259/http://www.outflopped.com/questions/286/obfuscated-datamined-hand-histories


# Libraries Needed
library(dplyr)
options(scipen =999)
# Load Data
games_list <- list()
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_2players.Rdata")
games_list$players2 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_3players.Rdata")
games_list$players3 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_4players.Rdata")
games_list$players4 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_5players.Rdata")
games_list$players5 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_6players.Rdata")
games_list$players6 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_7players.Rdata")
games_list$players7 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_8players.Rdata")
games_list$players8 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_9players.Rdata")
games_list$players9 <- games
load("~/Desktop/Miscellaneous/Data Blog/TexasHoldEm/Data/sim100kgames_10players.Rdata")
games_list$players10 <- games

# Data handling
games_list <- lapply(games_list, function(x) {
  x$hand_type <- factor(x$hand_type, levels = c("Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight",
                                                "Three of a Kind", "Two Pair", "Pair", "High Card"))
  return(x)
})

# Basic Probabilities
hand_win_tables <- lapply(games_list, function(x) {
  # What are chances a hand wins given you have it
  win_given_hand <- as.data.frame(prop.table(table(x$hand_type, x$winner), margin = 1))
  colnames(win_given_hand) <- c("Hand", "Win", "Percentage")
  win_given_hand$Comparison <- "WinGivenHand"
  # What are chances a hand wins ever
  win_overall <- as.data.frame(prop.table(table(x$hand_type, x$winner), margin = 2))
  colnames(win_overall) <- c("Hand", "Win", "Percentage")
  win_overall$Comparison <- "WinOverall"
  win_pct <- rbind(win_given_hand, win_overall)
  win_pct <- win_pct[win_pct$Win == TRUE, ]
  
  return(win_pct)
})
hand_win_tables

# Plot
library(ggplot2)
n.players <- 2
hand_win_plots <- lapply(hand_win_tables, function(x) {
  title <- paste(n.players, "Players", sep = " ")
  n.players <<- n.players + 1
  p <- ggplot(data = x) + 
    geom_bar(aes(x = Hand, y = Percentage, fill = Comparison), stat = "identity", position = "dodge") +
    ylim(0, 1) + ggtitle(title)
  
  return(p)
})
hand_win_plots

# Why does given a flush have a smaller pct of winning than a straight?
#   hypothesis, when a flush wins, another flush loses more often than a straight losing to a straight
fullhousewins <- games[games$iter %in% games$iter[games$winner == FALSE & games$hand_type == "Full House"], ]
flushwins <- games[games$iter %in% games$iter[games$winner == FALSE & games$hand_type == "Flush"], ]
straightwins <- games[games$iter %in% games$iter[games$winner == FALSE & games$hand_type == "Straight"], ]
kind3wins <- games[games$iter %in% games$iter[games$winner == FALSE & games$hand_type == "Three of a Kind"], ]
table(fullhousewins$hand_type, fullhousewins$winner)
table(flushwins$hand_type, flushwins$winner)
table(straightwins$hand_type, straightwins$winner)
table(kind3wins$hand_type, kind3wins$winner)


one_winner$single_high_pocket <- one_winner$high_pocket & !one_winner$both_high_pocket
logit <- glm(winner ~ single_high_pocket + both_high_pocket + suited_pocket + paired_pocket + straight_pocket, 
             data = one_winner, family = "binomial")
summary(logit)

test <- one_winner[, c("single_high_pocket", "both_high_pocket", "suited_pocket", "paired_pocket", "straight_pocket")]
test <- unique(test)
test$prob <- predict(logit, newdata = test, type = "response")
test <- test[order(test$prob, decreasing = TRUE), ]

# Single high card - one_winner$high_pocket & !one_winner$both_high_pocket
#   suited vs non
#   straight chance vs non
# Two high cards - one_winner$both_high_pocket
#   suited vs non
#   paired vs non
#   straight vs non
# suited cards - one_winner$suited_pocket
#   Single high vs two high vs none
#   straight vs non
# paired cards - one_winner$paired_pocket
#   high vs non
# straight cards - one_winner$straight_pocket
#   Single high vs two high vs none
#   suited vs non

# Decision tree to winning

games$Rockets <- grepl("14", games$pocket_card1) & grepl("14", games$pocket_card2)
logit <- glm(winner ~ Rockets, 
             data = games, family = "binomial")
summary(logit)
exp(coef(logit))

predict(logit, newdata = data.frame(Rockets = c(T, F)), type = "response")

# Could build a censured model with decision points
# i.e.
#     Given pocket cards: do I pay to see the flop
#     Given pocket cards and the flop: do I pay to see the turn
#     Given pocket cards, the flop and the turn: do I pay to see the river
#     Given all the cards, how confident am I in a win


