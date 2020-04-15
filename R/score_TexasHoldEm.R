#' Score a hand or hands of Texas Hold'em
#'
#' @description
#' # Hand Rank - Description
#' 9 - Royal Flush (RF): Suited Ace high Straight (no tie breaker)
#' 8 - Straight Flush (SF): Suited Straight (highest card in straight breaks tie)
#' 7 - Four of a Kind (FK): Self explanatory (highest FK value breaks tie, highest 5th card breaks subsequent tie)
#' 6 - Full House (FH): TK and P together (highest TK value breaks tie, highest P breaks subsequent tie)
#' 5 - Flush (F): Suited hand (highest card breaks tie, highest 2nd card breaks subsequent tie, ..., highest 5th
#'     card breaks subsequent tie)
#' 4 - Straight (S): Five in a row (highest card in straight breaks tie)
#' 3 - Three of a Kind (TK): Self explanatory (highest TK value breaks tie, highest 4th card breaks subsequent tie,
#'     highest 5th card breaks subsequent tie)
#' 2 - Two Pair (TP): Self explanatory (highest pair determines win, highest 2nd pair breaks subsequent tie,
#'     highest 5th card breaks subsequent tie)
#' 1 - Pair (P): Self explanatory (highest pair determines win, highest 3rd card breaks subsequent tie, highest 4th
#'     card breaks subsequent tie, highest 5th card breaks subsequent tie)
#' 0 - High card (HC): Self explanatory (highest card breaks tie, highest 2nd card breaks subsequent tie, ...,
#'     highest 5th card breaks subsequent tie)
#' @param games a dealt hand of Texas Hold'Em from deal_TexasHoldEm
#'
#' @note This can be turned into the list version by simply changing games to games\[\["players"\]\]
#'
#' @return Each hand is given a score based on their 7 cards available
#' @export
score_TexasHoldEm <- function(games, verbose = FALSE) {
  if (verbose) {print("Pocket Information")}
  # Pocket Hand Information ----------------------------------------
  # Pull values/suits of Pocket Cards
  pocket_value <- pull_value(games[, grep("pocket", colnames(games))])
  pocket_suits <- pull_suits(games[, grep("pocket", colnames(games))])

  # Number of high cards in pocket
  games$num_high_pocket <- (pocket_value[, 1] > 9) + (pocket_value[, 2] > 9)

  # Do the two cards have a similar suit
  games$suited_pocket <- pocket_suits[, 1] == pocket_suits[, 2]

  # Were the two cards a pair
  games$paired_pocket <- pocket_value[, 1] == pocket_value[, 2]

  # Were the two cards close enough to help form a straight (while not being a pair). Need to count Ace as either 14 or 1
  # Flag where the aces are
  ace1 <- pocket_value[, 1] == 14
  ace2 <- pocket_value[, 2] == 14
  # Ace as high
  dist_try1 <- abs(pocket_value[, 1] - pocket_value[, 2])
  # Switch to Ace as low - don't use valu(i) after this so we can change it and not worry about it
  pocket_value[ace1, 1] <- 1
  pocket_value[ace2, 2] <- 1
  dist_try2 <- abs(pocket_value[, 1] - pocket_value[, 2])
  # Pull which ever combinations are the best chance of a straight
  games$pocket_distance <- pmin(dist_try1, dist_try2)

  # Cards are close enough to both help make a straight and not be a pair?
  games$straight_pocket <- games$pocket_distance < 5 & games$pocket_distance != 0

  # Game results
  rivers <- grepl("card", colnames(games))
  games <- cbind(games, score_all_hands(dealt_cards = games[, rivers], stage = "river", verbose = verbose))

  return(games)
}

#########################################################################################################################
# # Fascinating
# Need to do for a bunch of data sizes and plot the results
# Using Vectorize version as a trade-off between clean code and spee
# Vectorize is just an easy to use wrapper for mapply...
#   to understand why it is better than apply we need to dive deeper into it.
#   I believe it is because apply is doing a vectorize version on only part of the code than for loop on the rest
#   where mapply is vectorize on everything - in other words, Vectorize turns the entire object into one single vector
#   then applies the function to that vector. Apply turns one margin into a vector then applies the function to that
#   vector but it you are getting one result per cell with apply, you are effectively returning to a loop for the other
#   margin in apply. Thus theoretically an apply within an apply would have a similar result
# suits <- function(x) x %/% 100
# suits_vec <- Vectorize(suits, "x")
# games2 <- games[sample(nrow(games), 100*1, replace = T), ]
# Timing <- bench::mark(check = FALSE,
#                       app = {
#                         apply(games2[, c("pocket_card1", "pocket_card2")], MARGIN = 2, FUN = suits)
#                       },
#                       vec_sep = {
#                         games2$pocket_card1 %/% 100
#                         games2$pocket_card2 %/% 100
#                       },
#                       vec_fn = {
#                         suits_vec <- Vectorize(suits, "x")
#                         suits_vec(games2[, c("pocket_card1", "pocket_card2")])
#                       }
# )
# Timing$N <- 100*1
# for (i1 in 2:1000) {
#   print(i1)
#   games2 <- games[sample(nrow(games), 100*i1, replace = T), ]
#   tmp <- bench::mark(check = FALSE,
#                      app = {
#                        apply(games2[, c("pocket_card1", "pocket_card2")], MARGIN = 2, FUN = suits)
#                      },
#                      vec_sep = {
#                        games2$pocket_card1 %/% 100
#                        games2$pocket_card2 %/% 100
#                      },
#                      vec_fn = {
#                        suits_vec <- Vectorize(suits, "x")
#                        suits_vec(games2[, c("pocket_card1", "pocket_card2")])
#                      }
#   )
#   tmp$N <- 100*i1
#   Timing <- rbind(Timing, tmp)
# }
# Timing$med_time <- as.numeric(Timing$median)
# Timing$expression <- as.character(Timing$expression)
# Timing2 <- as.data.frame(Timing[, c("med_time", "N", "expression")])
# library(ggplot2)
# ggplot(Timing2) +
#   geom_line(aes(y = med_time, x = N, colour = expression))
