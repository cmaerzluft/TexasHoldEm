#########################################################################################################################
#                                                                                                                       #
#  Title: Score a round of Texas Hold'em
#  Author: Christopher Maerzluft
#  Description: Returns the ranks of each hand in a given round of Texas Hold'em
#  Last Edit: 10/08/18
#                                                                                                                       #
#########################################################################################################################
score_TexasHoldEm <- function(round) {
  # Pull values of Pocket Cards
  valu1 <- as.numeric(substr(round$pocket_card1, 2, 3))
  suit1 <- substr(round$pocket_card1, 1, 1)
  valu2 <- as.numeric(substr(round$pocket_card2, 2, 3))
  suit2 <- substr(round$pocket_card2, 1, 1)
  
  # Classify the pocket cards
  # Did the two cards contain a "high" card? i.e. Was it a 10 or better?
  round$high_pocket <- valu1 > 9 | valu2 > 9
  round$both_high_pocket <- valu1 > 9 & valu2 > 9
  # Did the two cards have a similar suit
  round$suited_pocket <- suit1 == suit2
  # Were the two cards a pair
  round$paired_pocket <- valu1 == valu2
  # Were the two cards close enough to help form a straight (while not being a pair). Need to count Ace as either 14 or 1
  # Flag where the aces are
  ace1 <- valu1 == 14
  ace2 <- valu2 == 14
  # Ace as high
  dist_try1 <- abs(valu1 - valu2)
  # Switch to Ace as low - don't use valui after this so we can change it and not worry about it
  valu1[ace1] <- 1
  valu2[ace2] <- 1
  dist_try2 <- abs(valu1 - valu2)
  # Pull which ever combinations are the best chance of a straight - A 6 and 7 have a better chance of forming a straight
  #   than a 10 and Ace because the 10 and Ace need Jack, Queen, King to make a straight but a 6, 7 can use 3, 4, 5 or
  #   8, 9, 10 and certain combinations of those.
  round$value_distance <- pmin(dist_try1, dist_try2)
  # Cards are close enough to both help make a straight and not be a pair?
  round$straight_pocket <- round$value_distance < 5 & round$value_distance != 0
  
  # Final hand scores
  round$final_hand_points <- 0
  round$final_hand <- ""
  round$hand_type <- ""
  round$hand_name <- ""
  score <- sapply(1:nrow(round), FUN = function(i1, round) {
    # Pull values and suits
    hand_i <- as.character(round[i1, grep("card", colnames(round))])
    suit <- substr(hand_i, 1, 1)
    valu <- as.numeric(substr(hand_i, 2, 3))
    
    # Find which combinations exist
    # Flush
    flush <- max(table(suit)) >= 5
    # Straight
    # Need to treat Ace as both a 1 and a 13
    # Sort values, calculate difference between consecutive cards, then count groups of differences
    strai <- rle(diff(sort(valu)))
    # if there is a group that had four ones consecutively that means there are five consecutive cards in a row
    straight <- any(strai$lengths >= 4 & strai$values == 1)
    ace_low <- FALSE
    # The previous one assumed the ace was a 14 (worth one more than a King/13) - Try with ace as a 1
    if (any(valu == 14) & !straight) {
      tmp <- valu
      tmp[tmp == 14] <- 1
      strai <- rle(diff(sort(tmp)))
      straight <- any(strai$lengths >= 4 & strai$values == 1)
      ace_low <- TRUE
    }
    # Straight Flush - if they have this turn straight and flush to FALSE after using it so its all good later
    straight_flush <- FALSE
    if (straight & flush) {
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      flush_valu <- valu[suit == flush_suit]
      consec_init <- split(sort(flush_valu), cumsum(c(1, diff(sort(flush_valu)) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      if (length(consec_init) >= 5) {
        straight_flush <- TRUE
      } else {
        straight_flush <- FALSE
      }
    }
    # Four of a kind
    four_of_a_kind <- max(table(valu)) == 4 & 
      sum(straight_flush) == 0 # Can't have a better hand
    # Full house
    full_house <- sum(table(valu) > 1) > 1 & max(table(valu)) == 3 & 
      sum(four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Flush
    if (sum(full_house, four_of_a_kind, straight_flush) != 0) { # Can't have a better hand
      flush <- FALSE
    }
    # Straight
    if (sum(flush, full_house, four_of_a_kind, straight_flush) != 0) { # Can't have a better hand
      straight <- FALSE
    }
    # Three of a kind
    three_of_a_kind <- max(table(valu)) == 3 & 
      sum(straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Two pairs
    two_pair <- sum(table(valu) > 1) > 1 & max(table(valu)) == 2 & 
      sum(three_of_a_kind, straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Pair
    pair <- max(table(valu)) == 2 & 
      sum(two_pair, three_of_a_kind, straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # All other hands are high cards
    
    # Choose best 5 cards and assign points
    if (straight_flush) {
      # Suit of the straight flush - the most common suit
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      # Cards of the straight flush - must come from flush suit
      cards_tmp <- valu[suit == flush_suit]
      # Find consecutive values
      consec_init <- split(sort(cards_tmp), cumsum(c(1, diff(sort(cards_tmp)) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      
      # Record and tally points
      if (length(consec_init) == 5) {
        # Default is that each card is worth its face value points
        # If we find 5 consecutive cards on first try then it means we are in the default scenario
        straight_flush_cards <- consec_init
        points <- sum(straight_flush_cards)
      } else if (length(consec_init) > 5) {
        # If we find more than 5 cards in a sequence we choose the largest 5 cards (cards were sorted in previous step)
        straight_flush_cards <- consec_init[(length(consec_init) - 4):length(consec_init)]
        points <- sum(straight_flush_cards)
      } else if (length(consec_init) == 4 & ace_low & 14 %in% cards_tmp) {
        # If we find only four cards but made it here, it means that we have a low ace scenario - we include the extra checks
        #   just to be sure
        # This means we can just take the four cards in a row and add the ace but we have to treat the ace as a 1 when it
        #   comes to the points so we just subtract 13 from the total
        straight_flush_cards <- c(consec_init, 14)
        points <- sum(straight_flush_cards) - 13
      } else {
        stop("Error in Method for finding straight")
      }
      if (min(consec_init) == 10 & max(consec_init) == 14) {
        card <- "Royal Flush"
      } else {
        card <- paste("Straight Flush", max(consec_init), "High", sep = " ")
      }
      # Score and save
      round$final_hand_points[i1] <<- 10000000000000000000000000 + points
      round$final_hand[i1] <<- paste(paste(flush_suit, straight_flush_cards, sep = ""), collapse = ", ")
      round$hand_type[i1] <<- "Straight Flush"
      round$hand_name[i1] <<- card
    } else if (four_of_a_kind) {
      # Find value that appears four times
      four_valu <- names(table(valu)[table(valu) == 4])
      # Find best card not in the four
      fifth <- max(valu[which(valu != as.numeric(four_valu))])
      fifth_suit <- suit[valu == fifth][1]
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000000000000000000 + as.numeric(four_valu)*4*1000 + fifth
      round$final_hand[i1] <<- paste(paste(paste(c("H", "D", "C", "S"), four_valu, sep = ""), collapse = ", "),
                                     paste(fifth_suit, fifth, sep = ""), sep = ", ")
      round$hand_type[i1] <<- "Four of a Kind"
      round$hand_name[i1] <<- paste("Four of a Kind -", four_valu, sep = " ")
    } else if (full_house) {
      # Find the three of a kind
      three_valu <- names(table(valu)[table(valu) == 3])
      # If more than 1 three of a kind choose the highest valued as the three and the lowest value as the pair
      if (length(three_valu) > 1) {
        pair_valu <- min(as.numeric(three_valu))
        three_valu <- max(as.numeric(three_valu))
      } else {
        # If only one three of a kind, find the highest pair value
        pair_valu <- names(table(valu)[table(valu) == 2])
        pair_valu <- max(as.numeric(pair_valu))
      }
      threes <- paste(paste(suit[valu %in% three_valu], three_valu, sep = ""), collapse = ", ")
      twos <- paste(paste(suit[valu %in% pair_valu][1:2], pair_valu, sep = ""), collapse = ", ")
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000000000000000 + as.numeric(three_valu)*3*1000 + pair_valu*2
      round$final_hand[i1] <<- paste(threes, twos, collapse = ", ")
      round$hand_type[i1] <<- "Full House"
      round$hand_name[i1] <<- paste(three_valu, "Full of", pair_valu, sep = " ")
    } else if (flush) {
      # Suit of the flush - the most common suit
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      flush_valu <- sort(valu[suit == flush_suit])
      flush_valu <- flush_valu[(length(flush_valu) - 4):length(flush_valu)]
      
      # Record and tally points
      points <- flush_valu[1] + flush_valu[2]*100 + flush_valu[3]*10000 + flush_valu[4]*1000000 + flush_valu[5]*100000000
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000000000000 + points
      round$final_hand[i1] <<- paste(paste(flush_suit, flush_valu, sep = ""), collapse = ", ")
      round$hand_type[i1] <<- "Flush"
      round$hand_name[i1] <<- paste("Flush", max(flush_valu), "High", sep = " ")
    } else if (straight) {
      # Find consecutive values
      consec_init <- split(sort(valu), cumsum(c(1, diff(sort(valu)) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      # Take care of ace low
      if (length(consec_init) == 4 & ace_low) {
        straight_valu <- c(consec_init, 14)
        points <- sum(straight_valu) - 13
      } else {
        straight_valu <- consec_init[(length(consec_init) - 4):length(consec_init)]
        points <- sum(straight_valu)
      }
      straight_suit <- suit[valu %in% straight_valu]
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000000000 + points
      round$final_hand[i1] <<- paste(paste(straight_suit, straight_valu, sep = ""), collapse = ", ")
      round$hand_type[i1] <<- "Straight"
      round$hand_name[i1] <<- paste("Straight", max(consec_init), "High", sep = " ")
    } else if (three_of_a_kind) {
      # Find value that appears three times
      three_valu <- names(table(valu)[table(valu) == 3])
      not_three <- sort(valu[!valu %in% three_valu])
      not_three <- not_three[(length(not_three) - 1):length(not_three)]
      best_kicker <- max(not_three)
      leas_kicker <- min(not_three)
      
      # Find suits
      three_suit <- suit[valu == three_valu]
      best_kicker_suit <- suit[valu == best_kicker]
      leas_kicker_suit <- suit[valu == leas_kicker]
      three_cards <- paste(paste(three_suit, three_valu, sep = ""), collapse = ", ")
      kick1_cards <- paste(paste(best_kicker_suit, best_kicker, sep = ""), collapse = ", ")
      kick2_cards <- paste(paste(leas_kicker_suit, leas_kicker, sep = ""), collapse = ", ")
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000000 + as.numeric(three_valu)*3*10000 + best_kicker*100 + leas_kicker
      round$final_hand[i1] <<- paste(three_cards, kick1_cards, kick2_cards, sep = ", ")
      round$hand_type[i1] <<- "Three of a Kind"
      round$hand_name[i1] <<- paste("Three of a Kind -", three_valu, sep = " ")
    } else if (two_pair) {
      # Find value that appears two times
      twos_valu <- names(table(valu)[table(valu) == 2])
      twos_valu <- sort(as.numeric(twos_valu))
      twos_valu <- twos_valu[(length(twos_valu) - 1):length(twos_valu)]
      not_two <- max(valu[!valu %in% twos_valu])
      
      # Find suits
      two1_suit <- suit[valu %in% twos_valu[1]]
      two2_suit <- suit[valu %in% twos_valu[2]]
      kick_suit <- suit[valu %in% not_two][1]
      two2_cards <- paste(paste(two2_suit, twos_valu[2], sep = ""), collapse = ", ")
      two1_cards <- paste(paste(two1_suit, twos_valu[1], sep = ""), collapse = ", ")
      kick_cards <- paste(paste(kick_suit, not_two, sep = ""), collapse = ", ")
      
      # Score and save
      round$final_hand_points[i1] <<- 10000000 + twos_valu[2]*2*10000 + twos_valu[1]*2*100 + not_two
      round$final_hand[i1] <<- paste(two2_cards, two1_cards, kick_cards, sep = ", ")
      round$hand_type[i1] <<- "Two Pair"
      round$hand_name[i1] <<- paste("Pair of", twos_valu[2], "and", twos_valu[1], sep = " ")
    } else if (pair) {
      # Find value that appears two times
      two_valu <- names(table(valu)[table(valu) == 2])
      not_twos <- sort(valu[!valu %in% two_valu])
      not_twos <- not_twos[(length(not_twos) - 2):length(not_twos)]
      kick1 <- not_twos[3]
      kick2 <- not_twos[2]
      kick3 <- not_twos[1]
      
      # Find suits
      two_suit <- suit[valu == two_valu]
      kick1_suit <- suit[valu == kick1]
      kick2_suit <- suit[valu == kick2]
      kick3_suit <- suit[valu == kick3]
      two_cards <- paste(paste(two_suit, two_valu, sep = ""), collapse = ", ")
      kick1_cards <- paste(kick1_suit, kick1, sep = "")
      kick2_cards <- paste(kick2_suit, kick2, sep = "")
      kick3_cards <- paste(kick3_suit, kick3, sep = "")
      
      # Score and save
      round$final_hand_points[i1] <<- 10000 + as.numeric(two_valu)*2*10 + kick1 + kick2/100 + kick3/10000
      round$final_hand[i1] <<- paste(two_cards, kick1_cards, kick2_cards, kick3_cards, sep = ", ")
      round$hand_type[i1] <<- "Pair"
      round$hand_name[i1] <<- paste("Pair of", two_valu, sep = " ")
    } else {
      # Find best values that appears (and subsequent cards)
      high_valu <- sort(valu, decreasing = TRUE)[1]
      kick1_valu <- sort(valu, decreasing = TRUE)[2]
      kick2_valu <- sort(valu, decreasing = TRUE)[3]
      kick3_valu <- sort(valu, decreasing = TRUE)[4]
      kick4_valu <- sort(valu, decreasing = TRUE)[5]
      
      # Find Suits
      high_suit <- suit[valu == high_valu]
      kick1_suit <- suit[valu == kick1_valu]
      kick2_suit <- suit[valu == kick2_valu]
      kick3_suit <- suit[valu == kick3_valu]
      kick4_suit <- suit[valu == kick4_valu]
      high_card <- paste(high_suit, high_valu, sep = "")
      kick1_card <- paste(kick1_suit, kick1_valu, sep = "")
      kick2_card <- paste(kick2_suit, kick2_valu, sep = "")
      kick3_card <- paste(kick3_suit, kick3_valu, sep = "")
      kick4_card <- paste(kick4_suit, kick4_valu, sep = "")
      
      # Score and save
      round$final_hand_points[i1] <<- high_valu*100 + kick1_valu*10 + kick2_valu + kick3_valu/10 + kick3_valu/100
      round$final_hand[i1] <<- paste(high_card, kick1_card, kick2_card, kick3_card, kick4_card, sep = ", ")
      round$hand_type[i1] <<- "High Card"
      round$hand_name[i1] <<- paste(high_card, "High", sep = " ")
    }
  }, round = round)
  
  return(round)
}