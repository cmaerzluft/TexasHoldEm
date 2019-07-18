#########################################################################################################################
#                                                                                                                       #
#  Title: Score a round of Texas Hold'em
#  Author: Christopher Maerzluft
#  Description: Returns the ranks of each hand in a given round of Texas Hold'em
#  Last Edit: 3/05/19
#                                                                                                                       #
#########################################################################################################################
# Hand Rank - Description
#         9 - Royal Flush (RF): Suited Ace high Straight (no tie breaker)
#         8 - Straight Flush (SF): Suited Straight (highest card in straight breaks tie)
#         7 - Four of a Kind (FK): Self explanatory (highest FK value breaks tie, highest 5th card breaks subsequent tie)
#         6 - Full House (FH): TK and P together (highest TK value breaks tie, highest P breaks subsequent tie)
#         5 - Flush (F): Suited hand (highest card breaks tie, highest 2nd card breaks subsequent tie, ..., highest 5th 
#                         card breaks subsequent tie)
#         4 - Straight (S): Five in a row (highest card in straight breaks tie)
#         3 - Three of a Kind (TK): Self explanatory (highest TK value breaks tie, highest 4th card breaks subsequent tie,
#                                   highest 5th card breaks subsequent tie)
#         2 - Two Pair (TP): Self explanatory (highest pair determines win, highest 2nd pair breaks subsequent tie,
#                             highest 5th card breaks subsequent tie)
#         1 - Pair (P): Self explanatory (highest pair determines win, highest 3rd card breaks subsequent tie, highest 4th 
#                       card breaks subsequent tie, highest 5th card breaks subsequent tie)
#         0 - High card (HC): Self explanatory (highest card breaks tie, highest 2nd card breaks subsequent tie, ..., 
#                             highest 5th card breaks subsequent tie)
score_TexasHoldEm <- function(round) {
  # Pull values of Pocket Cards
  valu1 <- as.numeric(substr(round$pocket_card1, 2, 3))
  suit1 <- substr(round$pocket_card1, 1, 1)
  valu2 <- as.numeric(substr(round$pocket_card2, 2, 3))
  suit2 <- substr(round$pocket_card2, 1, 1)
  
  # Classify the pocket cards for analysis later
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
  # Our scoring system will use a number of 11 digits to determine the best hand. Each hand has a rank from 0 - 9 and 
  #   each card has a value 01 - 14 (the Ace can be worth 01 or 14 depending on its use, numbers are worth face value, 
  #   and Jack, Queen and King are worth 11, 12, and 13 respectively). For our system, you put the rank value first,
  #   followed by the values used to determine the best hand based on the tie breaking schema outlined above. For example,
  #   if a player had a two pair (for this example we will use a pair of 9's, a pair of 7's and their fifth card being a 
  #   king), they would get: a rank score of 2, the next two numbers would be 09 and 09 (their highest pair), the following
  #   two number would be 07 and 07, and finally they would have a 13 (for their king). This translates to a final score 
  #   of 20,909,070,713. The highest score in a hand wins the round. If two people have the same score and it's the 
  #   highest score, both are labeled as winners.
  round$final_hand_points <- 0
  round$final_hand <- ""
  round$hand_type <- ""
  round$hand_name <- ""
  score <- sapply(1:nrow(round), FUN = function(i1, round) {
    # Pull values and suits for a given hand
    hand_i <- as.character(round[i1, grep("card", colnames(round))])
    suit <- substr(hand_i, 1, 1)
    valu <- as.numeric(substr(hand_i, 2, 3))
    
    # Before going through and assigning a specific score to each hand, we first look to see what type of hand it is. We
    #   do this because a hand can have multiple types (e.g. a full house can be a three of a kind or a pair as well).
    #   Picking the hand first, allows us to make decide on one hand and keep the code that does this together and thus
    #   easier to read/follow.
    # Is there a flush?
    flush <- max(table(suit)) >= 5
    # Is there a Straight?
    # Sort unique values, calculate difference between consecutive cards, then count groups of differences
    strai <- rle(diff(sort(unique(valu))))
    # We need a set of 4 or more differences that are only different by 1 value each
    straight <- any(strai$lengths >= 4 & strai$values == 1)
    ace_low <- FALSE # only used when looking at straights
    # The previous one assumed the ace was a 14, need to try with ace as a 1. Only necessary if there is an Ace and there
    #   isn't a straight already (since an Ace low straight is worth the least amount of points of the straights)
    if (any(valu == 14) & !straight) {
      tmp <- valu
      tmp[tmp == 14] <- 1
      strai <- rle(diff(sort(unique(tmp))))
      straight <- any(strai$lengths >= 4 & strai$values == 1)
      ace_low <- TRUE
    }
    # Is there a Straight Flush?
    straight_flush <- FALSE
    if (straight & flush) { # Need a straight and a flush individually for this to be TRUE
      # Use only cards that can make a flush
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      if (ace_low) { # if using a low Ace look at it as a 1
        valu_tmp <- as.numeric(gsub(14, 1, valu))
        flush_valu <- valu_tmp[suit == flush_suit]
      } else {
        flush_valu <- valu[suit == flush_suit]
      }
      # Take the consecutive differences again (unique shouldn't be required since only one value of each card exists 
      #   within a flush but if we moved to two+ decks this would be require). Turn differences not equal to 1 to TRUE
      #   and differences equal to 1 to FALSE. We then start that vector with a 1 (this also converts T/F to 1/0) and
      #   take the cumulative sum of that vector. This means that the four cards where the previous number was one less
      #   than it will have values of 0 and when we add the cumulative sum, they won't change the sum creating a grouping
      #   of 5 identical numbers. These are five consecutive values (i.e. a straight)
      consec_init <- split(sort(unique(flush_valu)), cumsum(c(1, diff(sort(unique(flush_valu))) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      # If we can make a straight using only values from cards in the flush we have a Straight Flush
      if (length(consec_init) >= 5) {
        straight_flush <- TRUE
      } else {
        straight_flush <- FALSE
      }
    }
    # We only want to use the best hand available so from here on out we will also check to see if we found a better hand
    #   already.
    # Is there a Four of a kind
    four_of_a_kind <- max(table(valu)) == 4 & 
      sum(straight_flush) == 0 # Can't have a better hand
    # Is there a Full house
    full_house <- sum(table(valu) > 1) > 1 & max(table(valu)) == 3 & 
      sum(four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Is there a Flush (already determined if one exists, just need to switch it to FALSE, if better hand exists)
    if (sum(full_house, four_of_a_kind, straight_flush) != 0) { # Can't have a better hand
      flush <- FALSE
    }
    # Is there a Straight (already determined if one exists, just need to switch it to FALSE, if better hand exists)
    if (sum(flush, full_house, four_of_a_kind, straight_flush) != 0) { # Can't have a better hand
      straight <- FALSE
    }
    # Is there a Three of a kind
    three_of_a_kind <- max(table(valu)) == 3 & 
      sum(straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Is there a Two pairs
    two_pair <- sum(table(valu) > 1) > 1 & max(table(valu)) == 2 & 
      sum(three_of_a_kind, straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # Is there a Pair
    pair <- max(table(valu)) == 2 & 
      sum(two_pair, three_of_a_kind, straight, flush, full_house, four_of_a_kind, straight_flush) == 0 # Can't have a better hand
    # All other hands are high cards
    
    # Now that we know what is the best hand that exists, we can score them using their specific score profile
    if (straight_flush) {
      # Hand Type Value
      hand_value <- "8"
      # Suit of the straight flush - the most common suit
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      # Cards of the straight flush - must come from flush suit
      cards_tmp <- valu[suit == flush_suit]
      # Find consecutive values
      consec_init <- split(sort(unique(cards_tmp)), cumsum(c(1, diff(sort(unique(cards_tmp))) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      consec_init <- sort(consec_init, decreasing = TRUE)
      # Take care of ace low
      if (length(consec_init) == 4 & ace_low) {
        straight_valu <- c(consec_init, 1)
      } else {
        straight_valu <- consec_init[1:5]
      }
      # The best possible hand (a Royal Flush) is just an Ace High Straight Flush
      if (max(straight_valu) == 14) {
        hand_value <- "9"
      }
      # Top 5 Card Values
      card1_value <- sprintf(straight_valu[1], fmt = "%02s")
      card2_value <- sprintf(straight_valu[2], fmt = "%02s")
      card3_value <- sprintf(straight_valu[3], fmt = "%02s")
      card4_value <- sprintf(straight_valu[4], fmt = "%02s")
      card5_value <- sprintf(straight_valu[5], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- flush_suit
      card2_suit <- flush_suit
      card3_suit <- flush_suit
      card4_suit <- flush_suit
      card5_suit <- flush_suit
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # If an Ace is low, the score value is 01, but we still want to read it as a 14 in the card coding
      card5 <- gsub("01", "14", card5)
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Straight Flush"
      # Hand Name
      if (hand_value == "9") {
        round$hand_name[i1] <<- "Royal Flush"
      } else {
        round$hand_name[i1] <<- paste("Straight Flush -", max(straight_valu), "High", sep = " ")
      }
    } else if (four_of_a_kind) {
      # Hand Type Value
      hand_value <- "7"
      # Find value that appears four times
      four_valu <- as.numeric(names(table(valu)[table(valu) == 4]))
      # Find best card not in the four
      fifth_valu <- max(valu[which(valu != four_valu)])[1]
      # Top 5 Card Values
      card1_value <- sprintf(four_valu, fmt = "%02s")
      card2_value <- sprintf(four_valu, fmt = "%02s")
      card3_value <- sprintf(four_valu, fmt = "%02s")
      card4_value <- sprintf(four_valu, fmt = "%02s")
      card5_value <- sprintf(fifth_valu, fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[valu %in% four_valu][1]
      card2_suit <- suit[valu %in% four_valu][2]
      card3_suit <- suit[valu %in% four_valu][3]
      card4_suit <- suit[valu %in% four_valu][4]
      card5_suit <- suit[valu %in% fifth_valu][1]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Four of a Kind"
      # Hand Name
      round$hand_name[i1] <<- paste("Four of a Kind - ", four_valu, "'s", sep = "")
    } else if (full_house) {
      # Hand Type Value
      hand_value <- "6"
      # Find the three of a kind
      trips_valu <- as.numeric(names(table(valu)[table(valu) == 3]))
      # If more than 1 three of a kind choose the highest valued as the three and the lowest value as the pair
      if (length(trips_valu) > 1) {
        pair_valu <- min(trips_valu)[1]
        trips_valu <- max(trips_valu)[1]
      } else {
        # If only one three of a kind, find the highest pair value
        pair_valu <- names(table(valu)[table(valu) == 2])
        pair_valu <- max(as.numeric(pair_valu))[1]
      }
      # Top 5 Card Values
      card1_value <- sprintf(trips_valu, fmt = "%02s")
      card2_value <- sprintf(trips_valu, fmt = "%02s")
      card3_value <- sprintf(trips_valu, fmt = "%02s")
      card4_value <- sprintf(pair_valu, fmt = "%02s")
      card5_value <- sprintf(pair_valu, fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[valu %in% trips_valu][1]
      card2_suit <- suit[valu %in% trips_valu][2]
      card3_suit <- suit[valu %in% trips_valu][3]
      card4_suit <- suit[valu %in% pair_valu][1]
      card5_suit <- suit[valu %in% pair_valu][2]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Full House"
      # Hand Name
      round$hand_name[i1] <<- paste(trips_valu, "'s Full of ", pair_valu, "'s", sep = "")
    } else if (flush) {
      # Hand Type Value
      hand_value <- "5"
      # Suit of the flush - the most common suit
      flush_suit <- names(table(suit)[table(suit) == max(table(suit))])
      flush_valu <- sort(valu[suit == flush_suit], decreasing = TRUE)
      flush_valu <- flush_valu[1:5]
      # Top 5 Card Values
      card1_value <- sprintf(flush_valu[1], fmt = "%02s")
      card2_value <- sprintf(flush_valu[2], fmt = "%02s")
      card3_value <- sprintf(flush_valu[3], fmt = "%02s")
      card4_value <- sprintf(flush_valu[4], fmt = "%02s")
      card5_value <- sprintf(flush_valu[5], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- flush_suit
      card2_suit <- flush_suit
      card3_suit <- flush_suit
      card4_suit <- flush_suit
      card5_suit <- flush_suit
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Flush"
      # Hand Name
      round$hand_name[i1] <<- paste("Flush -", as.numeric(card1_value), "High", sep = " ")
    } else if (straight) {
      # Hand Type Value
      hand_value <- "4"
      # Find consecutive values
      consec_init <- split(sort(unique(valu)), cumsum(c(1, diff(sort(unique(valu))) != 1)))
      # Pull cards that make up the longest consecutive list
      lengths <- lapply(consec_init, length)
      longest <- which.max(lengths)
      consec_init <- consec_init[[longest]]
      consec_init <- sort(consec_init, decreasing = TRUE)
      # Take care of ace low
      if (length(consec_init) == 4 & ace_low) {
        straight_valu <- c(consec_init, 1)
        straight_suit <- suit[match(c(consec_init, 14), valu)]
      } else {
        straight_valu <- consec_init[1:5]
        straight_suit <- suit[match(straight_valu, valu)]
      }
      # Top 5 Card Values
      card1_value <- sprintf(straight_valu[1], fmt = "%02s")
      card2_value <- sprintf(straight_valu[2], fmt = "%02s")
      card3_value <- sprintf(straight_valu[3], fmt = "%02s")
      card4_value <- sprintf(straight_valu[4], fmt = "%02s")
      card5_value <- sprintf(straight_valu[5], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- straight_suit[1]
      card2_suit <- straight_suit[2]
      card3_suit <- straight_suit[3]
      card4_suit <- straight_suit[4]
      card5_suit <- straight_suit[5]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      card5 <- gsub("01", "14", card5)
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Straight"
      # Hand Name
      round$hand_name[i1] <<- paste("Straight -", as.numeric(card1_value), "High", sep = " ")
    } else if (three_of_a_kind) {
      # Hand Type Value
      hand_value <- "3"
      # Find the Three of a Kind
      trip_valu <- names(table(valu)[table(valu) == 3])
      not_trip <- sort(valu[!valu %in% trip_valu], decreasing = TRUE)
      not_trip <- not_trip[1:2]
      # Top 5 Card Values
      card1_value <- sprintf(trip_valu, fmt = "%02s")
      card2_value <- sprintf(trip_valu, fmt = "%02s")
      card3_value <- sprintf(trip_valu, fmt = "%02s")
      card4_value <- sprintf(not_trip[1], fmt = "%02s")
      card5_value <- sprintf(not_trip[2], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[valu %in% trip_valu][1]
      card2_suit <- suit[valu %in% trip_valu][2]
      card3_suit <- suit[valu %in% trip_valu][3]
      card4_suit <- suit[valu %in% not_trip[1]]
      card5_suit <- suit[valu %in% not_trip[2]]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Three of a Kind"
      # Hand Name
      round$hand_name[i1] <<- paste("Three of a Kind - ", trip_valu, "'s", sep = "")
    } else if (two_pair) {
      # Hand Type Value
      hand_value <- "2"
      # Find the Pairs
      pairs_valu <- names(table(valu)[table(valu) == 2])
      pairs_valu <- sort(as.numeric(pairs_valu), decreasing = TRUE)
      pairs_valu <- pairs_valu[1:2]
      not_pairs <- max(valu[!valu %in% pairs_valu])[1]
      # Top 5 Card Values
      card1_value <- sprintf(pairs_valu[1], fmt = "%02s")
      card2_value <- sprintf(pairs_valu[1], fmt = "%02s")
      card3_value <- sprintf(pairs_valu[2], fmt = "%02s")
      card4_value <- sprintf(pairs_valu[2], fmt = "%02s")
      card5_value <- sprintf(not_pairs[1], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[valu %in% pairs_valu[1]][1]
      card2_suit <- suit[valu %in% pairs_valu[1]][2]
      card3_suit <- suit[valu %in% pairs_valu[2]][1]
      card4_suit <- suit[valu %in% pairs_valu[2]][2]
      card5_suit <- suit[valu == card5_value][1]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Two Pair"
      # Hand Name
      round$hand_name[i1] <<- paste("Pair of ", as.numeric(pairs_valu[1]), "'s and ", as.numeric(pairs_valu[2]), "'s", sep = "")
    } else if (pair) {
      # Hand Type Value
      hand_value <- "1"
      # Find the Pair
      pair_valu <- names(table(valu)[table(valu) == 2])
      not_pair <- sort(valu[!valu %in% pair_valu], decreasing = TRUE)
      not_pair <- not_pair[1:3]
      # Top 5 Card Values
      card1_value <- sprintf(pair_valu, fmt = "%02s")
      card2_value <- sprintf(pair_valu, fmt = "%02s")
      card3_value <- sprintf(not_pair[1], fmt = "%02s")
      card4_value <- sprintf(not_pair[2], fmt = "%02s")
      card5_value <- sprintf(not_pair[3], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[valu == pair_valu][1]
      card2_suit <- suit[valu == pair_valu][2]
      card3_suit <- suit[valu == card3_value]
      card4_suit <- suit[valu == card4_value]
      card5_suit <- suit[valu == card5_value]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "Pair"
      # Hand Name
      round$hand_name[i1] <<- paste("Pair of ", as.numeric(pair_valu), "'s", sep = "")
    } else {
      # Hand Type Value
      hand_value <- "0"
      # Top 5 Card Values
      card1_value <- sprintf(sort(valu, decreasing = TRUE)[1], fmt = "%02s")
      card2_value <- sprintf(sort(valu, decreasing = TRUE)[2], fmt = "%02s")
      card3_value <- sprintf(sort(valu, decreasing = TRUE)[3], fmt = "%02s")
      card4_value <- sprintf(sort(valu, decreasing = TRUE)[4], fmt = "%02s")
      card5_value <- sprintf(sort(valu, decreasing = TRUE)[5], fmt = "%02s")
      # Top 5 Card Suits
      card1_suit <- suit[order(valu, decreasing = TRUE)][1]
      card2_suit <- suit[order(valu, decreasing = TRUE)][2]
      card3_suit <- suit[order(valu, decreasing = TRUE)][3]
      card4_suit <- suit[order(valu, decreasing = TRUE)][4]
      card5_suit <- suit[order(valu, decreasing = TRUE)][5]
      # Top 5 Cards
      card1 <- paste(card1_suit, card1_value, sep = "") 
      card2 <- paste(card2_suit, card2_value, sep = "") 
      card3 <- paste(card3_suit, card3_value, sep = "") 
      card4 <- paste(card4_suit, card4_value, sep = "") 
      card5 <- paste(card5_suit, card5_value, sep = "")
      # Hand Points
      round$final_hand_points[i1] <<- as.numeric(
        paste(hand_value, card1_value, card2_value, card3_value, card4_value, card5_value, sep = "")
      )
      # Final Hand
      round$final_hand[i1] <<- paste(card1, card2, card3, card4, card5, sep = ", ")
      # Hand Type Name
      round$hand_type[i1] <<- "High Card"
      # Hand Name
      round$hand_name[i1] <<- paste(as.numeric(card1_value), "High", sep = " ")
    }
  }, round = round)
  
  return(round)
}
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################