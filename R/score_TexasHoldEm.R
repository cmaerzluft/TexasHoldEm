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
#' @return Each hand is given a score based on their 7 cards available
#' @export
score_TexasHoldEm <- function(games) {
  # Pull values of Pocket Cards
  valu1 <- as.numeric(substr(games$pocket_card1, 2, 3))
  suit1 <- substr(games$pocket_card1, 1, 1)
  valu2 <- as.numeric(substr(games$pocket_card2, 2, 3))
  suit2 <- substr(games$pocket_card2, 1, 1)

  # Classify the pocket cards for analysis later
  # Did the two cards contain a "high" card? i.e. Was it a 10 or better?
  games$high_pocket <- valu1 > 9 | valu2 > 9
  games$both_high_pocket <- valu1 > 9 & valu2 > 9
  # Did the two cards have a similar suit
  games$suited_pocket <- suit1 == suit2
  # Were the two cards a pair
  games$paired_pocket <- valu1 == valu2
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
  games$value_distance <- pmin(dist_try1, dist_try2)
  # Cards are close enough to both help make a straight and not be a pair?
  games$straight_pocket <- games$value_distance < 5 & games$value_distance != 0

  score <- function(dealt_cards, stage = "river", verbose = FALSE) {
    ### Helping Functions ###
    # Find Flush Suit (NA if no flush)
    find_flush_suit <- function(suits) {
      freq <- sort(table(suits), decreasing = TRUE)

      return(
        ifelse(max(freq) >= 5, names(freq[1]), NA)
      )
    }
    # Find highest card in straight (NA if no straight)
    find_straight_top <- function(values) {
      # sort removes NA automatically
      straights <- cumsum(c(1, diff(sort(unique(values))) != 1))
      chk <- rle(straights)
      if (any(chk$lengths >= 5)) {
        consec_init <- split(sort(unique(values)), straights)
        lengths <- lapply(consec_init, length)
        longest <- which.max(lengths)
        st <- max(consec_init[[longest]])
      } else if (any(values %in% 14) & any(chk$lengths == 4)) {
        values[values == 14] <- 1
        st <- find_straight_top(values)
      } else {
        st <- NA
      }

      return(st)
    }
    # Find four of a kind (NA if no quads)
    find_quads <- function(values) {
      freq <- table(values)

      return(
        ifelse(any(freq == 4), max(as.integer(names(freq)[freq == 4])), NA)
      )
    }
    # Find top 2 three of a kind
    find_trips <- function(values) {
      # Pull Values for all three of a kinds in order of value
      freq <- table(values)
      trips <- sort(as.integer(names(freq)[freq == 3]), decreasing = TRUE)

      # Pull the one with the most value first
      trip.top <- ifelse(length(trips) >= 1, trips[1], NA)
      # If more than one three of a kind, pull the second highest value
      trip.bot <- ifelse(length(trips) >= 2, trips[2], NA)

      return(
        c(trip.top, trip.bot)
      )
    }
    # Find top 2 pairs
    find_pairs <- function(values) {
      # Pull Values for all three of a kinds in order of value
      freq <- table(values)
      dubs <- sort(as.integer(names(freq)[freq == 2]), decreasing = TRUE)

      # Pull the one with the most value first
      dub.top <- ifelse(length(dubs) >= 1, dubs[1], NA)
      # If more than one three of a kind, pull the second highest value
      dub.bot <- ifelse(length(dubs) >= 2, dubs[2], NA)

      return(
        c(dub.top, dub.bot)
      )
    }

    ### Prep game ###
    suit1 <- apply(dealt_cards, MARGIN = 2, FUN = function(x) substr(x, 1, 1))
    valu1 <- apply(dealt_cards, MARGIN = 2, FUN = function(x) as.numeric(substr(x, 2, 3)))
    cards <- list(card = dealt_cards, suit = suit1, value = valu1)

    ### Prep results data ###
    final_info <- data.frame(
      final_hand_points = rep(0, nrow(cards$card)),
      final_hand = rep("", nrow(cards$card)),
      hand_type = rep("", nrow(cards$card)),
      hand_name = rep("", nrow(cards$card))
    )

    ### Straight Flush ###
    if (verbose) {print("Start Calculations")}
    # Requires to know if flush exists and straight exists first
    fl_suit <- apply(cards$suit, MARGIN = 1, FUN = find_flush_suit)
    st_top <- apply(cards$value, MARGIN = 1, find_straight_top)
    sf_pot <- !is.na(fl_suit) & !is.na(st_top)
    st_fl_top <- rep(NA_integer_, nrow(cards$suit))
    if (any(sf_pot)) {
      # Only use values that are part of the flush
      tmpry_v <- cards$value[sf_pot, , drop = FALSE]
      tmpry_s <- cards$suit[sf_pot, , drop = FALSE]
      n_cols <- ncol(tmpry_s)
      valid_cards <- matrix(rep(fl_suit[sf_pot], n_cols), ncol = n_cols)
      tmpry_v[tmpry_s != valid_cards] <- NA
      st_fl_top[sf_pot] <- apply(tmpry_v, MARGIN = 1, find_straight_top)

      st_fl_found <- !is.na(st_fl_top)
      st_top[st_fl_found] <- NA

      final_info[st_fl_found, ] <- do.call(rbind, lapply(seq_along(st_fl_top[st_fl_found]), FUN = function(x, high, suit) {
        hand_val <- ifelse(high[x] == 14, "09", "08")
        values <- sprintf(high[x]:(high[x] - 4), fmt = "%02s")
        fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
        values <- ifelse(values == "01", "14", values)
        fin_cards <- paste(paste(suit[x], values, sep = ""), collapse = ", ")
        hand_type <- "Straight Flush"
        hand_name <- ifelse(high[x] == 14, "Royal Flush", paste("Straight Flush -", high[x], "High", sep = " "))

        data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
      }, high = st_fl_top[st_fl_found], suit = fl_suit[st_fl_found]))
    }
    not_done <- is.na(st_fl_top)
    if (verbose) {print(sum(not_done))}

    ### Four of a Kind ###
    quads <- rep(NA_integer_, nrow(cards$value))
    quads[not_done] <- apply(cards$value[not_done, ], MARGIN = 1, find_quads)
    quads_found <- !is.na(quads)
    final_info[quads_found, ] <- do.call(rbind, lapply(seq_along(quads[quads_found]), FUN = function(x, qua, val, sui) {
      hand_val <- "07"
      if (!is.null(dim(val))) {
        kicker <- max(val[x,][val[x,] != qua[x]])
        kick_suit <- sui[x, val[x, ] == kicker][1]
      } else {
        kicker <- max(val[x][val[x] != qua[x]])
        kick_suit <- sui[val == kicker][1]
      }
      values <- sprintf(c(rep(qua[x], 4), kicker), fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      fin_cards <- paste(paste(c("H", "D", "C", "S", kick_suit), values, sep = ""), collapse = ", ")
      hand_type <- "Four of a Kind"
      hand_name <- paste("Four of a Kind - ", qua[x], "'s", sep = "")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, qua = quads[quads_found], val = cards$value[quads_found, ], sui = cards$suit[quads_found, ]))
    not_done <- not_done & is.na(quads)
    if (verbose) {print(sum(not_done))}

    ### Full House ###
    trips <- matrix(rep(NA_integer_, nrow(cards$value)*2), nrow = 2)
    pairs <- matrix(rep(NA_integer_, nrow(cards$value)*2), nrow = 2)
    flhs_top <- rep(NA_integer_, nrow(cards$value))
    flhs_bot <- rep(NA_integer_, nrow(cards$value))
    trips[, not_done] <- apply(cards$value[not_done, ], MARGIN = 1, find_trips)
    pairs[, not_done] <- apply(cards$value[not_done, ], MARGIN = 1, find_pairs)
    num_trips <- colSums(!is.na(trips))
    num_dubs <- colSums(!is.na(pairs))

    flhs_pot <- (num_trips >= 2) | (num_trips >= 1 & num_dubs >= 1)
    if (any(flhs_pot)) {
      flhs_top[flhs_pot] <- trips[1, flhs_pot]
      flhs_bot[flhs_pot] <- ifelse(!is.na(trips[2, flhs_pot]), trips[2, flhs_pot], pairs[1, flhs_pot])


      flhs_found <- !is.na(flhs_top)

      final_info[flhs_found, ] <- do.call(rbind, lapply(seq_along(flhs_top[flhs_found]), FUN = function(x, tri, dub, val, sui) {
        hand_val <- "06"
        values <- c(rep(tri[x], 3), rep(dub[x], 2))
        if (!is.null(dim(val))) {
          tri_suits <- sui[x, val[x, ] == tri[x]][1:3]
          dub_suits <- sui[x, val[x, ] == dub[x]][1:2]
        } else {
          tri_suits <- sui[val == tri[x]][1:3]
          dub_suits <- sui[val == dub[x]][1:2]
        }
        values <- sprintf(c(rep(tri[x], 3), rep(dub[x], 2)), fmt = "%02s")
        fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
        fin_cards <- paste(c(paste(tri_suits, values[1:3], sep = ""), paste(dub_suits, values[4:5], sep = "")), collapse = ", ")
        hand_type <- "Full House"
        hand_name <- paste(tri[x], "'s Full of ", dub[x], "'s", sep = "")

        data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
      }, tri = flhs_top[flhs_found], dub = flhs_bot[flhs_found], val = cards$value[flhs_found, ], sui = cards$suit[flhs_found, ]))
    }
    trips <- trips[1, , drop = TRUE]
    trips[!is.na(flhs_top)] <- NA
    not_done <- not_done & is.na(flhs_top)
    if (verbose) {print(sum(not_done))}

    ### Flush ###
    flushs <- rep(NA_integer_, nrow(cards$value))
    flushs[not_done] <- fl_suit[not_done]

    flushs_found <- !is.na(flushs)
    final_info[flushs_found, ] <- do.call(rbind, lapply(seq_along(flushs[flushs_found]), FUN = function(x, flu, val, sui) {
      hand_val <- "05"
      if (!is.null(dim(val))) {
        fl_cards <- sort(val[x,][sui[x, ] == flu[x]], decreasing = TRUE)[1:5]
      } else {
        print(val)
        fl_cards <- sort(val[sui == flu[x]], decreasing = TRUE)[1:5]
      }
      values <- sprintf(fl_cards, fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      fin_cards <- paste(paste(flu[x], values, sep = ""), collapse = ", ")
      hand_type <- "Flush"
      hand_name <- paste("Flush -", max(fl_cards), "High", sep = " ")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, flu = flushs[flushs_found], val = cards$value[flushs_found, ], sui = cards$suit[flushs_found, ]))
    not_done <- not_done & is.na(flushs)
    if (verbose) {print(sum(not_done))}

    ### Straight ###
    straights <- rep(NA_integer_, nrow(cards$value))
    straights[not_done] <- st_top[not_done]

    straights_found <- !is.na(straights)
    final_info[straights_found, ] <- do.call(rbind, lapply(seq_along(straights[straights_found]), FUN = function(x, top, val, sui) {
      hand_val <- "04"
      if (!is.null(dim(val))) {
        vals <- top[x]:(top[x] - 4)
        face <- vals
        face <- ifelse(face == 1, 14, face)
        st_suits <- sui[x, ][match(x = face, table = val[x, ])]
      } else {
        vals <- top:(top - 4)
        face <- vals
        face <- ifelse(face == 1, 14, face)
        st_suits <- sui[match(x = face, table = val)]
      }
      values <- sprintf(vals, fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      values <- sprintf(face, fmt = "%02s")
      fin_cards <- paste(paste(st_suits, values, sep = ""), collapse = ", ")
      hand_type <- "Straight"
      hand_name <- paste("Straight -", max(vals), "High", sep = " ")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, top = straights[straights_found], val = cards$value[straights_found, ], sui = cards$suit[straights_found, ]))
    not_done <- not_done & is.na(straights)
    if (verbose) {print(sum(not_done))}

    ### Three of a Kind ###
    triples <- rep(NA_integer_, nrow(cards$value))
    triples[not_done] <- trips[not_done]
    triples_found <- !is.na(triples)

    final_info[triples_found, ] <- do.call(rbind, lapply(seq_along(triples[triples_found]), FUN = function(x, three, val, sui) {
      hand_val <- "03"
      if (!is.null(dim(val))) {
        three <- three[x]
        tri_su <- sui[x, ][val[x, ] == three]
        kickers <- sort(val[x, ][val[x, ] != three], decreasing = TRUE)[1:2]
        kick_suits <- sui[x, ][match(x = kickers, table = val[x, ])][1:2]
      } else {
        tri_su <- sui[val == three]
        kickers <- sort(val[val != three], decreasing = TRUE)[1:2]
        kick_suits <- sui[match(x = kickers, table = val)][1:2]
      }
      values <- sprintf(c(rep(three, 3), kickers), fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      fin_cards <- paste(c(paste(tri_su, values[1:3], sep = ""), paste(kick_suits, values[4:5], sep = "")), collapse = ", ")
      hand_type <- "Three of a Kind"
      hand_name <- paste("Three of a Kind - ", three, "'s", sep = "")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, three = triples[triples_found], val = cards$value[triples_found, ], sui = cards$suit[triples_found, ]))
    not_done <- not_done & is.na(triples)
    if (verbose) {print(sum(not_done))}

    ### Two Pair ###
    topr_top <- rep(NA_integer_, nrow(cards$value))
    topr_bot <- rep(NA_integer_, nrow(cards$value))
    topr_pot <- num_trips == 0 & num_dubs >= 2 & not_done
    if (any(topr_pot)) {
      topr_top[topr_pot] <- pairs[1, topr_pot]
      topr_bot[topr_pot] <- pairs[2, topr_pot]

      final_info[topr_pot, ] <- do.call(rbind, lapply(seq_along(topr_top[topr_pot]), FUN = function(x, top, bot, val, sui) {
        hand_val <- "02"
        if (!is.null(dim(val))) {
          top <- top[x]
          bot <- bot[x]
          top_su <- sui[x, ][val[x, ] == top]
          bot_su <- sui[x, ][val[x, ] == bot]
          kicker <- sort(val[x, ][!val[x, ] %in% c(top, bot)], decreasing = TRUE)[1]
          kick_suit <- sui[x, ][match(x = kicker, table = val[x, ])][1]
        } else {
          top_su <- sui[val == top]
          bot_su <- sui[val == bot]
          kicker <- sort(val[!val %in% c(top, bot)], decreasing = TRUE)[1]
          kick_suit <- sui[match(x = kicker, table = val)][1]
        }
        values <- sprintf(c(rep(top, 2), rep(bot, 2), kicker), fmt = "%02s")
        fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
        fin_cards <- paste(c(paste(top_su, values[1:2], sep = ""), paste(bot_su, values[3:4], sep = ""), paste(kick_suit, values[5], sep = "")), collapse = ", ")
        hand_type <- "Two Pair"
        hand_name <- paste("Pair of ", top, "'s and ", bot, "'s", sep = "")
        data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
      }, top = topr_top[topr_pot], bot = topr_bot[topr_pot], val = cards$value[topr_pot, ], sui = cards$suit[topr_pot, ]))
    }
    pairs <- pairs[1, , drop = TRUE]
    not_done <- not_done & is.na(topr_top)
    if (verbose) {print(sum(not_done))}

    ### Pair ###
    doubles <- rep(NA_integer_, nrow(cards$value))
    doubles[not_done] <- pairs[not_done]
    doubles_found <- !is.na(doubles)

    final_info[doubles_found, ] <- do.call(rbind, lapply(seq_along(doubles[doubles_found]), FUN = function(x, two, val, sui) {
      hand_val <- "01"
      if (!is.null(dim(val))) {
        two <- two[x]
        bi_su <- sui[x, ][val[x, ] == two]
        kickers <- sort(val[x, ][val[x, ] != two], decreasing = TRUE)[1:3]
        kick_suits <- sui[x, ][match(x = kickers, table = val[x, ])][1:3]
      } else {
        bi_su <- sui[val == two]
        kickers <- sort(val[val != two], decreasing = TRUE)[1:3]
        kick_suits <- sui[match(x = kickers, table = val)][1:3]
      }
      values <- sprintf(c(rep(two, 2), kickers), fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      fin_cards <- paste(c(paste(bi_su, values[1:2], sep = ""), paste(kick_suits, values[3:5], sep = "")), collapse = ", ")
      hand_type <- "Pair"
      hand_name <- paste("Pair of ", two, "'s", sep = "")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, two = doubles[doubles_found], val = cards$value[doubles_found, ], sui = cards$suit[doubles_found, ]))
    not_done <- not_done & is.na(doubles)
    if (verbose) {print(sum(not_done))}

    ### High Card ###
    final_info[not_done, ] <- do.call(rbind, lapply(seq_along(not_done[not_done]), FUN = function(x, val, sui) {
      hand_val <- "00"
      if (!is.null(dim(val))) {
        kickers <- sort(val[x, ], decreasing = TRUE)[1:5]
        kick_suits <- sui[x, ][match(x = kickers, table = val[x, ])][1:5]
      } else {
        kickers <- sort(val, decreasing = TRUE)[1:5]
        kick_suits <- sui[match(x = kickers, table = val)][1:5]
      }
      values <- sprintf(kickers, fmt = "%02s")
      fin_points <- as.numeric(paste(c(hand_val, values), collapse = ""))
      fin_cards <- paste(paste(kick_suits, values, sep = ""), collapse = ", ")
      hand_type <- "High Card"
      hand_name <- paste(max(kickers), "High", sep = " ")
      data.frame(final_hand_points = fin_points, final_hand = fin_cards, hand_type = hand_type, hand_name = hand_name)
    }, val = cards$value[not_done, ], sui = cards$suit[not_done, ]))

    if (stage == "flop") {
      colnames(final_info) <- c("flop_hand_points", "flop_hand", "flop_hand_type", "flop_hand_name")
    } else if (stage == "turn") {
      colnames(final_info) <- c("turn_hand_points", "turn_hand", "turn_hand_type", "turn_hand_name")
    } else {
      colnames(final_info) <- c("final_hand_points", "final_hand", "final_hand_type", "final_hand_name")
    }

    return(final_info)
  }



  # flops <- grepl("card", colnames(games)) & (grepl("pocket", colnames(games)) | grepl("flop", colnames(games)))
  # games <- cbind(games, score(games = games[, flops], ret = "flop"))
  # turns <- grepl("card", colnames(games)) & (grepl("pocket", colnames(games)) |
  #                                              grepl("flop", colnames(games)) |
  #                                              grepl("turn", colnames(games)))
  # games <- cbind(games, score(games = games[, turns], ret = "turn"))
  rivers <- grepl("card", colnames(games)) & (grepl("pocket", colnames(games)) |
                                                grepl("flop", colnames(games)) |
                                                grepl("turn", colnames(games)) |
                                                grepl("river", colnames(games)))
  games <- cbind(games, score(dealt_cards = games[, rivers], stage = "river"))

  return(games)
}
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
