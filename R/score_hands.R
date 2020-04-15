#' Score Straight Flush
#'
#' @param high_card The top value of the straight for a straight flush
#' @param flush_suit The suit of the flush for a straight flush
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_straight_flush <- function(high_card, flush_suit) {
  if (length(high_card) == 0 | length(flush_suit) == 0) return()

  # Final Points
  hand_val <- ifelse(high_card == 14, "09", "08")
  values <- t(mapply(seq, high_card, high_card - 4))
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  values <- ifelse(values == 1, 14, values)
  cards <- flush_suit*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Straight Flush"

  # Specific Hand Name
  hand_name <- ifelse(high_card == 14, "Royal Flush", paste("Straight Flush -", high_card, "High", sep = " "))

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Four of a Kind
#'
#' @param quad_value The card value of the four of a kind
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_four_of_a_kind <- function(quad_value, card_values, suit_values) {
  if (length(quad_value) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(quad_value)
  values <- matrix(rep(quad_value, each = 4), nrow = n_toscore, byrow = TRUE)
  values <- cbind(values, rep.int(0L, n_toscore))
  suits <- matrix(1:4, nrow = n_toscore, ncol = 4, byrow = TRUE)
  suits <- cbind(suits, rep.int(0L, n_toscore))

  # Find fifth card
  for (i1 in seq_len(n_toscore)) {
    vals <- card_values[i1, ]
    opts_v <- as.integer(vals[vals != quad_value[i1]])
    opts_s <- as.integer(suit_values[i1, ][vals != quad_value[i1]])

    values[i1, 5] <- max(opts_v)
    suits[i1, 5] <- opts_s[which.max(opts_v)]
  }

  # Final Points
  hand_val <- "07"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Four of a Kind"

  # Specific Hand Name
  hand_name <- paste("Four of a Kind - ", quad_value, "'s", sep = "")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Full House
#'
#' @param three the card value that makes up the three of a kind within a full house
#' @param two the card value that makes up the pair within a full house
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_full_house <- function(three, two, card_values, suit_values) {
  if (length(three) == 0 | length(two) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(three)

  values <- matrix(rep(three, each = 3), nrow = n_toscore, byrow = TRUE)
  values <- cbind(values, matrix(rep(two, each = 2), nrow = n_toscore, byrow = TRUE))
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits
  for (i1 in seq_len(n_toscore)) {
    suits[i1, 1:3] <- as.integer(suit_values[i1, which(card_values[i1, ] %in% values[i1, 1])])
    suits[i1, 4:5] <- as.integer(suit_values[i1, which(card_values[i1, ] %in% values[i1, 4])])[1:2]
  }

  # Final Points
  hand_val <- "06"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Full House"

  # Specific Hand Name
  hand_name <- paste(three, "'s Full of ", two, "'s", sep = "")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Flush
#'
#' @param suit The suit that makes up the flush
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_flush <- function(suit, card_values, suit_values) {
  if (length(suit) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(suit)
  suits <- matrix(rep(suit, each = 5), nrow = n_toscore, byrow = TRUE)
  values <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits
  for (i1 in seq_len(n_toscore)) {
    values[i1, ] <- sort(as.integer(card_values[i1, which(suit_values[i1, ] %in% suits[i1, 1])]), decreasing = TRUE)[1:5]
  }

  # Final Points
  hand_val <- "05"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Flush"

  # Specific Hand Name
  hand_name <- paste("Flush -", do.call(pmax.int, as.data.frame(values)), "High", sep = " ")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Straight
#'
#' @param straight_top The value of the high end of the straight
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_straight <- function(straight_top, card_values, suit_values) {
  if (length(straight_top) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(straight_top)
  values <- t(mapply(seq, straight_top, straight_top - 4))
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits
  for (i1 in seq_len(n_toscore)) {
    suits[i1, ] <- as.integer(suit_values[i1, match(values[i1, ], card_values[i1, ])])
  }

  # Final Points
  hand_val <- "04"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Straight"

  # Specific Hand Name
  hand_name <- paste("Straight -", straight_top, "High", sep = " ")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Three of a kind
#'
#' @param three The value the cards that makes up the three of a kind
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_three_of_a_kind <- function(three, card_values, suit_values) {
  if (length(three) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(three)
  values <- matrix(rep(three, each = 3), nrow = n_toscore, ncol = 3, byrow = TRUE)
  values <- cbind(values, matrix(0L, nrow = n_toscore, ncol = 2))
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits and kickers
  for (i1 in seq_len(n_toscore)) {
    vals <- card_values[i1, ]
    suits[i1, 1:3] <- as.integer(suit_values[i1, which(vals %in% values[i1, 1])])
    values[i1, 4:5] <- sort(as.integer(vals[vals != three[i1]]), decreasing = TRUE)[1:2]
    suits[i1, 4:5] <- as.integer(suit_values[i1, match(values[i1, 4:5], card_values[i1, ])])
  }

  # Final Points
  hand_val <- "03"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Three of a Kind"

  # Specific Hand Name
  hand_name <- paste("Three of a Kind - ", three, "'s", sep = "")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Two Pair
#'
#' @param twoone The value of the highest valued pair
#' @param twotwo The value of the lowest valued pair
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_two_pair <- function(twoone, twotwo, card_values, suit_values) {
  if (length(twoone) == 0 | length(twotwo) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(twoone)
  values <- matrix(rep(twoone, each = 2), nrow = n_toscore, ncol = 2, byrow = TRUE)
  values <- cbind(values, matrix(rep(twotwo, each = 2), nrow = n_toscore, ncol = 2, byrow = TRUE))
  values <- cbind(values, matrix(0L, nrow = n_toscore, ncol = 1))
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits and kicker
  for (i1 in seq_len(n_toscore)) {
    vals <- card_values[i1, ]
    suits[i1, 1:2] <- as.integer(suit_values[i1, which(vals %in% values[i1, 1])])
    suits[i1, 3:4] <- as.integer(suit_values[i1, which(vals %in% values[i1, 3])])
    values[i1, 5] <- sort(as.integer(vals[!vals %in% c(twoone[i1], twotwo[i1])]), decreasing = TRUE)[1]
    suits[i1, 5] <- as.integer(suit_values[i1, match(values[i1, 5], card_values[i1, ])])
  }

  # Final Points
  hand_val <- "02"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Two Pair"

  # Specific Hand Name
  hand_name <- paste("Pair of ", twoone, "'s and ", twotwo, "'s", sep = "")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score Pair
#'
#' @param pair The value of the card that makes up the pair
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_pair <- function(pair, card_values, suit_values) {
  if (length(pair) == 0) return()

  # Initialize final cards matrices
  n_toscore <- length(pair)
  values <- matrix(rep(pair, each = 2), nrow = n_toscore, ncol = 2, byrow = TRUE)
  values <- cbind(values, matrix(0L, nrow = n_toscore, ncol = 3))
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits
  for (i1 in seq_len(n_toscore)) {
    vals <- card_values[i1, ]
    suits[i1, 1:2] <- as.integer(suit_values[i1, which(vals %in% values[i1, 1])])
    values[i1, 3:5] <- sort(as.integer(vals[vals != pair[i1]]), decreasing = TRUE)[1:3]
    suits[i1, 3:5] <- as.integer(suit_values[i1, match(values[i1, 3:5], card_values[i1, ])])
  }

  # Final Points
  hand_val <- "01"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "Pair"

  # Specific Hand Name
  hand_name <- paste("Pair of ", pair, "'s", sep = "")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score High Card
#'
#' @param card_values The card values of the cards in a hand
#' @param suit_values The suit values of the cards in a hand
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_high_card <- function(card_values, suit_values) {
  if (nrow(card_values) == 0) return()

  # Initialize final cards matrices
  n_toscore <- nrow(card_values)
  values <- matrix(0L, nrow = n_toscore, ncol = 5)
  suits <- matrix(0L, nrow = n_toscore, ncol = 5)

  # Find suits
  for (i1 in seq_len(n_toscore)) {
    vals <- card_values[i1, ]
    values[i1, ] <- sort(as.integer(card_values[i1, ]), decreasing = TRUE)[1:5]
    suits[i1, ] <- as.integer(suit_values[i1, match(values[i1, ], card_values[i1, ])])
  }

  # Final Points
  hand_val <- "00"
  card_score <- sprintf(
    fmt = "%010s",
    values %*% 10^(seq(from = 8, to = 0, by = -2))
  )
  fin_points <- as.numeric(sprintf("%s%s", hand_val, card_score))

  # Final Cards
  cards <- suits*100 + values
  fin_cards <- paste(cards[,1], cards[,2], cards[,3], cards[,4], cards[,5], sep = ", ")

  # Generic Hand Name
  hand_type <- "High Card"

  # Specific Hand Name
  hand_name <- paste(values[, 1], "High", sep = " ")

  return(data.frame(
    final_hand_points = fin_points,
    final_hand = fin_cards,
    hand_type = hand_type,
    hand_name = hand_name
  ))
}

#' Score All Hands
#'
#' @param dealt_cards The cards dealt to all players in all hands
#' @param stage The stage where the hand is being analyzed (flop, turn, or river)
#' @param verbose Should R print updates as it assigns scores to individuals
#'
#' @return a data.frame with final_hand_points, final_hand, hand_type, and hand_name
#' @export
score_all_hands <- function(dealt_cards, stage = "river", verbose = FALSE) {
  # Prep results data ----------------------------------------
  n_hands <- nrow(dealt_cards)
  final_info <- data.frame(
    final_hand_points = rep(0, n_hands),
    final_hand = rep("", n_hands),
    hand_type = rep("", n_hands),
    hand_name = rep("", n_hands)
  )

  if (verbose) {print("Start Calculations")}
  # Find best hand  ----------------------------------------
  # Pull Values/Suits
  game_value <- pull_value(dealt_cards)
  game_suits <- pull_suits(dealt_cards)

  # Find Flushs
  fl_suit <- find_flush_suit(game_suits)

  # Find Straights
  st_top <- find_straight_top(game_value)

  # Find Straight Flushes using known flush/straight combo-hands
  sf_pot <- !is.na(fl_suit) & !is.na(st_top)
  st_fl_top <- rep(NA_integer_, n_hands)
  if (any(sf_pot)) {
    tmpry_v <- game_value[sf_pot, , drop = FALSE]
    tmpry_s <- game_suits[sf_pot, , drop = FALSE]
    n_cols <- ncol(tmpry_s)
    # Only use values that are part of the flush should be looked at to see if they are a straight as well
    valid_cards <- matrix(rep(fl_suit[sf_pot], n_cols), ncol = n_cols)
    tmpry_v[tmpry_s != valid_cards] <- NA_integer_
    st_fl_top[sf_pot] <- find_straight_top(tmpry_v)

    # Score the straight flushes
    st_fl_found <- !is.na(st_fl_top)
    final_info[st_fl_found, ] <- score_straight_flush(
      high_card = st_fl_top[st_fl_found],
      flush_suit = fl_suit[st_fl_found]
    )
  } else {
    st_fl_found <- rep.int(FALSE, n_hands)
  }
  not_done <- !st_fl_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Straight Flushes: %s", sum(not_done)))}

  # Find Four of a Kinds
  quads <- rep(NA_integer_, n_hands)
  quads[not_done] <- find_quads(game_value[not_done, ])

  # Score the four-of-a-kinds
  quads_found <- !is.na(quads)
  final_info[quads_found, ] <- score_four_of_a_kind(
    quad_value = quads[quads_found],
    card_values = game_value[quads_found, , drop = FALSE],
    suit_values = game_suits[quads_found, , drop = FALSE]
  )
  not_done <- not_done & !quads_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Four of a Kinds: %s", sum(not_done)))}

  # Find Three of a Kinds
  top_trip <- rep(NA_integer_, n_hands)
  top_trip[not_done] <- find_tripsNpairs(game_value[not_done, ], find = 1)

  # Find Second Three of a Kind or Top Pair
  bt_or_tp <- rep(NA_integer_, n_hands)
  bt_or_tp[not_done] <- find_tripsNpairs(game_value[not_done, ], find = 2,
                                         top_tripple = top_trip[not_done])

  # Find Second Pair
  bot_pair <- rep(NA_integer_, n_hands)
  bot_pair[not_done] <- find_tripsNpairs(game_value[not_done, ], find = 3,
                                         top_tripple = top_trip[not_done],
                                         second_dup = bt_or_tp[not_done])

  # Find Full House using known Trips/Pair combo hands
  flhs_found <- !is.na(top_trip) & !is.na(bt_or_tp)

  # Score the Full House
  final_info[flhs_found, ] <- score_full_house(
    three = top_trip[flhs_found],
    two = bt_or_tp[flhs_found],
    card_values = game_value[flhs_found, , drop = FALSE],
    suit_values = game_suits[flhs_found, , drop = FALSE]
  )
  not_done <- not_done & !flhs_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Full Houses: %s", sum(not_done)))}

  # Find Remaining Flushes
  flushs <- rep(NA_integer_, n_hands)
  flushs[not_done] <- fl_suit[not_done]
  flushs_found <- !is.na(flushs)

  # Score Flush
  final_info[flushs_found, ] <- score_flush(
    suit = fl_suit[flushs_found],
    card_values = game_value[flushs_found, , drop = FALSE],
    suit_values = game_suits[flushs_found, , drop = FALSE]
  )
  not_done <- not_done & !flushs_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Flushes: %s", sum(not_done)))}

  # Find Remaining Straights
  straights <- rep(NA_integer_, n_hands)
  straights[not_done] <- st_top[not_done]
  straights_found <- !is.na(straights)

  # Score Straight
  final_info[straights_found, ] <- score_straight(
    straight_top = st_top[straights_found],
    card_values = game_value[straights_found, , drop = FALSE],
    suit_values = game_suits[straights_found, , drop = FALSE]
  )
  not_done <- not_done & !straights_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Straights: %s", sum(not_done)))}

  # Find Remaining Three of a Kinds
  triples_found <- !is.na(top_trip) & not_done

  # Score Three of a Kind
  final_info[triples_found, ] <- score_three_of_a_kind(
    three = top_trip[triples_found],
    card_values = game_value[triples_found, , drop = FALSE],
    suit_values = game_suits[triples_found, , drop = FALSE]
  )
  not_done <- not_done & !triples_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Three of a Kinds: %s", sum(not_done)))}

  # Find Remaining Two Pairs
  twpa_found <- !is.na(bt_or_tp) & !is.na(bot_pair) & not_done

  # Score Two Pair
  final_info[twpa_found, ] <- score_two_pair(
    twoone = bt_or_tp[twpa_found],
    twotwo = bot_pair[twpa_found],
    card_values = game_value[twpa_found, , drop = FALSE],
    suit_values = game_suits[twpa_found, , drop = FALSE]
  )
  not_done <- not_done & !twpa_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Two Pairs: %s", sum(not_done)))}

  # Find Remaining Pairs
  pair_found <- !is.na(bt_or_tp) & is.na(bot_pair) & not_done

  # Score Pair
  final_info[pair_found, ] <- score_pair(
    pair = bt_or_tp[pair_found],
    card_values = game_value[pair_found, , drop = FALSE],
    suit_values = game_suits[pair_found, , drop = FALSE]
  )
  not_done <- not_done & !pair_found
  if (verbose) {print(sprintf("Hands Unscored after searching for Pairs: %s", sum(not_done)))}

  # Score High Card
  final_info[not_done, ] <- score_high_card(
    card_values = game_value[not_done, , drop = FALSE],
    suit_values = game_suits[not_done, , drop = FALSE]
  )

  if (stage == "flop") {
    colnames(final_info) <- c("flop_hand_points", "flop_hand", "flop_hand_type", "flop_hand_name")
  } else if (stage == "turn") {
    colnames(final_info) <- c("turn_hand_points", "turn_hand", "turn_hand_type", "turn_hand_name")
  } else {
    colnames(final_info) <- c("final_hand_points", "final_hand", "final_hand_type", "final_hand_name")
  }

  return(final_info)
}
