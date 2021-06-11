#' Score Games of Texas Hold'em
#'
#' @param games
#'
#' @return
#' @export
# CM NOTE: Want to try bit wise method:
#   https://www.codeproject.com/Articles/569271/A-Poker-hand-analyzer-in-JavaScript-using-bit-math
#   Need to find vectorized bit functions in R though (or make them?)
poker_hands <- c(
  "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "Pair", "High Card"
)
#' @export
score_games <- function(games) {
  UseMethod("score_games")
}
#' @export
score_games.data.frame <- function(games) {
  # Initialize Results
  n_hands <- nrow(games)
  hand_rank <- rep.int(0, n_hands)
  hand_score <- rep(0, n_hands)

  # Pull cards
  cards <- games[, grepl("card", colnames(games))]

  # Pull Values/Suits
  card_value <- pull_value(x = cards)
  card_suits <- pull_suits(x = cards)

  # Find Duplicates (Quads, Trips, and Pairs)
  dup_cards <- find_duplicates(values = card_value, n_hands = n_hands)

  # Find Straights
  st_top <- find_straight(values = card_value, n_hands = n_hands)

  # Find Flushes
  fl_suit <- find_flush(suits = card_suits, n_hands = n_hands)

  # Find Straight Flush
  stfl_top <- find_straightflush(
    suits = card_suits, values = card_value,
    fl_suit = fl_suit, st_top = st_top,
    n_hands = n_hands
  )

  # Score Straight Flushes
  PickOut <- stfl_top != 0
  hand_rank[PickOut] <- 1L
  hand_score[PickOut] <- score_straightflush(top_card = stfl_top[PickOut])
  unscored <- !PickOut

  # Score Four of a Kinds
  PickOut <- unscored & dup_cards[, 1] != 0
  hand_rank[PickOut] <- 2L
  hand_score[PickOut] <- score_quad(quad_card = dup_cards[PickOut, 1], values = card_value[PickOut, ])
  unscored <- unscored & !PickOut

  # Score Full-houses
  PickOut <- unscored & dup_cards[, 2] != 0 & (dup_cards[, 3] != 0 | dup_cards[, 4] != 0)
  hand_rank[PickOut] <- 3L
  hand_score[PickOut] <- score_fullhouse(
    trip_card = dup_cards[PickOut, 2],
    # Pull the column that completes the full house (either second three of a kind or highest pair)
    pair_card = dup_cards[cbind(which(PickOut), c(3, 4)[as.numeric(PickOut & dup_cards[, 4] != 0) + 1][PickOut])]
  )
  unscored <- unscored & !PickOut

  # Score Flush
  PickOut <- unscored & fl_suit != 0
  hand_rank[PickOut] <- 4L
  hand_score[PickOut] <- score_flush(
    suit_card = fl_suit[PickOut],
    suits = card_suits[PickOut, ],
    values = card_value[PickOut, ]
  )
  unscored <- unscored & !PickOut

  # Score Straight
  PickOut <- unscored & st_top != 0
  hand_rank[PickOut] <- 5L
  hand_score[PickOut] <- score_straight(top_card = st_top[PickOut])
  unscored <- unscored & !PickOut

  # Score Three of a Kind
  PickOut <- unscored & dup_cards[, 2] != 0
  hand_rank[PickOut] <- 6L
  hand_score[PickOut] <- score_trips(trip_card = dup_cards[PickOut, 2], values = card_value[PickOut, , drop = FALSE])
  unscored <- unscored & !PickOut

  # Score Two Pair
  PickOut <- unscored & dup_cards[, 4] != 0 & dup_cards[, 5] != 0
  hand_rank[PickOut] <- 7L
  hand_score[PickOut] <- score_twopair(
    pair_cards = dup_cards[PickOut, c(4, 5), drop = FALSE],
    values = card_value[PickOut, , drop = FALSE]
    )
  unscored <- unscored & !PickOut

  # Score Pair
  PickOut <- unscored & dup_cards[, 4] != 0
  hand_rank[PickOut] <- 8L
  hand_score[PickOut] <- score_pair(pair_card = dup_cards[PickOut, 4], values = card_value[PickOut, , drop = FALSE])
  unscored <- unscored & !PickOut

  # Score High Card
  hand_rank[unscored] <- 9L
  hand_score[unscored] <- score_highcard(values = card_value[unscored, , drop = FALSE])

  # Add hand types, scores, place, and winner info to games
  games$hand_type <- factor(hand_rank, levels = 1:9, labels = poker_hands)
  levels(games$hand_type) <- poker_hands
  games$hand_score <- hand_score
  games$hand_place <- ave(-games$hand_score, games$hand_id, FUN = function(x) rank(x, ties.method = "min"))
  games$winner <- games$hand_place == 1

  return(games)
}
#' @export
score_games.tbl_df <- function(games) {
  # Initialize Results
  flat_games <- games %>% unnest(cols = player)
  n_hands <- nrow(flat_games)

  # Pull Values/Suits
  card_value <- flat_games %>% select(contains("card")) %>% pull_value()
  card_suits <- flat_games %>% select(contains("card")) %>% pull_suits()

  # Find Duplicates (Quads, Trips, and Pairs)
  dup_cards <- card_value %>% find_duplicates()

  # Find Straights
  st_top <- card_value %>% find_straight()

  # Find Flushes
  fl_suit <- card_suits %>% find_flush()

  # Find Straight Flush
  stfl_top <- card_suits %>%
    find_straightflush(
      values = card_value,
      fl_suit = fl_suit, st_top = st_top,
      n_hands = n_hands
    )

  # Put final hand info back together
  games <- flat_games %>%
    select(hand_id, player_id, pocket_card1, pocket_card2) %>%
    bind_cols(stfl_top = stfl_top, st_top = st_top, fl_suit = fl_suit, dup_cards) %>%
    # Hand Type
    mutate(
      hand_type = case_when(
        stfl_top != 0                                       ~ 1L, # Straight Flush
        out_four != 0                                       ~ 2L, # Four of a Kind
        out_three1 != 0 & (out_three2 != 0 | out_two1 != 0) ~ 3L, # Full House
        fl_suit != 0                                        ~ 4L, # Flush
        st_top != 0                                         ~ 5L, # Straight
        out_three1 != 0                                     ~ 6L, # Three of a Kind
        out_two1 != 0 & out_two2 != 0                       ~ 7L, # Two Pair
        out_two1 != 0                                       ~ 8L, # One Pair
        TRUE                                                ~ 9L  # High Card
      ),
      # Hand Score
      hand_score = case_when(
        hand_type == 1L ~ score_straightflush(top_card = stfl_top),
        hand_type == 2L ~ score_quad(quad_card = out_four, values = as.matrix(card_value)),
        hand_type == 3L & out_three2 != 0 ~ score_fullhouse(trip_card = out_three1, pair_card = out_three2),
        hand_type == 3L & out_two1 != 0 ~ score_fullhouse(trip_card = out_three1, pair_card = out_two1),
        hand_type == 4L ~ score_flush(suit_card = fl_suit, suits = as.matrix(card_suits), values = as.matrix(card_value)),
        hand_type == 5L ~ score_straight(top_card = st_top),
        hand_type == 6L ~ score_trips(trip_card = out_three1, values = as.matrix(card_value)),
        hand_type == 7L ~ score_twopair(
          pair_cards = as.matrix(.[, c("out_two1", "out_two2")]),
          values = as.matrix(card_value)
        ),
        hand_type == 8L ~ score_pair(pair_card = out_two1, values = as.matrix(card_value)),
        hand_type == 9L ~ score_highcard(values = as.matrix(card_value))
      ),
      hand_type = factor(hand_type, levels = 1:9, labels = poker_hands)
    ) %>%
    select(hand_id, player_id, pocket_card1, pocket_card2, hand_type, hand_score) %>%
    group_by(hand_id) %>%
    mutate(
      hand_place = rank(-hand_score, ties.method = "min"),
      winner = hand_place == 1
    ) %>%
    nest(player = !hand_id) %>%
    left_join(games %>% select(!player), by = "hand_id") %>%
    ungroup() %>%
    new_TexasHoldEm_tidy()

  return(games)
}

#' @export
score_games.data.table <- function(games) {
  # Initialize Results
  games <- copy(games)
  n_hands <- nrow(games)
  hand_rank <- rep.int(0, n_hands)
  hand_score <- rep(0, n_hands)

  # Pull cards
  cards <- games[, .SD, .SDcols = grep("card", colnames(games), value = TRUE)]

  # Pull Values/Suits
  card_value <- pull_value(x = cards)
  card_suits <- pull_suits(x = cards)

  # Find Duplicates (Quads, Trips, and Pairs)
  dup_cards <- find_duplicates(values = card_value, n_hands = n_hands)

  # Find Straights
  st_top <- find_straight(values = card_value, n_hands = n_hands)

  # Find Flushes
  fl_suit <- find_flush(suits = card_suits, n_hands = n_hands)

  # Find Straight Flush
  stfl_top <- find_straightflush(
    suits = card_suits, values = card_value,
    fl_suit = fl_suit, st_top = st_top,
    n_hands = n_hands
  )

  # Add hand types, scores, place, and winner info to games
  dup_cards <- as.matrix(dup_cards)
  card_value <- as.matrix(card_value)
  card_suits <- as.matrix(card_suits)

  # Hand Type
  games[, hand_type := factor(fcase(
    stfl_top != 0                       , 1L, # Straight Flush
    dup_cards[, "out_four"] != 0        , 2L, # Four of a Kind
    dup_cards[, "out_three1"] != 0 &
      (dup_cards[, "out_three2"] != 0 |
         dup_cards[, "out_two1"] != 0)  , 3L, # Full House
    fl_suit != 0                        , 4L, # Flush
    st_top != 0                         , 5L, # Straight
    dup_cards[, "out_three1"] != 0      , 6L, # Three of a Kind
    dup_cards[, "out_two1"] != 0 &
      dup_cards[, "out_two2"] != 0      , 7L, # Two Pair
    dup_cards[, "out_two1"] != 0        , 8L, # One Pair
    default                             = 9L  # High Card
  ), levels = 1:9, labels = poker_hands)]
  # Hand Score
  games[, hand_score := fcase(
    hand_type == "Straight Flush", score_straightflush(
      top_card = stfl_top),
    hand_type == "Four of a Kind", score_quad(
      quad_card = dup_cards[, "out_four"], values = card_value),
    hand_type == "Full House" & dup_cards[, "out_three2"] != 0, score_fullhouse(
      trip_card = dup_cards[, "out_three1"], pair_card = dup_cards[, "out_three2"]),
    hand_type == "Full House" & dup_cards[, "out_two1"] != 0, score_fullhouse(
      trip_card = dup_cards[, "out_three1"], pair_card = dup_cards[, "out_two1"]),
    hand_type == "Flush", score_flush(
      suit_card = fl_suit, suits = card_suits, values = card_value),
    hand_type == "Straight", score_straight(
      top_card = st_top),
    hand_type == "Three of a Kind", score_trips(
      trip_card = dup_cards[, "out_three1"], values = card_value),
    hand_type == "Two Pair", score_twopair(
      pair_cards = dup_cards[, c("out_two1", "out_two2")], values = card_value),
    hand_type == "Pair", score_pair(
      pair_card = dup_cards[, "out_two1"], values = card_value),
    hand_type == "High Card", score_highcard(
      values = card_value),
    default = NA
  )]
  # Hand Place
  games[, hand_place := rank(-hand_score, ties.method = "min"), by = hand_id]
  games[, winner := hand_place == 1]

  return(games)
}
