#########################################################################################################################
test_that("Test Dealing Cards - base method", {
  method <- "base"
  # One player, One hand
  tmp <- deal_cards(n_players = 1, n_hands = 1, method = method)
  expect_s3_class(tmp, class = c("data.frame", "TexasHoldEm"))
  expect_equal(length(dim(tmp)), 2)
  expect_equal(ncol(tmp), 9)
  expect_equal(nrow(tmp), 1)
  expect_setequal(
    colnames(tmp), c("hand_id", "flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card", "player_id",
                     "pocket_card1", "pocket_card2"
    )
  )
  # One player, Two hands
  tmp <- deal_cards(n_players = 1, n_hands = 2, method = method)
  expect_equal(nrow(tmp), 2)
  # Twenty-two players, One hand
  expect_error(deal_cards(n_players = 22, n_hands = 1, method = method)) # Error
  # Two players, One hand
  tmp <- deal_cards(n_players = 2, n_hands = 1, method = method)
  expect_equal(nrow(tmp), 2)
  # Twenty-one players, ten hands
  tmp <- deal_cards(n_players = 21, n_hands = 10, method = method)
  expect_equal(nrow(tmp), 210)
})

test_that("Test Dealing Cards - tidy method", {
  method <- "tidy"
  # One player, One hand
  tmp <- deal_cards(n_players = 1, n_hands = 1, method = method)
  expect_s3_class(tmp, class = c("tbl_df", "TexasHoldEm"))
  expect_equal(length(dim(tmp)), 2)
  expect_equal(ncol(tmp), 7)
  expect_equal(nrow(tmp), 1)
  expect_setequal(
    colnames(tmp), c("hand_id", "flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card", "player")
  )
  expect_equal(nrow(tmp$player[[1]]), 1)
  # One player, Two hands
  tmp <- deal_cards(n_players = 1, n_hands = 2, method = method)
  expect_equal(nrow(tmp), 2)
  expect_equal(nrow(tmp$player[[1]]), 1)
  # Twenty-two players, One hand
  expect_error(deal_cards(n_players = 22, n_hands = 1, method = method)) # Error
  # Two players, One hand
  tmp <- deal_cards(n_players = 2, n_hands = 1, method = method)
  expect_equal(nrow(tmp), 1)
  expect_equal(nrow(tmp$player[[1]]), 2)
  # Twenty-one players, ten hands
  tmp <- deal_cards(n_players = 21, n_hands = 10, method = method)
  expect_equal(nrow(tmp), 10)
  expect_equal(nrow(tmp$player[[1]]), 21)
})

test_that("Test Dealing Cards - dt method", {
  method <- "dt"
  # One player, One hand
  tmp <- deal_cards(n_players = 1, n_hands = 1, method = method)
  expect_s3_class(tmp, class = c("data.table", "TexasHoldEm"))
  expect_equal(length(dim(tmp)), 2)
  expect_equal(ncol(tmp), 9)
  expect_equal(nrow(tmp), 1)
  expect_setequal(
    colnames(tmp), c("hand_id", "flop_card1", "flop_card2", "flop_card3", "turn_card", "river_card", "player_id",
                     "pocket_card1", "pocket_card2"
    )
  )
  # One player, Two hands
  tmp <- deal_cards(n_players = 1, n_hands = 2, method = method)
  expect_equal(nrow(tmp), 2)
  # Twenty-two players, One hand
  expect_error(deal_cards(n_players = 22, n_hands = 1, method = method)) # Error
  # Two players, One hand
  tmp <- deal_cards(n_players = 2, n_hands = 1, method = method)
  expect_equal(nrow(tmp), 2)
  # Twenty-one players, ten hands
  tmp <- deal_cards(n_players = 21, n_hands = 10, method = method)
  expect_equal(nrow(tmp), 210)
})

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
