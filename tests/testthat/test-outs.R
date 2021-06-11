#########################################################################################################################
test_that("Straight Flush outs Function Work", {
  # After the Flop
  expect_equal(outs_sf(c(309, 202, 410, 307, 102), stage = "flop", output = "counts"), 0) # Nothing
  expect_equal(outs_sf(c(309, 310, 204, 302, 303), stage = "flop", output = "counts"), 0) # Flush potential, no straight
  expect_equal(outs_sf(c(310, 311, 307, 302, 303), stage = "flop", output = "counts"), 2) # Wide gap but still possible
  expect_equal(outs_sf(c(309, 202, 310, 307, 302), stage = "flop", output = "counts"), 3) # Have 3, Need: One in, one out
  expect_equal(outs_sf(c(309, 202, 311, 307, 403), stage = "flop", output = "counts"), 2) # Have 3, Need: Two in
  expect_equal(outs_sf(c(309, 202, 310, 308, 403), stage = "flop", output = "counts"), 4) # Have 3, Need: Two out
  expect_equal(outs_sf(c(309, 202, 310, 307, 306), stage = "flop", output = "counts"), 2) # Have 4, Need: One in
  expect_equal(outs_sf(c(309, 202, 310, 308, 307), stage = "flop", output = "counts"), 3) # Have 4, Need: One out
  expect_equal(outs_sf(c(314, 202, 302, 303, 304), stage = "flop", output = "counts"), 2) # Have 4, Need: One out Ace-lo
  expect_equal(outs_sf(c(314, 313, 312, 311, 304), stage = "flop", output = "counts"), 1) # Have 4, Need: One out Ace-hi
  expect_equal(outs_sf(c(302, 202, 303, 304, 412), stage = "flop", output = "counts"), 3) # Have 3 with two, Need: Two
  expect_equal(outs_sf(c(314, 302, 303, 312, 313), stage = "flop", output = "counts"), 4) # Have 3, Ace either way
  expect_equal(outs_sf(c(306, 308, 309, 310, 312), stage = "flop", output = "counts"), 3) # Have 5 Need: One in
  expect_equal(outs_sf(c(309, 310, 308, 312, 311), stage = "flop", output = "counts"), 2) # Have Straight with room above
  expect_equal(outs_sf(c(314, 310, 313, 312, 311), stage = "flop", output = "counts"), 0) # Have Royal Flush
  # After the Turn
  expect_equal(outs_sf(c(313, 202, 410, 310, 307, 304), stage = "turn", output = "counts"), 0) # Flush, no straight
  expect_equal(outs_sf(c(313, 312, 309, 308, 303, 304), stage = "turn", output = "counts"), 0) # Wide gap not possible
  expect_equal(outs_sf(c(313, 312, 311, 309, 303, 304), stage = "turn", output = "counts"), 1) # Have 4, Need: One in
  expect_equal(outs_sf(c(309, 202, 310, 308, 307, 207), stage = "turn", output = "counts"), 2) # Have 4, Need: One out
  expect_equal(outs_sf(c(309, 310, 308, 312, 311, 208), stage = "turn", output = "counts"), 1) # Have Straight with room
})

#########################################################################################################################
test_that("Four of a Kind outs Function Work", {
  # After the Flop
  expect_equal(outs_fk(c(304, 208, 301, 111, 412), stage = "flop", output = "counts"), 0) # Nothing
  expect_equal(outs_fk(c(304, 204, 312, 202, 411), stage = "flop", output = "counts"), 2) # One Pair
  expect_equal(outs_fk(c(304, 204, 402, 202, 411), stage = "flop", output = "counts"), 4) # Two Pair
  expect_equal(outs_fk(c(304, 204, 404, 202, 411), stage = "flop", output = "counts"), 1) # Three of a Kind
  expect_equal(outs_fk(c(304, 204, 404, 111, 411), stage = "flop", output = "counts"), 3) # Full House
  expect_equal(outs_fk(c(304, 204, 104, 404, 312), stage = "flop", output = "counts"), 0) # Four of a Kind
  # After the Turn
  expect_equal(outs_fk(c(304, 204, 301, 111, 412, 207), stage = "turn", output = "counts"), 0) # One Pair
  expect_equal(outs_fk(c(304, 204, 301, 112, 412, 207), stage = "turn", output = "counts"), 0) # Two Pair
  expect_equal(outs_fk(c(304, 204, 404, 202, 411, 207), stage = "turn", output = "counts"), 1) # Three of a Kind
  expect_equal(outs_fk(c(304, 204, 404, 111, 411, 207), stage = "turn", output = "counts"), 1) # Full House
  expect_equal(outs_fk(c(304, 204, 404, 202, 402, 302), stage = "turn", output = "counts"), 2) # Two Three of a Kinds
  expect_equal(outs_fk(c(304, 204, 104, 404, 312, 207), stage = "turn", output = "counts"), 0) # Four of a kind
})

#########################################################################################################################
test_that("Full House outs Function Work", {
  # After the Flop
  expect_equal(outs_fh(c(204, 205, 307, 112, 402), stage = "flop", output = "counts"),  0) # Nothing
  expect_equal(outs_fh(c(204, 205, 304, 112, 402), stage = "flop", output = "counts"), 11) # One Pair
  expect_equal(outs_fh(c(204, 205, 304, 105, 402), stage = "flop", output = "counts"),  7) # Two Pair
  expect_equal(outs_fh(c(204, 205, 304, 104, 402), stage = "flop", output = "counts"), 46) # Three of a Kind
  expect_equal(outs_fh(c(204, 205, 304, 104, 405), stage = "flop", output = "counts"),  0) # Full House
  # After the Turn
  expect_equal(outs_fh(c(204, 205, 304, 112, 402, 313), stage = "turn", output = "counts"), 0) # One Pair
  expect_equal(outs_fh(c(204, 205, 304, 105, 402, 313), stage = "turn", output = "counts"), 4) # Two Pair
  expect_equal(outs_fh(c(204, 205, 304, 105, 402, 302), stage = "turn", output = "counts"), 6) # Three Pair
  expect_equal(outs_fh(c(204, 205, 304, 104, 402, 313), stage = "turn", output = "counts"), 9) # Three of a Kind
  expect_equal(outs_fh(c(204, 205, 304, 104, 405, 313), stage = "turn", output = "counts"), 0) # Full House
  expect_equal(outs_fh(c(204, 205, 304, 104, 405, 305), stage = "turn", output = "counts"), 0) # Two Three of a Kinds
})

#########################################################################################################################
test_that("Flush outs Function Work", {
  # After the Flop
  expect_equal(outs_fl(c(204, 205, 307, 112, 402), stage = "flop", output = "counts"),  0) # Nothing
  expect_equal(outs_fl(c(204, 205, 304, 212, 402), stage = "flop", output = "counts"), 10) # 3 suited cards
  expect_equal(outs_fl(c(204, 205, 304, 212, 213), stage = "flop", output = "counts"),  9) # 4 suited cards
  expect_equal(outs_fl(c(202, 205, 208, 212, 213), stage = "flop", output = "counts"),  8) # Flush with a 2 low kicker
  expect_equal(outs_fl(c(206, 205, 208, 212, 213), stage = "flop", output = "counts"),  5) # Flush with a 5 low kicker
  # After the Turn
  expect_equal(outs_fl(c(204, 205, 307, 212, 402, 102), stage = "turn", output = "counts"), 0) # Less than 4 suited cards
  expect_equal(outs_fl(c(204, 205, 304, 212, 213, 102), stage = "turn", output = "counts"), 9) # 4 suited, no straight
  expect_equal(outs_fl(c(204, 205, 304, 206, 208, 102), stage = "turn", output = "counts"), 8) # 4 suited, straight
  expect_equal(outs_fl(c(204, 205, 208, 212, 213, 102), stage = "turn", output = "counts"), 6) # 5 card Flush
  expect_equal(outs_fl(c(204, 205, 208, 206, 207, 102), stage = "turn", output = "counts"), 0) # 5 card Straight Flush
  expect_equal(outs_fl(c(204, 205, 208, 212, 213, 211), stage = "turn", output = "counts"), 5) # 6 card Flush
})

#########################################################################################################################
test_that("Straight outs Function Work", {
  # After the Flop
  expect_equal(outs_st(c(312, 310, 207, 402, 103), stage = "flop", output = "counts"),  0) # Nothing
  expect_equal(outs_st(c(310, 411, 307, 102, 303), stage = "flop", output = "counts"),  8) # Wide gap but still possible
  expect_equal(outs_st(c(309, 202, 310, 307, 302), stage = "flop", output = "counts"), 12) # Have 3, Need: One in, out
  expect_equal(outs_st(c(309, 202, 311, 307, 403), stage = "flop", output = "counts"),  8) # Have 3, Need: Two in
  expect_equal(outs_st(c(309, 202, 310, 308, 403), stage = "flop", output = "counts"), 16) # Have 3, Need: Two out
  expect_equal(outs_st(c(309, 202, 310, 307, 306), stage = "flop", output = "counts"),  8) # Have 4, Need: One in
  expect_equal(outs_st(c(309, 202, 310, 308, 307), stage = "flop", output = "counts"), 12) # Have 4, Need: One out
  expect_equal(outs_st(c(314, 202, 302, 303, 304), stage = "flop", output = "counts"),  8) # Have 4, Need: One out Ace-lo
  expect_equal(outs_st(c(314, 313, 312, 311, 304), stage = "flop", output = "counts"),  4) # Have 4, Need: One out Ace-hi
  expect_equal(outs_st(c(302, 202, 303, 304, 412), stage = "flop", output = "counts"), 12) # Have 3 with two, Need: Two
  expect_equal(outs_st(c(314, 302, 303, 312, 313), stage = "flop", output = "counts"), 16) # Have 3, Ace either way
  expect_equal(outs_st(c(306, 308, 309, 310, 312), stage = "flop", output = "counts"), 12) # Have 5 Need: One in
  expect_equal(outs_st(c(309, 310, 308, 312, 311), stage = "flop", output = "counts"),  8) # Have Straight with room
  expect_equal(outs_st(c(314, 310, 313, 312, 311), stage = "flop", output = "counts"),  0) # Have Royal Flush
  # After the Turn
  expect_equal(outs_st(c(313, 202, 410, 310, 307, 304), stage = "turn", output = "counts"), 0) # Flush, no straight
  expect_equal(outs_st(c(313, 312, 309, 308, 303, 304), stage = "turn", output = "counts"), 0) # Wide gap not possible
  expect_equal(outs_st(c(313, 312, 311, 309, 303, 304), stage = "turn", output = "counts"), 4) # Have 4, Need: One in
  expect_equal(outs_st(c(309, 202, 310, 308, 307, 207), stage = "turn", output = "counts"), 8) # Have 4, Need: One out
  expect_equal(outs_st(c(309, 310, 308, 312, 311, 208), stage = "turn", output = "counts"), 4) # Have Straight with room
})

#########################################################################################################################
test_that("Three of a Kind outs Function Work", {
  # After the Flop
  expect_equal(outs_tk(c(408, 304, 102, 211, 312), stage = "flop", output = "counts"), 15) # High Card
  expect_equal(outs_tk(c(304, 204, 312, 202, 411), stage = "flop", output = "counts"),  2) # One Pair
  expect_equal(outs_tk(c(304, 204, 402, 202, 411), stage = "flop", output = "counts"),  0) # Two Pair
  expect_equal(outs_tk(c(304, 204, 404, 202, 411), stage = "flop", output = "counts"),  0) # Three of a Kind
  # After the Turn
  expect_equal(outs_tk(c(304, 208, 301, 111, 412, 207), stage = "turn", output = "counts"), 0) # High Card
  expect_equal(outs_tk(c(304, 204, 301, 111, 412, 207), stage = "turn", output = "counts"), 2) # One pair
  expect_equal(outs_tk(c(304, 204, 301, 112, 412, 207), stage = "turn", output = "counts"), 0) # Two pair
  expect_equal(outs_tk(c(304, 204, 404, 202, 411, 207), stage = "turn", output = "counts"), 0) # Three of a Kind
})

#########################################################################################################################
test_that("Two Pair outs Function Work", {
  # After the Flop
  expect_equal(outs_tp(c(408, 304, 102, 211, 312), stage = "flop", output = "counts"), 47) # High Card
  expect_equal(outs_tp(c(304, 204, 312, 202, 411), stage = "flop", output = "counts"), 45) # One Pair
  expect_equal(outs_tp(c(304, 204, 402, 202, 411), stage = "flop", output = "counts"), 43) # Two Pair
  expect_equal(outs_tp(c(314, 214, 412, 211, 411), stage = "flop", output = "counts"),  7) # Two Pair
  expect_equal(outs_tp(c(314, 214, 412, 212, 411), stage = "flop", output = "counts"),  4) # Two Pair
  expect_equal(outs_tp(c(314, 214, 413, 213, 411), stage = "flop", output = "counts"),  0) # Two Pair
  # After the Turn
  expect_equal(outs_tp(c(304, 208, 302, 111, 412, 207), stage = "turn", output = "counts"),  0) # Nothing
  expect_equal(outs_tp(c(304, 204, 302, 111, 412, 207), stage = "turn", output = "counts"), 12) # One pair
  expect_equal(outs_tp(c(304, 204, 305, 112, 412, 207), stage = "turn", output = "counts"),  6) # Two pair can improve
  expect_equal(outs_tp(c(308, 208, 305, 112, 412, 207), stage = "turn", output = "counts"),  0) # Two pair can't improve
  expect_equal(outs_tp(c(304, 204, 307, 112, 412, 207), stage = "turn", output = "counts"),  0) # Three pair
})

#########################################################################################################################
test_that("Pair outs Function Work", {
  # After the Flop
  expect_equal(outs_pa(c(408, 304, 102, 211, 312), stage = "flop", output = "counts"), 47) # High Card
  expect_equal(outs_pa(c(304, 204, 312, 202, 411), stage = "flop", output = "counts"),  0) # One Pair
  # After the Turn
  expect_equal(outs_pa(c(304, 208, 302, 111, 412, 207), stage = "turn", output = "counts"), 18) # High Card

})

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
