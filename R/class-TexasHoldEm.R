#' TexasHoldEm Class
#'
#' @param x data.frame, tibble, or data.table
#'
#' @details
#' Start creating hands and games from an OOP: https://adv-r.hadley.nz/oo.html
#' hand_id, flop_card1, flop_card2, flop_card3, turn_card, river_card, players
#'    players: player_id, pocket_card1, pocket_card2, pocket_info, flop_info, turn_info, river_info
#'      pocket_info: num_high_pocket, suited_pocket, paired_pocket, pocket_distance, straight_pocket
#'      flop_info: outs, hand_rank, hand_type, hand_name
#'        outs: sf, fk, fh, fl, st, tk tp, pa
#'      turn_info: outs, hand_rank, hand_type, hand_name
#'        outs: sf, fk, fh, fl, st, tk tp, pa
#'      river_info: hand_rank, hand_type, hand_name
#'
#' @return data for a round of Texas Hold'em with the appropriate type
#' @export
new_TexasHoldEm_base <- function(x) {
  stopifnot(is.data.frame(x))
  new_TexasHoldEm(x, class = "base")
}

#' @export
new_TexasHoldEm_tidy <- function(x) {
  stopifnot(tibble::is_tibble(x))
  new_TexasHoldEm(x, class = "tidy")
}

#' @export
new_TexasHoldEm_dt <- function(x) {
  stopifnot(data.table::is.data.table(x))
  new_TexasHoldEm(x, class = "dt")
}

#' @export
new_TexasHoldEm <- function(x = data.frame(), ..., class = character()) {
  structure(
    x, class = c("TexasHoldEm", class(x))
  )
}

# CM NOTE: SAVE FOR LATE: Validator
# validate_TexasHoldEm <- function(x) {
#   # Should only have one row if tibble or
#   # stopifnot(
#   #   "A hand of Texas Hold'em should only have one row"
#   # )
#   # should have community card columns (flop_card1, etc.) and players column
#   # players should be a data.frame
#   # players should have 2 or more rows
#   # players should have pocket_card1 and pocket_card2
# }
# CM NOTE: SAVE FOR LATE: Helper
# I think deal_cards actually serves this purpose
