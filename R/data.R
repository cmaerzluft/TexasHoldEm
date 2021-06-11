#' A Standard Deck of Cards
#'
#' A dataset containing a representation of cards in a standard deck.
#'
#' @format A data frame with 52 rows and 5 columns:
#' \describe{
#'   \item{Suit}{Standard suit names ("Spades", "Hearts", "Clubs", "Diamonds")}
#'   \item{Value}{Standard card names (Ace, 2, 3, ..., Jack, Queen, King)}
#'   \item{suit_num}{A numeric ID for each suit (1 - 4)}
#'   \item{card_num}{A numeric ID for each card value (14, 2, 3, ..., 11, 12, 13)}
#'   \item{card_cd}{A numeric ID for each card (suit_num*100 + card_num)}
#' }
"card_deck"
