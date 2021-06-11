devtools::load_all()
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
full_deck <- expand.grid(values, suits, stringsAsFactors = FALSE)
colnames(full_deck) <- c("Value", "Suit")
full_deck$card_pt <- c(14, 2:10, 11:13)
full_deck$card_cd <- as.integer(sprintf("%s%02d", as.numeric(factor(full_deck$Suit)), full_deck$card_pt))

AllHands <- as.data.frame(t(combn(full_deck$card_cd, 5)))
# AllHands <- AllHands[sample(nrow(AllHands), size = 10000), ]
colnames(AllHands) <- c("pocket_card1", "pocket_card2", "flop_card1", "flop_card2", "flop_card3")
scored_hands <- score_TexasHoldEm(AllHands, verbose = TRUE)

write.csv(scored_hands, "data-raw/AllPossibleHandScores.csv", row.names = FALSE)
