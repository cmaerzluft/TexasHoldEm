
# https://speakerdeck.com/jennybc/row-oriented-workflows-in-r-with-the-tidyverse?slide=33
# http://www.win-vector.com/blog/2018/04/neglected-r-super-functions/
find_flush_suit <- function(suits) {
  freq <- sort(table(suits), decreasing = TRUE)

  return(
    ifelse(max(freq) >= 5, as.integer(names(freq[1])), NA)
  )
}
test_df <- as.data.frame(game_suits[c(1:10, 798), ])

apply(test_df, MARGIN = 1, FUN = find_flush_suit)

as.integer(sapply(as.data.frame(t(test_df)), find_flush_suit))

find_flush_suit <- function(df) { # pFlush
  out <- vector(mode = "integer", length = nrow(df))
  for (i1 in seq_along(out)) {
    freq <- sort(table(as.numeric(df[i1, ])), decreasing = TRUE)

    out[i1] <- ifelse(max(freq) >= 5, as.integer(names(freq[1])), NA_integer_)
  }

  return(out)
}

pFlush(test_df)

for (i1 in 1:1)
bench::mark(
  app = {
    apply(test_df, MARGIN = 1, FUN = find_flush_suit)
  },
  sap = {
    as.integer(sapply(as.data.frame(t(test_df)), find_flush_suit))
  },
  pap = {
    pFlush(test_df)
  }
)



games2 <- as.data.frame(game_suits[sample(nrow(game_suits), 100*1, replace = T), ])
Timing <- bench::mark(check = FALSE,
                      app = {
                        apply(games2, MARGIN = 1, FUN = find_flush_suit)
                      },
                      sap = {
                        as.integer(sapply(as.data.frame(t(games2)), find_flush_suit))
                      },
                      pap = {
                        pFlush(games2)
                      }
)
Timing$N <- 100*1
for (i1 in 2:1000) {
  print(i1)
  games2 <- game_suits[sample(nrow(game_suits), 100*i1, replace = T), ]
  tmp <- bench::mark(check = FALSE,
                     app = {
                       apply(games2, MARGIN = 1, FUN = find_flush_suit)
                     },
                     sap = {
                       as.integer(sapply(as.data.frame(t(games2)), find_flush_suit))
                     },
                     pap = {
                       pFlush(games2)
                     }
  )
  tmp$N <- 100*i1
  Timing <- rbind(Timing, tmp)
}
Timing$med_time <- as.numeric(Timing$median)
Timing$expression <- as.character(Timing$expression)
Timing2 <- as.data.frame(Timing[, c("med_time", "N", "expression")])
library(ggplot2)
ggplot(Timing2) +
  geom_line(aes(y = med_time, x = N, colour = expression))
