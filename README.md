
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TexasHoldEm

<!-- badges: start -->
<!-- badges: end -->

The goal of TexasHoldEm is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("cmaerzluft/TexasHoldEm")
```

Rename “fn2()” to “fn()” and remove old fn() function, Applies to: \*
score\_flush/score\_flush2 and score\_games (which calls score\_flush2)
\* score\_pair/score\_pair2 and score\_games (which calls score\_pair2)

## To do

1.  Code for number of outs to make a better hand

<!-- -->

1.  need to include code to save actual outs (e.g. 307, 214, etc.) if
    possible

<!-- -->

2.  Test pull\_value\_nonvec vs pull\_value - see pull\_values\_suits.R
3.  Versions of code in base R, dplyr/tidyr, dtplyr, and data.table for
    speed comparison
4.  Start working with the real data

<!-- -->

1.  will need to model betting - including bluffing and stuff. Don’t
    need to figure out each individual bet though
2.  will need to figure out how to handle censored data

<!-- -->

5.  Develop code that determines how close a hand is to being made using
    what is available

<!-- -->

1.  for example, if someone has a 2, 3, 5, & 6. The code should say one
    card away from a straight.

<!-- -->

6.  Develop code that figures out how many potential hands can beat the
    current hand using the community cards plus two unknown cards.

<!-- -->

1.  for example, if I have a straight is it possible for a higher
    straight to exist? or can someone make a flush with the cards in the
    community pile
2.  want to include a prediction for someone’s range based on their
    betting pattern?

<!-- -->

7.  Start exploring adversarial networks

Agent based model: Need to model a players likelihood of
check/raise/fold function(opponent possible hands, P(bluffing),
P(successful bluff), change of getting a better hand) Need to model a
players hand range: possible\_hands = function(P(bluffing),
previous\_bets\*weights\_previous/pot\_size or bank\_size) P(bluffing) -
should be truncated normal with min = 0, max = 1, calibrated average
(how aggressive a player is) and calibrated standard deviation (how
predictable are they) should be a function of community card
characteristics
