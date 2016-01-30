\name{wildpoker-package}
\alias{wildpoker-package}
\alias{wildpoker}
\docType{package}
\title{
\packageTitle{wildpoker}
}
\description{
\packageDescription{wildpoker}
This package is provides insight into how the best hand for a poker game changes based on the following variables:
  \itemize{
    \item Game Variant    (which may include rules to split pot, wildcards that emerge in play, various dealing options)
    \item Player Number   (a good hand changes significantly as more players are added to the game)
    \item Wildcards Added (additional wildcards incorporated beyond the basic game variant)
  }
All hands are dealt completely and all players are assumed to stay in.  Therefore the package makes no assumptions 
about betting strategy and largely because of this fact, poker variants involving drawing, discarding, passing or
bidding on cards are not included at this time.
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{wildpoker}
\packageIndices{wildpoker}

This package is organized into three specfic functions and two data elements
  \itemize{
    \item \code{\link{wpgame}}  - Play a poker variant, see who won, game-level stats
    \item \code{\link{wprules}} - Display rules of a poker variant
    \item \code{\link{wpstats}} - Deal many games of a poker variant, capture statistics
    \item \code{\link{wpgraphs}} - Plot data from wpstats output into useful graphs
    \item \code{\link{wpsupportedgames}} - lists all supported games & basic description of each
    \item \code{\link{wpnotsupported}} - lists unsupported games & reason they are not yet included
  }
For most purposes calling wpgraphs is sufficient - results have been precalculated for
each supported game with from zero to 8 additional wildcards and from 2 to 8 players
(or less, if a 52 card deck can not deal 8 hands), using 1000 hands dealt and a random
number seed of 52.
}
\author{
\packageAuthor{wildpoker}
Maintainer: Bradley Shanrock-Solberg  
\email{greblosb@gmail.com}
}
\references{
Shanrock-Solberg, B (2015)
\href{https://docs.google.com/document/d/1dWBspfUFCDkYSXgYT64P7HuUigujfdISJV3R-qXgBOo}{Monte Carlo Methods for Decision Support (or Winning More at Dealer's Choice Poker)}
}

