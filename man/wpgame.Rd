\name{wpgame}
\alias{wpgame}
\title{Play a Wild Poker Variant Game}

\description{This function will play a single game of a wildpoker variant,
and return a visual display of the hands dealt to each player and/or
community cards, a showdown matrix with the best hands chosen by each player,
their score and pot percent won, as well as some game-level statistics.}

\usage{wpgame(ngame, players, wcards = NULL)}

\arguments{
  \item{ngame}{ This is any supported game, such as "Seven Card Stud" or "Baseball". See
    \code{\link{wpsupportedgames}}.}

  \item{players}{An integer value, must be at least 2 and is limited by the number
    of cards that can be physically dealt.  (eg, Seven Card Stud has a max of 7 players)}

  \item{wcards}{A vector of string values which are either supported wildcard aliases or
    individual cards.  These cards are in addition to any wildcards that are a natural
    part of the poker game variant chosen with "ngame".  The table below shows the 
    supported aliases and their individual card vector equivalents:
    \itemize{
      \item \code{"Suicide King" or "KH"}	
      \item \code{"One Eyed Jacks" or c("JH", "JS")}   
      \item \code{"Deuces" or c("2H", "2C", "2D", "2S")}	
      \item \code{"Heinz 57" or c("5H", "5C", "5D", "5S", "7H", "7C", "7D", "7S")}
      \item \code{"Pregnant Threes" or  c("3H", "3C", "3D", "3S", "6H", "6C", "6D", "6S", "9H", "9C", "9D", "9S")}
      \item \code{"Dr Pepper" or c("2H", "2C", "2D", "2S", "4H", "4C", "4D", "4S", "10H", "10C", "10D", "10S")}
    }
  }	
}
\details{This program shuffles a deck, deals the cards and uses the cards allowed to each player to evaluate the best
poker hand for both the main hand and (if the variant includes a split hand) the alternate hand, then presents the
results in a list form, suitable to just display to a user, or pass along to another program}
\value{
A "pgame" list with the following structure:

 \item{$ntable}{A list of cards showing the deal visually, divided into $Extra, $Community and $Players.  Extra cards 
are not used in hands, but affect gameplay.  Community cards are shared by all players.  Player cards are used only
by individual players, and may include hole cards (prefixed with a #).  Wildcards are suffixed by a "w". }
 
 \item{$showdown}{A data frame with one row per player (rowname), $maintype and $splittype represent the type of hand
used by the player, eg 3-Kind or 7-High.  $mainhand and $splithand show the cards used in the hand, using the same
format as $ntable.  $mainscore and $splitscore are calculated using the value of the cards in the hand, including
"kickers", which allows two-pair Aces and 8s with Queen kicker to beat two-pair Aces and 8s with Jack kicker. The
$potpct column shows how much of the pot the individual player won.  The sum of all $potpct will be 1 if anybody won,
but it is possible in some games for nobody to win, and nobody wins, all $potpct will equal zero.}

 \item{$summary}{A character vector with basic information $game and $wild capture the ngame and wcard parameters,
$mwtype and $swtype capture the winning hands in the game}

 \item{$detail}{A numeric vector with other statistics, $mwscore and $ssscore capture the winning scores, 
$msscore and ssscore capture the number of players whose hand score matched the winning hand, $mstype and
$sstype capture the number of players whose hand type matched the winning hand, $wdeck is the number of wildcards
in the deck before the deal, $wcdeal is the number of wildcards in play after the deal, pnum captures the 
players parameter, and $mppct, $sppct and $bppct are used by \code{wpstats} to evaluate the effect of ties on pot
percentage for each player who won either main pot, split pot or both.} 
}

\references{\href{http://www.poker.com/game/index.htm}{poker.com} has an excellent reference on game variants which was the 
primary source for games rules not encountered in my own play.}

\note{This function is intended to give a sense of how an individual game plays, as well as a way to test that the underlying model 
is correctly dealing the cards and assigning the best hands to the showdown for each player.}

\author{Maintainer: Bradley Shanrock-Solberg  
\email{greblosb@gmail.com}}
\seealso{\code{\link{wpstats}}}
\examples{
set.seed(52)
wpgame("Iron Cross Last Wild", 6, "Deuces")
wpgame("Seven Stud Hi-Lo", 4, c("6C", "Suicide King"))
wpgame("Low Chicago", 4)


}
