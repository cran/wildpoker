\name{wpstats}
\alias{wpstats}
\title{Deal Many Hands of a Wild Poker Variant Game}
\description{This function deals a large number of hands of poker and captures
the game statistics as observations in a "raw" data frame, then summarizes the
results into a "gstat" list, suitable for graphing with the \code{wpgraphs} 
function.}
\usage{wpstats(ngame, players, wcards = NULL, numdeal = 1000, seed = 52, raw = FALSE)}

\arguments{
  \item{ngame}{ This is any supported game, such as "Seven Card Stud" or "Baseball". See
    \code{\link{wpsupportedgames}}.}

  \item{players}{An integer value, must be at least 2 and is limited by the number
    of cards that can be physically dealt.  (eg, Seven Card Stud has a max of 7 players)}

  \item{wcards}{A vector of string values which are either supported wildcard aliases or
    individual cards.  These cards are in addition to any wildcards that are a natural
    part of the poker game variant chosen with "ngame".  The table below shows the 
    supported aliases and their individual card vector equivalnets:
    \itemize{
      \item \code{"Suicide King" or "KH"}	
      \item \code{"One Eyed Jacks" or c("JH", "JS")}   
      \item \code{"Deuces" or c("2H", "2C", "2D", "2S")}	
      \item \code{"Heinz 57" or c("5H", "5C", "5D", "5S", "7H", "7C", "7D", "7S")}
      \item \code{"Pregnant Threes" or  c("3H", "3C", "3D", "3S", "6H", "6C", "6D", "6S", "9H", "9C", "9D", "9S")}
      \item \code{"Dr Pepper" or c("2H", "2C", "2D", "2S", "4H", "4C", "4D", "4S", "10H", "10C", "10D", "10S")}
    }}

  \item{numdeal}{Number of hands to deal, must be positive integer}

  \item{seed}{random number seed set at beginning, must be positive integer}

  \item{raw}{If TRUE the full "raw" data frame is returned instead of the gstat list}	
}
\details{This program calls the \code{wpgame} function \code{numdeal} times,
   generating a raw data frame.  This data frame is then consolidated into
   a gstat list, whose format depends on \code{ngame}.}
\value{
if raw is TRUE, returns a rawstat data frame, # of rows = numdeal
  \tabular{rlll}{
    [,1] \tab game \tab chr \tab pname parameter \cr
    [,2] \tab pnum \tab numeric \tab players parameter \cr
    [,3] \tab wild  \tab chr \tab wcard parameter\cr
    [,4] \tab mwtype \tab chr \tab Winning hand type of Main hand\cr
    [,5] \tab swtype \tab chr \tab Winning hand type of Split hand\cr
    [,6] \tab mwscore \tab numeric \tab score of winning main hand\cr
    [,7] \tab msscore \tab numeric \tab Num players score = winning Main score \cr
    [,8] \tab mstype  \tab numeric \tab Num players type = winning Main type \cr 
    [,9] \tab swscore \tab numeric \tab score of winning Split hand\cr
    [,10] \tab ssscore \tab numeric \tab Num players score = winning Split score \cr
    [,11] \tab sstype  \tab numeric \tab Num players type = winning Split type \cr 
    [,12] \tab mppct \tab numeric \tab percent won if Main hand is won\cr
    [,13] \tab sppct \tab numeric \tab percent won if Split hand is won \cr
    [,14] \tab bppct  \tab numeric \tab percent won if Both hands are won \cr 
    [,15] \tab wcdeck \tab numeric \tab wildcards in deck before the deal \cr
    [,16] \tab wcdeal  \tab numeric \tab wildcards in deck after the deal \cr         
  }
if raw is FALSE, returns a gstat list
  \item{$game}{a vector of the pname, players and wcards parameters}
  \item{$stats}{a vector of the numdeal & seed parameters, and the number of
wildcards added to the deck by the wcards parameter}
  \item{$pmain}{a table used for graphing win percentage chance of Main hand type by number of players}
  \item{$cmain}{a table used for graphing count of winnning Main hand types by number of players}
  \item{$wmain}{a table used for graphing count of winning Main hand types by wildcard count}
  \item{$psplit}{a table used for graphing win percentage chance of Split hand type by number of players}
  \item{$csplit}{a table used for graphing count of winnning Split hand types by number of players}
  \item{$wsplit}{a table used for graphing count of winning Split hand types by wildcard count}
  \item{$potpct}{a table used for graphing count of percentage of pot won if a hand wins}
}
\references{\href{http://www.poker.com/game/index.htm}{poker.com} has an excellent reference on game variants which was the 
primary source for games rules not encountered in my own play.}

\note{This function is intended to allow exposing the raw data behind the pre-calculated results
normally used by wpgraphs or to allow running for different random number seeds, number of hands
dealt, nonstandard additional wildcards or (when games allow) more than eight players.

It takes about .005 seconds to evaluate each hand - 6 players takes 3 times as long as 2 players,
a split hand variant takes twice as long as a variant that has only a main hand.  Dealing 1000
rounds of most variants takes a minute or two, but Omaha and Courcheval build hands out of subsets
of the player hands, requiring up to 70 separate hand evaluations for each player.  This means that
1000 hands of those variants can take a couple of hours to generate.

For this reason, gstat values for all supported game variants have already been calculated for
2-8 players and 0-7 additional wildcards - if interested in any of these standard combinations,
there is no need to call the \code{wpstats} function, \code{wpgraphs} has the required gstat list 
already.}

\author{Maintainer: Bradley Shanrock-Solberg  
\email{greblosb@gmail.com}}
\seealso{\code{\link{wpgame}}, \code{\link{wpgraphs}}}
\examples{
# example of the wpstats run to generate raw statistics
rawstat <- wpstats("Iron Cross Last Wild", 6, "Deuces", numdeal = 10, raw = TRUE)
head(rawstat)

# example of wpstats run to generate a gstat object, used by wpgraphs
wpstats("Seven Stud Hi-Lo", 4, numdeal = 10, seed = 100)


}