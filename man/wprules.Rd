\name{wprules}
\alias{wprules}
\title{Display Wild Poker Variant Game Rules}

\description{This function will Print the rules of a single Wild Poker game variant}

\usage{wprules(ngame)}

\arguments{
  \item{ngame}{ This is any supported game, such as "Seven Card Stud" or "Baseball". See
    \code{\link{wpsupportedgames}}.}	
}
\details{This program displays values from the \code{\link{wpsupportedgames}} data frame with formatting}
\value{
Prints the following output.  Note that max players is capped at 8, even if a deck can support more.
  \item{GAME :}{ngame parameter (game type and max players in parentheses)} 
  \item{DEAL :}{cards dealt expressed as hole card, up card or community card - eg Seven Card Stud is "2 hole 4 up 1 hole"} 
  \item{BETS :}{who leads, ante vs blind bid, betting rounds by card number - eg Seven Card Stud is "ante & high up leads - cards 3 4 5 6 7"} 
  \item{WILD :}{any wildcards intrinsic to the game - eg Baseball is "3s and 9s" indicating that all 3 and 9 cards are wild} 
  \item{MAIN :}{Victory conditions for main hand - eg "High - Pair or better to win"} 
  \item{SPLIT:}{Victory conditions for split hand - eg "None" for no split hand, can be quite odd like "Sum of Spots in 7 card hand"} 
  \item{NOTES:}{NOTES: any other information about the game or common variants - eg in Baseball you sometimes have to pay for 3s, 4s and 9s} 
}

\references{\href{http://www.poker.com/game/index.htm}{poker.com} has an excellent reference on game variants which was the 
primary source for games rules not encountered in my own play.}


\author{Maintainer: Bradley Shanrock-Solberg  
\email{greblosb@gmail.com}}
\seealso{\code{\link{wpsupportedgames}}}
\examples{
set.seed(52)
wprules("Iron Cross Last Wild")
wprules("Seven Stud Hi-Lo")
wprules("Low Chicago")


}
