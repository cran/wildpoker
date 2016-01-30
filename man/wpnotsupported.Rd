\name{wpnotsupported}
\docType{data}
\alias{wpnotsupported}
\title{Poker Games not supported by the wildpoker package}
\description{
  This data set shows games not supported and brief indications
  of the enhancements required to add them.  Anything involving
  modeling human behavior (drawing, discarding, bidding, passing)
  will be much more difficult to add in future than purely rules
  based variants (like extra hands dealt, win conditions, etc)
}
\usage{wpnotsupported}
\format{
A data frame with rownames describing the game and 2 variables:

rowname = name of the game (ngame parameter, eg, "Pineapple")
  \tabular{rlll}{
    [,1] \tab Issue \tab chr \tab Brief description of the issue type \cr
    [,2] \tab Notes \tab chr \tab Brief rules notes touching on game differences \cr
  }
Issue codes, in rough order of difficulty to add to existing logic
  \enumerate{
    \item \bold{strip}        = Need to discard cards from deck before dealing
    \item \bold{wild logic}   = additional wildcard logic required
    \item \bold{graphs}       = change scale of grouping logic for graphs
    \item \bold{win}          = Nonstandard Win conditions
    \item \bold{fold logic}   = Fold player hands under some conditions
    \item \bold{card steal}   = steal the best visible card from another player
    \item \bold{card bid}     = choose visible vs no card or blind card logic
    \item \bold{discards}     = introduce discard-only logic
    \item \bold{Draw Round}   = introduce discard and draw logic
    \item \bold{card pass}    = choose card to pass rather than discard
  }
}
\source{This data set assembled manually based on poker rules}
\references{
\href{http://www.poker.com/game/index.htm}{poker.com} has an excellent reference on game variants which was the 
primary source for games rules not encountered in my own play.

}
\seealso{\code{\link{wprules}}, \code{\link{wpsupportedgames}}}
\keyword{datasets}