\name{newBlock}
\alias{newBlock}
\alias{BlockProto}
\alias{ancestors}
\alias{ancestors<-}
\alias{descendants}
\alias{descendants<-}
\title{The block object}
\description{Returns an object of class \code{BlockProto}.}
\usage{
newBlock(stratum = 0, index = 0, position = matrix(rep(0, 6), ncol = 3),
         visible = TRUE, color = "Grey", label = "Label",
         label.position = c(0, 0, 0), ancestors = NULL)
}
\arguments{
  \item{stratum}{ Integer for the \code{stratum} of the block. 
	The block is identified by this stratum, and thus the
        strata of the blocks are different and unique.
        Each vertex can only belong to one block,
	and thus has assigned one stratum. }
  \item{index}{ Integer for the \code{index} of the block. }
  \item{position}{ Numeric vector for the \code{position} of the block,
      the two diagonal opposite corners. }
  \item{visible}{ Logical, if TRUE then the block is drawn. }
  \item{color}{ Text string for the \code{color} of the block. }
  \item{label}{ Text string for the \code{label} of the block. }
  \item{label.position}{ Numeric vector for the \code{label.position} 
                         of the block label. }
  \item{ancestors}{ Integer vector for the \code{ancestor} blocks of the
                    block. 
                    The slot with the \code{descendant} blocks is set
                    after the creation of the block.}
}
% \details{ }
\value{A object of class \code{BlockProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{
    The \code{BlockProto} class has the methods
    \code{\link{name}}, \code{\link{label}}, \code{\link{labelPosition}},
    \code{\link{position}}, \code{\link{color}}, \code{\link{stratum}},
    \code{\link{index}}, \code{\link{visible}},
    \code{\link{ancestors}} and \code{\link{descendants}},
    for extracting values
    and the replacement methods
    % \code{\link{name<-}}, 
    \code{\link{label<-}}, \code{\link{labelPosition<-}},
    \code{\link{position<-}}, \code{\link{color<-}}, \code{\link{stratum<-}},
    \code{\link{index<-}}, \code{\link{visible<-}},
    \code{\link{ancestors<-}} and \code{\link{descendants<-}}.
    Items are added to the pop up menu of a block
    by the method \code{\link{addToPopups}}.

    I have not yet had any use of a block name.
    Thus this slot is not available for the block,
    as neither for edges.
}
\seealso{\code{\link{newVertex}}}
\examples{
b <- newBlock()
str(b)

color(b)
label(b)
labelPosition(b)
name(b)
index(b)
position(b)
stratum(b)
ancestors(b)
descendants(b)
visible(b)

color(b) <- "grey"
label(b) <- "NameAndLabel"
labelPosition(b) <- c(1, 2, 3)
# name(b) <- "NameAndLabel" # Not possible!!!
index(b) <- 3
position(b) <- matrix(c( 10,  20,  30,  40, 
                        110, 120, 130, 140), byrow = TRUE, ncol = 4)
stratum(b) <- 1
ancestors(b) <- c(1, 2)
descendants(b) <- c(4, 5)
visible(b) <- FALSE

str(b)

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
