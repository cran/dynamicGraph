\name{returnVertexList}
\alias{vertexList}
\alias{returnVertexList}
\alias{Names}
\alias{Names<-}
\alias{Colors}
\alias{Colors<-}
\alias{Labels}
\alias{Labels<-}
\alias{LabelPositions}
\alias{LabelPositions<-}
\alias{Positions}
\alias{Positions<-}
\alias{Strata}
\alias{Strata<-}
\alias{Indices}
% \alias{Indices<-}
\alias{Visible}
\alias{Visible<-}

\title{ The vertex list }
\description{
  Return a list of vertices of classes containing the class \code{VertexProto}.
}
\usage{
returnVertexList(names, labels = NULL, types = NULL, 
                 strata = NULL, line = FALSE, N = 3, 
                 colors = ifelse(types == "TextVertex", 
                                 "FloralWhite", "DarkRed"), 
                 vertexClasses = validVertexClasses())
}
\arguments{
  \item{names}{ Vector of text strings with the \code{names} of the vertices. }
  \item{labels}{ Vector of text strings with the \code{labels} of the vertices. }
  \item{types}{ Vector of text strings with the \code{types} of the vertices. }
  \item{strata}{ Vector of integers with the \code{strata} of the vertices. }
  \item{line}{ Logical, if TRUE then the vertices are positioned on a
               \code{line}, else in a regular polygone, in a circle. }
  \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
  \item{colors}{ Vector of text strings with \code{colors} of the vertices. }
  \item{vertexClasses}{ The valid \code{\link{vertexClasses}}. }
}
% \details{ }
\value{
 A list of vertices of classes containing the class \code{VertexProto}.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{The methods
      \code{\link{Names}}, 
      \code{\link{Names<-}}, 
      \code{\link{Colors}}, 
      \code{\link{Colors<-}}, 
      \code{\link{Labels}}, 
      \code{\link{Labels<-}}, 
      \code{\link{LabelPositions}}, 
      \code{\link{LabelPositions<-}}, 
      \code{\link{Positions}}, 
      \code{\link{Positions<-}}, 
      \code{\link{Strata}}, 
      \code{\link{Strata<-}} and 
      \code{\link{Indices}} 
      are available for vertex lists.
      % \code{\link{Indices<-}}, 
      % \code{\link{Visible}}, 
      % \code{\link{Visible<-}}, 
}
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{
vertices <- returnVertexList(c("A", "B", "C", "D"),
                             labels = c("OrdinalVertex", "TextVertex", 
                               "ContinuousVertex", "DiscreteVertex"), 
                             types = c("Ordinal", "TextVertex",
                                       "Continuous", "Discrete"), N = 2)
Names(vertices)
Colors(vertices)
Labels(vertices)
LabelPositions(vertices)
Positions(vertices)
Strata(vertices)
Indices(vertices)
Names(vertices) <- c("a", "b", "c", "d")
Colors(vertices) <- rep("Blue", 4)
Labels(vertices) <- c("A", "B", "C", "D")
LabelPositions(vertices) <- matrix(rep(0, 12), ncol = 3)
Positions(vertices) <- matrix(rep(0, 12), ncol = 3)
Strata(vertices) <- rep(1, 4)
Names(vertices)
Colors(vertices)
Labels(vertices)
LabelPositions(vertices)
Positions(vertices)
Strata(vertices)
Indices(vertices)
}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
