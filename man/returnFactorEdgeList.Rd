\name{returnFactorEdgeList}
\alias{factorEdgeList}
\alias{returnFactorEdgeList}
\title{ The factor edge list }
\description{
  Return a list of factor edges, each of class \code{FactorEdgeProto}.
}
\usage{
returnFactorEdgeList(edge.list, vertices, factorvertices = NULL,
                     width = 2, color = "DarkSlateGrey",
                     % oriented = FALSE,
                     type = NULL)
}
\arguments{
  \item{edge.list}{ A list of vectors identifying the edges (between vertices).
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or
                    a vector of text strings with the names of the vertices. }
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{VertexProto}. }
  \item{factorvertices}{ The list of \code{factorvertices}, 
      each of a class containing \code{FactorVertexProto}. }
  \item{width}{ A numeric with the initial \code{width} of the factor edges. }
  \item{color}{ A text string giving the initial \code{color}
        of the factor edges. }
  % \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{type}{ A text string giving the \code{type} of the factor edges. }
}
% \details{ }
\value{
    A list of edges, each of class \code{FactorEdgeProto}.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
    The methods of the edge list, \code{\link{returnEdgeList}},
    also applies for factor edge lists.

    No example is given here since the factor edge list usually
    will be returned by the function 
    \code{\link{returnFactorVerticesAndEdges}}.
    The application is similar to \code{\link{returnBlockEdgeList}},
    see example here.

 }
\seealso{
\code{\link{returnFactorVerticesAndEdges}}
 and \code{\link{returnBlockEdgeList}}. }
% \examples{ }
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
