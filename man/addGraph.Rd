\name{addGraph}
\alias{addGraph}

\title{ Add graph to existing dynamic graph window }

\description{
  Add graph to existing dynamic graph window
}

\usage{
addGraph(linkDynamicGraph, from = NULL, to = NULL, edge.list = NULL, 
         Object = NULL, slave = FALSE, 
         oriented = FALSE, edgeColor = "black", 
         factorVertexColor = "default", factorEdgeColor = "brown", 
         blockEdgeColor = "default", title = "Add", 
         returnLink = FALSE, ...)
}

\arguments{
  \item{linkDynamicGraph}{ A "reference" to an active dynamic graph window,
	       returned from \code{\link{dynamicGraphMain}},
	       \code{\link{DynamicGraph}}, 
               \code{\link{redrawGraphWindow}}, 
               or \code{\link{addGraph}}. }
  \item{from}{ If not \code{edge.list} is given:
        The indices of the first endpoints of the edges. }
  \item{to}{ If not \code{edge.list} is given:
        The indices of the second endpoints of the edges. }
  \item{edge.list}{ If not \code{from} and \code{to} are given:
        A list where each item specifies an edge by a vector
        of the indices or names of the vertices. }
  \item{Object}{ ~~Describe \code{Object} here~~ }
  \item{slave}{ ~~Describe \code{slave} here~~ }
  \item{oriented}{ Logical. If \code{oriented} is set to TRUE
        then the edges are oriented, also when no block structure is given. }
  \item{edgeColor}{ Single text string with the \code{edgeColor}
        of the edges. }
  \item{factorVertexColor}{ Single text string with the
        \code{factorVertexColor} of the factor vertices. 
        If \code{factorVertexColor} is "default" then
        the color of a factor vertex will depend on the
        type of the generator of the factor.}
  \item{factorEdgeColor}{ Single text string with the \code{factorEdgeColor}
        of the factor edges. }
  \item{blockEdgeColor}{ "default", or list with two text strings for colors.
        % \code{blockEdgeColor} is similar to \code{edgeColor}.
        The two colors are used for respectively edges
        between blocks and for edges between blocks 
        and vertices.}
  \item{title}{ Text string with the title set on the graph window. }
  \item{returnLink}{ Logical, if \code{returnReference} is TRUE
        then a reference to the dynamic graph is returned, else an object
        with the graph lattice is returned. }
  \item{\dots}{ Additional arguments to \code{\link{dynamicGraphMain}}. }
}

% \details{ }

% \value{}

% \references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg}

% \note{ ~~further notes~~ }

\seealso{See also \code{\link{DynamicGraph}} and all the other functions
	of this package. }

\examples{
}

\keyword{ dplot }
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graphs }
\keyword{ methods }
\keyword{ multivariate }
