\name{newFactorEdge}
\alias{newFactorEdge}
\alias{FactorEdgeProto}
\title{The factor edge object}
\description{Returns an object of the class \code{FactorEdgeProto}
  for an edge from a vertex to a factor node in the bipartite graph.}
\usage{
newFactorEdge(node.indices, nodes = NULL, width = 2,
              color = "DarkOliveGreen", type = NULL,
              label = ifelse(is.null(nodes), "",
                             paste(Labels(nodes), collapse = "~")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node.indices}{ Vector with \code{abs(node.indices)}
                       the indices of the nodes of the factor edge.
                       If the index is positiv then the node is a vertex,
                       else it is the factor vertex. }
  \item{nodes}{ List with the \code{nodes} of the factor edge. Used for
      setting the default label. }
  \item{width}{ Numeric for the \code{width} of the factor edge. }
  \item{color}{ Text string with the \code{color} of the factor edge. }
  \item{type}{ Text string with the \code{type} of the factor edge. }
  \item{label}{ Text string with the \code{label} of the factor edge. }
}
% \details{}
\value{A object of class \code{FactorEdgeProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
   The methods (except \code{\link{oriented}})
   of \code{\link{EdgeProto}}
   also applies for \code{FactorEdgeProto}.
 }
\seealso{\code{\link{newVertexEdge}}}
\examples{
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
factor <- newFactor(1:3, vertices[1:3], type = "Generator")
factoredge <- newFactorEdge(c(1, -1), list(vertices[[1]], factor))
str(factoredge)

color(factoredge)
labelOfEdge(factoredge)
label(factoredge)
labelPosition(factoredge)
widthOfEdge(factoredge)
width(factoredge)
nodeIndicesOfEdge(factoredge)
nodeTypesOfEdge(factoredge)

color(factoredge) <- "Black"
labelOfEdge(factoredge) <- "V1~V123"
label(factoredge) <- "V1~V1:2:3"
labelPosition(factoredge) <- c(0, 1, 2)
widthOfEdge(factoredge) <- 0
width(factoredge) <- 1
nodeIndicesOfEdge(factoredge) <- c(1, -1)
str(factoredge)
}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
