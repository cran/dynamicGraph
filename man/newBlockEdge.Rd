\name{newBlockEdge}
\alias{newBlockEdge}
\alias{BlockEdgeProto}.
\title{The block edge object}
\description{Returns an object of the class \code{BlockEdgeProto}
  for an edge between blocks or between a block and a vertex.}
\usage{
newBlockEdge(node.indices, nodes = NULL, width = 2,
             color = "DarkOliveGreen", oriented = TRUE, type = NULL,
             label = ifelse(is.null(nodes), "",
                            paste(Labels(nodes), collapse = "~")))
}
\arguments{
  \item{node.indices}{ Vector with \code{abs(node.indices)}
                       the indices of the nodes of the block edge.
                       If the index is positiv then the node is a vertex,
                       else it is a block. }
  \item{nodes}{ List with the \code{nodes} of the block edge. Used for
      setting the default label. }
  \item{width}{ Numeric for the \code{width} of the block edge. }
  \item{color}{ Text string with the \code{color} of the block edge. }
  \item{oriented}{ Logical. If TRUE then the block edge is \code{oriented}. }
  \item{type}{ Text string with the \code{type} of the block edge. }
  \item{label}{ Text string with the \code{label} of the block edge. }
}
\details{
The function is used in \code{\link{returnBlockEdgeList}}.
\code{\link{dynamicGraphMain}} will automatic update block edges
when vertices are moved between blocks.
}
\value{A object of class \code{BlockEdgeProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
   The methods of \code{\link{EdgeProto}}
   also applies for \code{BlockEdgeProto}.
}
\seealso{\code{\link{newVertexEdge}}}
\examples{ 
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
block <- newBlock(stratum = 1)
blockedge <- newBlockEdge(c(4, -1), list(vertices[[1]], block))

str(blockedge)

color(blockedge)
label(blockedge)
labelPosition(blockedge)
width(blockedge)
nodeIndicesOfEdge(blockedge)
nodeTypesOfEdge(blockedge)

color(blockedge) <- "Black"
label(blockedge) <- "V1~1"
labelPosition(blockedge) <- c(0, 1, 2)
width(blockedge) <- 1
nodeIndicesOfEdge(blockedge) <- c(1, -1)

str(blockedge)
}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
