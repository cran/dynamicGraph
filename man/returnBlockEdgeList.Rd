\name{returnBlockEdgeList}
\alias{blockEdgeList}
\alias{returnBlockEdgeList}
\title{ The block edge list }
\description{
  Return a list of block edges, each of class \code{BlockEdgeProto}.
}
\usage{
returnBlockEdgeList(edge.list, vertices, blocks, width = 2, 
                    color = "default", oriented = FALSE, type = NULL)
}
\arguments{
  \item{edge.list}{ A list of vectors identifying the edges (between vertices).
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or
                    a vector of text strings with the names of the vertices. }
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{VertexProto}. }
  \item{blocks}{ The list of \code{blocks}, 
      each of a class \code{BlockProto}. }
  \item{width}{ A numeric with the initial \code{width} of block edges. }
  \item{color}{
      "default", or list with one or two text strings for colors 
      giving the initial \code{color} of block edges.
      The two colors are used for respectively edges
      between blocks and for edges between blocks 
      and vertices.
    }
  \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{type}{ A text string giving the \code{type} of block edges. }
}
% \details{ }
\value{
    A list of block edges, each of class \code{BlockEdgeProto}.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
    The methods of the edge list, \code{\link{returnEdgeList}},
    also applies for block edge lists.
 }
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{
Block.tree <- list(label = "W", Vertices = c("country"),
                   X = list(Vertices = c("sex", "race"),
                            A = list(Vertices = c("hair", "eye"),
                                     horizontal = FALSE),
                            B = list(Vertices = c("age"),
                                     C = list(Vertices = c("education")))))
Names <- unlist(Block.tree)
Names <- Names[grep("Vertices", names(Names))]
Types <- rep("Discrete", length(Names))
vertices <- returnVertexList(Names, types = Types)
blocktree <- setTreeBlocks(Block.tree, vertices)
blocks <- blockTreeToList(blocktree$BlockTree)
from <- c("country", "country", "race", "race",       "sex", "sex")
to   <- c(    "sex",    "race", "hair",  "eye", "education", "age")
from <- match(from, Names)
to <- match(to, Names)
edge.list <- vector("list", length(to))
for (j in seq(along = to)) edge.list[[j]] <- c(from[j], to[j])
edges <- returnEdgeList(edge.list, vertices, color = "red", oriented = TRUE)
vertices <- blocktree$Vertices
blockedges <- returnBlockEdgeList(edge.list, vertices, blocks,
                                  color = "red", oriented = TRUE)

Names(blockedges)
Colors(blockedges)
Labels(blockedges)
LabelPositions(blockedges)
# Positions(blockedges)
# Strata(blockedges)
# Indices(blockedges)
str(NodeTypes(blockedges))
str(NodeIndices(blockedges))
Widths(blockedges)
Oriented(blockedges)
Widths(blockedges) <- rep(1,  7)
Widths(blockedges) <- rep(1, 14)
Widths(blockedges)

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
