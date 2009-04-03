\name{returnBlockEdgeList}

\alias{blockEdgeList}
\alias{returnBlockEdgeList}

\alias{dg.BlockEdgeList-class}
\alias{initialize,dg.BlockEdgeList-method}

\title{Class dg.BlockEdgeList: The block edge list }

\description{
 Return a list of block edges, each of class \code{dg.BlockEdge}.

 Objects can be created by calls of the form \code{new("dg.BlockEdgeList", ...)}.
}

\usage{
returnBlockEdgeList(edge.list, vertices, blocks, 
                    visibleBlocks = 1:length(blocks), width = 2, 
                    color = "default", N = 3, oriented = NA, type = NULL) 
}

\arguments{
  \item{edge.list}{ A list of vectors identifying the edges (between vertices).
                    Each vector of \code{edge.list} should be a vector of 
                    integers giving the indices of the vertices of an edge, or
                    a vector of text strings with the names of the vertices. }
  \item{vertices}{ The list of \code{vertices}, 
      each of a class containing \code{dg.Vertex}. }
  \item{blocks}{ The list of \code{blocks}, 
      each of a class \code{dg.Block}. }
  \item{visibleBlocks}{ A numeric vector with the indices of the
			\code{visibleBlocks}. The argument is for
			view where some blocks are not visible in the view,
			but the vertices of the blocks are drawn. }
  \item{width}{ A numeric with the initial \code{width} of block edges. }
  \item{color}{
      "default", or list with one or two text strings for colors 
      giving the initial \code{color} of block edges.
      The two colors are used for respectively edges
      between blocks and for edges between blocks 
      and vertices.
    }
  \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
  \item{oriented}{ Logical, if TRUE then the edges are \code{oriented}. }
  \item{type}{ A text string giving the \code{type} of block edges. }
}

\value{
    A list of block edges, each of class \code{dg.BlockEdge}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
% Class \code{"dg.EdgeList"}, directly.
% Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.
% Class \code{"dg.list"}, by class \code{"dg.EdgeList"}.
% Class \code{"list"}, by class \code{"dg.EdgeList"}.

% Class \code{"list"}, from data part.
% Class \code{"dg.list"}, directly.
% Class \code{"dg.EdgeList"}, directly.
% Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.

Class \code{"dg.EdgeList"}, directly.
Class \code{"dg.list"}, directly.
Class \code{"list"}, from data part.
Class \code{"dg.NodeList"}, by class \code{"dg.EdgeList"}.
% Class \code{"vector"}, by class \code{"dg.EdgeList"}.
% Class \code{"vector"}, by class \code{"dg.EdgeList"}.
Class \code{"vector"}, by class \code{"dg.EdgeList"}.
Class \code{"vector"}, by class \code{"dg.list"}.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "dg.BlockEdgeList")}: ... }
    \item{ancestorsBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
    \item{descendantsBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
    \item{checkBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\note{ 
    The methods of the edge list, \code{\link{returnEdgeList}},
    also applies for block edge lists.
 }

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
blockedges <- new("dg.BlockEdgeList", edge.list = edge.list, 
                                      vertices = vertices, blocks = blocks,
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
asDataFrame(blockedges)
}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
