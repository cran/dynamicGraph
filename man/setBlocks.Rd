\name{setBlocks}

\title{Class dg.BlockList: The block list}

\alias{setBlocks}
\alias{blockList}

\alias{dg.BlockList-class}

\alias{ancestorsBlockList}
\alias{checkBlockList}
\alias{descendantsBlockList}

\alias{ancestorsBlockList,dg.BlockList-method}
\alias{checkBlockList,dg.BlockList-method}
\alias{descendantsBlockList,dg.BlockList-method}

\description{
 Create a block list with positioning the vertices in the blocks.    

 Objects can be created by calls of the form \code{new("dg.BlockList", ...)}.
}

\usage{
setBlocks(block.list, vertices, labels = NULL, 
          right.to.left = FALSE, nested.blocks = FALSE,
          % draw.blocks = TRUE,
          blockColors = NULL, color = "Grey", N = 3)
}

\arguments{
  \item{block.list}{ The list of vectors identifying the \code{blocks}.
          Each item in the list is a vector with of the indices (or text
	  strings for names) of vertices of a block. 
      }
  \item{vertices}{ The list of \code{vertices}, each
      containing the class \code{dg.Vertex}. Returned with positions
      set in the interval of the blocks. }
  \item{labels}{ List of text strings with the \code{labels} of the
      blocks. If \code{labels} is set to \code{NULL} then labels are
      found as \code{names(block.list)}.
  }
  \item{right.to.left}{ Logical. If \code{right.to.left} is set to TRUE
        then the explanatory blocks are drawn to the right.
        }
  \item{nested.blocks}{ Logical. If \code{nested.blocks} then the
        blocks are drawn nested. }
  % \item{draw.blocks}{ ~~Describe \code{draw.blocks} here~~ }
  \item{blockColors}{ Vector of text string with the \code{blockColors}
        of the blocks. }
  \item{color}{ Single text string with \code{color} of the blocks.
    Only used when \code{blockColors} is not given. }
  \item{N}{ Integer, \code{N} is the number of coordinates of the
      vertices and block corners. }
}

\value{
  A list with components
  \item{Blocks}{The list of blocks, each of class 
        \code{\link{dg.Block}}.}
  \item{Vertices}{The list of vertices,
        with the positions of the vertices updated such the vertices
        has positions within the blocks. }
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. }
  }
}

\section{Extends}{
% Class \code{"dg.NodeList"}, directly.
% Class \code{"dg.list"}, by class \code{"dg.NodeList"}.
% Class \code{"list"}, by class \code{"dg.NodeList"}.

% Class \code{"list"}, from data part.
% Class \code{"dg.list"}, directly.
% Class \code{"dg.NodeList"}, directly.

Class \code{"dg.NodeList"}, directly.
Class \code{"dg.list"}, directly.
Class \code{"list"}, from data part.
% Class \code{"vector"}, by class \code{"dg.NodeList"}.
Class \code{"vector"}, by class \code{"dg.NodeList"}.
Class \code{"vector"}, by class \code{"dg.list"}.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{checkBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
    \item{ancestorsBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
    \item{descendantsBlockList}{\code{signature(blockList = "dg.BlockList")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\examples{
require(tcltk)

require(dynamicGraph)

V.Types <- c("Discrete", "Ordinal", "Discrete",
             "Continuous", "Discrete", "Continuous")

V.Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Names <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

# A block recursive model:

Blocks <- list(Basic = c(2, 1), Intermediate = c(5, 4, 3),  Now = c(6))

V.Names <- paste(V.Names, c(1, 1, 2, 2, 2, 3), sep =":")

graph <- new("dg.simple.graph", vertex.names = V.Names, types = V.Types,
             from = From, to = To, blocks = Blocks)

W <- dg(graph,
        control = dg.control(width = 600, height = 600, drawblocks = TRUE,
                             drawBlockBackground = FALSE, title = "DrawBlocks", 
                             namesOnEdges = FALSE))

# A block recursiv model, without drawing blocks:

W <- dg(simpleGraphToGraph(graph, control = dg.control(drawblocks = FALSE)),
        control = dg.control(width = 600, height = 600, title = "No blocks drawn"))

# A block recursive model with nested blocks:

W <- dg(simpleGraphToGraph(graph, 
          control = dg.control(nested.blocks = TRUE,
                               blockColors = 
                               paste("Grey", 100 - 2 * (1:10), sep = ""))),
        control = dg.control(width = 600, height = 600, title = "Nested blocks"))

# The block list of the last example:

vertices <- returnVertexList(V.Names, types = V.Types)
blockList <- setBlocks(Blocks, vertices = vertices,  nested.blocks = TRUE,
                       blockColors = paste("Grey", 100 - 2 * (1:10), sep = ""))

names(blockList)
str(blockList$Blocks[[1]])

names(blockList$Blocks)
Names(blockList$Blocks)
Labels(blockList$Blocks)
LabelPositions(blockList$Blocks)
Positions(blockList$Blocks)
Strata(blockList$Blocks)
Colors(blockList$Blocks)
NodeAncestors(blockList$Blocks)
NodeDescendants(blockList$Blocks)
Visible(blockList$Blocks)
Indices(blockList$Blocks)

names(blockList$Vertices)
Names(blockList$Vertices)
Labels(blockList$Vertices)
LabelPositions(blockList$Vertices)
Positions(blockList$Vertices)
Strata(blockList$Vertices)
Colors(blockList$Vertices)
Indices(blockList$Vertices)

asDataFrame(blockList$Vertices)
asDataFrame(blockList$Blocks)

}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
