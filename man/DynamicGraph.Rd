\name{DynamicGraph}
\alias{DynamicGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple interface to dynamicGraph}
\description{
    A simple interface to \code{dynamicGraph} in the
    sense that the graph should not be given as list of objects
    as to \code{\link{dynamicGraphMain}}. Here vertices can be 
    specified by a vector of text strings with names,
    and/or edges by pairs of the indices of the vertices.
}
\usage{
DynamicGraph(names = NULL, types = NULL, 
             from = NULL, to = NULL, edge.list = NULL,
             blocks = NULL, block.tree = NULL, oriented = FALSE,
             factors = NULL, texts = NULL, object = NULL,
             vertexClasses = validVertexClasses(),
             factorClasses = validFactorClasses(), N = 3,
             drawblocks = TRUE, right.to.left = FALSE,
             nested.blocks = FALSE, overlaying = TRUE,
             vertexColor = "red", extraVertexColor = "white",
             edgeColor = "black", factorVertexColor = "default",
             factorEdgeColor = "brown", blockEdgeColor = "default",
             blockColors = NULL, ...)
}
\arguments{
  \item{names}{ A vector with text strings for the \code{names} 
        of the vertices. }
  \item{types}{ A vector with text strings for the \code{types},
        labels of \code{\link{VertexProto}}, of the vertices. }
  \item{from}{ If not \code{edge.list} is given:
        The indices of the first endpoints of the edges. }
  \item{to}{ If not \code{edge.list} is given:
        The indices of the second endpoints of the edges. }
  \item{edge.list}{ If not \code{from} and \code{to} are given:
        A list where each item specifies an edge by a vector
        of the indices or names of the vertices. }
  \item{blocks}{ 
        A list defining the \code{blocks}:
        Each item is the vector of the indices of the vertices of the
	block, or the vector with text strings for the names of the vertices
	of the block. 
        The arguments \code{right.to.left}, \code{nested.blocks} 
        and \code{blockColors} are here used in \code{\link{setBlocks}}
        to control the layout of the blocks.
    }
  \item{block.tree}{ If not the argument \code{blocks} is used:
	A structure with the blocks in a \code{block.tree}.
        The arguments \code{overlaying} and \code{blockColors} are here used
        in \code{\link{setTreeBlocks}} to control the layout of the blocks.
        }
  \item{oriented}{ Logical. If \code{oriented} is set to TRUE
        then the edges are oriented, also when no block structure is given. }
  \item{factors}{A list defining the \code{factor vertices}:
        Each item is the vector of the indices of the vertices of a factor.
        }
  \item{texts}{ A vector of text strings, for additional labels. }
  \item{object}{ The model \code{object}, or NULL,
        see \code{\link{newDefaultModelObject}}. }
  \item{vertexClasses}{ The valid \code{vertexClasses}. }
  \item{factorClasses}{ The valid \code{factorClasses}. }
  \item{N}{ The number, \code{N} > 1, of coordinates for the positions of the
        vertices and block corners. }
  \item{drawblocks}{ Logical. If \code{drawblocks} is set to FALSE,
        then the blocks are not drawn. The strata of the vertices are
        then not updated when the vertices are moved. }
  \item{right.to.left}{ Logical. If \code{right.to.left} is set to TRUE
        then the explanatory blocks are drawn to the right.
        See \code{\link{setBlocks}}. }
  \item{nested.blocks}{ Logical. If \code{nested.blocks} is set to TRUE
        then the blocks are drawn nested. See \code{\link{setBlocks}}. }
  \item{overlaying}{ Logical. If \code{overlaying} is set to FALSE
        then childern of a block are not drawn inside the block.
        See \code{\link{setTreeBlocks}}. }
  \item{vertexColor}{ Single text string with the \code{vertexColor}
        of the vertices. }
  \item{extraVertexColor}{ Single text string with the \code{extraVertexColor}
        of the nodes for text labels. }
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
  \item{blockColors}{ Vector of text string with the \code{blockColors}
        of the blocks.
        See \code{\link{setBlocks}} and \code{\link{setTreeBlocks}}.  }
  \item{\dots}{ Additional arguments to \code{\link{dynamicGraphMain}}. }
}
\details{
    After converting the first arguments to lists of objects
    the \code{\link{dynamicGraphMain}} does all the work.
    You can look into the source code of \code{\link{DynamicGraph}} about
    how to create these lists of objects for building your applications.
    The list of objects can be exported from
    \code{\link{dynamicGraphMain}}, also after modifying the graph.
}
\value{
  The returned value from \code{\link{dynamicGraphMain}}.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{

require(tcltk)
require(dynamicGraph)

# Example 1:

Z <- DynamicGraph(paste("", 1:5), title = "Very simple")


# Example 2:

Z <- DynamicGraph(from = 1:4, to = c(2:4, 1), title = "Simply edges")


# Example 3:

V.Types <- c("Discrete", "Ordinal", "Discrete",
             "Continuous", "Discrete", "Continuous")

V.Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Names <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

Z <- DynamicGraph(V.Names, V.Types, From, To, texts = c("Gryf", "Gaf"),
                  title = "With labels")


# Example 4: Oriented (cyclic) edges, without causal structure:

Z <- DynamicGraph(V.Names, V.Types, From, To, oriented = TRUE,
                  title = "Oriented edges")


# Example 5: A factor graph:

Factors <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))

Z <- DynamicGraph(V.Names, V.Types, from = NULL, to = NULL, factors = Factors,
                  title = "Factorgraph", namesOnEdges = FALSE)


# Example 6: Edges with more than two vertices:

EdgeList <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))

Z <- DynamicGraph(V.Names, V.Types, edge.list = EdgeList, 
                  title = "Multiple edges", namesOnEdges = FALSE)

}
\keyword{ dplot }
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graphs }
\keyword{ methods }
\keyword{ multivariate }
