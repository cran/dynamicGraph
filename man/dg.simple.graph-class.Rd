\name{dg.simple.graph-class}

\docType{class}

\alias{dg.simple.graph-class}

\alias{dg,dg.simple.graph-method}

\alias{coerce,dg.simple.graph,dg.graph-method}

\alias{addModel,dg.simple.graph-method}
\alias{addView,dg.simple.graph-method}
\alias{replaceModel,dg.simple.graph-method}
\alias{replaceView,dg.simple.graph-method}

\title{Class "dg.simple.graph"}

\description{  A simple representation of a graph for dynamicGraph. 
               Vertices are represented by text-strings for names or numbers
               for indices, edges by indices or names of vertices, etc. }

\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("dg.simple.graph", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{viewType}:}{Object of class \code{"character"}:
          A text string with the type of view. }

    \item{\code{vertex.names}:}{Object of class \code{"vector"}:
        A vector with text strings for the \code{names} of the vertices.}
    \item{\code{types}:}{Object of class \code{"character"}:
        A vector with text strings for the \code{types},
        labels of \code{\link{dg.Vertex}}, of the vertices. }
    \item{\code{labels}:}{Object of class \code{"vector"}:
        A vector with text strings for the \code{labels} of the vertices. }
    \item{\code{from}:}{Object of class \code{"vector"}:
        If not \code{edge.list} is given:
        The indices of the first endpoints of the edges. }
    \item{\code{to}:}{Object of class \code{"vector"}:
        If not \code{edge.list} is given:
        The indices of the second endpoints of the edges. }
    \item{\code{edge.list}:}{Object of class \code{"list"}:
        If not \code{from} and \code{to} are given:
        A list where each item specifies an edge by a vector
        of the indices or names of the vertices. }
    \item{\code{edge.types}:}{Object of class \code{"character"}:
        A vector of text strings giving the \code{types} of the
        edges, identify which classes the edges should be of,
        containing the \code{dg.VertexEdge}. }


    \item{\code{blocks}:}{Object of class \code{"list"}:
        A list defining the \code{blocks}:
        Each item is the vector of the indices of the vertices of the
	block, or the vector with text strings for the names of the vertices
	of the block. 
        The arguments \code{right.to.left}, \code{nested.blocks} 
        and \code{blockColors} are here used in \code{\link{setBlocks}}
        to control the layout of the blocks. }
    \item{\code{block.tree}:}{Object of class \code{"list"}:
        If not the argument \code{blocks} is used:
	A structure with the blocks in a \code{block.tree}.
        The arguments \code{overlaying} and \code{blockColors} are here used
        in \code{\link{setTreeBlocks}} to control the layout of the blocks. }
    \item{\code{oriented}:}{Object of class \code{"logical"}:
        Logical. If \code{oriented} is set to TRUE
        then the edges are oriented, also when no block structure is given. }

    \item{\code{factors}:}{Object of class \code{"list"}: 
        A list defining the \code{factor vertices}:
        Each item is the vector of the indices of the vertices of a factor. }
    \item{\code{texts}:}{Object of class \code{"character"}:
                A vector of text strings, for additional labels. 
		These labels will be set by \code{'ExtraVertices'}
                of class \code{\link{dg.TextVertex-class}}. }
    \item{\code{extra.from}:}{Object of class \code{"vector"}:
        If not \code{extra.edge.list} is given:
        The indices of the first endpoints of the extra edges,
        negative for extra vertices. }
    \item{\code{extra.to}:}{Object of class \code{"vector"}:
        If not \code{extra.edge.list} is given:
        The indices of the second endpoints of the extra edges,
        negative for extra vertices. }
    \item{\code{extra.edge.list}:}{Object of class \code{"list"}:
        If not \code{extra.from} and \code{extra.to}
        are given: A list where each item specifies an extra edge by a vector
        of the indices or names of the vertices or extra vertices,
        negative indices for extra vertices. }
  }
}

\section{Methods}{
  \describe{
    \item{addModel}{\code{signature(object = "dg.simple.graph")}: ... }
    \item{addView}{\code{signature(object = "dg.simple.graph")}: ... }
    \item{coerce}{\code{signature(from = "dg.simple.graph", to = "dg.graph")}: ... }
    \item{dg}{\code{signature(object = "dg.simple.graph")}: ... }
    \item{replaceModel}{\code{signature(object = "dg.simple.graph")}: ... }
    \item{replaceView}{\code{signature(object = "dg.simple.graph")}: ... }
  }
}

% \usage{
% dg(object, 
%    oriented = NA, modelObject = NULL, objectName = NULL,
%    viewType = "Simple", control = dg.control(...), 
%    frameModels = NULL, frameViews = NULL, graphWindow = NULL,
%    addModel = FALSE, addView = FALSE, 
%    overwrite = FALSE, returnNewMaster = FALSE, redraw = FALSE, ...)
% }

% \note{The method \code{dg} for \code{signature(object = "dg.simple.graph")}
%       also have the arguments 
%       \code{frameModels},
%       \code{frameViews},
%       \code{graphWindow},
%       \code{addModel},
%       \code{addView}, 
%       \code{overwrite},
%       \code{returnNewMaster}, and
%       \code{redraw} of
%       \code{\link{DynamicGraph}}
%       }

\author{Jens Henrik Badsberg}

\seealso{
  \code{\link{dg.graph-class}}, and \code{\link{DynamicGraph}}.
}

\examples{
x <- new("dg.simple.graph", vertex.names = c("a", "b"), 
         from = 1, to = 2) ; str(x)
dg(new("dg.simple.graph", vertex.names = c("a", "b"), 
       edge.list = list(c(1, 2))))
}

\keyword{classes}
