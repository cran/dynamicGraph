\name{dg.graph-class}

\docType{class}

% \alias{coerce,dg.simple.graph,dg.graph-method}

\alias{dg.graph-class}

\alias{dg,dg.graph-method}

\alias{addModel,dg.graph-method}
\alias{addView,dg.graph-method}
\alias{replaceModel,dg.graph-method}
\alias{replaceView,dg.graph-method}

% \alias{dg,DynamicGraph-method}

\alias{dg}

\alias{addModel}
\alias{addView}
\alias{replaceModel}
\alias{replaceView}

\alias{viewType}
\alias{visibleBlocks}
\alias{visibleVertices}
% \alias{oriented}
\alias{edgeList}
% \alias{blockEdgeList}
% \alias{factorVertexList}
% \alias{factorEdgeList}
\alias{extraList}
% \alias{extraEdgeList}

\title{Class "dg.graph"}

\description{ The representation of a graph for dynamicGraph. 
              Vertices, blocks, viewType, edges, etc. 
              are here the dynamicGraph objects. }

\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("dg.graph", ...)}.
}

\section{Slots}{
  \describe{

    \item{\code{vertexList}:}{Object of class \code{"dg.VertexList"}:
        List of vertices
        (each of class containing the class \code{dg.Vertex})
        created by \code{\link{returnVertexList}}
        or exported from \code{\link{dynamicGraphMain}}. }

    \item{\code{blockList}:}{Object of class \code{"dg.BlockList"}:
        List of blocks (each of class \code{dg.Block})
        created by \code{\link{setBlocks}}
        or exported from \code{\link{dynamicGraphMain}}.
        % If the \code{blockTree} argument is given, then
        % the argument \code{blockList} should not also be given.
        }

    % \item{\code{blockTree}:}{Object of class \code{"list"}:
    %       Structure with blocks (of class \code{dg.Block})
    %     created by \code{\link{setTreeBlocks}}
    %     or exported from \code{\link{dynamicGraphMain}}. }

    \item{\code{viewType}:}{Object of class \code{"character"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{visibleVertices}:}{Object of class \code{"numeric"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{visibleBlocks}:}{Object of class \code{"numeric"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{oriented}:}{Object of class \code{"logical"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{edgeList}:}{Object of class \code{"dg.VertexEdgeList"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{blockEdgeList}:}{Object of class \code{"dg.BlockEdgeList"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{factorVertexList}:}{Object of class \code{"dg.FactorVertexList"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{factorEdgeList}:}{Object of class \code{"dg.FactorEdgeList"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{extraList}:}{Object of class \code{"dg.VertexList"},
                            See \code{\link{dg.graphedges-class}}. }
    \item{\code{extraEdgeList}:}{Object of class \code{"dg.ExtraEdgeList"},
                            See \code{\link{dg.graphedges-class}}. }


    % \item{\code{viewType}:}{Object of class \code{"character"}
    %                         with the type of view. }

    % \item{\code{visibleVertices}:}{Object of class \code{"numeric"}:
    %     Numeric vector of the indices of the vertices of
    %     \code{vertexList} to plot. }

    % \item{\code{visibleBlocks}:}{Object of class \code{"numeric"}:
    %     Numeric vector of the indices of the blocks of
    %     \code{blockList} to plot. }

    % \item{\code{oriented}:}{Object of class \code{"logical"}:
    %     If TRUE (or FALSE) then edges are oriented (or not), 
    %     also when blocks are missing. 
    %     If NA then the edges are directed according to the blocks
    %     of the edge. }

    % \item{\code{edgeList}:}{Object of class \code{"dg.VertexEdgeList"}:
    %     List of edges (of class containing \code{dg.Edge})
    %     created by \code{\link{returnEdgeList}}
    %     or exported from \code{\link{dynamicGraphMain}}.  }

    % \item{\code{blockEdgeList}:}{Object of class \code{"dg.BlockEdgeList"}:
    %     List of blockedges
    %     (of class containing the class \code{dg.BlockEdge})
    %     created by \code{\link{returnBlockEdgeList}}
    %     or exported from \code{\link{dynamicGraphMain}}. }

    % \item{\code{factorVertexList}:}{Object of class
    %                                 \code{"dg.FactorVertexList"}:
    %     List of secondary vertices,
    %     called factor vertices, for, e.g., the generators of the model,
    %     (each of class containing \code{dg.FactorVertex})
    %     created by \code{\link{returnFactorVerticesAndEdges}}
    %     or exported from \code{\link{dynamicGraphMain}}. }

    % \item{\code{factorEdgeList}:}{Object of
    %     class \code{"dg.FactorEdgeList"}:
    %     List of bipartite graph edges, called factor edges,
    %     (each of class containing \code{dg.FactorEdge})
    %     created by the function \code{\link{returnFactorEdgeList}}
    %     or exported from \code{\link{dynamicGraphMain}}.
    %     Factor edges are edges between vertices and factor vertices. }

    % \item{\code{extraList}:}{Object of class \code{"dg.VertexList"}:
    %     List of vertices (of class containing the class	\code{dg.Vertex}) 
    %     created by the function \code{\link{returnVertexList}}
    %     or exported from the function \code{\link{dynamicGraphMain}},
    %     for, e.g., additional titles in the plot. }

    % \item{\code{extraEdgeList}:}{Object of class \code{"dg.ExtraEdgeList"}:
    %       List of edges between extra vertices and vertices. }

  }
}

\section{Extends}{
Class \code{"dg.graphedges"}, directly.
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "dg.simple.graph", to = "dg.graph")}: ... }
    \item{dg}{\code{signature(object = "dg.graph")}: ... }

    \item{addModel}{\code{signature(object = "dg.graph")}: ... }
    \item{addView}{\code{signature(object = "dg.graph")}: ... }
    \item{replaceModel}{\code{signature(object = "dg.graph")}: ... }
    \item{replaceView}{\code{signature(object = "dg.graph")}: ... }

  }
}

\usage{
dg(object, 
%  modelObject = NULL, modelObjectName = NULL, control = dg.control(...),
   ...)
}

\arguments{
    \item{object}{ The graph. }
%   \item{modelObject}{ NULL,
%       or object with the methods \code{\link{modifyModel}}
%       and \code{\link{testEdge}} - for respectively updating the
%       object when the graph is updated, and for computing the
%       test statistic when an edge is labeled. The returned object from
%       \code{\link{testEdge}} should have the methods 
%       \code{label} and \code{width}
%       % (was \code{labelOfTest} and \code{widthOfTest})
%       for extracting the label of a test for putting the label on the edge
%       and for extracting the width for the edge.
%       See \code{\link{dg.Model-class}}. }
%   \item{modelObjectName}{ If set to a text string then the object is
%         assigned with this name in .Global.Env when the object is updated. }
%   \item{control}{ Options for \code{\link{DynamicGraph}}
%                   and \code{\link{dynamicGraphMain}},
%                   see \code{\link{dg.control}}. }
    \item{\dots}{ Additional arguments. }
}

\author{Jens Henrik Badsberg}

\seealso{
  \code{\link{dg.simple.graph-class}}, and \code{\link{dynamicGraphMain}}.
}

\examples{
from <- c("contry", "contry", "race", "race",       "sex",  "sex")
to   <- c(   "sex",   "race", "hair",  "eye", "education",  "age")
vertexnames <- unique(sort(c(from, to)))
vertices <- returnVertexList(vertexnames)
edge.list <- vector("list", length(to))
for (j in seq(along = to)) edge.list[[j]] <- c(from[j], to[j])
edges <- returnEdgeList(edge.list, vertices, color = "red", oriented = TRUE)

graph <- new("dg.graph", vertexList = vertices, edgeList = edges); str(graph)
dg(graph)
}

\keyword{classes}
