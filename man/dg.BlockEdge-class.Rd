\name{dg.BlockEdge-class}

\docType{class}

\alias{dg.BlockEdge-class}
\alias{nodeTypesOfEdge,dg.BlockEdge-method}
\alias{oriented<-,dg.BlockEdge-method}
\alias{oriented,dg.BlockEdge-method}

\alias{newBlockEdge}

\alias{newBlockEdge}
\alias{dg.BlockEdge}.


\title{Class "dg.BlockEdge" }

\description{  The class for edges between blocks and for
               edges between vertices and blocks. }

% \section{Objects from the Class}{
% Objects can be created by calls of the form \code{new("dg.BlockEdge", ...)}.
% }

% \usage{
% newBlockEdge(node.indices, nodes = NULL, width = 2,
%              color = "DarkOliveGreen", oriented = NA, type = NULL,
%              label = ifelse(is.null(nodes), "",
%                             paste(Labels(nodes), collapse = "~")),
%              dash = "")
% }

% \arguments{
%   \item{node.indices}{ Vector with \code{abs(node.indices)}
%                        the indices of the nodes of the block edge.
%                        If the index is positiv then the node is a vertex,
%                        else it is a block. }
%   \item{nodes}{ List with the \code{nodes} of the block edge. Used for
%       setting the default label. }
%   \item{width}{ Numeric for the \code{width} of the block edge. }
%   \item{color}{ Text string with the \code{color} of the block edge. }
%   \item{oriented}{ Logical. % If TRUE then the block edge is \code{oriented}. 
%                    }
%   \item{type}{ Text string with the \code{type} of the block edge. }
%   \item{label}{ Text string with the \code{label} of the block edge. }
%   \item{dash}{ Text string for the \code{color} pattern of the edge. }
% }

\details{
The function is used in \code{\link{returnBlockEdgeList}}.
\code{\link{dynamicGraphMain}} will automatic update block edges
when vertices are moved between blocks.
}

\section{Slots}{
  \describe{
    \item{\code{oriented}:}{Object of class \code{"logical"},
       see \code{"dg.VertexEdge"}. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"},
       see also \code{"dg.Edge"}.
       Vector with \code{abs(vertex.indices)}
       the indices of the nodes of the block edge.
       If the index is positiv then the node is a vertex,
       else it is a block.  }
    \item{\code{width}:}{Object of class \code{"numeric"},
       see \code{"dg.Edge"}. }
    \item{\code{dash}:}{Object of class \code{"character"},
       see \code{"dg.Edge"}. }
    \item{\code{color}:}{Object of class \code{"character"},
       see \code{"dg.Edge"}. }
    \item{\code{label}:}{Object of class \code{"character"},
       see \code{"dg.Edge"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"},
       see \code{"dg.Edge"}. }
  }
}

\section{Extends}{
% Class \code{"dg.Edge"}, directly.
% Class \code{"dg.Node"}, by class \code{"dg.Edge"}.
Class \code{"dg.Edge"}, directly.
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{nodeTypesOfEdge}{\code{signature(object = "dg.BlockEdge")}:
       Extract the types ("super classes": \code{"Vertex"} or \code{"Block"})
       of the vertices (nodes) of the edge. }
    \item{oriented<-}{\code{signature(x = "dg.BlockEdge")}: ... }
    \item{oriented}{\code{signature(object = "dg.BlockEdge")}: ... }
    % \item{propertyDialog}{\code{signature(object = "dg.BlockEdge")}: ... }
    % \item{propertyDialog}{\code{signature(object = "dg.BlockEdge")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newBlockEdge}},
\code{\link{returnBlockEdgeList}},
\code{\link{dg.Edge-class}}.
}

\note{ 
   The methods of \code{\link{dg.Edge}} also applies for \code{dg.BlockEdge}.

   The method \code{new} also accepts the argument \code{vertices} or
   \code{vertexList}. The \code{label} is then extracted from these vertices.
   The length of \code{vertices} should match \code{vertex.indices}, where 
   \code{vertex.indices} is used to select vertices form \code{vertexList}.
}

% \seealso{\code{\link{dg.VertexEdge-class}}}

% # "newBlock" <-
% # function (stratum = 0, index = 0, position = matrix(rep(0, 6), 
% #     ncol = 3), closed = FALSE, visible = TRUE, color = "Grey", 
% #     label = "Label", label.position = c(0, 0, 0), ancestors = NULL, 
% #     parent = NULL) 
% # {
% #     result <- new("dg.Block", stratum = stratum, index = index, 
% #         position = position, closed = closed, visible = visible, 
% #         color = color, label = label, label.position = label.position, 
% #         ancestors = if (is.null(ancestors)) 
% #             0
% #         else ancestors, parent = if (is.null(parent)) 
% #             if (is.null(ancestors)) 
% #                 0
% #             else ancestors[length(ancestors)]
% #         else parent)
% #     return(result)
% # }

% # "newBlockEdge" <-
% # function (node.indices, nodes = NULL, width = 2, color = "DarkOliveGreen", 
% #     oriented = NA, type = NULL, label = ifelse(is.null(nodes), 
% #         "", paste(Labels(nodes), collapse = "~")), dash = "") 
% # {
% #     result <- new("dg.BlockEdge", vertex.indices = node.indices, 
% #         width = width, color = color, oriented = oriented, label = label, 
% #         dash = dash)
% #     return(result)
% # }

% vertices <- returnVertexList(paste("V", 1:4, sep = ""))

% # block <- newBlock(stratum = 1)

% block <- new("dg.Block", stratum = 1)

% # Blockedge <- newBlockEdge(c(4, -1), 
% #                           new("dg.VertexList", list(vertices[[1]], block)))

\examples{ 
vertices  <- returnVertexList(paste("V", 1:4, sep = ""))
block     <- new("dg.Block", stratum = 1)
blockedge <- new("dg.BlockEdge", vertex.indices = c(4, -1), 
                 vertices = new("dg.VertexList", list(vertices[[1]], block)))

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

\keyword{classes}
