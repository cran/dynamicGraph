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

\usage{
newBlockEdge(node.indices, nodes = NULL, width = 2,
             color = "DarkOliveGreen", oriented = TRUE, type = NULL,
             label = ifelse(is.null(nodes), "",
                            paste(Labels(nodes), collapse = "~")),
             dash = "")
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
  \item{dash}{ Text string for the \code{color} pattern of the edge. }
}

\details{
The function is used in \code{\link{returnBlockEdgeList}}.
\code{\link{dynamicGraphMain}} will automatic update block edges
when vertices are moved between blocks.
}

\section{Slots}{
  \describe{
    \item{\code{oriented}:}{Object of class \code{"logical"}, see \code{"dg.VertexEdge"}. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}, see \code{"dg.Edge"}. }
    \item{\code{width}:}{Object of class \code{"numeric"}, see \code{"dg.Edge"}. }
    \item{\code{dash}:}{Object of class \code{"character"}, see \code{"dg.Edge"}. }
    \item{\code{color}:}{Object of class \code{"character"}, see \code{"dg.Edge"}. }
    \item{\code{label}:}{Object of class \code{"character"}, see \code{"dg.Edge"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, see \code{"dg.Edge"}. }
  }
}

\section{Extends}{
Class \code{"dg.Edge"}, directly.
Class \code{"dg.Node"}, by class \code{"dg.Edge"}.
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
   The methods of \code{\link{dg.Edge}}
   also applies for \code{dg.BlockEdge}.
}

% \seealso{\code{\link{newVertexEdge}}}

\examples{ 
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
block <- newBlock(stratum = 1)
blockedge <- newBlockEdge(c(4, -1), 
                          new("dg.VertexList", list(vertices[[1]], block)))

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
