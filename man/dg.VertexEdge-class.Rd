\name{dg.VertexEdge-class}


\docType{class}

\alias{dg.VertexEdge-class}
\alias{nodeTypesOfEdge,dg.VertexEdge-method}
\alias{oriented<-,dg.VertexEdge-method}
\alias{oriented,dg.VertexEdge-method}

\alias{dg.VertexEdge-class}
\alias{dg.DashedEdge-class}
\alias{dg.DottedEdge-class}
\alias{dg.DoubleArrowEdge-class}
\alias{dg.DoubleConnectedEdge-class}
\alias{dg.TripleConnectedEdge-class}

\alias{newVertexEdge}

\alias{dg.Edge}
\alias{dg.VertexEdge}
\alias{dg.DashedEdge}
\alias{dg.DottedEdge}
\alias{dg.DoubleArrowEdge}
\alias{dg.DoubleConnectedEdge}
\alias{dg.TripleConnectedEdge}

\alias{nodeIndicesOfEdge}
\alias{nodeIndicesOfEdge<-}
\alias{nodeTypesOfEdge}
\alias{width}
\alias{width<-}
\alias{dash}
\alias{dash<-}
\alias{oriented}
\alias{oriented<-}

\title{Class "dg.VertexEdge" }

\description{  The class for edges between vertices. }

\section{Objects from the Class}{
Objects has the methods for extracting and setting the slots for edges,
and the method for drawing the edge.

% Objects can be created by calls of the form \code{new("dg.VertexEdge", ...)}.
}

% \usage{
% newVertexEdge(vertex.indices, vertices = NULL, width = 2,
%               color = "DarkOliveGreen", oriented = NA, type = "VertexEdge",
%               label = ifelse(is.null(vertices), "",
%                              paste(Labels(vertices), collapse = "~")),
%               dash = "", N = 3, edgeClasses = validEdgeClasses())
% }

% \arguments{
%   \item{vertex.indices}{ Integer vector of the \code{vertex.indices} 
%       of the vertices of the edge. These are the indices of the
%       vertices in the list of vertices. }
%   \item{vertices}{ All the \code{vertices} of the edge, and only the
%      vertices of the edge. 
%      Currently, only used for setting the default label. }
%   \item{width}{ Numeric, for the \code{width} of the edge. }
%   \item{color}{ Text string for the \code{color} of the edge. }
%   \item{oriented}{ Logical. If \code{oriented} is \code{NA} then
%       the edge is drawn as an arrow if the vertices of the edge
%       are in different blocks, oriented according to the strata
%       of the blocks. 
%       If \code{oriented} is \code{NA} and the vertices of the
%       edge are in the same block, then an undirected edge is drawn.
%       If \code{oriented} is TRUE then an arrow is drawn from
%       the first vertex of the edge to the second.
%       If \code{oriented} is FALSE then an undirected edge is drawn,
%       also between blocks.
%       }
%   \item{type}{ Text string for the \code{type} of the edge. 
%       Currently not used. 
%       Intended to set which class the edge should be class of
%       containing the  class \code{dg.Edge}.}
%   \item{label}{ Text string for the \code{label} of the edge.
%                 By default set by concatenating the labels of the vertices. }
%   \item{dash}{ Text string for the \code{color} pattern of the edge. }
%   \item{N}{ Integer, \code{N} is the number of coordinates of the vertices. }
%   \item{edgeClasses}{ Returned value from \code{\link{validEdgeClasses}},
%                       or extension of this matrix. }
% }

\details{
    Edges are not constrained to have two vertices.
}

\section{Slots}{
  \describe{
    \item{\code{oriented}:}{Object of class \code{"logical"}.
       If \code{oriented} is \code{NA} then
       the edge is drawn as an arrow if the vertices of the edge
       are in different blocks, oriented according to the strata
       of the blocks. 
       If \code{oriented} is \code{NA} and the vertices of the
       edge are in the same block, then an undirected edge is drawn.
       If \code{oriented} is TRUE then an arrow is drawn from
       the first vertex of the edge to the second.
       If \code{oriented} is FALSE then an undirected edge is drawn,
       also between blocks.
       }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"},
                                  see \code{"dg.Edge"}. 
        These are the indices of the vertices in the list of vertices.}
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
    \item{nodeTypesOfEdge}{\code{signature(object = "dg.VertexEdge")}: ... }
    \item{oriented<-}{\code{signature(x = "dg.VertexEdge")}: ... }
    \item{oriented}{\code{signature(object = "dg.VertexEdge")}: ... }
    \item{propertyDialog}{\code{signature(object = "dg.VertexEdge")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{newVertexEdge}},
\code{\link{returnEdgeList}},
\code{\link{dg.Edge-class}}.
}

\note{
    The \code{dg.Edge} class has
    beside the methods of \code{\link{dg.Vertex}}
    the methods \code{\link{oriented}} and \code{\link{oriented<-}}.

    The method \code{\link{nodeIndicesOfEdge}} will extract the indices of the
    vertices of the edge, and the method \code{\link{nodeTypesOfEdge}} will
    extract the types
    ("super classes": \code{vertex}, \code{factor} or \code{block})
    of the vertices (nodes) of an edge.
    The method \code{\link{draw}} is used to draw the edge,
    and items are added to the pop up menu of an edge
    by the method \code{\link{addToPopups}}.

    Some of these methods also applies for
    block edges (\code{dg.BlockEdge})
    and factor edges (\code{dg.FactorEdge}).
}

% # "newVertexEdge" <-
% # function (vertex.indices, vertices = NULL, width = 2, 
% #     color = "DarkOliveGreen", oriented = NA, type = "VertexEdge", 
% #     label = ifelse(is.null(vertices), 
% #                    "", paste(Labels(vertices), collapse = "~")), 
% #                              dash = "", 
% #     N = 3, edgeClasses = validEdgeClasses()) 
% # {
% #     if (!is.na(type) && type == "VertexEdge") 
% #         prototype <- "dg.VertexEdge"
% #     else {
% #         prototype <- "dg.VertexEdge"
% #         x <- match(type, edgeClasses[, 1])
% #         if (is.null(x) || all(is.na(x))) 
% #             x <- match(type, edgeClasses[, 2])
% #         if (!is.null(x) && !all(is.na(x))) 
% #             prototype <- paste(edgeClasses[, 2][x])
% #     }
% #     result <- new(prototype, vertex.indices = vertex.indices, 
% #         width = width, color = color, oriented = oriented, label = label, 
% #         label.position = rep(0, N), dash = dash)
% #     return(result)
% # }

% vertices <- returnVertexList(paste("V", 1:4, sep = ""))

% # f <- newVertexEdge(c(1, 2, 3), vertices = new("dg.VertexList", 
% #                                               vertices[1:3]))

\examples{
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
e <- new("dg.VertexEdge", vertex.indices = c(1, 2, 3), 
         vertices = new("dg.VertexList", vertices[1:3]))

str(e)

color(e)
label(e)
labelPosition(e)
width(e)
oriented(e)
nodeIndicesOfEdge(e)
nodeTypesOfEdge(e)

color(e) <- "Black"
label(e) <- "1-2"
labelPosition(e) <- c(10, 20, 30)
width(e) <- 1
oriented(e) <- TRUE
nodeIndicesOfEdge(e) <- c(1, 2)

str(e)
}

\keyword{classes}
