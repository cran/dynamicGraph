\name{dg.ExtraEdge-class}

\docType{class}

\alias{dg.ExtraEdge-class}
\alias{nodeTypesOfEdge,dg.ExtraEdge-method}

\alias{newExtraEdge}

\alias{dg.ExtraEdge}

\title{Class dg.ExtraEdge}

\description{  The class for the edges between vertices and extra vertices. }

% \section{Objects from the Class}{
% Objects can be created by calls of the form \code{new("dg.ExtraEdge", ...)}.
% }

% \usage{
% newExtraEdge(node.indices, nodes = NULL, width = 2,
%              color = "DarkOliveGreen", type = NULL,
%              label = ifelse(is.null(nodes), "",
%                             paste(Labels(nodes), collapse = "~")),
%              dash = "")
% }

% \arguments{
%   \item{node.indices}{ Vector with \code{abs(node.indices)}
%                        the indices of the nodes of the extra edge.
%                        If the index is positiv then the node is a vertex,
%                        else it is the extra vertex. }
%   \item{nodes}{ List with the \code{nodes} of the extra edge. 
%       Used for setting the default label. }
%   \item{width}{ Numeric for the \code{width} of the extra edge. }
%   \item{color}{ Text string with the \code{color} of the extra edge. }
%   \item{type}{ Text string with the \code{type} of the extra edge. }
%   \item{label}{ Text string with the \code{label} of the extra edge. }
%   \item{dash}{ Text string for the \code{dash} pattern of the edge. }
% }

\section{Slots}{
  \describe{
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"},
          see \code{"dg.Edge"}.
          If the index is positiv then the node is a vertex,
          else it is the extra vertex. }
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
    \item{nodeTypesOfEdge}{\code{signature(object = "dg.ExtraEdge")}:
      Extract the types ("super classes": \code{"Vertex"} or \code{"Extra"})
      of the vertices (nodes) of the edge. }
    % \item{propertyDialog}{\code{signature(object = "dg.ExtraEdge")}: ... }
    % \item{addToPopups}{\code{signature(object = "dg.ExtraEdge")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newExtraEdge}},
\code{\link{returnExtraEdgeList}},
\code{\link{dg.Edge-class}}, and
\code{\link{dg.TextVertex-class}}.
}

% \seealso{\code{\link{dg.VertexEdge-class}}}

\note{ 
   The methods (except \code{\link{oriented}}) of \code{\link{dg.Edge}}
   also applies for \code{dg.ExtraEdge}.

   The method \code{new} also accepts the argument \code{vertices} or
   \code{vertexList}. The \code{label} is then extracted from these vertices.
   The length of \code{vertices} should match \code{vertex.indices}, where 
   \code{vertex.indices} is used to select vertices form \code{vertexList}.

   Extra vertices and nodes are used in \code{demo(dg.USArrests)}
   to display the loadings in a biplot.
 }

% # "newExtraEdge" <-
% # function (node.indices, nodes = NULL, width = 2, color = "DarkOliveGreen", 
% #     type = NULL, label = ifelse(is.null(nodes), "", paste(Labels(nodes), 
% #         collapse = "~")), dash = "") 
% # {
% #     result <- new("dg.ExtraEdge", vertex.indices = node.indices, 
% #         width = width, color = color, label = label, dash = dash)
% #     return(result)
% # }

% vertices  <- returnVertexList(paste("V", 1:4, sep = ""))
% extra     <- returnVertexList(paste("E", 1:4, sep = ""))

% # extraedge <- newExtraEdge(c(3, -2), 
% #                           new("dg.VertexList", c(vertices[3], extra[2])))

\examples{
vertices  <- returnVertexList(paste("V", 1:4, sep = ""))
extra     <- returnVertexList(paste("E", 1:4, sep = ""))
extraedge <- new("dg.ExtraEdge", vertex.indices = c(3, -2), 
                 vertices = new("dg.VertexList", 
                                c(vertices[3], extra[2])))

str(extraedge)

color(extraedge)
label(extraedge)
labelPosition(extraedge)
width(extraedge)
nodeIndicesOfEdge(extraedge)
nodeTypesOfEdge(extraedge)

color(extraedge) <- "Black"
label(extraedge) <- "Gryf"
labelPosition(extraedge) <- c(0, 1, 2)
width(extraedge) <- 1
nodeIndicesOfEdge(extraedge) <- c(1, -1)
str(extraedge)
}

\keyword{classes}
