\name{dg.FactorEdge-class}

\docType{class}

\alias{dg.FactorEdge-class}
\alias{nodeTypesOfEdge,dg.FactorEdge-method}

\alias{newFactorEdge}

\alias{newFactorEdge}
\alias{dg.FactorEdge}

\title{Class "dg.FactorEdge" }

\description{  The class for the bipartite graph edges between vertices and factors. }

% \section{Objects from the Class}{
% Objects can be created by calls of the form \code{new("dg.FactorEdge", ...)}.
% }

\usage{
newFactorEdge(node.indices, nodes = NULL, width = 2,
              color = "DarkOliveGreen", type = NULL,
              label = ifelse(is.null(nodes), "",
                             paste(Labels(nodes), collapse = "~")),
              dash = "")
}

\arguments{
  \item{node.indices}{ Vector with \code{abs(node.indices)}
                       the indices of the nodes of the factor edge.
                       If the index is positiv then the node is a vertex,
                       else it is the factor vertex. }
  \item{nodes}{ List with the \code{nodes} of the factor edge. Used for
      setting the default label. }
  \item{width}{ Numeric for the \code{width} of the factor edge. }
  \item{color}{ Text string with the \code{color} of the factor edge. }
  \item{type}{ Text string with the \code{type} of the factor edge. }
  \item{label}{ Text string with the \code{label} of the factor edge. }
  \item{dash}{ Text string for the \code{dash} pattern of the edge. }
}

\section{Slots}{
  \describe{
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"},
          see \code{"dg.Edge"}. }
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
Class \code{"dg.Edge"}, directly.
Class \code{"dg.Node"}, by class \code{"dg.Edge"}.
}

\section{Methods}{
  \describe{
    \item{nodeTypesOfEdge}{\code{signature(object = "dg.FactorEdge")}:
      Extract the types ("super classes": \code{"Vertex"} or \code{"Factor"})
      of the vertices (nodes) of the edge. }
    % \item{propertyDialog}{\code{signature(object = "dg.FactorEdge")}: ... }
    % \item{addToPopups}{\code{signature(object = "dg.FactorEdge")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newFactorEdge}},
\code{\link{returnFactorEdgeList}},
\code{\link{dg.Edge-class}}, and
\code{\link{dg.FactorVertex-class}}.
}

% \seealso{\code{\link{newVertexEdge}}}

\note{ 
   The methods (except \code{\link{oriented}})
   of \code{\link{dg.Edge}}
   also applies for \code{dg.FactorEdge}.
 }

\examples{
vertices <- returnVertexList(paste("V", 1:4, sep = ""))
factor <- newFactor(1:3, new("dg.VertexList", vertices[1:3]),
                    type = "Generator")
factoredge <- newFactorEdge(c(1, -1), 
                            new("dg.VertexList", list(vertices[[1]], factor)))
str(factoredge)

color(factoredge)
label(factoredge)
labelPosition(factoredge)
width(factoredge)
nodeIndicesOfEdge(factoredge)
nodeTypesOfEdge(factoredge)

color(factoredge) <- "Black"
label(factoredge) <- "V1~V1:2:3"
labelPosition(factoredge) <- c(0, 1, 2)
width(factoredge) <- 1
nodeIndicesOfEdge(factoredge) <- c(1, -1)
str(factoredge)
}

\keyword{classes}
