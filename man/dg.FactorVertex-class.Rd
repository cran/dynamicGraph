\name{dg.FactorVertex-class}

\docType{class}

\alias{dg.FactorVertex-class}
\alias{fixed.positions<-,dg.FactorVertex-method}
\alias{fixed.positions,dg.FactorVertex-method}
\alias{nodeIndices<-,dg.FactorVertex-method}
\alias{nodeIndices,dg.FactorVertex-method}
\alias{initialize,dg.FactorVertex-method}
\alias{index<-,dg.FactorVertex-method}
\alias{index,dg.FactorVertex-method}

\alias{newFactor}

\alias{dg.FactorVertex}

\alias{nodeIndices}
\alias{nodeIndices<-}  

\title{Class dg.FactorVertex}

\description{ A skeleton class for the classes of factor vertices. }

% \section{Objects from the Class}{
% Objects can be created by calls of 
% the form \code{new("dg.FactorVertex", ...)}.
% }

% \usage{
% newFactor(vertex.indices, vertices = NULL, 
%           name = ifelse(is.null(vertices), "", 
%                         paste(Labels(vertices), collapse = ":")),
%           label = name, type = "Generator",
%           index = 0, width = 2, color = "default",
%           factorClasses = validFactorClasses())
% }

% \arguments{
%   \item{vertex.indices}{ Integer vector of the \code{vertex.indices} 
%                          of the vertices of the factor. }
%   \item{vertices}{ The \code{vertices} of the factor. Used for setting
%       the default label. }
%   \item{name}{ Text string for the \code{name} of the factor vertex. }
%   \item{label}{ Text string for the \code{label} of the factor vertex. }
%   \item{type}{ Text string for the \code{type}, e.i. the class,
%       of the factor vertex. Should be in \code{factorClasses}. }
%   \item{index}{ Integer for the \code{index} of the factor vertex. }
%   \item{width}{ Numeric, for the \code{width} of the factor vertex. }
%   \item{color}{ Text string for the \code{color} of the factor vertex. }
%   \item{factorClasses}{ The valid \code{factorClasses}. }
% }

\section{Slots}{
  \describe{
%    \item{\code{test.position}:}{Object of class \code{"numeric"}:
%       The \code{position} of a test statistic for the factor. }
    \item{\code{fixed.positions}:}{Object of class \code{"logical"}:
       If FALSE, then the factor will follow the vertices when
       the positions of the vertices are changed. }
    \item{\code{vertex.indices}:}{Object of class \code{"numeric"}:
       The \code{vertex.indices} of the vertices of the factor. }
    \item{\code{name}:}{Object of class \code{"character"},
       see \code{"dg.Vertex"}. }
    \item{\code{index}:}{Object of class \code{"numeric"},
       see \code{"dg.Vertex"}. }
    \item{\code{position}:}{Object of class \code{"numeric"},
       see \code{"dg.Vertex"}. }
    \item{\code{blockindex}:}{Object of class \code{"numeric"},
       see \code{"dg.Vertex"}. }
    \item{\code{stratum}:}{Object of class \code{"numeric"},
       see \code{"dg.Vertex"}. }
    \item{\code{constrained}:}{Object of class \code{"logical"}, 
       see \code{"dg.Vertex"}. }
    \item{\code{color}:}{Object of class \code{"character"},
       see \code{"dg.Vertex"}. }
    \item{\code{label}:}{Object of class \code{"character"},
       see \code{"dg.Vertex"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"},
       see \code{"dg.Vertex"}. }
  }
}

\section{Extends}{
% Class \code{"dg.Vertex"}, directly.
% Class \code{"dg.Node"}, by class \code{"dg.Vertex"}.
Class \code{"dg.Vertex"}, directly.
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{fixed.positions<-}{\code{signature(x = "dg.FactorVertex")}: ... }
    \item{fixed.positions}{\code{signature(object = "dg.FactorVertex")}: ... }
    \item{index<-}{\code{signature(x = "dg.FactorVertex")}: ... }
    \item{index}{\code{signature(object = "dg.FactorVertex")}: ... }
    \item{initialize}{\code{signature(.Object = "dg.FactorVertex")}:
      The method \code{new} also accepts the argument
      \code{vertices} or \code{vertexList}.
      The \code{name}, \code{label}, and \code{position} is
      then extracted from these vertices.
      The length of \code{vertices} should match \code{vertex.indices},
      where \code{vertex.indices} is used to select vertices form
      \code{vertexList}.
    }
    % \item{propertyDialog}{\code{signature(object = "dg.FactorVertex")}: ... }
    % \item{addToPopups}{\code{signature(object = "dg.FactorVertex")}: ... }
    \item{nodeIndices<-}{\code{signature(x = "dg.FactorVertex")}: ... }
    \item{nodeIndices}{\code{signature(object = "dg.FactorVertex")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newFactor}},
\code{\link{returnFactorVerticesAndEdges}},
\code{\link{dg.FactorEdge-class}},
\code{\link{dg.Vertex-class}},
\code{\link{dg.Node-class}}, and
\code{\link{validFactorClasses}}.
}

\note{ 
   The methods (except \code{\link{stratum}}) of \code{\link{dg.Vertex}}
   also applies for \code{dg.FactorVertex}.
 }

% \seealso{\code{\link{dg.Vertex-class}}, \code{\link{validVertexClasses}}}

% # "newFactor" <-
% # function (vertex.indices, vertices = NULL,
% #     name = ifelse(is.null(vertices), 
% #     "", paste(Labels(vertices), collapse = ":")), label = name, 
% #     type = "Generator", index = 0, width = 2, color = "default", 
% #     factorClasses = validFactorClasses()) 
% # {
% #     prototype <- "dg.Generator"
% #     x <- match(type, factorClasses[, 1])
% #     if (!is.null(x)) 
% #         prototype <- paste(factorClasses[, 2][x])
% #     positions <- Positions(vertices)
% #     if (color == "default") 
% #         color <- c("yellow", "cyan", "magenta", "blue")[x]
% #     result <- new(prototype, vertex.indices = vertex.indices, 
% #         position = apply(positions, 2, mean), index = index, 
% #         color = color, name = name, label = label)
% #     return(result)
% # }

% vertices <- returnVertexList(paste("V", 1:4, sep = ""), 
%                              types = rep("Discrete", 4))

% # factor <- newFactor(c(1, 2, 3), new("dg.VertexList", vertices[1:3]),
% #                     type = "Generator")

\examples{
vertices       <- returnVertexList(paste("V", 1:4, sep = ""), 
                                   types = rep("Discrete", 4))
vertex.indices <- c(1, 2, 3)
vertices       <- new("dg.VertexList", vertices[c(1, 2, 3)])
name           <- paste(Labels(vertices), collapse = ":")

factor <- new("dg.Generator", vertex.indices = vertex.indices,
              position = apply(Positions(vertices), 2, mean),
              index = 0, color = "yellow", name = name, label = name)

factor <- new("dg.FactorVertex", vertex.indices = 1:3, vertices = vertices)

factor <- new("dg.FactorVertex", vertex.indices = 1:3, vertexList = vertices)

str(factor)

color(factor)
label(factor)
labelPosition(factor)
name(factor)
index(factor)
position(factor)
nodeIndices(factor)

color(factor) <- "green"
label(factor) <- "v-1-2-3"
labelPosition(factor) <- c(1, 2, 3)
name(factor) <- "V-123" 
index(factor) <- 3
position(factor) <- c( 10,  20,  30,  40)

str(factor)
}

\keyword{classes}
