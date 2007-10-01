\name{dg.Vertex-class}

\docType{class}

\alias{dg.Vertex-class}
\alias{ancestors<-,dg.Vertex-method}
\alias{ancestors,dg.Vertex-method}
\alias{blockindex<-,dg.Vertex-method}
\alias{blockindex,dg.Vertex-method}
\alias{descendants<-,dg.Vertex-method}
\alias{descendants,dg.Vertex-method}
\alias{index<-,dg.Vertex-method}
\alias{index,dg.Vertex-method}
\alias{initialize,dg.Vertex-method}
\alias{name<-,dg.Vertex-method}
\alias{name,dg.Vertex-method}
\alias{position<-,dg.Vertex-method}
\alias{position,dg.Vertex-method}
\alias{stratum<-,dg.Vertex-method}
\alias{stratum,dg.Vertex-method}
% \alias{vertexIndices<-,dg.Vertex-method}
% \alias{vertexIndices,dg.Vertex-method}
\alias{visible<-,dg.Vertex-method}
\alias{visible,dg.Vertex-method}
\alias{constrained<-,dg.Vertex-method}
\alias{constrained,dg.Vertex-method}

\alias{newVertex}

\alias{dg.Node}
\alias{dg.Vertex}
\alias{newVertex}
\alias{draw}
\alias{color}
\alias{color<-}
% \alias{label}
% \alias{label<-}
\alias{labelPosition}
\alias{labelPosition<-}
\alias{name}
\alias{name<-}
\alias{index}
\alias{index<-}  
\alias{position}
\alias{position<-}
\alias{stratum}
\alias{stratum<-}
\alias{blockindex}
\alias{blockindex<-}
\alias{visible}
\alias{visible<-}
\alias{constrained}
\alias{constrained<-}
\alias{addToPopups}
\alias{propertyDialog}

\title{Class "dg.Vertex" }

\description{  A skeleton class for the classes of vertices. }

\section{Objects from the Class}{
Objects has the methods for extracting and setting the slots for vertices.

% Objects can be created by calls of the form \code{new("dg.Vertex", ...)}.
}

% \usage{
% newVertex(name, label = name, type = "dg.Vertex", index = 1,
%           position = c(0, 0, 0), blockindex = 0, stratum = 0,
%           color = ifelse(type == "TextVertex", "GhostWhite", "SaddleBrown"),
%           vertexClasses = validVertexClasses())
% }

% \arguments{
%   \item{name}{ Text string with the \code{name} of the vertex. 
%      Should be a valid name on variables for your model object. }
%   \item{label}{ Text string with the \code{label} of the vertex. }
%   \item{type}{ Text string with the \code{type} of the vertex. 
%      This \code{type} is the label of class of the vertex,
%      the labels of \code{\link{vertexClasses}}. }
%   \item{index}{ Integer with the \code{index} of the vertex,
%      the position in a vertex list. }
%   \item{position}{ Numeric vector with the \code{position} of the vertex.
%      Vertices in the same dynamic graph should
%      have the same number of coordinates. }
%   \item{blockindex}{ Integer with the \code{blockindex} of the vertex. }
%   \item{stratum}{ Integer with the \code{stratum} of the vertex. }
%   \item{color}{ Text string with the \code{color} of the vertex. }
%   \item{vertexClasses}{ The valid \code{vertexClasses}. }
% }

\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} 
       with the \code{name} of the vertex. 
       Should be a valid name on variables for your model object.}
    \item{\code{index}:}{Object of class \code{"numeric"} 
       with the \code{index} of the vertex,
       the position of the vertex in a vertex list. }
    \item{\code{position}:}{Object of class \code{"numeric"} 
       with the \code{position} of the vertex.
       Vertices in the same dynamic graph should
       have the same number of coordinates. }
    \item{\code{blockindex}:}{Object of class \code{"numeric"}
       with the \code{blockindex} of the vertex. }
    \item{\code{stratum}:}{Object of class \code{"numeric"} 
       with the \code{stratum} of the vertex. }
    \item{\code{constrained}:}{Object of class \code{"logical"}:
       If TRUE, then the vertex can not be dragged
       out of the block of the vertex. }
    \item{\code{color}:}{Object of class \code{"character"}
       with the \code{color} of the vertex. }
    \item{\code{label}:}{Object of class \code{"character"}
       with the \code{label} of the vertex. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}
       with the \code{label.position} of the vertex.
       Labels of vertices in the same dynamic graph
       should have the same number of coordinates. }
  }
}

\section{Extends}{
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{blockindex<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{blockindex}{\code{signature(object = "dg.Vertex")}: ... }
    \item{index<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{index}{\code{signature(object = "dg.Vertex")}: ... }
    \item{initialize}{\code{signature(.Object = "dg.Vertex")}: ... }
    \item{name<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{name}{\code{signature(object = "dg.Vertex")}: ... }
    \item{position<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{position}{\code{signature(object = "dg.Vertex")}: ... }
    \item{stratum<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{stratum}{\code{signature(object = "dg.Vertex")}: ... }
%     \item{vertexIndices<-}{\code{signature(x = "dg.Vertex")}: ... }
%     \item{vertexIndices}{\code{signature(object = "dg.Vertex")}: ... }
    \item{visible<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{visible}{\code{signature(object = "dg.Vertex")}: ... }
    \item{constrained<-}{\code{signature(x = "dg.Vertex")}: ... }
    \item{constrained}{\code{signature(object = "dg.Vertex")}: ... }
    \item{propertyDialog}{\code{signature(object = "dg.Node")}: ... }

    \item{ancestors<-}{\code{signature(x = "dg.Vertex")}: Not implemented. }
    \item{ancestors}{\code{signature(object = "dg.Vertex")}: Not implemented. }
    \item{descendants<-}{\code{signature(x = "dg.Vertex")}: Not implemented. }
    \item{descendants}{\code{signature(object = "dg.Vertex")}: Not implemented. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newVertex}},
\code{\link{returnVertexList}},
\code{\link{dg.Node-class}}.
}

\note{ 
    The \code{dg.Vertex} class has the methods
    \code{\link{name}}, \code{\link{label}}, \code{\link{labelPosition}},
    \code{\link{position}}, \code{\link{stratum}},
    \code{\link{blockindex}},
    \code{\link{constrained}},
    % \code{\link{visible}},
    \code{\link{color}}, and \code{\link{index}}
    for extracting values of the object and the replacement methods
    \code{\link{name<-}}, \code{\link{label<-}}, \code{\link{labelPosition<-}},
    \code{\link{position<-}}, \code{\link{stratum<-}}, 
    \code{\link{blockindex<-}},
    \code{\link{constrained<-}},
    % \code{\link{visible<-}},
    \code{\link{color<-}}, and \code{\link{index<-}}.
    The method \code{\link{draw}} is used to draw a vertex,
    and items can be added to the pop up menu of the vertex
    by the method \code{\link{addToPopups}}.
    
    Some of these methods also applies for 
    edges (\code{\link{dg.Edge}}),
    blocks (\code{\link{dg.Block}}), 
%    edges between block and between blocks and vertices
    block edges
    (\code{\link{dg.BlockEdge}}),
    factor vertices (\code{\link{dg.FactorVertex}})
    and edges from vertices to factors (\code{\link{dg.FactorEdge}}).
}

% # "newVertex" <-
% # function (name, label = name, type = "dg.Vertex", index = 1, 
% #     position = c(0, 0, 0), blockindex = 0, stratum = 0, 
% #     color = ifelse(type == "TextVertex", "GhostWhite", "SaddleBrown"), 
% #     vertexClasses = validVertexClasses()) 
% # {
% #     if (!is.na(type) && type == "TextVertex") 
% #         prototype <- "dg.TextVertex"
% #     else {
% #         prototype <- "dg.Vertex"
% #         x <- match(type, vertexClasses[, 1])
% #         if (!is.null(x) && !all(is.na(x))) 
% #             prototype <- paste(vertexClasses[, 2][x])
% #     }
% #     if (prototype == "dg.TextVertex") 
% #         index <- -index
% #     result <- new(prototype, name = name, label = label, index = index, 
% #         position = position, label.position = rep(0, length(position)), 
% #         color = color, blockindex = blockindex, stratum = stratum)
% #     return(result)
% # }

% # a <- newVertex("a", "A", "Discrete", 1, c(0, 0, 0))

\examples{
a <- new("dg.DiscreteVertex", name = "a", label = "A", 
         index = 1, position = c(0, 0, 0))

str(a)

color(a)
label(a)
labelPosition(a)
name(a)
index(a)
position(a)
stratum(a)

color(a) <- "red"
label(a) <- "A vertex"
labelPosition(a) <- c(1, 2, 3)
name(a) <- "Capital.A"
index(a) <- -1
position(a) <- c(10, 20, 30)
stratum(a) <- 1

str(a)
}

\keyword{classes}
