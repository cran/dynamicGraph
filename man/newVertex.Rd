\name{newVertex}
\alias{NodeProto}
\alias{VertexProto}
\alias{newVertex}
\alias{draw}
\alias{color}
\alias{color<-}
\alias{label}
\alias{label<-}
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
\alias{visible}
\alias{visible<-}
\alias{addToPopups}
\title{The vertex object}
\description{Returns an object of a class containing \code{VertexProto}.}
\usage{
newVertex(name, label = name, type = "VertexProto", index = 1,
          position = c(0, 0, 0), stratum = 0, 
          color = ifelse(type == "TextVertex", "GhostWhite", "SaddleBrown"),
          vertexClasses = validVertexClasses())
}
\arguments{
  \item{name}{ Text string with the \code{name} of the vertex. 
     Should be a valid name on variables for your model object. }
  \item{label}{ Text string with the \code{label} of the vertex. }
  \item{type}{ Text string with the \code{type} of the vertex. 
     This \code{type} is the label of class of the vertex,
     the labels of \code{\link{vertexClasses}}. }
  \item{index}{ Integer with the \code{index} of the vertex,
     the position in a vertex list. }
  \item{position}{ Numeric vector with the \code{position} of the vertex.
     Vertices in the same dynamic graph should
     have the same number of coordinates. }
  \item{stratum}{ Integer with the \code{stratum} of the vertex. }
  \item{color}{ Text string with the \code{color} of the vertex. }
  \item{vertexClasses}{ The valid \code{vertexClasses}. }
}
% \details{ }
\value{A object of a class containing the class \code{VertexProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
    The \code{VertexProto} class has the methods
    \code{\link{name}}, \code{\link{label}}, \code{\link{labelPosition}},
    \code{\link{position}}, \code{\link{stratum}},
    % \code{\link{visible}}
    \code{\link{color}} and \code{\link{index}}
    for extracting values of the object and the replacement methods
    \code{\link{name<-}}, \code{\link{label<-}}, \code{\link{labelPosition<-}},
    \code{\link{position<-}}, \code{\link{stratum<-}}, 
    % \code{\link{visible<-}}.
    \code{\link{color<-}} and \code{\link{index<-}}.
    The method \code{\link{draw}} is used to draw a vertex,
    and items can be added to the pop up menu of the vertex
    by the method \code{\link{addToPopups}}.
    
    Some of these methods also applies for 
    edges (\code{\link{EdgeProto}}),
    blocks (\code{\link{BlockProto}}), 
%    edges between block and between blocks and vertices
    block edges
    (\code{\link{BlockEdgeProto}}),
    factor vertices (\code{\link{FactorVertexProto}})
    and edges from vertices to factors (\code{\link{FactorEdgeProto}}).
}
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{

a <- newVertex("a", "A", "Discrete", 1, c(0, 0, 0))

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
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
