\name{newFactor}
\alias{FactorVertexProto}
\alias{newFactor}
\alias{nodeIndices}
\alias{nodeIndices<-}  
\title{The factor vertex object}
\description{Returns an object of the class
    containing the class \code{FactorVertexProto}.}
\usage{
newFactor(vertex.indices, vertices = NULL, 
          name = ifelse(is.null(vertices), "", 
                        paste(Labels(vertices), collapse = ":")),
          label = name, type = "Generator",
          index = 0, width = 2, color = "default",
          factorClasses = validFactorClasses())
}
\arguments{
  \item{vertex.indices}{ Integer vector of the \code{vertex.indices} 
                         of the vertices of the factor. }
  \item{vertices}{ The \code{vertices} of the factor. Used for setting
      the default label. }
  \item{name}{ Text string for the \code{name} of the factor vertex. }
  \item{label}{ Text string for the \code{label} of the factor vertex. }
  \item{type}{ Text string for the \code{type}, the class, of the factor
      vertex. Should be in \code{factorClasses}. }
  \item{index}{ Integer for the \code{index} of the factor vertex. }
  \item{width}{ Numeric, for the \code{width} of the factor vertex. }
  \item{color}{ Text string for the \code{color} of the factor vertex. }
  \item{factorClasses}{ The valid \code{factorClasses}. }
}
% \details{ }
\value{A object of class containing \code{FactorVertexProto}. }
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 
   The methods (except \code{\link{stratum}})
   of \code{\link{VertexProto}}
   also applies for \code{FactorVertexProto}.
 }
\seealso{\code{\link{newVertex}}, \code{\link{validVertexClasses}}}
\examples{
vertices <- returnVertexList(paste("V", 1:4, sep = ""), 
                             types = rep("Discrete", 4))
factor <- newFactor(c(1, 2, 3), vertices[1:3], type = "Generator")

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
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
