\name{nameToVertexIndex}
\alias{nameToVertexIndex}
\title{ The indices of vertices }
\description{
  For each name, find in a list of vertices the index of the vertex with that name.
}
\usage{
nameToVertexIndex(vertexnames, vertices)
}
\arguments{
  \item{vertexnames}{ Vector of text strings of the \code{vertexnames}
      of the vertices for which the indices should be found.  }
  \item{vertices}{ A list of \code{vertices},
                   of each of class containing the class \code{NodeProto}. }
}
% \details{ }
\value{
  Integer vector with the indices of the vertices.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{
Names <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
Types <- rep("Discrete", 6)
vertices <- returnVertexList(Names, types = Types)
nameToVertexIndex(c("Sex", "Eye"), vertices)
}
\keyword{ dynamic }
\keyword{ graphs }
