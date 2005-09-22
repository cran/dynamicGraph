\name{validFactorClasses}

\alias{factorClasses}
\alias{validFactorClasses}

\title{Valid factor vertex classes}

\description{Return matrix with labels of valid factor vertex classes
and the valid factor vertex classes. }

\usage{validFactorClasses()}

\details{The argument \code{factorClasses} to \code{\link{DynamicGraph}},
 and to \code{\link{returnFactorVerticesAndEdges}} and \code{\link{newFactor}}
 is by default the returned value of this function.
 If new factor vertex classes are created then \code{factorClasses}
 should be set to a value with this returned value extended appropriate. }

\value{Matrix of text strings with labels (used in dialog windows)
       of valid factor vertex classes and the valid factor 
       vertex classes (used to create the factor vertices).}

\author{Jens Henrik Badsberg}

\seealso{\code{\link{validVertexClasses}}}

\examples{validFactorClasses()}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
