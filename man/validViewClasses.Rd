\name{validViewClasses}

\alias{viewClasses}
\alias{validViewClasses}

\title{Valid view classes}

\description{Return matrix with labels of valid view classes
and the valid view classes}

\usage{validViewClasses()}

\details{The argument \code{viewClasses} to \code{\link{dynamicGraphMain}} and
    \code{\link{DynamicGraph}} is by default the returned value of this
    function. If new view classes are created then \code{viewClasses}
    should be set to a value with this returned value extended appropriate. }

\value{Matrix of text strings with labels (used in dialog windows) of valid
       view classes and the valid view classes (used to create the views).}

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{DynamicGraphView-class}}. }

\examples{validViewClasses()}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
