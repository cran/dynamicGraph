\name{replaceControls}
\alias{replaceControls}

\title{ Replace the controls of model frame }

\description{
  Replace the controls of model frame.
}

\usage{
replaceControls(control, frameModels = NULL, 
                frameViews = frameModels@models[[modelIndex]], 
                modelIndex = 1, 
                graphWindow = frameViews@graphs[[viewIndex]], 
                viewIndex = 1, ...)
}

\arguments{
  \item{control}{ Structure as returned from \code{\link{dg.control}}c. }
  \item{frameModels}{ See \code{\link{replaceVertexList}}. }
  \item{frameViews}{ See \code{\link{replaceVertexList}}. }
  \item{modelIndex}{ See \code{\link{replaceVertexList}}. }
  \item{graphWindow}{ See \code{\link{replaceVertexList}}. }
  \item{viewIndex}{ See \code{\link{replaceVertexList}}. }
  \item{\dots}{ See \code{\link{replaceVertexList}}. }
}

% \details{}

% \value{}

\author{ Jens Henrik Badsberg }

% \note{ }

% \seealso{  }

% \examples{ }

\keyword{classes}
