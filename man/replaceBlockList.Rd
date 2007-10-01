\name{replaceBlockList}
\alias{replaceBlockList}

\title{ Replace the block list of model frame }

\description{
  Replace the block list of model frame.
}

\usage{
replaceBlockList(blockList, frameModels = NULL, 
                 frameViews = frameModels@models[[modelIndex]], 
                 modelIndex = 1, 
                 graphWindow = frameViews@graphs[[viewIndex]], 
                 viewIndex = 1, ...)
}

\arguments{
  \item{blockList}{ Object of class \code{"dg.BlockList"}:
                    \code{blockList} is the new block list. }
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

\examples{ }

\keyword{classes}
