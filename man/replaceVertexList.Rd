\name{replaceVertexList}
\alias{replaceVertexList}

\title{ Replace the vertex list of model frame }

\description{
  Replace the vertex list of model frame
}

\usage{
replaceVertexList(vertexList, frameModels = NULL, 
                  frameViews = frameModels@models[[modelIndex]], 
                  modelIndex = 1, 
                  graphWindow = frameViews@graphs[[viewIndex]], 
                  viewIndex = 1, ...)
}

\arguments{
  \item{vertexList}{ Object of class \code{"dg.VertexList"}:
                     \code{vertexList} is the new vertex list. }
  \item{frameModels}{ The vertex list is replaced in \code{frameModels},
                      and \code{frameModels} is redrawn
                      in \code{graphWindow}. }
  \item{frameViews}{ The \code{frameViews} to redraw. }
  \item{modelIndex}{ The \code{modelIndex} of the model to redraw. }
  \item{graphWindow}{ The \code{graphWindow} to redraw. }
  \item{viewIndex}{ The \code{viewIndex} of the view to redraw. }
  \item{\dots}{ Optional arguments. }
}

% \details{}

% \value{}

\author{ Jens Henrik Badsberg }

\note{ The models is redrawn as stored in \code{frameModels}.
       If you by interaction with the graph window has changed the model
       of \code{graphWindow}, please export the \code{frameModels}
       from the graph window by selection "Assign 'frameModels' .."
       from the "Export" menu of the graph window before replacing. }

% \seealso{  }

\examples{ }

\keyword{classes}
