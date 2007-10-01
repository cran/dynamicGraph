\name{wDG}
\alias{wDG}

\title{DEPRECATED: Interface to dynamicGraph}

\description{

  A wrapper to \code{\link{dynamicGraphMain}} for adding models
  and views, represented by a simple dynamic graph,
  to an existing \code{dynamicGraph}.

  ( The function was a part of the deprecated interface function
    \code{\link{DynamicGraph}}: Use the method \code{\link{dg}}
    on object of \code{\link{dg.simple.graph-class}} in stead. )

 The wrapper is depricated: use the methods \code{\link{dg}}, 
 \code{\link{addModel}}, \code{\link{addView}},
 \code{\link{replaceModel}}, or \code{\link{replaceView}}.

}

\usage{
wDG(sdg = NULL, object = NULL, 
    frameModels = NULL, frameViews = NULL, graphWindow = NULL, 
    dg = NULL, addModel = FALSE, addView = FALSE, overwrite = FALSE, 
    returnNewMaster = FALSE, redraw = FALSE, control = dg.control(...), ...)
}

\arguments{
  \item{sdg}{ Object of class \code{\link{dg.simple.graph-class}} }
  \item{object}{ The model \code{object}, or NULL,
        see \code{\link{dg.Model-class}}. }
  \item{frameModels}{ An object of class \code{\link{DynamicGraph-class}}.
		      \code{frameModels} is the object for a dataset
                      (defining vertics and blocks)
		      and the models on that dataset. }
  \item{frameViews}{ An object of class \code{\link{DynamicGraphModel-class}}.
		     \code{frameViews} is the object for a model
		     and the views of that model. }
  \item{graphWindow}{ An object of class \code{\link{DynamicGraphView-class}}.
		      \code{graphWindow} is the object for a view of a model. }
  \item{dg}{ An optional object of class \code{\link{dg.graphedges-class}}.
             If this agrument is given then edges and factors are
	     extracted from the argument. Similar with an optional
	     argument \code{Arguments}. }
  \item{addModel}{ Logical, if \code{addModel} then a model is added
		   to the argument \code{frameModels}, and a view of
		   the model is drawn. 
		   If the argument \code{overwrite} is TRUE and
                   the argument \code{graphWindow} is given 
		   then the model of \code{graphWindow} is replaced
		   by the model argument \code{object}.
		   If the argument \code{overwrite} is TRUE and
                   the argument \code{frameViews} is given 
		   then the model of \code{frame}\-{Views} is replaced
		   by the model argument \code{object}.
		   }
  \item{addView}{ Logical, if \code{addView} then a view of type set
		  by the argument \code{viewType}
		  for the model of the argument \code{frameViews} is added. }
  \item{overwrite}{ Logical, see the argument \code{addModel}. 
                    The argument \code{returnLink} must be set to TRUE
		    to overwrite a view. }
  \item{redraw}{ Logical. If TRUE then the dynamicGraph of the
                 arguments \code{frameModels} is 'redrawn'.
                 New instances of the windows are made. }
  \item{returnNewMaster}{ Logical. Alternative implementation of 
                          \code{addModel}, using the code of \code{redraw}.
                          As \code{redraw}, but the windows of 
			  \code{frameModels} exists, 
                          and a new model is added. }
  \item{control}{ Options for \code{\link{DynamicGraph}} and
                  \code{\link{dynamicGraphMain}}, see
                  \code{\link{dg.control}}. }
  \item{\dots}{ Additional arguments to \code{\link{dynamicGraphMain}}. }
}

% \details{ }

\value{
  The returned value from \code{\link{dynamicGraphMain}}.
}

\author{Jens Henrik Badsberg}

\examples{
require(tcltk); require(dynamicGraph)
}

\keyword{ dplot }
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graphs }
\keyword{ methods }
\keyword{ multivariate }
