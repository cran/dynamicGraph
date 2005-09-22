\name{drawModel}

\alias{drawModel}
\alias{redrawView}
\alias{redrawGraphWindow}
\alias{menu}

\title{DEPRECATED: Draw the dynamicGraph window and slaves }

% \newpage

\description{
  The functions \code{drawModel} and \code{redrawView} within
  \code{dynamicGraph} is for adding models to \code{dynamicGraph},
  for adding new views of a model, and for overwriting an existing
  view with an other model.

  The functions can not be found at top level.

  The functions are called by the methods
  \code{\link{addModel}}, \code{\link{addView}},
  \code{\link{replaceModel}}, and \code{\link{replaceView}}.

}

% \usage{


\arguments{
  \item{frameModels}{ An object of class \code{\link{DynamicGraph-class}}.
		      NULL,
                      or \code{frameModels} of \code{list(...)$Arguments}. }
  \item{frameViews}{ An object of class \code{\link{DynamicGraphModel-class}}.
                     NULL,
                     or \code{frameViews} of \code{list(...)$Arguments}.
                     If \code{frameViews} is set to \code{NULL}, 
                     the default value, then a new model frame will be created
		     by \code{drawModel}. }
  \item{graphWindow}{ An object of class \code{\link{DynamicGraphView-class}}.
		      If \code{graphWindow} is set to the value of 
                      \code{list(...)$Arguments$graphWindow} then
                      the calling graph window will be redrawn.
                      If \code{graphWindow} is set to \code{NULL}, 
                      the default value,
                      then a new slave graph window will be drawn. }

  \item{dg}{ As for \code{\link{dynamicGraphMain}}.
        If \code{dg} is given (set to a value different from NULL)
        then this value is used, else the value extracted from
        \code{list(...)$Arguments} is used. }

  \item{object}{ As for \code{\link{dynamicGraphMain}}.
        If \code{object} is given then this value is used,
        else the value extracted from \code{list(...)$Arguments} is used. }

  \item{frameModelsEnv}{ \code{frameModelsEnv} is then environment
        for storing hidden values of the \code{frameModels}.
        Extracted from \code{frameModels} by default. }

%   \item{graphComponent}{ Returned structure from \code{graphComponent}. }
% 
%   \item{edgeList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{edgeList} is given (set to a value different from NULL)
%         then this value is used, else the value extracted from
%         \code{graphComponent} or \code{list(...)$Arguments} is used.
%         If the value is not NULL in \code{graphComponent} then this
% 	value is used, else the value from \code{list(...)$Arguments}
%         is used when \code{edgeList} is NULL. }
% 
%   \item{oriented}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{oriented} is given then this value is used,
%         else the value extracted from \code{list(...)$Arguments} is used. }
% 
%   \item{blockEdgeList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{blockEdgeList} ... (as for \code{edgeList}). }
%   \item{factorVertexList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{factorVertexList} ... (as for \code{edgeList}). }
%   \item{factorEdgeList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{factorEdgeList} ... (as for \code{edgeList}). }
%   \item{extraEdgeList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{extraEdgeList} ... (as for \code{edgeList}). }
%   \item{visibleVertices}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{visibleVertices} ... (as for \code{edgeList}). }
%   \item{visibleBlocks}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{visibleBlocks} ... (as for \code{edgeList}). }
%   \item{extraList}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{extraList} ... (as for \code{edgeList}). }
% 
%   \item{object}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{object} ... (as for \code{oriented}). }
% 
%   \item{viewType}{ As for \code{\link{dynamicGraphMain}}.
%         If \code{viewType} ... (as for \code{oriented}). }

  % \item{title}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{title} ... (as for \code{oriented}). }
  % \item{transformation}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{transformation} ... (as for \code{oriented}). }
  % \item{width}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{width} ... (as for \code{oriented}). }
  % \item{height}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{height} ... (as for \code{oriented}). }
  % \item{w}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{w} ... (as for \code{oriented}). }
  % \item{vertexColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{vertexColor} ... (as for \code{oriented}). }
  % \item{extraVertexColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{extraVertexColor} ... (as for \code{oriented}). }
  % \item{edgeColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{edgeColor} ... (as for \code{oriented}). }
  % \item{factorVertexColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{factorVertexColor} ... (as for \code{oriented}). }
  % \item{factorEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{factorEdgeColor} ... (as for \code{oriented}). }
  % \item{blockEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{blockEdgeColor} ... (as for \code{oriented}). }
  % \item{blockColors}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{blockColors} ... (as for \code{oriented}). }
  % \item{extraEdgeColor}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{extraEdgeColor} ... (as for \code{oriented}). }
  % \item{background}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{background} ... (as for \code{oriented}). }

  \item{initialWindow}{ Logical, if \code{initialWindow} is TRUE
  			then the labels of the edges are updated. }

  \item{returnNewMaster}{ Logical, if \code{returnNewMaster} is TRUE
  			then .... }

  \item{redraw}{ Logical, if \code{redraw} is TRUE
  			then .... }

  % \item{returnLink}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{returnLink} ... (as for \code{oriented}). }
  % \item{returnNull}{ As for \code{\link{dynamicGraphMain}}.
  %       If \code{returnNull} ... (as for \code{oriented}). }

  \item{setUpdateCountModelMain}{ Logical. If
                                  \code{setUpdateCountModelMain} is TRUE then
                                  views of the same model will be updated. }

  \item{returnFrameModel}{ Logical, if \code{returnFrameModel} is TRUE
  			then .... }

  \item{control}{ Options for \code{\link{dynamicGraphMain}},
                  see \code{\link{dg.control}}. }

  \item{\dots}{ Used to porting \code{list(...)$Arguments}. }
}

\details{
  The \code{drawModel} and \code{redrawView} functions can be called
  from the functions of menus (main menu and pop up menus) of 
  \code{\link{dynamicGraphMain}}, from .GlobalEnv in 
  \code{\link{DynamicGraph}}) via returned values from
  \code{\link{dynamicGraphMain}} (and from the methods of the model
  object in the scope of the function \code{\link{dynamicGraphMain}}).
  As a result the graph window will be redrawn with an other view of the model,
  possible with, e.g., other edges, an other model is drawn,
  or a new slave graph window will appear.

  If the value of a argument to \code{drawModel} or \code{redrawView} is set,
  then this value is used, else the value from the calling window is used.
  The value of the calling window is given in the argument
  \code{Arguments} in the call of the function of the menu item.

  Below is an example, where items for labeling all the edges
  of the graph are added to the menu.
  The edges are visited, a test is computed for each edge,
  the label and width of the edge is updated, and the
  graph is drawn with the updated edge list.
}

\value{
  The returned value from \code{\link{dynamicGraphMain}}.
}


\author{Jens Henrik Badsberg}

\note{ The functions can not be called from top level, that is,
       the functions does not exists at .GlobalEnv, but only
       in returned values from \code{\link{dynamicGraphMain}}.

       It is recommended that the functions not are called, but that
       \code{\link{DynamicGraph}} is used with the arguments
       \code{frameModels}, \code{frameViews}, \code{graphWindow},
       \code{addModel}, \code{addView}, and/or \code{overwrite}
       to call the functions.
 }

\seealso{ See also \code{\link{dynamicGraphMain}},
          \code{\link{DynamicGraph}}
	  \code{\link{DynamicGraph-class}}, and
	  \code{\link{DynamicGraphModel-class}}.  }

% \examples{}

\keyword{ methods }
