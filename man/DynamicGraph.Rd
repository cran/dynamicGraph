\name{DynamicGraph}

\alias{DynamicGraph}

\title{DEPRECATED: Simple interface to dynamicGraph}

\description{
    A simple interface to \code{dynamicGraph} in the
    sense that the graph should not be given as an object
    as to \code{\link{dynamicGraphMain}}. Here vertices can be 
    specified by a vector of text strings with names,
    and/or edges by pairs of the indices of the vertices.

    The function can also be used to add models and views to
    an existing \code{dynamicGraph}.

    The interface is deprecated: Use the method \code{\link{dg}}
    on an object of class \code{\link{dg.simple.graph-class}} instead,
    or the methods \code{\link{addModel}}, \code{\link{addView}},
    \code{\link{replaceModel}}, or \code{\link{replaceView}}.

}

\usage{
DynamicGraph(names = character(), types = character(), 
             from = vector(), to = vector(), edge.list = list(NULL), 
             labels = names, edge.types = character(), 
             blocks = list(NULL), block.tree = list(NULL), oriented = NA, 
             factors = list(NULL), texts = character(), 
             extra.from = vector(), extra.to = vector(), 
             extra.edge.list = list(NULL), 
             object = NULL, viewType = "Simple", 
             frameModels = NULL, frameViews = NULL, graphWindow = NULL, 
             addModel = FALSE, addView = FALSE, overwrite = FALSE, 
             returnNewMaster = FALSE, redraw = FALSE, 
             control = dg.control(...), ...)
}

\arguments{
  \item{names}{ See \code{vertex.names} of 
                \code{\link{dg.simple.graph-class}}. }
  \item{types}{ See \code{\link{dg.simple.graph-class}}. }
  \item{from}{  See \code{\link{dg.simple.graph-class}}. }
  \item{to}{ See \code{\link{dg.simple.graph-class}}. }
  \item{edge.types}{ See \code{\link{dg.simple.graph-class}}. } 
  \item{edge.list}{ See \code{\link{dg.simple.graph-class}}. }
  \item{labels}{ See \code{\link{dg.simple.graph-class}}. }
  \item{blocks}{ See \code{\link{dg.simple.graph-class}}. }
  \item{block.tree}{ See \code{\link{dg.simple.graph-class}}. }
  \item{oriented}{ See \code{\link{dg.simple.graph-class}}. }
  \item{factors}{ See \code{\link{dg.simple.graph-class}}. }
  \item{texts}{ See \code{\link{dg.simple.graph-class}}. }
  \item{extra.from}{ See \code{\link{dg.simple.graph-class}}. }
  \item{extra.to}{ See \code{\link{dg.simple.graph-class}}. }
  \item{extra.edge.list}{ See \code{\link{dg.simple.graph-class}}. }
  \item{viewType}{ See \code{\link{dg.simple.graph-class}}. }

  % \item{names}{ A vector with text strings for the \code{names} 
  %       of the vertices. }
  % \item{types}{ A vector with text strings for the \code{types},
  %       labels of \code{\link{dg.Vertex}}, of the vertices. }
  % \item{from}{ If not \code{edge.list} is given:
  %       The indices of the first endpoints of the edges. }
  % \item{to}{ If not \code{edge.list} is given:
  %       The indices of the second endpoints of the edges. }
  % \item{edge.types}{ A vector of text strings giving the \code{types} of the
  %    edges, identify which classes the edges should be of,
  %    containing the \code{dg.VertexEdge}.
  %    } 
  % \item{edge.list}{ If not \code{from} and \code{to} are given:
  %       A list where each item specifies an edge by a vector
  %       of the indices or names of the vertices. }
  % \item{labels}{ A vector with text strings for the \code{labels} 
  %       of the vertices. }
  % \item{blocks}{ 
  %       A list defining the \code{blocks}:
  %       Each item is the vector of the indices of the vertices of the
  %       block, or the vector with text strings for the names of the vertices
  %       of the block. 
  %       The arguments \code{right.to.left}, \code{nested.blocks} 
  %       and \code{blockColors} are here used in \code{\link{setBlocks}}
  %       to control the layout of the blocks.
  %   }
  % \item{block.tree}{ If not the argument \code{blocks} is used:
  %    A structure with the blocks in a \code{block.tree}.
  %       The arguments \code{overlaying} and \code{blockColors} are here used
  %       in \code{\link{setTreeBlocks}} to control the layout of the blocks.
  %       }
  % \item{oriented}{ Logical. If \code{oriented} is set to TRUE
  %       then the edges are oriented, also when no block structure is given. }
  % \item{factors}{A list defining the \code{factor vertices}:
  %       Each item is the vector of the indices of the vertices of a factor.
  %       }
  % \item{texts}{ A vector of text strings, for additional labels. 
  %         These labels will be set by \code{'ExtraVertices'}
  %               of class \code{\link{dg.TextVertex-class}}. }
  % \item{extra.from}{ If not \code{extra.edge.list} is given:
  %       The indices of the first endpoints of the extra edges,
  %       negative for extra vertices. }
  % \item{extra.to}{ If not \code{extra.edge.list} is given:
  %       The indices of the second endpoints of the extra edges,
  %       negative for extra vertices. }
  % \item{extra.edge.list}{ If not \code{extra.from} and \code{extra.to}
  %       are given: A list where each item specifies an extra edge by a vector
  %       of the indices or names of the vertices or extra vertices,
  %       negative indices for extra vertices. }

  \item{object}{ The model \code{object}, or NULL,
       see \code{\link{dg.Model-class}}. }

  % \item{viewType}{ A text string with the type of view. }

  \item{frameModels}{ An object of class \code{\link{DynamicGraph-class}}.
		      \code{frameModels} is the object for a dataset
		      and the models on that dataset. }
  \item{frameViews}{ An object of class \code{\link{DynamicGraphModel-class}}.
		     \code{frameViews} is the object for a model
		     and the views of that model. }
  \item{graphWindow}{ An object of class \code{\link{DynamicGraphView-class}}.
		      \code{graphWindow} is the object for a view of a model. }
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
                    % The argument \code{returnLink} must be set to TRUE
		    % to overwrite a view. 
                  }
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

\details{
    After converting the arguments for the graph
    first to an object of class \code{\link{dg.simple.graph-class}}
    then to an object of class \code{\link{dg.graph-class}}
    the function \code{\link{dynamicGraphMain}} does all the work.

    % For building your applications you can look into the source code
    % of \code{\link{simpleGraphToGraph}} about how the lists of objects
    % of the object \code{\link{dg.graph-class}} are made.

    The list of objects can be exported from
    \code{\link{dynamicGraphMain}}, also after modifying the graph.

    % If the argument \code{returnLink} is set to TRUE then the returned
    % value can be use in subsequential call to \code{\link{DynamicGraph}}
    % to add models and views to the created graph window.

}

\value{
  The returned value from \code{\link{dynamicGraphMain}}.
}

\author{Jens Henrik Badsberg}

\examples{
require(tcltk); require(dynamicGraph)

# Example 1:

W <- dg(as(new("dg.simple.graph", vertex.names = 1:5), "dg.graph"),
        control = dg.control(title = "Very simple"))

# Example 2:

W <- dg(new("dg.simple.graph", from = 1:4, to = c(2:4, 1)), 
        control = dg.control(title = "Simply edges"))

# Example 3:

V.Types <- c("Discrete", "Ordinal", "Discrete",
             "Continuous", "Discrete", "Continuous")

V.Names  <- c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize")
V.Labels <- paste(V.Names, 1:6, sep ="/")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

W <- dg(new("dg.simple.graph", vertex.names = V.Names, types = V.Types,
            labels = V.Labels, from = From, to = To), 
        control = dg.control(title = "With labels (extraVertices)"))


# Example 4: Oriented (cyclic) edges, without causal structure:

W <- dg(new("dg.simple.graph", vertex.names = V.Names, types = V.Types,
            labels = V.Labels, from = From, to = To, oriented = TRUE),
        control = dg.control(title = "Oriented edges"))


# Example 5: A factor graph:

Factors <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))

W <- dg(new("dg.simple.graph", vertex.names = V.Names, types = V.Types,
          labels = V.Labels, factors = Factors, viewType = "Factor"),
        control = dg.control(title = "Factorgraph", namesOnEdges = FALSE))


# Example 6: Edges with more than two vertices:

EdgeList <- list(c(1, 2, 3, 4), c(3, 4, 5), c(4, 5, 6))

W <- dg(new("dg.simple.graph", vertex.names = V.Names, types = V.Types,
          labels = V.Labels, edge.list = EdgeList),
        control = dg.control(title = "Multiple edges", namesOnEdges = FALSE))

W
}

\keyword{ dplot }
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graphs }
\keyword{ methods }
\keyword{ multivariate }
