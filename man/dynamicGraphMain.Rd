\name{dynamicGraphMain}
\alias{dynamicGraphMain}
\alias{dynamicGraph}
\alias{GraphLatticeProto}
\alias{CanvasProto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dynamic Graph }
\description{
    Interactive plot for manipulating graphs.
}
\usage{
dynamicGraphMain(vertexList, visibleVertices = 1:length(vertexList),
                 edgeList = NULL, blockList = NULL, blockEdgeList = NULL,
                 blockTree = NULL, oriented = FALSE,
                 factorVertexList = NULL, factorEdgeList = NULL, 
                 extraList = NULL,
                 object = NULL, objectName = "", 
                 vertexClasses = validVertexClasses(),
                 title = "dynamicGraph", transformation = NULL, 
                 width = 400, height = 400, w = 6, 
                 vertexColor = "blue", extraVertexColor = "black", 
                 edgeColor = "blue",
                 factorVertexColor = "green", factorEdgeColor = "green",
                 blockEdgeColor = "default", blockColors = NULL,
                 background = "white", closeenough = 2,
                 drawBlockFrame = TRUE, drawBlockBackground = TRUE,
                 UserMenus = NULL, 
                 hasMethods = TRUE, enterLeaveUpdate = TRUE,
                 namesOnEdges = TRUE, updateEdgeLabels = TRUE,
                 debug.strata = FALSE, debug.edges = FALSE,
                 debug.position = FALSE, debug.update = FALSE, 
                 returnLink = FALSE, returnNull = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{vertexList}{ List of vertices
        (each of class containing the class \code{VertexProto})
        created by \code{\link{returnVertexList}}
        or exported from \code{\link{dynamicGraphMain}}. }
    \item{visibleVertices}{ Numeric vector of the indices of the vertices of
        \code{vertexList} to plot. }
    \item{edgeList}{ List of edges (of class containing \code{EdgeProto})
        created by \code{\link{returnEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}.  }
    \item{blockList}{ List of blocks (each of class \code{BlockProto})
        created by \code{\link{setBlocks}}
        or exported from \code{\link{dynamicGraphMain}}.
        If the \code{blockTree} argument is given, then
        the argument \code{blockList} should not also be given. }
    \item{blockEdgeList}{ List of blockedges
        (of class containing the class \code{BlockEdgeProto})
        created by \code{\link{returnBlockEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}. }
    \item{blockTree}{ Structure with blocks (of class \code{BlockProto})
        created by \code{\link{setTreeBlocks}}
        or exported from \code{\link{dynamicGraphMain}}. }
    \item{oriented}{ Logical.  If TRUE then
        edges are oriented, also when blocks are missing. }
    \item{factorVertexList}{ List of secondary vertices,
        called factor vertices, for, e.g., the generators of the model,
        (each of class containing \code{FactorVertexProto})
        created by \code{\link{returnFactorVerticesAndEdges}}
        or exported from \code{\link{dynamicGraphMain}}. }
    \item{factorEdgeList}{ List of bipartite graph edges,
        called factor edges,
        (each of class containing \code{FactorEdgeProto})
        created by \code{\link{returnFactorEdgeList}}
        or exported from \code{\link{dynamicGraphMain}}.
        Factor edges are edges between vertices and factor vertices. }
    \item{extraList}{ List of vertices (of class containing the class
	\code{VertexProto}) 
        created by \code{\link{returnVertexList}}
        or exported from \code{\link{dynamicGraphMain}},
        for, e.g., additional titles in the plot. }
    \item{object}{ NULL, or object with the methods \code{\link{modifyModel}}
        and \code{\link{testEdge}} - for respectively updating the
        object when the graph is updated, and for computing the
	test statistic when an edge is labeled. The returned object from
        \code{\link{testEdge}} should have the methods 
        \code{label} and \code{width}
        % (was \code{labelOfTest} and \code{widthOfTest})
        for extracting the label of a test for putting the label on the edge
        and for extracting the width for the edge.
        See \code{\link{newDefaultModelObject}}. }
    \item{objectName}{ If set to a text string then the object is assigned with
	this name in .Global.Env when the object is updated. }
    \item{vertexClasses}{ Returned value from \code{\link{validVertexClasses}},
        or extension of this matrix. Used when creating
        new vertices in \code{\link{dynamicGraphMain}}. }
    \item{title}{ Text string with the title set on the graph window. }
    \item{transformation}{ NULL,
                or rotation matrix for projecting the positions
        of the vertices, blocks, etc. onto the canvas. }
    \item{width}{ Integer with the width of the plot canvas. }
    \item{height}{ Integer with the height of the plot canvas. }
    \item{w}{ The \code{radius} of the vertices. Send as argument to the
        \code{draw} method for vertices, and edges are shortened
        by the quantity in each end. }
    \item{vertexColor}{ Single text string. 
        Colors of new vertices created in
        \code{\link{dynamicGraphMain}}
        (by \code{\link{newVertex}}).
        The colors of the vertices of \code{vertexList} are
        given in this list. \code{vertexColor} is given as
        argument to the \code{draw} method of vertices, but this 
        argument are by default not used in the draw method. }
    \item{extraVertexColor}{ Single text string. 
        As \code{vertexColor}, but for the vertices of
        \code{extraList}. }
    \item{edgeColor}{ Single text string. 
        \code{edgeColor} is similar to \code{vertexColor}. }
    \item{factorVertexColor}{ Single text string. 
        \code{factorVertexColor} is similar to
        \code{vertexColor}.
        If \code{factorVertexColor} is "default" then
        the color of a factor vertex will depend on the
        type of the generator of the factor. }
    \item{factorEdgeColor}{ Single text string. 
        \code{factorEdgeColor}
        is similar to \code{edgeColor}. }
    \item{blockEdgeColor}{ "default", or list with two text strings for colors.
        \code{blockEdgeColor} is similar to \code{edgeColor}.
        The two colors are used for respectively edges
        between two blocks and for edges between blocks 
        and vertices. }
    \item{blockColors}{ List of colors of blocks. 
        Similar to \code{vertexColor}: Only used when creating
        new blocks, else the colors set in \code{blockList}
        and \code{blockTree} are used. }
    \item{background}{ The color of the canvas of the graph window,
        default: "white". }
    \item{closeenough}{ Parameter for whether the mouse is close enough
        when clicking an object of the graph window,
        see \code{\link[tcltk]{tkcanvas}}. }
    \item{drawBlockFrame}{ Logical.
                If TRUE then frames are drawn around blocks. }
    \item{drawBlockBackground}{ Logical. If TRUE then a \code{block canvas} is
        drawn, with color set by \code{blockColors}. }
    \item{UserMenus}{ List with user defined menu items for main menu
        and pop up menus. See \code{\link{redrawGraphWindow}}
        for an example of a user specified menu. }
    \item{hasMethods}{ Logical. 
        If TRUE then the \code{object} should have the methods
        \code{\link{modifyModel}} and \code{\link{testEdge}}. 
        (I do not know why the R-function
         \code{\link[methods]{hasMethod}} does not
        work on \code{objects} (defined outside the package
	\code{\link{dynamicGraph}})
	inside \code{\link{dynamicGraphMain}}).}
    \item{enterLeaveUpdate}{ Logical. If FALSE then the graph window
        is not redrawn when the mouse enters and leaves the graph window. }
    \item{namesOnEdges}{ Logical. If FALSE then the names of the vertices 
        are not set on the edge as label. }
    \item{updateEdgeLabels}{ Logical.  If FALSE then the edge labels
        are not cleared when the model is updated. }
    \item{debug.strata}{ Logical for tracing strata of vertices
                (also in plot). }
    \item{debug.edges}{ Logical for tracing edges (also by labels in plot). }
    \item{debug.position}{ Logical for tracing positions of vertices. }
    \item{debug.update}{ Logical for tracing redraw of graph window. }
%   \item{GraphLattice}{ ~~Describe \code{GraphLattice} here~~ }
%   \item{returnNewMaster}{ ~~Describe \code{returnNewMaster} here~~ }
    \item{returnLink}{ Logical, if \code{returnLink} is TRUE then a reference
		       to the dynamic graph window is returned, else an object 
		       of class \code{\link{GraphLatticeProto-class}}
                       is returned (if not \code{returnNull} is TRUE).
    }
    \item{returnNull}{ Logical, if \code{returnNull}
                       is TRUE then NULL is returned. }
    \item{\dots}{ Additional arguments. }
}

\details{

    This is a dynamic plotting tool for handling graphs.
    The lay out of the graph can be edited by moving
    the vertices of the graph by the mouse, and edges can be 
    added by clicking vertices and dropping by clicking the edges.

    The function is incremental in the sense that the user can add a method
    for updating the model object of the window when the graph is updated,
    and a method for computing the test of an edge when the edge is clicked
    by the mouse.
    
    Edges can be oriented, drawn by arrows.
    
    Blocks can be used to define a causal structure of the variables
    represented by vertices, and edges between blocks are then oriented.
    Blocks can be given in a structure such that descendant blocks of a blocks
    also are closed when a block is closed.
    
    A secondary set of vertices, factor vertices, can be used to represent
    hypergraphs.
    
    "Slave graph windows" can be created: The windows will share vertices
    and blocks, thus when a vertex is moved in one window, the position of
    the vertex will also change in all slave windows. The edges are not
    shared among windows, since the individual windows will typical
    represent different models.
    Thus factors (vertices and edges) are not shared between graph windows.

    }

% Rotate:            Right click and drag

% Rock plot:         Double right click (not working!)

\value{
    If \code{returnLink} is TRUE then a reference (an object of class "list")
    to the dynamic graph window is returned, else an object of class 
    \code{\link{GraphLatticeProto-class}}
    is returned (if not \code{returnNull} is TRUE)

    The function \code{\link{redrawGraphWindow}} of the returned reference
    is used to draw other graphs in the graph window or to create slave
    graphs windows, see \code{\link{addGraph}}.
   
    The object of class \code{\link{GraphLatticeProto}} will
    beside the list of canvases of the graph window and slave windows,
    also hold lists of vertices and blocks (block trees).

    \item{...@graphs}{Slot 'graphs': List of class \code{\link{CanvasProto}}.
	Might, beside slots for accessing the graph window, also hold 
	slots for edges, factors, factor edges, block edges, etc.}
    \item{...@graphs[[i]]@top}{Slot 'top' of each object in the list of the
        'graphs' slot of the returned object:
        The 'top' of the canvas of the i-the 
        graph window.}
}

    \section{All object in the graph window:}{
	\itemize{
	    \item{Left click, hold and drag:}{The object will move.}
	    \item{Left click:}{Action to the object:
                Vertices and blocks will highlight, 
                at vertices and blocks edges are added after highlighting,
                at edges the edge will be deleted,
                tests are computed for edge labels.}
	    \item{Left or right click, with SHIFT or/and CONTROL:}{The object
                  will be marked.}
	    \item{Double left click:}{Action to object:
                Vertices are deleted,
                blocks will close,
                closed blocks will open.}
	    \item{Right click:}{The pop up menu of the object will appear.}
        }
    }

    \section{Vertices (vertices and factor vertices):}{
	Right click the vertex to get the pop up menu of the vertex:
	\itemize{
	    \item{Highlight a vertex:}{For adding an edge -
                Left click the vertex.}
	    \item{Mark a vertex:}{For adding edges, etc. - 
	        Left or right click the vertex
                while holding SHIFT or/and CONTROL down.}
	    \item{Cancel highlighting:}{Left click (or drag) the vertex.}
	    \item{Add an edge:}{After first highlighting a vertex - 
                Left click the other vertex.}
	    \item{Move a vertex:}{Left click and drag the vertex.}
	    \item{Move a vertex label:}{Left click and
                drag the label of the vertex.}
	    \item{Delete a vertex:}{Double left click the vertex.}
	    \item{Create new graph:}{A slave window with the vertex delete -
		Select "Drop vertex" from pop up menu at the vertex.}
	    \item{Change a vertex label:}{Double left click the label,
		and enter the new label in the appearing dialog window.}
	    \item{Delete a vertex label:}{Select "Delete vertex label" from
		the pop up menu at the vertex or at the vertex label.}
	    \item{Create new vertex:}{At mouse position -
		Middle click the canvas.}
	    \item{Create new vertex:}{At mouse position
                with the edge to last vertex -
		Double middle click the canvas.}
	}
	The main menu "Variables":
	\itemize{
	    \item{Create new variable:}{With setting the class of the new
		vertex and defining an expression for updating the
		\code{object} - Select "Create new variable".} 
	    \item{Display, add, a vertex:}{In the current window -
		Select "Select vertex among variables not displayed (here)". }
	    \item{Display, add, a vertex:}{In a slave window -
		Select "Select vertex among variables not displayed (slave)". }
	    \item{Export the vertex list:}{Select
                "Assign Vertices in .GlobalEnv". }
	    \item{Export the labels:}{Select
                "Assign ExtraVertices in .GlobalEnv". }
	}
    }
    
    \section{Edges (edges to/from vertices, blocks and factors):}{
	Right click the edge to get the pop up menu of the edge:
	\itemize{
	    \item{Add an edge:}{Left click first the vertex (or block)
                to highlight, and then left click other vertex (or block).}
	    \item{Delete an edge:}{And update in the current window -
		Left click the edge.}
	    \item{Delete an edge:}{And create a slave graph with the
		resulting graph
		- Select "Drop edge" from the pop up menu at the edge.}
	    \item{Move an edge (2 vertices):}{Left
                click the edge and drag the edge.}
	    \item{Move an edge label:}{Left click the edge label and
		drag the label.}
	    \item{Set an edge label:}{Select "Set edge label" from the
		edge pop up menu.}
	    \item{Compute an edge label:}{Left click the edge label,
		or select "Compute edge label" from pop up menu at the edge.}
	    \item{Compute an edge label:}{Force computation for
		"harder models" - Double left  click the edge label,
		or select "Force compute edge label" from pop up menu of
		the edge.}
	    \item{Delete an edge label:}{Triple left click the edge label,
		or select "Delete label of edge" from the pop up menu at
		the edge.}
	}
	The main menu "Edges":
	\itemize{
	    \item{Delete all edge labels:}{Select "Delete all edge labels". }
	    \item{Export the edge list:}{Select "Assign Edges in .GlobalEnv". }
	}
    }

    \section{Blocks, opened:}{
	Right click the block label (or colored  block canvas if
	\code{drawBlockBackground} is set to TRUE) to get the pop up
	menu of opened block:
	\itemize{
	\item{Move a block:}{With its sub blocks and vertices - 
                Left click the block label or colored block canvas and
		drag the block. [ Slow !!!] } 
	\item{Resize a block:}{Left click a block corner or a block
                edge and drag. }
	\item{Minimize a block:}{With its sub blocks and vertices - 
                Double left click the block label or the colored block canvas.
              }
	\item{Maximize a block:}{Zoom to the block - Right click the
                block label or colored the block canvas and select
		"Maximize" in the appering popup menu. }
	\item{Zoom out to the full graph:}{Right click the block label
	    or colored block canvas and select "Redraw (Zoom to) full
	    graph" in the appearing block pop up menu. }
	    }
    }			  			   	

    \section{Blocks, closed:}{
        Right click the block to get the pop up menu of the closed block:
	\itemize{
	    \item{Highlight a block:}{For adding edges from all vertices
		of the block to a vertex or a block -
                Left click the block.}
	    \item{Mark a block:}{For adding edges, etc. - 
	        Left or right click the block
                while holding SHIFT or/and CONTROL down.}
	    \item{Cancel highlighting:}{Of a block - Left click (or drag)
		the block.}
	    \item{Add edges:}{To all vertices of block
		after highlighting first a block or a vertex - Click
                the other block.}
	    \item{Move a closed block:}{Left click and drag the block.}
	    \item{Move a block label:}{Left click and drag the label
                of the block.}
	    \item{Open a closed block:}{Double left click the block.}
	    \item{Change a block label:}{Double left click the label of
		the closed block, 
		and enter the new label in the appearing dialog window.}
	    \item{Delete a block label:}{Select "Delete label of block" from
		the pop up menu at the block or the block label.}
	}
	The main menu "Blocks":
	\itemize{	    
	    \item{Export the block list:}{Select
                "Assign BlockList in .GlobalEnv". }
	    \item{Export the block tree:}{Select
                "Assign BlockTree in .GlobalEnv". }
	    \item{Export the block edges:}{Select
                "Assign BlockEdges in .GlobalEnv". }
	}
    }			  			   	

    \section{Factor vertices:}{
        Right click the factor vertex to get the pop up menu of the
	factor: Actions as for vertices.
% 	\itemize{
% 	    \item{Pop up menu for factor vertex:}{Right click the
%                 factor vertex:
% 		Actions as for vertices.}
% 	}
	The main menu "Generators":
	\itemize{	    
	    \item{Export the factor vertices:}{Select
                "Assign FactorVertices in .GlobalEnv". }
	    \item{Export the factor edges:}{Select
                "Assign FactorEdges in .GlobalEnv". }
		}			  			   	
	    }  			   	
	    
    \section{Factor edges:}{
        Right click the factor edge to get the pop up menu of the factor
	edge: Actions are as for edges.
%	\itemize{
%	    \item{Pop up menu for factor edges:}{Right click the factor edge:
%		Actions are as for edges.}
%		}			  			   	
	    }

    \section{The graph:}{
	\itemize{
	    \item{Create a slave window:}{Select "Copy: Make slave window".}
	    \item{Redraw (zoom out):}{From the lists of objects - Select
		"Redraw the graph window".}
	    \item{Update:}{Faster fix of "corrupted" window:
                  Select "Update the graph window".}
	    \item{Export the linkGraphLattice :}{Select
		"Assign linkGraphLattice in .GlobalEnv".}
	    \item{Export the graph lattice :}{Select
		"Assign Graph-lattice in .GlobalEnv".}
	    \item{Export the graph window:}{Select
		"Assign Graph-window in .GlobalEnv".}
	    \item{Export the object:}{Select
		"Assign Object in .GlobalEnv".}
	}			  			   	
    }			  			   	
    
    \section{Rotation:}{
	\itemize{
	    \item{Enable rotation:}{Select
		"Enable (reset) transformation" from the main menu "Graph".}
	    \item{Rotate the graph:}{Middle click the canvas, and drag.}
	    \item{Disable rotation:}{Select
		"Disable transformation" from the main menu "Graph".}
	    \item{Export transformation:}{Export the projection
		matrix - Select
		"Assign Transformation in .GlobalEnv"
                from the main menu "Graph".}
	}			  			   	
    }			  			   	
    
{}

% http://www.math.auc.dk/gr/material/CoCo/
% http://www.jbs.agrsci.dk/Biometry/Software-Datasets/CoCo/CoCo.1.6/

\section{Acknowledgments}{
Many thanks to the gR-group for useful discussions, especially to 
Claus Dethlefsen for testing early versions of this package on DEAL.
}

\references{
     CoCo, avaliable form \url{http://www.math.auc.dk/gr/material/CoCo} and
     \url{http://www.jbs.agrsci.dk/}
     \code{Biometry/Software-Datasets/CoCo/CoCo.1.6/}
     with a guide at \url{http://www.jstatsoft.org/v06/i04/},
     has an interface to \code{\link{dynamicGraph}}. }

\author{Jens Henrik Badsberg}

\note{
     Vertices, edges, blocks, block edges,
     factors, and factor edges are objects of the following classes:
     \code{\link{VertexProto}}, \code{\link{EdgeProto}},
     and \code{\link{BlockProto}} contains \code{\link{NodeProto}},
     \code{\link{FactorVertexProto}} contains \code{\link{VertexProto}}, and
     \code{\link{VertexEdgeProto}}, \code{\link{BlockEdgeProto}}, 
     \code{\link{FactorEdgeProto}} contains \code{\link{EdgeProto}}.

     The methods 
     \code{\link{draw}},
     \code{\link{color}},
     \code{\link{color<-}},
     \code{\link{label}},
     \code{\link{label<-}},
     \code{\link{labelPosition}},
     \code{\link{labelPosition<-}},
     \code{\link{name}},
     \code{\link{name<-}},
     \code{\link{index}},
     \code{\link{index<-}},  
     \code{\link{position}},
     \code{\link{position<-}},
     \code{\link{stratum}},
     \code{\link{stratum<-}},
     \code{\link{visible}},
     \code{\link{visible<-}},
     \code{\link{addToPopups}},
     \code{\link{oriented}},
     \code{\link{oriented<-}},
     \code{\link{width}},
     \code{\link{width<-}},
     \code{\link{nodeIndicesOfEdge}},
     \code{\link{nodeIndicesOfEdge<-}},
     \code{\link{nodeTypesOfEdge}},
     % ( \code{\link{labelOfEdge}},
     %   \code{\link{labelOfEdge<-}},
     %   \code{\link{widthOfEdge}},
     %   \code{\link{widthOfEdge<-}} ),
     \code{\link{ancestors}},
     \code{\link{ancestors<-}},
     \code{\link{descendants}}, and
     \code{\link{descendants<-}},
     are implemented for objects of these classes.

     For lists of vertices, edges, blocks, block edges,
     factors, and factor edges the methods
     \code{\link{Names}},
     \code{\link{Names<-}},
     \code{\link{Colors}},
     \code{\link{Colors<-}},
     \code{\link{Labels}},
     \code{\link{Labels<-}},
     \code{\link{LabelPositions}},
     \code{\link{LabelPositions<-}},
     \code{\link{Positions}},
     \code{\link{Positions<-}},
     \code{\link{Strata}},
     \code{\link{Strata<-}},
     \code{\link{Indices}},
%    \code{\link{Indices<-}},
%     \code{\link{Visible}},
%     \code{\link{Visible<-}},
     \code{\link{NodeAncestors}},
     \code{\link{NodeAncestors<-}},
     \code{\link{NodeDescendants}}, and
     \code{\link{NodeDescendants<-}} are available.

     The block tree created by \code{\link{setTreeBlocks}} is unlisted by
     \code{\link{blockTreeToList}} such that the above methods
     can be applied.

     The model object of the call of \code{\link{dynamicGraphMain}}
     should have the methods \code{modifyModel} and \code{testEdge}.
     When the graph is modified, by adding or dropping vertices or edge,
     the method \code{modifyModel} is called on the argument \code{object}
     of \code{\link{dynamicGraphMain}}. If an \code{object} is returned in
     the list of the returned value from \code{modifyModel} then
     \code{object} in \code{\link{dynamicGraphMain}} is replaced by this
     object, and the object is also assigned in the top level environment,
     if \code{objectName} was given to \code{\link{dynamicGraphMain}}.

     The method \code{testEdge} of \code{object} should return an object with
     the methods \code{label} and \code{width} for labeling edges,
     see \code{\link{newDefaultTestObject}}.

 }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{See also \code{\link{DynamicGraph}} and all the other functions
	of this package. 

      An example has been divided on the following 4 manual pages:
      \code{\link{newDefaultModelObject}} and
      \code{\link{newDefaultTestObject}}
      gives an example of a model object with test object.
      The pages of \code{\link{redrawGraphWindow}} show how the user 
      can add menu items with actions that redraws the graph
      after modification of edges.
      Finally, \code{\link{validVertexClasses}} show how to create
      a new vertex class with a new symbol for drawing the vertex 
      and an item added to the pop up menu of the new vertex class.
      The files \code{".../dynamicGraph/Tests/Circle.newClass.R"} of
      the example catalog of \code{dynamicGraph} will do this example
      collected from these 4 pages.
}

\examples{

require(tcltk)
require(dynamicGraph)

V.Names <- paste(c("Sex", "Age", "Eye", "FEV", "Hair", "Shosize"),
                 1:6, sep ="/")

V.Types <- c("Discrete", "Ordinal", "Discrete",
             "Continuous", "Discrete", "Continuous")

Vertices <- returnVertexList(V.Names, types = V.Types, color = "red")

From <- c(1, 2, 3, 4, 5, 6)
To   <- c(2, 3, 4, 5, 6, 1)

EdgeList <- vector("list", length(To))
for (j in seq(along = To)) EdgeList[[j]] <- c(From[j], To[j])
Edges <- returnEdgeList(EdgeList, Vertices, color = "black")

Z <- dynamicGraphMain(Vertices, edgeList = Edges, w = 4)

}
\keyword{ dplot }
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graphs }
\keyword{ methods }
\keyword{ multivariate }
