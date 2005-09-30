\name{dg.control}
\alias{dg.control}

\title{ Options of dynamicGraphMain and simpleGraphtoGraph }

\description{ Allow the user to set some characteristics of
              'dynamicGraphMain' and 'DynamicGraph'. }

\usage{
dg.control(label = "dynamicGraph", 
           width = 400, height = 400, w = 6, margin = 100, 
           closeenough = 2, background = "white", transformation = NULL, 
           permitZoom = TRUE, UserMenus = NULL, constrained = FALSE, 
           vertexColor = "red", extraVertexColor = "white", 
           edgeColor = "black", 
           factorVertexColor = "default", factorEdgeColor = "brown", 
           blockEdgeColor = "default", blockColors = NULL, 
           extraEdgeColor = "peru", 
           drawblocks = TRUE, right.to.left = FALSE, 
           nested.blocks = FALSE, overlaying = TRUE, 
           fixedFactorPositions = FALSE, diagonal = TRUE, N = 3, 
           vertexClasses = validVertexClasses(), 
           factorClasses = validFactorClasses(), 
           edgeClasses = validEdgeClasses(), 
           viewClasses = validViewClasses(), 
           drawBlockFrame = TRUE, drawBlockBackground = FALSE, 
           useNamesForLabels = TRUE, namesOnEdges = TRUE, 
           updateEdgeLabels = TRUE, enterLeaveUpdate = TRUE, 
           updateAllViews = TRUE, 
           saveTkReferences = TRUE, saveFunctions = TRUE, 
           returnNull = FALSE, hasMethods = TRUE, variableFrame = TRUE,
           debug.strata = FALSE, debug.edges = FALSE, 
           debug.position = FALSE, debug.update = TRUE, ...)
}

\arguments{
    \item{label}{ Text string with the title set on the graph window. 
                  (\code{\link{dynamicGraphMain}})}
    \item{width}{ Integer with the width of the plot canvas. 
                  (\code{\link{dynamicGraphMain}})}
    \item{height}{ Integer with the height of the plot canvas. 
                  (\code{\link{dynamicGraphMain}})}
    \item{w}{ The \code{radius} of the vertices. Send as argument to the
        \code{draw} method for vertices, and edges are shortened
        by the quantity in each end. 
                  (\code{\link{dynamicGraphMain}})}
    \item{margin}{ Integer, the width of the margin round the canvas. 
                  (\code{\link{dynamicGraphMain}})}
    \item{closeenough}{ Parameter for whether the mouse is close enough
        when clicking an object of the graph window,
        see \code{\link[tcltk]{tkcanvas}}. 
                  (\code{\link{dynamicGraphMain}})}
    \item{background}{ The color of the canvas of the graph window,
        default: "white". 
                  (\code{\link{dynamicGraphMain}})}
    \item{transformation}{ NULL,
                or rotation matrix for projecting the positions
        of the vertices, blocks, etc. onto the canvas. 
                  (\code{\link{dynamicGraphMain}})}
    \item{permitZoom}{ Logical. If FALSE then zooming is disabled,
		       and no margin around the canvas is set. 
                  (\code{\link{dynamicGraphMain}})}
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
       \code{factorEdgeColor} is similar to \code{edgeColor}. }
   \item{blockEdgeColor}{ "default", or list with two text strings for colors.
       \code{blockEdgeColor} is similar to \code{edgeColor}.
       The two colors are used for respectively edges
       between two blocks and for edges between blocks 
       and vertices. }
   \item{blockColors}{ List of colors of blocks. 
       Similar to \code{vertexColor}: Only used when creating
       new blocks, else the colors set in \code{blockList}
       and \code{blockTree} are used. }
   \item{extraEdgeColor}{ Single text string. 
       \code{extraEdgeColor} is similar to \code{edgeColor}. }
  \item{drawblocks}{ Logical. If \code{drawblocks} is set to FALSE,
        then the blocks are not drawn. The strata of the vertices are
        then not updated when the vertices are moved. 
        (\code{\link{simpleGraphToGraph}})}
  \item{right.to.left}{ Logical. If \code{right.to.left} is set to TRUE
        then the explanatory blocks are drawn to the right.
        See \code{\link{setBlocks}}.
        (\code{\link{simpleGraphToGraph}})}
  \item{nested.blocks}{ Logical. If \code{nested.blocks} is set to TRUE
        then the blocks are drawn nested. See \code{\link{setBlocks}}. 
        (\code{\link{simpleGraphToGraph}})}
  \item{overlaying}{ Logical. If \code{overlaying} is set to FALSE
        then children of a block are not drawn inside the block.
        See \code{\link{setTreeBlocks}}. 
        (\code{\link{simpleGraphToGraph}})}
  \item{fixedFactorPositions}{ Logical. If \code{fixedFactorPositions} is set
        to TRUE then the factor vertices will not follow the moved vertices.
        (\code{\link{simpleGraphToGraph}})}
  \item{diagonal}{ Logical. If \code{diagonal} is set to TRUE
        then the extra vertices are by default positioned along a diagonal. 
        (\code{\link{simpleGraphToGraph}})}
  \item{N}{ The number, \code{N} > 1, of coordinates for the positions of the
        vertices and block corners. 
        (\code{\link{simpleGraphToGraph}})}
   \item{vertexClasses}{ Returned value from \code{\link{validVertexClasses}},
       or extension of this matrix. Used when creating
       new vertices in \code{\link{dynamicGraphMain}}. }
   \item{factorClasses}{ Returned value from \code{\link{validFactorClasses}},
       or extension of this matrix. Used when creating
       new factor vertices in \code{\link{dynamicGraphMain}}. }
   \item{edgeClasses}{ Returned value from \code{\link{validEdgeClasses}},
       or extension of this matrix. Used when creating
       new edges in \code{\link{dynamicGraphMain}}. }
   \item{viewClasses}{ Returned value from \code{\link{validViewClasses}},
       or extension of this matrix. Used when creating
       new views in \code{\link{dynamicGraphMain}}. }
    \item{drawBlockFrame}{ Logical.
         If TRUE then frames are drawn around blocks. 
         (\code{\link{dynamicGraphMain}})}
    \item{drawBlockBackground}{ Logical. If TRUE then a \code{block canvas} is
         drawn, with color set by \code{blockColors}. 
         (\code{\link{dynamicGraphMain}})}
    \item{useNamesForLabels}{ Logical. If \code{useNamesForLabels} is TRUE
         then names are sued for labels. 
         (\code{\link{dynamicGraphMain}})}
    \item{namesOnEdges}{ Logical. If FALSE then the names of the vertices 
         are not set on the edge as label. 
         (\code{\link{dynamicGraphMain}})}
    \item{updateEdgeLabels}{ Logical.  If FALSE then the edge labels
         are not cleared when the model is updated. 
         (\code{\link{dynamicGraphMain}})}
    \item{enterLeaveUpdate}{ Logical. If FALSE then the graph window
         is not redrawn when the mouse enters and leaves the graph window. 
         (\code{\link{dynamicGraphMain}})}

    \item{UserMenus}{ List with user defined menu items for main menu
        and pop up menus. See \code{\link{dg.graphedges-class}}
        for an example of a user specified menu. 
         (\code{\link{dynamicGraphMain}})}
    \item{constrained}{ Logical. If \code{constrained} is then
			the vertices can not be dragged out of blocks. }
    \item{hasMethods}{ Logical. 
         If TRUE then the \code{object} should have the methods
         \code{\link{modifyModel}} and \code{\link{testEdge}}. 
         (I do not know why the R-function
         \code{\link[methods]{hasMethod}} does not
         work on \code{objects} (defined outside the package
         \code{\link{dynamicGraph}})
 	 inside \code{\link{dynamicGraphMain}}). 
         (\code{\link{dynamicGraphMain}})}

    \item{saveTkReferences}{ Logical, if \code{saveTkReferences} is TRUE
                             then references to Tk-variables
		             of the dynamic graph window are saved
                             in environments in the returned object 
                             of \code{\link{dynamicGraphMain}}
                             (if not \code{returnNull} is TRUE).
         (\code{\link{dynamicGraphMain}}) }

    \item{saveFunctions}{ Logical, if \code{saveFunctions} is TRUE
                          then draw and update functions of 
		          the dynamic graph window are saved
                          in environments in the returned object 
                          of \code{\link{dynamicGraphMain}}
		          (if not \code{returnNull} is TRUE).
         (\code{\link{dynamicGraphMain}}) }

    \item{returnNull}{ Logical, if \code{returnNull}
                       is TRUE then NULL is returned. 
         (\code{\link{dynamicGraphMain}})}

    \item{updateAllViews}{ Logical.
                If TRUE then all windows are updated when one is changed. 
         (\code{\link{dynamicGraphMain}})}

    \item{variableFrame}{ Logical.
                If \code{variableFrame} is TRUE the a frame/box/panel
		for variables is made left in the graph window.
         (\code{\link{dynamicGraphMain}})}

    \item{debug.strata}{ Logical for tracing the strata of the vertices
                (also in plot). 
         (\code{\link{dynamicGraphMain}})}
    \item{debug.edges}{ Logical for tracing edges (also by labels in plot). 
         (\code{\link{dynamicGraphMain}})}
    \item{debug.position}{ Logical for tracing positions of the vertices. 
         (\code{\link{dynamicGraphMain}})}
    \item{debug.update}{ Logical for tracing redrawing of the graph window. 
         (\code{\link{dynamicGraphMain}})}

   \item{\dots}{ Additional deprecated arguments, e.g., \code{returnLink}. }
}

% \details{}

\value{A 'list' with components with meanings as explained under 'Arguments'.}

% \references{ }

\author{Jens Henrik Badsberg}

\note{ The arguments of \code{\link{dg.control}} can also be given to
       \code{\link{dynamicGraphMain}} and \code{\link{DynamicGraph}}
       (for backward compatibility). 
       But if the argument \code{control} is used for
       \code{\link{dynamicGraphMain}} or \code{\link{DynamicGraph}}
       then these arguments are ignored. }

\seealso{\code{\link{dynamicGraphMain}} and \code{\link{DynamicGraph}}}

\examples{
require(dynamicGraph)
str(dg.control())
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
