\name{setTreeBlocks}
\alias{setTreeBlocks}
\alias{NodeAncestors}
\alias{NodeAncestors<-}
\alias{NodeDescendants}
\alias{NodeDescendants<-}
\title{The block tree}
\description{
    Create a block tree with positioning the vertices in to blocks.
}
\usage{
setTreeBlocks(block.tree, vertices, root.label = "", N = 3,
              delta = ifelse(overlaying, 1, 0), Delta = ifelse(overlaying, 0, 1.5),
              d = 5, f = 1/4, blockColors = NULL, overlaying = FALSE)
}

%  \itemize{
     % \item{...$Vertices}{ ~~Describe \code{Vertices} here~~ }
     % \item{...$label}{ ~~Describe \code{label} here~~ }
     % \item{...$d}{ ~~Describe \code{d} here~~ }
     % \item{...$g}{ ~~Describe \code{g} here~~ }
     % \item{...$f}{ ~~Describe \code{f} here~~ }
     % \item{...$G}{ ~~Describe \code{G} here~~ }
     % \item{...$F}{ ~~Describe \code{F} here~~ }
     % \item{...$horizontal}{ ~~Describe \code{horizontal} here~~ }
     % \item{...$vertices.last}{ ~~Describe \code{vertices.last} here~~ }
     % \item{...$"block-name" = list(...) }{
     %                ~~Describe \code{"block-name" = list(...)} here~~ }
     % \item{...$common.children}{ ~~Describe \code{common.children}
     %                              here~~ }
%          }

\arguments{
  \item{block.tree}{ A structure with the blocks in a \code{block.tree}.
	See below. }
  \item{vertices}{ The list of \code{vertices}, each
      containing the class \code{VertexProto}. Returned with positions
      set in the interval of the blocks. }
  \item{root.label}{ A text string with the \code{root.label}
      of the root block. }
  \item{N}{ Integer, \code{N} is the number of coordinates of the
      vertices and block corners. }
  \item{delta}{ Numeric. Decrement of block size for nested blocks,
	        and space between blocks when \code{overlaying} is TRUE.
                The decrement is  \code{delta} divided by 100 times
                the the size of the window canvas, width or height. } 
  \item{Delta}{ Numeric. Decrement of block size for nested blocks,
	        and space between blocks when \code{overlaying} is FALSE. 
                The decrement is \code{Delta} divided by 100 times
                the the size of the window canvas, width or height. }
  \item{d}{ Numeric. If not \code{d} is given in \code{block.tree}, see below:
            The heading bar (with the label) has a height of
            (\code{d} + 2) divided by 100,
            times height of the window canvas. }
  \item{f}{ Numeric. If not \code{f} or \code{g} is given in \code{block.tree},
            see below:
            The the vertices of the block are placed in an array with a height
            (width if \code{horizontal} is set to FALSE)
            of \code{f} divided by 100,
            times height (width) of the block. 
            Thus this size is relative to the block size. }
  \item{blockColors}{ Vector of text string with the \code{blockColors}
        of the blocks. }
  \item{overlaying}{ Logical. If \code{overlaying} is set to FALSE
        then children blocks of a block are not drawn inside the block. }
}
\details{ 

  A recursive definition:
  \code{Block.tree} is a list with the vertices of the "current" blocks,
  some parameters for controlling the layout,
  and possible some \code{block.trees}:

  \itemize{
      \item{\code{...\$Vertices}}{ The \code{vertices} of the block. }
      \item{\code{...\$label}}{ A text string for
            the \code{label} of the block. 
	    Will overwrite \code{"block-name"} and  \code{root.label}. }
      \item{\code{...\$d}}{ Numeric.
            The heading bar (with the label) has a height of
            (\code{d} + 2) divided by 100,
            times the height of the window canvas. }
      \item{\code{...\$g}}{ Numeric.
            The vertices of the block are placed in an array with a height 
            (width if \code{horizontal} is set to FALSE)
            of \code{g} divided by 100,
            times the height (width) of the window canvas. 
            Thus this size will not decrease with the block size.
            }
      \item{\code{...\$f}}{ Numeric.
            If not \code{g} is given:
            The the vertices of the block are placed in an array with a height
            (width if \code{horizontal} is set to FALSE)
            of \code{f} divided by 100,
            times the height (width) of the block. 
            Thus this size is relative to the block size. }
      \item{\code{...\$G}}{ Numeric.
            (If the height of the block is 100 we are now left with 
             \code{100 - 2 * delta - d - 2 - g} for the blocks. )
            The sub blocks (apart from \code{common.children})
            then have a of height 
            (width, if \code{horizontal} is set to FALSE)
            of \code{G} divided by 100,
            times the height (width) of the window canvas. 
            Thus the sub block size will not decrease with the block size.
            (If the height of the block is 100 we are now left with 
             \code{100 - 2 * delta - d - 2 - g - G}
             for the \code{common.children}. )
            }
      \item{\code{...\$F}}{ Numeric.
            If not \code{G} is given:
            The proportion \code{G} of the remaining space 
            are used for sub blocks (apart from \code{common.children})
            and the proportion \code{G} of the space for blocks
            are used for \code{common.children}.
            }
      \item{\code{...\$horizontal}}{ Logical.
            If \code{horizontal} is set to TRUE,
            then the sub blocks, but \code{common.children},
            are placed side by side,
	    else the blocks are placed vertically. }
      \item{\code{...\$vertices.last}}{  Logical. If \code{vertices.last} then
	    the vertices of the block are placed after the sub blocks. }
      \item{\code{...\$"block-name"}}{\code{= list(...)}
                    Repeated zero, one or more times for sub blocks.
                    \code{"block-name"} is the label of the block,
                    and \code{list(...)} is a \code{Block.tree}. }
      \item{\code{...\$common.children}}{\code{= list(...)}
                    Omitted, or a list with common children
                    of the other sub blocks of the block. 
                    The list is again a \code{Block.tree}. }
  }
}
\value{
  A list with components
  \item{BlockTree}{A tree of blocks, each of class \code{\link{BlockProto}}. }
  \item{Vertices}{The list of vertices,
        with the positions updated such that the vertices
        has positions within the blocks. }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
\note{ 

      Ancestors and descendants are set in \code{\link{setTreeBlocks}}.
      Ancestors are used in the function \code{\link{returnBlockEdgeList}} to
      find the edges between blocks and between blocks and vertices.
      Descendants are used in \code{\link{dynamicGraphMain}}
      when closing, opening and moving blocks,
      and when adding or dropping edges from and to blocks.
      The methods \code{\link{NodeAncestors}}, \code{\link{NodeAncestors<-}}
      \code{\link{NodeDescendants}} and \code{\link{NodeDescendants<-}}
      can be used on the block list resulting of 
      \code{\link{blockTreeToList}} on the block tree.

}
% \seealso{ ~~objects to See Also as \code{\link{dynamicGraphMain}}, ~~~ }
\examples{
# Example 1:

Block.tree <- list(label = "W", Vertices = c("country"),
                   X = list(Vertices = c("race", "sex"),
                            A = list(Vertices = c("hair", "eye"),
                                     horizontal = FALSE),
                            B = list(Vertices = c("education"),
                                     C = list(Vertices = c("age")))))
V.Names <- unlist(Block.tree)
vertices <- returnVertexList(V.Names[grep("Vertices", names(V.Names))])
blocktree <- setTreeBlocks(Block.tree, vertices)

Positions(blockTreeToList(blocktree$BlockTree))
Positions(blocktree$Vertices)
NodeAncestors(blockTreeToList(blocktree$BlockTree))
NodeDescendants(blockTreeToList(blocktree$BlockTree))

vertexStrata <- Strata(blocktree$Vertices)
vertexStrata
vertexNames <- Names(blocktree$Vertices)
names(vertexNames) <- NULL
vertexNames

# Indices of the vertices in blocks:

indicesInBlock <- vector("list", max(vertexStrata))
for (i in seq(along = vertexStrata))
  indicesInBlock[[vertexStrata[i]]] <- 
    append(indicesInBlock[[vertexStrata[i]]], i) 
str(indicesInBlock)

# Names of the vertices in blocks:

vertexNamesInblock <- vector("list", max(vertexStrata))
for (i in seq(along = vertexStrata))
  vertexNamesInblock[[vertexStrata[i]]] <- 
    append(vertexNamesInblock[[vertexStrata[i]]], vertexNames[i]) 
str(vertexNamesInblock)

# A useful function, replace "k" (block index k) 
# in block "i" by "x[k]", the content of block k:

f <- function(A, x) {
  result <- vector("list", length(A))
  names(result) <- names(A)
  for (i in seq(along = A))
    if ((length(A[[i]]) > 0) && (A[[i]] != 0))
      for (k in A[[i]])
        result[[i]] <- append(result[[i]], x[k])
  return(result)
}

# For each block, names of vertices in ancestor blocks:

vertexAncOfBlock <- f(NodeAncestors(blockTreeToList(blocktree$BlockTree)), 
                      vertexNamesInblock)
str(vertexAncOfBlock)

for (i in seq(along = vertexAncOfBlock))
  if (length(vertexAncOfBlock[[i]]) > 0)
    vertexAncOfBlock[[i]] <- unlist(vertexAncOfBlock[[i]])
str(vertexAncOfBlock)

# For each block, names of vertices in descendant blocks:

vertexDesOfBlock <- f(NodeDescendants(blockTreeToList(blocktree$BlockTree)),
                      vertexNamesInblock)
str(vertexDesOfBlock)

for (i in seq(along = vertexDesOfBlock))
  if (length(vertexDesOfBlock[[i]]) > 0)
    vertexDesOfBlock[[i]] <- unlist(vertexDesOfBlock[[i]])
str(vertexDesOfBlock)


# Example 2:

Block.tree <-
  list(g = 0, G = 54, label = "Pedegree.G",
       Male.Side = 
       list(g = 0, G = 33,
            Father = 
            list(g = 0, G = 12,
                 P.G.Father = list(Vertices = c("P.G.Father.1")),
                 P.G.Mother = list(Vertices = c("P.G.Mother.1")),
                 common.children = list(g = 0, label = "Father.1",
                                        Vertices = c("Father.1"))),
            Mother = 
            list(g = 0, G = 12,
                 M.G.Father = list(Vertices = c("M.G.Father.1")),
                 M.G.Mother = list(Vertices = c("M.G.Mother.1")),
                 common.children = list(g = 0, label = "Mother.1",
                                        Vertices = c("Mother.1"))),
            common.children = list(g = 2, Vertices = c("Male"))),
  Female.Side = list(g = 0, G = 12,
    P.G.Father = list(Vertices = c("P.G.Father.2")),
    P.G.Mother = list(Vertices = c("P.G.Mother.2")),
    M.G.Father = list(Vertices = c("M.G.Father.2")),
    M.G.Mother = list(Vertices = c("M.G.Mother.2")),
    common.children = list(g = 0, G = 12, label = "Female",
      Father = list(Vertices = c("Father.2")),
      Mother = list(Vertices = c("Mother.2")),
      common.children = list(g = 2, Vertices = c("Female")))),
  common.children = list(Vertices = c("Marriage"), g = 3, label = "Children",
    Son = list(Vertices = c("Son"), g = 3, 
       P.G.Son = list(Vertices = c("P.G.Son"), g = 2),
       P.G.Dat = list(Vertices = c("P.G.Dat"), g = 1)),
    Dat = list(Vertices = c("Dat"), g = 2,
       M.G.Son = list(Vertices = c("M.G.Son")),
       M.G.Dat = list(Vertices = c("M.G.Dat")))
    )
)

v <- unlist(Block.tree)
V.Names <- v[grep("Vertices", names(v))]
rm(v)

FromTo <- matrix(c("P.G.Father.1", "Father.1", "P.G.Father.2", "Father.2", 
                   "P.G.Mother.1", "Father.1", "P.G.Mother.2", "Father.2", 
                   "M.G.Father.1", "Mother.1", "M.G.Father.2", "Mother.2", 
                   "M.G.Mother.1", "Mother.1", "M.G.Mother.2", "Mother.2", 
                   "Father.1",     "Male",     "Father.2",     "Female",   
                   "Mother.1",     "Male",     "Mother.2",     "Female",   
                   "Male",         "Marriage", "Female",       "Marriage",
                   "Marriage",     "Son",      "Marriage",     "Dat",
                   "Son",          "P.G.Son",  "Dat",          "M.G.Son", 
                   "Son",          "P.G.Dat",  "Dat",          "M.G.Dat"), 
                   byrow = TRUE, ncol = 2)

From <- match(FromTo[,1], V.Names)
To   <- match(FromTo[,2], V.Names)

V.Types <- rep("Discrete", length(V.Names))

Z <- DynamicGraph(V.Names, V.Types, From, To, block.tree = Block.tree, 
                  object = Object, width = 600, height = 600, 
                  drawblocks = TRUE, drawBlockFrame = TRUE, 
                  overlaying = TRUE, title = "Pedegree.G")

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
