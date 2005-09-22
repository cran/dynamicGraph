\name{blockTreeToList}

\alias{blockTreeToList}

\title{Extract the list of blocks from a block tree}

\description{Returns the list of blocks from a block tree.
   The block tree is an (intermediate) result of the
   function \code{\link{setTreeBlocks}}.
   \code{\link{setTreeBlocks}} is used in \code{\link{simpleGraphToGraph}}
   on the slot \code{block.tree} of the object of class
   \code{\link{dg.simple.graph-class}}.
   
 }

\usage{blockTreeToList(tree)}

\arguments{
  \item{tree}{ The \code{BlockTree} part of the result of
               \code{\link{setTreeBlocks}}. }
}

\value{A list of blocks, each block of class \code{dg.Block}.}

\author{Jens Henrik Badsberg}

\examples{
Block.tree <- list(label = "W", Vertices = c("contry"),
                   X = list(Vertices = c("sex", "race"),
                            A = list(Vertices = c("hair", "eye"),
                                     horizontal = FALSE),
                            B = list(Vertices = c("age"),
                                     C = list(Vertices = c("education")))))
Names <- unlist(Block.tree)
Names <- Names[grep("Vertices", names(Names))]
Types <- rep("Discrete", length(Names))
vertices <- returnVertexList(Names, types = Types)
blocktree <- setTreeBlocks(Block.tree, vertices)
blocklist <- blockTreeToList(blocktree$BlockTree)
Labels(blocklist)
str(Parents(blocklist))
str(Children(blocklist))
str(NodeAncestors(blocklist))
str(NodeDescendants(blocklist))
parent(blocklist[[5]])
children(blocklist[[1]])
ancestors(blocklist[[5]])
descendants(blocklist[[1]])
parent(blocklist[[3]]) <- 4
children(blocklist[[2]])
checkBlockList(blocklist)
}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
