\name{blockTreeToList}

\alias{blockTreeToList}

\title{Extract the list of blocks from a block tree}

\description{Returns the list of blocks from a block tree.}

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
Labels(blockTreeToList(blocktree$BlockTree))
}

\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
