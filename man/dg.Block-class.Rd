\name{dg.Block-class}

\docType{class}

\alias{dg.Block-class}
\alias{closed<-,dg.Block-method}
\alias{closed,dg.Block-method}
\alias{draw,dg.Block-method}
\alias{index<-,dg.Block-method}
\alias{index,dg.Block-method}
\alias{initialize,dg.Block-method}
\alias{name,dg.Block-method}
\alias{position<-,dg.Block-method}
\alias{position,dg.Block-method}
\alias{stratum<-,dg.Block-method}
\alias{stratum,dg.Block-method}
\alias{visible<-,dg.Block-method}
\alias{visible,dg.Block-method}

\alias{parent<-,dg.Block-method}
\alias{parent,dg.Block-method}
\alias{children<-,dg.Block-method}
\alias{children,dg.Block-method}
\alias{ancestors<-,dg.Block-method}
\alias{ancestors,dg.Block-method}
\alias{descendants<-,dg.Block-method}
\alias{descendants,dg.Block-method}

\alias{newBlock}

\alias{dg.Block}
\alias{closed}
\alias{closed<-}

\alias{parent<-}
\alias{parent}
\alias{children<-}
\alias{children}
\alias{ancestors}
\alias{ancestors<-}
\alias{descendants}
\alias{descendants<-}

\title{Class "dg.Block" }

\description{  The class for the blocks. }

% \section{Objects from the Class}{
% Objects can be created by calls of the form \code{new("dg.Block", ...)}.
% }

% \usage{
% newBlock(stratum = 0, index = 0, position = matrix(rep(0, 6), ncol = 3),
%          closed = FALSE, visible = TRUE, color = "Grey", label = "Label",
%          label.position = c(0, 0, 0), ancestors = NULL, parent = NULL)
% }

% \arguments{
%   \item{stratum}{ Integer for the \code{stratum} of the block. 
% 	% The block is identified by this stratum, and thus the
%         % strata of the blocks are different and unique.
%         % Each vertex can only belong to one block,
% 	% and thus has assigned one stratum.
%         }
%   \item{index}{ Integer for the \code{index} of the block. }
%   \item{position}{ Numeric vector for the \code{position} of the block,
%       the two diagonal opposite corners. }
%   \item{closed}{ Logical, if TRUE then the block is drawn as a "vertex". }
%   \item{visible}{ Logical, if TRUE then the block is drawn. }
%   \item{color}{ Text string for the \code{color} of the block. }
%   \item{label}{ Text string for the \code{label} of the block. }
%   \item{label.position}{ Numeric vector for the \code{label.position} 
%                          of the block label. }
%   \item{parent}{ Integer for the \code{index} of the parent block. 
%                  The slots \code{children}, \code{ancestors}, 
%                  and \code{descendants} are computed from \code{parent}. 
%                  When conflicts between these four slots in
%                  \code{\link{dynamicGraphMain}} the tree other slots are
%                  computed from \code{parent}. }
%   \item{ancestors}{ Integer vector for the \code{ancestor} blocks of the block. 
%                     The slots with the \code{children} and \code{descendant}
%                     blocks is set after the creation of the block
%                     (in \code{\link{setTreeBlocks}}). }
% }

\section{Slots}{
  \describe{
    \item{\code{stratum}:}{Object of class \code{"numeric"},
       the \code{stratum} of the block. }
    \item{\code{index}:}{Object of class \code{"numeric"} 
       with (minus) the \code{index} of the block,
       the position of the block in a block list. }
    \item{\code{parent}:}{Object of class \code{"numeric"}
        with the single parent of the block.
        The slots \code{children}, \code{ancestors}, 
        and \code{descendants} are computed from \code{parent}. 
        When conflicts between these four slots in
        \code{\link{dynamicGraphMain}}
        the tree other slots are computed from \code{parent}. }
    \item{\code{children}:}{Object of class \code{"numeric"} 
       Integer vector for the \code{children} blocks of the block. }
    \item{\code{ancestors}:}{Object of class \code{"numeric"}.
       Integer vector for the \code{ancestor} blocks of the block.
       The slots with the \code{children} and \code{descendant} blocks is set
       after the creation of the block (in \code{\link{setTreeBlocks}}). }
    \item{\code{descendants}:}{Object of class \code{"numeric"}.
       Integer vector for the \code{descendants} blocks of the block. 
       The slot with the \code{descendant} blocks
       is set after the creation of the block. }
    \item{\code{position}:}{Object of class \code{"matrix"},
       the \code{position} of the block, the two diagonal opposite corners. }
    \item{\code{closed}:}{Object of class \code{"logical"},
       if TRUE then the block is closed, e.i. drawn as a "vertex". }
    \item{\code{visible}:}{Object of class \code{"logical"},
       if TRUE then the block is drawn, else it is hidden in a closed block. }
    \item{\code{color}:}{Object of class \code{"character"},
       see \code{"dg.Node"}. }
    \item{\code{label}:}{Object of class \code{"character"},
       see \code{"dg.Node"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"},
       see \code{"dg.Node"}. } 
  }
}

\section{Extends}{
Class \code{"dg.Node"}, directly.
}

\section{Methods}{
  \describe{
    \item{ancestors<-}{\code{signature(x = "dg.Block")}: ... }
    \item{ancestors}{\code{signature(object = "dg.Block")}: ... }
    \item{closed<-}{\code{signature(x = "dg.Block")}: ... }
    \item{closed}{\code{signature(object = "dg.Block")}: ... }
    \item{children<-}{\code{signature(x = "dg.Block")}: ... }
    \item{children}{\code{signature(object = "dg.Block")}: ... }
    \item{descendants<-}{\code{signature(x = "dg.Block")}: ... }
    \item{descendants}{\code{signature(object = "dg.Block")}: ... }
    \item{draw}{\code{signature(object = "dg.Block")}:
       Method for drawing the closed block. }
    \item{index<-}{\code{signature(x = "dg.Block")}: ... }
    \item{index}{\code{signature(object = "dg.Block")}: ... }
    \item{initialize}{\code{signature(.Object = "dg.Block")}: ... }
    \item{name}{\code{signature(object = "dg.Block")}: 
       Extract the label of the block. }
    \item{parent<-}{\code{signature(x = "dg.Block")}: ... }
    \item{parent}{\code{signature(object = "dg.Block")}: ... }
    \item{position<-}{\code{signature(x = "dg.Block")}: ... }
    \item{position}{\code{signature(object = "dg.Block")}: ... }
    \item{stratum<-}{\code{signature(x = "dg.Block")}: ... }
    \item{stratum}{\code{signature(object = "dg.Block")}: ... }
    \item{visible<-}{\code{signature(x = "dg.Block")}: ... }
    \item{visible}{\code{signature(object = "dg.Block")}: ... }
    % \item{propertyDialog}{\code{signature(object = "dg.Block")}: ... }
    % \item{addToPopups}{\code{signature(object = "dg.Block")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
% \code{\link{newBlock}},
\code{\link{setBlocks}},
\code{\link{setTreeBlocks}},
\code{\link{dg.Node-class}}.
}

\note{
    The \code{dg.Block} class has the methods
    \code{\link{name}}, \code{\link{label}}, \code{\link{labelPosition}},
    \code{\link{position}}, \code{\link{color}}, \code{\link{stratum}},
    \code{\link{index}}, \code{\link{visible}}, \code{\link{closed}},
    \code{\link{parent}}, \code{\link{children}},
    \code{\link{ancestors}}, and \code{\link{descendants}},
    for extracting values
    and the replacement methods
    % \code{\link{name<-}}, 
    \code{\link{label<-}}, \code{\link{labelPosition<-}},
    \code{\link{position<-}}, \code{\link{color<-}}, \code{\link{stratum<-}},
    \code{\link{index<-}}, \code{\link{visible<-}}, \code{\link{closed<-}},
    \code{\link{parent<-}}, \code{\link{children<-}},
    \code{\link{ancestors<-}}, and \code{\link{descendants<-}}.
    Items are added to the pop up menu of a block
    by the method \code{\link{addToPopups}}.

    The methods \code{\link{draw}},
    and \code{\link{propertyDialog}} is also avaliable.

    % I have not yet had any use of a block name.
    % Thus this slot is not available for the block,
    % as neither for edges.
}

% \seealso{\code{\link{dg.Vertex-class}}}

% # "newBlock" <-
% # function (stratum = 0, index = 0, position = matrix(rep(0, 6), 
% #     ncol = 3), closed = FALSE, visible = TRUE, color = "Grey", 
% #     label = "Label", label.position = c(0, 0, 0), ancestors = NULL, 
% #     parent = NULL) 
% # {
% #     result <- new("dg.Block", stratum = stratum, index = index, 
% #         position = position, closed = closed, visible = visible, 
% #         color = color, label = label, label.position = label.position, 
% #         ancestors = if (is.null(ancestors)) 
% #             0
% #         else ancestors, parent = if (is.null(parent)) 
% #             if (is.null(ancestors)) 
% #                 0
% #             else ancestors[length(ancestors)]
% #         else parent)
% #     return(result)
% # }

% # b <- newBlock()

\examples{

b <- new("dg.Block")

str(b)

color(b)
label(b)
labelPosition(b)
name(b)
index(b)
position(b)
stratum(b)
ancestors(b)
descendants(b)
visible(b)

color(b) <- "grey"
label(b) <- "NameAndLabel"
labelPosition(b) <- c(1, 2, 3)
# name(b) <- "NameAndLabel" # Not possible!!!
index(b) <- 3
position(b) <- matrix(c( 10,  20,  30,  40, 
                        110, 120, 130, 140), byrow = TRUE, ncol = 4)
stratum(b) <- 1
ancestors(b) <- c(1, 2)
descendants(b) <- c(4, 5)
visible(b) <- FALSE

str(b)
}

\keyword{classes}
