\name{BlockProto-class}
\docType{class}
\alias{BlockProto-class}
\alias{ancestors<-,BlockProto-method}
\alias{ancestors,BlockProto-method}
\alias{descendants<-,BlockProto-method}
\alias{descendants,BlockProto-method}
\alias{draw,BlockProto-method}
\alias{index<-,BlockProto-method}
\alias{index,BlockProto-method}
\alias{name,BlockProto-method}
\alias{position<-,BlockProto-method}
\alias{position,BlockProto-method}
\alias{stratum<-,BlockProto-method}
\alias{stratum,BlockProto-method}
\alias{visible<-,BlockProto-method}
\alias{visible,BlockProto-method}

\title{Class "BlockProto" ~~~ }
\description{  The class for blocks. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BlockProto", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{stratum}:}{Object of class \code{"numeric"},
       the \code{stratum} of the block. }
    \item{\code{index}:}{Object of class \code{"numeric"} 
       with the \code{index} of the block,
       the position of the block in a block list. }
    \item{\code{ancestors}:}{Object of class \code{"numeric"}.
       Integer vector for the \code{ancestor} blocks of the block. }
    \item{\code{descendants}:}{Object of class \code{"numeric"}.
       Integer vector for the \code{descendants} blocks of the block. 
       The slot with the \code{descendant} blocks is set after the creation of the block. }
    \item{\code{position}:}{Object of class \code{"matrix"},
       the \code{position} of the block, the two diagonal opposite corners. }
    \item{\code{visible}:}{Object of class \code{"logical"},
       if TRUE then the block is drawn. }
    \item{\code{color}:}{Object of class \code{"character"}, see\code{"NodeProto"}. }
    \item{\code{label}:}{Object of class \code{"character"}, see\code{"NodeProto"}. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}, see\code{"NodeProto"}. } 
  }
}
\section{Extends}{
Class \code{"NodeProto"}, directly.
}
\section{Methods}{
  \describe{
    \item{ancestors<-}{\code{signature(x = "BlockProto")}: ... }
    \item{ancestors}{\code{signature(object = "BlockProto")}: ... }
    \item{descendants<-}{\code{signature(x = "BlockProto")}: ... }
    \item{descendants}{\code{signature(object = "BlockProto")}: ... }
    \item{draw}{\code{signature(object = "BlockProto")}: ... }
    \item{index<-}{\code{signature(x = "BlockProto")}: ... }
    \item{index}{\code{signature(object = "BlockProto")}: ... }
    \item{name}{\code{signature(object = "BlockProto")}: ... }
    \item{position<-}{\code{signature(x = "BlockProto")}: ... }
    \item{position}{\code{signature(object = "BlockProto")}: ... }
    \item{stratum<-}{\code{signature(x = "BlockProto")}: ... }
    \item{stratum}{\code{signature(object = "BlockProto")}: ... }
    \item{visible<-}{\code{signature(x = "BlockProto")}: ... }
    \item{visible}{\code{signature(object = "BlockProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{newBlock}},
\code{\link{setBlocks}},
\code{\link{setTreeBlocks}},
\code{\link{NodeProto-class}}.
}
% \examples{ }
\keyword{classes}
