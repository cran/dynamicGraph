\name{NodeProto-class}
\docType{class}
\alias{NodeProto-class}
\alias{addToPopups,NodeProto-method}
\alias{color<-,NodeProto-method}
\alias{color,NodeProto-method}
\alias{label<-,NodeProto-method}
\alias{label,NodeProto-method}
\alias{labelPosition<-,NodeProto-method}
\alias{labelPosition,NodeProto-method}

\title{Class "NodeProto" }
\description{  A skeleton class for the classes of vertices, edges and block objects. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NodeProto", ...)}.
The objects has the slots and methods relevant to vertices, edges and blocks.
}
\section{Slots}{
  \describe{
    \item{\code{color}:}{Object of class \code{"character"}
        with the \code{color} of the object. }
    \item{\code{label}:}{Object of class \code{"character"}
        with the \code{label} of the object. }
    \item{\code{label.position}:}{Object of class \code{"numeric"}
        with the \code{label.positions} of the object. }
  }
}
\section{Methods}{
  \describe{
    \item{addToPopups}{\code{signature(object = "NodeProto")}: 
        Add items to the pop up menu (\code{nodePopupMenu}) of the object
        by \code{tkadd}.
    }
    \item{color<-}{\code{signature(x = "NodeProto")}:
        Set the \code{color} of the object. }
    \item{color}{\code{signature(object = "NodeProto")}:
        Return the \code{color} of the object. }
    \item{label<-}{\code{signature(x = "NodeProto")}:
        Set the \code{label} of the object. }
    \item{label}{\code{signature(object = "NodeProto")}:
        Return the \code{label} of the object. }
    \item{labelPosition<-}{\code{signature(x = "NodeProto")}:
        Set the \code{label.position} of the object. }
    \item{labelPosition}{\code{signature(object = "NodeProto")}:
        Return the \code{label.position} of the object. }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{VertexProto-class}},
\code{\link{EdgeProto-class}},
\code{\link{BlockProto-class}}.
}
% \examples{ }
\keyword{classes}
