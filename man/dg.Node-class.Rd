\name{dg.Node-class}

\docType{class}

\alias{dg.Node-class}
\alias{addToPopups,dg.Node-method}
\alias{color<-,dg.Node-method}
\alias{color,dg.Node-method}
\alias{label<-,dg.Node-method}
\alias{label,dg.Node-method}
\alias{labelPosition<-,dg.Node-method}
\alias{labelPosition,dg.Node-method}
\alias{propertyDialog,dg.Node-method}

\title{Class "dg.Node" }

\description{  A skeleton class for the classes of vertices,
 edges and block objects. }

\section{Objects from the Class}{
The objects has the slots and methods relevant to vertices, edges and blocks.

% Objects can be created by calls of the form \code{new("dg.Node", ...)}.
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
    \item{addToPopups}{\code{signature(object = "dg.Node")}: 
        Add items to the pop up menu (\code{nodePop}\-\code{upMenu})
        of the object by \code{tkadd}.
    }
    \item{color<-}{\code{signature(x = "dg.Node")}:
        Set the \code{color} of the object. }
    \item{color}{\code{signature(object = "dg.Node")}:
        Return the \code{color} of the object. }
    \item{label<-}{\code{signature(x = "dg.Node")}:
        Set the \code{label} of the object. }
    \item{label}{\code{signature(object = "dg.Node")}:
        Return the \code{label} of the object. }
    \item{labelPosition<-}{\code{signature(x = "dg.Node")}:
        Set the \code{label.position} of the object. }
    \item{labelPosition}{\code{signature(object = "dg.Node")}:
        Return the \code{label.position} of the object. }
    \item{propertyDialog}{\code{signature(object = "dg.Node")}: 
        Open a Tk/tcl-window with the slots of the node. }
  }
}

\author{Jens Henrik Badsberg}

\seealso{
\code{\link{dg.Vertex-class}},
\code{\link{dg.Edge-class}},
\code{\link{dg.Block-class}}.
}

\keyword{classes}
