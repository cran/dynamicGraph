\name{defaultModelObjectProto-class}
\docType{class}
\alias{defaultModelObjectProto-class}
\alias{modifyModel,defaultModelObjectProto-method}
\alias{testEdge,defaultModelObjectProto-method}

\title{Class "defaultModelObjectProto" }
\description{  An example class for the model object of dynamicGraph. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("defaultModelObjectProto", ...)}.

This is an example of the object for interface between
\code{\link{dynamicGraphMain}} and your models.
The model object of the call of \code{\link{dynamicGraphMain}}
should have the methods \code{modifyModel} and \code{testEdge}.
When the graph is modified, by adding or dropping vertices or edge,
the method \code{modifyModel} is called on the argument \code{object}
of \code{\link{dynamicGraphMain}}. If an \code{object} is returned in
the list of the returned value from \code{modifyModel} then
\code{object} in \code{\link{dynamicGraphMain}} is replaced by this
object, and the object is also assigned in the top level environment,
if  \code{objectName} was given to \code{\link{dynamicGraphMain}}.

The methods \code{testEdge} of \code{object} should return an object with the methods 
\code{label} and \code{width} % (was \code{labelOfTest} and \code{widthOfTest})
for labeling edges, see \code{\link{newDefaultTestObject}}.

}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}:
      The \code{name} of the model. }
  }
}
\section{Methods}{
  \describe{
    \item{modifyModel}{\code{signature(object = "defaultModelObjectProto")}: ... }
    \item{testEdge}{\code{signature(object = "defaultModelObjectProto")}: ... }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{defaultTestObjectProto-class}},
\code{\link{newDefaultModelObject}},
\code{\link{newDefaultTestObject}}.
}
% \examples{ }
\keyword{classes}
