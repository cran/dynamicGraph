\name{defaultTestObjectProto-class}
\docType{class}
\alias{defaultTestObjectProto-class}
\alias{labelOfTest,defaultTestObjectProto-method}
\alias{widthOfTest,defaultTestObjectProto-method}
\alias{testLabel,defaultTestObjectProto-method}
\alias{testWidth,defaultTestObjectProto-method}

\title{Class "defaultTestObjectProto" ~~~ }
\description{  An example class for the test object for the model object of dynamicGraph. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("defaultTestObjectProto", ...)}.

    The methods \code{labelOfTest} and \code{widthOfTest} should be
    implemented by you for your test object returned by the method
    \code{testEdge} of your model \code{object}. 
}
\section{Slots}{
  \describe{
    \item{\code{deviance}:}{Object of class \code{"numeric"}: 
        The \code{deviance} of the test. }
    \item{\code{df}:}{Object of class \code{"numeric"}: 
        The \code{df} of the test. }
    \item{\code{p}:}{Object of class \code{"numeric"}: 
        The \code{p}-value of the test. }
  }
}
\section{Methods}{
  \describe{
    \item{labelOfTest}{\code{signature(object = "defaultTestObjectProto")}: 
       Same as the method \code{label}. 
       Return the \code{label} of the test. }
    \item{widthOfTest}{\code{signature(object = "defaultTestObjectProto")}: 
       Same as the method \code{label}. 
       Return the \code{label} of the test. }
    \item{testLabel}{\code{signature(object = "defaultTestObjectProto")}: 
       Same as \code{labelOfTest}. }
    \item{testWidth}{\code{signature(object = "defaultTestObjectProto")}: 
       Same as \code{labelOfTest}. }
  }
}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{
\code{\link{defaultModelObjectProto-class}},
\code{\link{newDefaultModelObject}},
\code{\link{newDefaultTestObject}}.
}
% \examples{ }
\keyword{classes}
