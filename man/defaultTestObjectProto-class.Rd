\name{defaultTestObjectProto-class}
\docType{class}
\alias{defaultTestObjectProto-class}
\alias{label,defaultTestObjectProto-method}
\alias{width,defaultTestObjectProto-method}
% \alias{labelOfTest,defaultTestObjectProto-method}
% \alias{widthOfTest,defaultTestObjectProto-method}
% \alias{testLabel,defaultTestObjectProto-method}
% \alias{testWidth,defaultTestObjectProto-method}

\title{Class "defaultTestObjectProto" }
\description{  An example class for the test object for the model object of dynamicGraph. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("defaultTestObjectProto", ...)}.

    The methods 
    \code{label} and \code{width} % (was \code{labelOfTest} and \code{widthOfTest})
    should be implemented by you for your test object returned by the method
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
    \item{label}{\code{signature(object = "defaultTestObjectProto")}: 
       Return the \code{label} of the test. }
    \item{width}{\code{signature(object = "defaultTestObjectProto")}:
       Return the \code{width} of the test. }
%     \item{labelOfTest}{\code{signature(object = "defaultTestObjectProto")}: 
%        Will be removed.  Same as the method \code{label}. 
%        Return the \code{label} of the test. }
%     \item{widthOfTest}{\code{signature(object = "defaultTestObjectProto")}: 
%        Will be removed.  Same as the method \code{width}. 
%        Return the \code{width} of the test. }
%     \item{testLabel}{\code{signature(object = "defaultTestObjectProto")}: 
%        Will be removed.  Same as \code{labelOfTest}. }
%     \item{testWidth}{\code{signature(object = "defaultTestObjectProto")}: 
%        Will be removed.  Same as \code{labelOfTest}. }
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
