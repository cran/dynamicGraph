\name{dg.Test-class}

\docType{class}

\alias{dg.Test-class}
\alias{label,dg.Test-method}
\alias{width,dg.Test-method}
\alias{initialize,dg.Test-method}
\alias{setSlots,dg.Test-method}

\alias{newDefaultTestObject}

\title{Class "dg.Test" }

\description{  An example class for the test object 
	       for the model object of dynamicGraph. }

\section{Objects from the Class}{
The methods \code{label} and \code{width} should be 
implemented by you for your test object returned by the method
\code{testEdge} of your model \code{object}. 

% Objects can be created by calls of the form \code{new("dg.Test", ...)}.
}

% \usage{newDefaultTestObject(name)}

% \arguments{\item{name}{ Text string with the \code{name}
%                         of the test object.}}

\value{An object of class \code{dg.Test}.}

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
    \item{label}{\code{signature(object = "dg.Test")}: 
       Return the \code{label} of the test. }
    \item{width}{\code{signature(object = "dg.Test")}:
       Return the \code{width} of the test. }
    \item{initialize}{\code{signature(.Object = "dg.Test")}: ... }
    \item{setSlots}{\code{signature(object = "dg.Test")}: ... }
  }
}

\author{Jens Henrik Badsberg}

\seealso{\code{\link{dg.Model-class}}.
% \code{\link{newDefaultModelObject}}, and \code{\link{newDefaultTestObject}}.
}

% # "newYourTestObject"<-
% #   function(name)
% #   {
% #     df <- round(runif(1, 1, 25))
% #     message("Just generating a random test!!!!!")
% #     deviance <- rchisq(1, df)
% #     p <- 1 - pchisq(deviance, df)
% #     result <- new("your.Test",
% #                    df = df, deviance = deviance, p = p)
% #     return(result)
% #   }

% # newYourTestObject("TestObject")

\examples{
# Part of the example "defaultObjects" of demo:

setClass("your.Test", 
         representation(name = "character",
                        deviance = "numeric", df = "numeric", p = "numeric"))

setMethod("setSlots", "your.Test",
          function(object, arguments) { 
            for (i in seq(along = arguments)) {
              name <- names(arguments)[i]
              if (is.element(name, slotNames(object)))
                slot(object, name) <- arguments[[i]]
              else
                message(paste("Argument '", name, "' not valid slot of '", 
                              class(object), "', thus ignored.",
                              sep = "")) }
            return(object)
          })

setMethod("initialize", "your.Test",
          function(.Object, ...) {
              # print(c("initialize", "your.Test", class(.Object)))
              Args <- list(...)
              if (!is.element("df", names(Args)) || 
                  !is.element("deviance", names(Args))) {
                Args <- (Args[!names(Args) == "df"])
                Args <- (Args[!names(Args) == "deviance"])
                .Object@df <- round(runif(1, 1, 25))
                .Object@deviance <- rchisq(1, .Object@df)
                .Object@p <- 1 - pchisq(.Object@deviance, .Object@df)
                message("Just generating a random test!!!!")
              }
              .Object <- setSlots(.Object, Args)
              return(.Object)
            }
          )


if (!isGeneric("label") && !isGeneric("label", where = 2)) {
  if (is.function("label"))
    fun <- label
  else
    fun <- function(object) standardGeneric("label")
  setGeneric("label", fun)
}

setMethod("label", "your.Test",
          function(object) format(object@p, digits = 4))

if (!isGeneric("width") && !isGeneric("width", where = 2)) {
  if (is.function("width"))
    fun <- width
  else
    fun <- function(object) standardGeneric("width")
  setGeneric("width", fun)
}

setMethod("width", "your.Test",
          function(object) round(2 + 5 * (1 - object@p)))

new("your.Test", name = "TestObject")
}

\keyword{classes}
