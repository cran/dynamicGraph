\name{newDefaultTestObject}
\alias{newDefaultTestObject}
% \alias{labelOfTest}
% \alias{widthOfTest}
\title{The test object}
\description{Returns an object of the class \code{defaultTestObjectProto}.}
\usage{newDefaultTestObject(name)}
\arguments{\item{name}{ Text string with the \code{name} of the test object.}}
\details{ 
    The methods
    \code{label} and \code{width} % (was \code{labelOfTest} and \code{widthOfTest})
    should be
    implemented by you for your test object returned by the method
    \code{testEdge} of your \code{object}. 
}
\value{An object of class \code{defaultTestObjectProto}.}
% \references{ ~put references to the literature/web site here ~ }
\author{Jens Henrik Badsberg}
% \note{ ~~further notes~~ }
\seealso{\code{\link{newDefaultModelObject}}}
\examples{

setClass("defaultTestObjectProto", 
         representation(deviance = "numeric", df = "numeric", p = "numeric"))

"newDefaultTestObject"<-
  function(name)
  {
    df <- round(runif(1, 1, 25))
    message("Just generating a random test!!!!!")
    deviance <- rchisq(1, df)
    p <- 1 - pchisq(deviance, df)
    result <- new("defaultTestObjectProto",
                   df = df, deviance = deviance, p = p)
    return(result)
  }

if (!isGeneric("label") && !isGeneric("label", where = 2)) {
  if (is.function("label"))
    fun <- label
  else
    fun <- function(object) standardGeneric("label")
  setGeneric("label", fun)
}

setMethod("label", "defaultTestObjectProto",
          function(object) format(object@p, digits = 4))

if (!isGeneric("width") && !isGeneric("width", where = 2)) {
  if (is.function("width"))
    fun <- width
  else
    fun <- function(object) standardGeneric("width")
  setGeneric("width", fun)
}

setMethod("width", "defaultTestObjectProto",
          function(object) round(2 + 5 * (1 - object@p)))


newDefaultTestObject("TestObject")

}
\keyword{ methods }
\keyword{ dynamic }
\keyword{ graphs }
