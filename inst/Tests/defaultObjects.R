
setClass("defaultModelObjectProto", 
         representation(name = "character"))

"newDefaultModelObject"<-
  function(name)
  {
    result <- new("defaultModelObjectProto", name = name)
    return(result)
  }

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

if (!isGeneric("testLabel")) {
  if (is.function("testLabel"))
    fun <- testLabel
  else
    fun <- function(object) standardGeneric("testLabel")
  setGeneric("testLabel", fun)
}

setMethod("testLabel", "defaultTestObjectProto",
          function(object) format(object@p, digits = 4))

if (!isGeneric("testWidth")) {
  if (is.function("testWidth"))
    fun <- testWidth
  else
    fun <- function(object) standardGeneric("testWidth")
  setGeneric("testWidth", fun)
}

# if (!isGeneric("label")) {
#   if (is.function("label"))
#     fun <- label
#   else
#     fun <- function(object) standardGeneric("label")
#   setGeneric("label", fun)
# }

setMethod("label", "defaultTestObjectProto",
          function(object) format(object@p, digits = 4))

# if (!isGeneric("width")) {
#   if (is.function("width"))
#     fun <- width
#   else
#     fun <- function(object) standardGeneric("width")
#   setGeneric("width", fun)
# }

setMethod("width", "defaultTestObjectProto",
          function(object) round(2 + 5 * (1 - object@p)))

# testEdge <- function(object, action, name.1, name.2, ...)
#  {
#     args <- list(...)
#     from.type <- args$from.type
#     to.type <- args$to.type
#     f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
#     message(paste("Should return a test object for the edge from",
#                   name.1, f(from.type), "to", name.2, f(from.type),
#                   "deleted from the argument object"))
#     return(object)
#  }

if (!isGeneric("testEdge")) {
  if (is.function("testEdge"))
    fun <- testEdge
  else
    fun <- function(object, action, name.1, name.2, ...) 
           standardGeneric("testEdge")
  setGeneric("testEdge", fun)
}

setMethod("testEdge", signature(object = "defaultModelObjectProto"),
          function(object, action, name.1, name.2, ...)
 {
    args <- list(...)
    from.type <- args$from.type
    to.type <- args$to.type
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    message(paste("Should return an object with the edge from",
                  name.1, f(from.type), "to", name.2, f(to.type),
                  "deleted from the argument object"))
    return(newDefaultTestObject())
 })

if (!isGeneric("modifyModel")) {
  if (is.function("modifyModel"))
    fun <- modifyModel
  else
    fun <- function(object, action, name, name.1, name.2, ...)
                    standardGeneric("modifyModel")
  setGeneric("modifyModel", fun)
}

setMethod("modifyModel", signature(object = "defaultModelObjectProto"),
          function(object, action, name, name.1, name.2, ...)
 {
    args <- list(...)
    # str(args)
    # print(names(args))
    # print(names(args$Arguments))
    # print(args$Arguments$visibleVertices)
    # str(args$Arguments$selectedNodes)
    str(args$selectedNodes)
    FactorVertices <- NULL
    FactorEdges <- NULL
    f <- function(type) if(is.null(type)) "" else paste("(", type, ")")
    if (action == "dropEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(args$from.type), "to", name.2, f(args$to.type),
                     "deleted from the argument object"))
    } else if (action == "addEdge") {
       message(paste("Should return an object with the edge from",
                     name.1, f(args$from.type), "to", name.2, f(args$to.type),
                     "added to the argument object"))
    } else if (action == "dropVertex")  {
       message(paste("Should return an object with the vertex", 
                     name, f(args$type),
                     "deleted from the argument object"))
       if (!is.null(args$Arguments) && (args$index > 0)
                       && !is.null(args$Arguments$factorVertexList)
                       && !is.null(args$Arguments$vertexList)) {
         x <- (args$Arguments$factorVertexList)
         factors <- lapply(x, function(i) i@vertex.indices)
         types <- lapply(x, function(i) class(i))
         factors <- lapply(factors, function(x) x[x != args$index])
         if (!(is.null(factors))) {
           result <- returnFactorVerticesAndEdges(
                                   args$Arguments$vertexList, factors, types, 
                                   factorClasses = validFactorClasses())
           FactorVertices <- result$FactorVertices
           FactorEdges <- result$FactorEdges
         }
       }
    } else if (action == "addVertex") {
       message(paste("Should return an object with the vertex", 
                     name, f(args$type), args$index, 
                     "added to the argument object"))
    }
    return(list(object = object,
                FactorVertices = FactorVertices,
                FactorEdges = FactorEdges))
 })
