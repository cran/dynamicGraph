
drawModel <- function(object, slave = FALSE,
                      oriented = FALSE, vertexColor = "red", 
                      edgeColor = "black", 
                      factorVertexColor = "default", 
                      factorEdgeColor = "brown", 
                      blockEdgeColor = "default", 
                      ...) 
 {

  two.to.pairs <- function(from, to)
  { 
      edge.list <- vector("list", length(to))
      for (j in seq(along = to)) edge.list[[j]] <- c(from[j], to[j])
      return(edge.list)
  }


  args <- list(...)
  Args <- args$Arguments
  Vertices <- Args$vertexList
  Edges <- returnEdges(model = object)
  edge.list <- two.to.pairs(Edges[, 1], Edges[, 2])
  Object <- makeModel(object)
  title <- Object@.title

  factors <- NULL
  FactorVertices <- NULL
  FactorEdges <- NULL
  if (!(is.null(factors))) {
    message("Not tested!")
    result <- returnFactorVerticesAndEdges(Vertices, factors,
                        factorVertexColor = factorVertexColor, 
                        factorEdgeColor = factorEdgeColor, 
                        # oriented = oriented, 
                        factorClasses = factorClasses)
    FactorVertices <- result$FactorVertices
    FactorEdges <- result$FactorEdges
    if ((is.null(edge.list))) {
      from <- result$PairEdges[,1]
      to   <- result$PairEdges[,2]
      edge.list <- two.to.pairs(from, to)
    }
  }

  edgeList <- returnEdgeList(edge.list, Vertices,
                             color = edgeColor, oriented = oriented)

  BlockList <- Args$blockList
  BlockTree <- Args$blockTree
  BlockEdges <- NULL
  if ((!is.null(BlockList) || !is.null(BlockTree))) {
    message("Not tested!")
    if (!(is.null(factors)))
      message("Edges between blocks and factors not implemented!")
    if (is.null(BlockList) && !is.null(BlockTree))
      BlockList <- blockTreeToList(BlockTree)
    BlockEdges <- returnBlockEdgeList(edge.list, Vertices, BlockList,
                                      color = blockEdgeColor,
                                      oriented = oriented)
  }

  if (slave)
    Args$redrawGraphWindow(graphWindow = NULL, 
                           edgeList = edgeList,
                           object = Object,
                           factorVertexList = FactorVertices,
                           factorEdgeList = FactorEdges, 
                           blockEdgeList = BlockEdges,
                           title = title, 
                           ...)
  else
    Args$redrawGraphWindow(graphWindow = Args$graphWindow,
                           edgeList = edgeList,
                           object = Object,
                           factorVertexList = FactorVertices,
                           factorEdgeList = FactorEdges, 
                           blockEdgeList = BlockEdges,
                           title = "Not used!", 
                           width = NULL, height = NULL, 
                           Arguments = Args)
 }

myLabelAllEdges <- function(object, slave = FALSE, ...) 
 {
  args <- list(...)
  Args <- args$Arguments

  getNodeName <- function(index, type)
    if (type == "Vertex")
      name(Args$vertexList[[index]])
    else if (type == "Factor")
      name(Args$factorVertexList[[abs(index)]])
    else if (type == "Block")
      label(Args$blockList[[abs(index)]])
    else
      NULL

  visitEdges <- function(edges) {
    for (i in seq(along = edges)) {
      vertices <- nodeIndicesOfEdge(edges[[i]])
      types    <- nodeTypesOfEdge(edges[[i]])

      name.f <- getNodeName(vertices[1], types[1])
      name.t <- getNodeName(vertices[2], types[2])

      R <- testEdge(object, action = "remove",
                    name.1 = name.f, name.2 = name.t,
                    from = vertices[1], to = vertices[2],
                    from.type = types[1], to.type = types[2],
                    edge.index = i, force = force, Arguments = Args)
      if (!is.null(R)) {
        # if (TRUE || (hasMethod("labelOfTest", class(R))))
        #   labelOfEdge(edges[[i]]) <- labelOfTest(R)
        # if (TRUE || (hasMethod("widthOfTest", class(R))))
        #   widthOfEdge(edges[[i]]) <- widthOfTest(R)
        if (TRUE || (hasMethod("label", class(R))))
          label(edges[[i]]) <- label(R)
        if (TRUE || (hasMethod("width", class(R))))
          width(edges[[i]]) <- width(R)
        # if (TRUE || (hasMethod("labelOfTest", class(R))))
        #   label(edges[[i]]) <- labelOfTest(R)
        # if (TRUE || (hasMethod("widthOfTest", class(R))))
        #   width(edges[[i]]) <- widthOfTest(R)
      }
    }
    return(edges)
  }

  edgeList <- visitEdges(Args$edgeList)
  factorEdgeList <- visitEdges(Args$factorEdgeList)
  blockEdgeList <- visitEdges(Args$blockEdgeList)

  if (slave)
    Args$redrawGraphWindow(graphWindow = NULL, 
                           edgeList = edgeList,
                           factorEdgeList = factorEdgeList,
                           blockEdgeList = blockEdgeList,
                           title = "A slave window", 
                           ...)
  else
    Args$redrawGraphWindow(graphWindow = Args$graphWindow,
                           edgeList = edgeList,
                           factorEdgeList = factorEdgeList,
                           blockEdgeList = blockEdgeList,
                           title = "Not used!", 
                           width = NULL, height = NULL, 
                           Arguments = Args)
 }


 Menus <- 
 list(MainUser = 
      list(label = "Test of user drag down menu - Position of \"vertices\"",
           command = function(object, ...) 
             print(Positions(list(...)$Arguments$vertexList))),
      MainUser = 
      list(label = "Label all edges, in this window",
           command = function(object, ...) 
                       myLabelAllEdges(object, slave = FALSE, ...)),
      MainUser = 
      list(label = "Label all edges, in slave window",
           command = function(object, ...) 
                       myLabelAllEdges(object, slave = TRUE, ...)),
      MainUser = 
      list(label = "Draw last model, in this window",
           command = function(object, ...) 
                       drawModel(object = "last", slave = FALSE, ...)),
      MainUser = 
      list(label = "Draw last model, in slave window",
           command = function(object, ...) 
                       drawModel(object = "last", slave = TRUE, ...)),
      MainUser = 
      list(label = "Test of user drag down menu - modalDialog",
           command = function(object, ...) 
           {
             Args <- list(...)$Arguments
             ReturnVal <- modalDialog("Test modalDialog Entry",
                                      "Enter name", Args$title,
                                      # top = Args$graphWindow@top,
                                      graphWindow = Args$graphWindow)
             print(ReturnVal)
             if (ReturnVal == "ID_CANCEL")
               return()
           }
          ),
      Vertex = 
      list(label = "Test of user popup menu for vertices",
           command = function(object, name, ...) 
           {
             print(name)
             print(c(list(...)$index))
           }
          ),
      Edge = 
      list(label = "Test of user popup menu for edges",
           command = function(object, name1, name2, ...) 
           {
             args <- list(...)
             print(c(name1, name2))
             print(c(args$edge.index, args$from, args$to))
           }
          ),
      ClosedBlock = 
      list(label = "Test of user popup menu for blocks",
           command = function(object, name, ...) 
           {
             print(name)
             print(c(list(...)$index))
           }
          ),
     )
