".First.lib" <-
function (lib, pkg) 
{
    .onLoad.dynamicGraph()
}
".onAttach" <-
function (lib, pkg) 
{
    require(tcltk)
}
".onLoad" <-
function (lib, pkg) 
{
    .onLoad.dynamicGraph()
}
".onLoad.dynamicGraph" <-
function () 
{
    library(methods)
    setClass("GraphLatticeProto", representation(vertices = "list", 
        blocks = "list", blockTree = "list", graphs = "list"))
    setClass("CanvasProto", representation(top = "tkwin", canvas = "tkwin", 
        tags = "list", id = "numeric", visibleVertices = "numeric", 
        graphEdges = "list", blockEdges = "list", factorVertices = "list", 
        factorEdges = "list"))
    setClass("NodeProto", representation(color = "character", 
        label = "character", label.position = "numeric"), prototype(color = "black", 
        label = "Label", label.position = c(0, 0, 0)))
    setClass("VertexProto", contains = "NodeProto", representation(name = "character", 
        index = "numeric", position = "numeric", stratum = "numeric"), 
        prototype(name = "Name", index = 0, position = c(0, 0, 
            0), stratum = 0))
    for (prototype in paste(validVertexClasses()[, 2])) setClass(prototype, 
        contains = "VertexProto")
    setClass("TextVertexProto", contains = "VertexProto")
    setClass("BlockProto", contains = "NodeProto", representation(stratum = "numeric", 
        index = "numeric", ancestors = "numeric", descendants = "numeric", 
        position = "matrix", visible = "logical"), prototype(stratum = 0, 
        index = 0, ancestors = 0, descendants = 0, position = matrix(rep(0, 
            6), ncol = 3), visible = TRUE, color = "black", label = "Label", 
        label.position = c(0, 0, 0)))
    setClass("FactorVertexProto", contains = "VertexProto", representation(vertex.indices = "numeric"), 
        prototype(vertex.indices = c(0, 0)))
    for (prototype in paste(validFactorClasses()[, 2])) setClass(prototype, 
        contains = "FactorVertexProto")
    setClass("EdgeProto", contains = "NodeProto", representation(vertex.indices = "numeric", 
        width = "numeric"), prototype(vertex.indices = c(0, 0), 
        width = 2))
    setClass("VertexEdgeProto", contains = "EdgeProto", representation(oriented = "logical"), 
        prototype(oriented = FALSE))
    setClass("FactorEdgeProto", contains = "EdgeProto", representation(), 
        prototype())
    setClass("BlockEdgeProto", contains = "EdgeProto", representation(oriented = "logical"), 
        prototype(oriented = TRUE))
    if (!isGeneric("color")) {
        if (is.function("color")) 
            fun <- color
        else fun <- function(object) standardGeneric("color")
        setGeneric("color", fun)
    }
    setMethod("color", "NodeProto", function(object) object@color)
    setGeneric("color<-", function(x, value) standardGeneric("color<-"))
    setReplaceMethod("color", "NodeProto", function(x, value) {
        x@color <- value
        x
    })
    if (!isGeneric("label")) {
        if (is.function("label")) 
            fun <- label
        else fun <- function(object) standardGeneric("label")
        setGeneric("label", fun)
    }
    setMethod("label", "NodeProto", function(object) object@label)
    setGeneric("label<-", function(x, value) standardGeneric("label<-"))
    setReplaceMethod("label", "NodeProto", function(x, value) {
        x@label <- value
        x
    })
    if (!isGeneric("labelPosition")) {
        if (is.function("labelPosition")) 
            fun <- labelPosition
        else fun <- function(object) standardGeneric("labelPosition")
        setGeneric("labelPosition", fun)
    }
    setMethod("labelPosition", "NodeProto", function(object) object@label.position)
    setGeneric("labelPosition<-", function(x, value) standardGeneric("labelPosition<-"))
    setReplaceMethod("labelPosition", "NodeProto", function(x, 
        value) {
        x@label.position <- value
        x
    })
    if (!isGeneric("name")) {
        if (is.function("name")) 
            fun <- name
        else fun <- function(object) standardGeneric("name")
        setGeneric("name", fun)
    }
    setMethod("name", "VertexProto", function(object) object@name)
    setMethod("name", "EdgeProto", function(object) object@label)
    setMethod("name", "BlockProto", function(object) object@label)
    setGeneric("name<-", function(x, value) standardGeneric("name<-"))
    setReplaceMethod("name", "VertexProto", function(x, value) {
        x@name <- value
        x
    })
    if (!isGeneric("index")) {
        if (is.function("index")) 
            fun <- index
        else fun <- function(object) standardGeneric("index")
        setGeneric("index", fun)
    }
    setMethod("index", "VertexProto", function(object) object@index)
    setMethod("index", "BlockProto", function(object) object@index)
    setGeneric("index<-", function(x, value) standardGeneric("index<-"))
    setReplaceMethod("index", "VertexProto", function(x, value) {
        x@index <- value
        x
    })
    setReplaceMethod("index", "BlockProto", function(x, value) {
        x@index <- value
        x
    })
    if (!isGeneric("nodeIndices")) {
        if (is.function("nodeIndices")) 
            fun <- nodeIndices
        else fun <- function(object) standardGeneric("nodeIndices")
        setGeneric("nodeIndices", fun)
    }
    setMethod("nodeIndices", "FactorVertexProto", function(object) object@vertex.indices)
    setGeneric("nodeIndices<-", function(x, value) standardGeneric("nodeIndices<-"))
    setReplaceMethod("nodeIndices", "FactorVertexProto", function(x, 
        value) {
        x@vertex.indices <- value
        x
    })
    if (!isGeneric("position")) {
        if (is.function("position")) 
            fun <- position
        else fun <- function(object) standardGeneric("position")
        setGeneric("position", fun)
    }
    setMethod("position", "VertexProto", function(object) object@position)
    setGeneric("position<-", function(x, value) standardGeneric("position<-"))
    setReplaceMethod("position", "VertexProto", function(x, value) {
        x@position <- value
        x
    })
    setMethod("position", "BlockProto", function(object) t(object@position))
    setReplaceMethod("position", "BlockProto", function(x, value) {
        x@position <- t(value)
        x
    })
    if (!isGeneric("stratum")) {
        if (is.function("stratum")) 
            fun <- stratum
        else fun <- function(object) standardGeneric("stratum")
        setGeneric("stratum", fun)
    }
    setMethod("stratum", "VertexProto", function(object) object@stratum)
    setGeneric("stratum<-", function(x, value) standardGeneric("stratum<-"))
    setReplaceMethod("stratum", "VertexProto", function(x, value) {
        x@stratum <- value
        x
    })
    setMethod("stratum", "BlockProto", function(object) object@stratum)
    setReplaceMethod("stratum", "BlockProto", function(x, value) {
        x@stratum <- value
        x
    })
    if (!isGeneric("ancestors")) {
        if (is.function("ancestors")) 
            fun <- ancestors
        else fun <- function(object, blockList = NULL, vertexList = NULL, 
            ...) standardGeneric("ancestors")
        setGeneric("ancestors", fun)
    }
    setMethod("ancestors", "VertexProto", function(object, blockList = NULL, 
        vertexList = NULL, ...) warning("Not implemented"))
    setGeneric("ancestors<-", function(x, value) standardGeneric("ancestors<-"))
    setReplaceMethod("ancestors", "VertexProto", function(x, 
        value) {
        warning("Not implemented")
    })
    setMethod("ancestors", "BlockProto", function(object, blockList = NULL, 
        vertexList = NULL, ...) object@ancestors)
    setReplaceMethod("ancestors", "BlockProto", function(x, value) {
        x@ancestors <- value
        x
    })
    if (!isGeneric("descendants")) {
        if (is.function("descendants")) 
            fun <- descendants
        else fun <- function(object, blockList = NULL, vertexList = NULL, 
            ...) standardGeneric("descendants")
        setGeneric("descendants", fun)
    }
    setMethod("descendants", "VertexProto", function(object, 
        blockList = NULL, vertexList = NULL, ...) warning("Not implemented"))
    setGeneric("descendants<-", function(x, value) standardGeneric("descendants<-"))
    setReplaceMethod("descendants", "VertexProto", function(x, 
        value) {
        warning("Not implemented")
    })
    setMethod("descendants", "BlockProto", function(object, blockList = NULL, 
        vertexList = NULL, ...) object@descendants)
    setReplaceMethod("descendants", "BlockProto", function(x, 
        value) {
        x@descendants <- value
        x
    })
    if (!isGeneric("visible")) {
        if (is.function("visible")) 
            fun <- visible
        else fun <- function(object) standardGeneric("visible")
        setGeneric("visible", fun)
    }
    setMethod("visible", "VertexProto", function(object) warning("Not implemented"))
    setGeneric("visible<-", function(x, value) standardGeneric("visible<-"))
    setReplaceMethod("visible", "VertexProto", function(x, value) {
        warning("Not implemented")
    })
    setMethod("visible", "BlockProto", function(object) object@visible)
    setReplaceMethod("visible", "BlockProto", function(x, value) {
        x@visible <- value
        x
    })
    if (!isGeneric("draw")) {
        if (is.function("draw")) 
            fun <- draw
        else fun <- function(object, canvas, position, x = position[1], 
            y = position[2], stratum = 0, w = 2, color = "green", 
            background = "white", ...) standardGeneric("draw")
        setGeneric("draw", fun)
    }
    setMethod("draw", "TextVertexProto", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "black", background = "white") {
        s <- w * sqrt(4/pi)/10
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "DiscreteVertexProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(4/pi)
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "OrdinalVertexProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        p <- tkcreate(canvas, "rectangle", x - w, y - w, x + 
            w, y + w, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "ContinuousVertexProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(4/pi)
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object))
        s <- 0.6 * w * sqrt(4/pi)
        q <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = background)
        return(list(dynamic = list(p), fixed = list(q)))
    })
    if (!isGeneric("addToPopups")) {
        if (is.function("addToPopups")) 
            fun <- draw
        else fun <- function(object, type, nodePopupMenu, ...) standardGeneric("addToPopups")
        setGeneric("addToPopups", fun)
    }
    setMethod("addToPopups", "NodeProto", function(object, type, 
        nodePopupMenu, ...) {
        if ((type == "Factor")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a factor!"), 
                command = function() {
                })
        if ((type == "Vertex")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a vertex!"), 
                command = function() {
                })
        else if ((type == "OpenBlock")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an open block!"), 
                command = function() {
                })
        else if ((type == "ClosedBlock")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a closed block!"), 
                command = function() {
                })
        else if ((type == "Edge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an edge!"), 
                command = function() {
                })
        else if ((type == "Extra")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an extra!"), 
                command = function() {
                })
        else tkadd(nodePopupMenu, "command", label = paste(" --- This is ??? "), 
            command = function() {
            })
    })
    setMethod("draw", "GeneratorProto", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - 0, y + s, x + s, 
            y + 0, x + 0, y - s, x - s, y - 0, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "DiscreteGeneratorProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "LinearGeneratorProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "QuadraticGeneratorProto", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object))
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "EdgeProto", function(object, canvas, position, 
        x = lapply(position, function(e) e[1]), y = lapply(position, 
            function(e) e[2]), stratum = as.vector(rep(0, length(position)), 
            mode = "list"), w = 2, color = "green", background = "white") {
        f <- function(i, j) {
            arrowhead = "none"
            if ((class(object) == "VertexEdgeProto") || (class(object) == 
                "BlockEdgeProto")) 
                if (object@oriented) 
                  arrowhead = "last"
                else if (stratum[[i]] == stratum[[j]]) 
                  arrowhead = "none"
                else if (stratum[[i]] < stratum[[j]]) 
                  arrowhead = "last"
                else arrowhead = "first"
            line <- tkcreate(canvas, "line", x[[i]], y[[i]], 
                x[[j]], y[[j]], arrow = arrowhead, width = w, 
                fill = color(object))
            label.position <- (position[[i]] + position[[j]])/2
            pos <- label.position + rep(0, length(label.position))
            label <- tkcreate(canvas, "text", pos[1], pos[2], 
                text = object@label, anchor = "nw", font = "8x16")
            return(list(line = line, from = object@vertex.indices[i], 
                to = object@vertex.indices[j], label = label, 
                label.position = label.position))
        }
        result <- NULL
        edge <- object@vertex.indices
        m <- length(edge)
        for (j in seq(along = edge)) if (j < length(edge)) 
            for (k in (j + 1):length(edge)) result <- append(result, 
                list(f(j, k)))
        return(result)
    })
    if (!isGeneric("edgeLabel")) {
        if (is.function("edgeLabel")) 
            fun <- edgeLabel
        else fun <- function(object) standardGeneric("edgeLabel")
        setGeneric("edgeLabel", fun)
    }
    setMethod("edgeLabel", "EdgeProto", function(object) object@label)
    setGeneric("edgeLabel<-", function(x, value) standardGeneric("edgeLabel<-"))
    setReplaceMethod("edgeLabel", "EdgeProto", function(x, value) {
        x@label <- value
        x
    })
    if (!isGeneric("labelOfEdge")) {
        if (is.function("labelOfEdge")) 
            fun <- labelOfEdge
        else fun <- function(object) standardGeneric("labelOfEdge")
        setGeneric("labelOfEdge", fun)
    }
    setMethod("labelOfEdge", "EdgeProto", function(object) object@label)
    setGeneric("labelOfEdge<-", function(x, value) standardGeneric("labelOfEdge<-"))
    setReplaceMethod("labelOfEdge", "EdgeProto", function(x, 
        value) {
        x@label <- value
        x
    })
    if (!isGeneric("width")) {
        if (is.function("width")) 
            fun <- width
        else fun <- function(object) standardGeneric("width")
        setGeneric("width", fun)
    }
    setMethod("width", "EdgeProto", function(object) object@width)
    setGeneric("width<-", function(x, value) standardGeneric("width<-"))
    setReplaceMethod("width", "EdgeProto", function(x, value) {
        x@width <- value
        x
    })
    if (!isGeneric("oriented")) {
        if (is.function("oriented")) 
            fun <- oriented
        else fun <- function(object) standardGeneric("oriented")
        setGeneric("oriented", fun)
    }
    setMethod("oriented", "VertexEdgeProto", function(object) object@oriented)
    setGeneric("oriented<-", function(x, value) standardGeneric("oriented<-"))
    setReplaceMethod("oriented", "VertexEdgeProto", function(x, 
        value) {
        x@oriented <- value
        x
    })
    setMethod("oriented", "BlockEdgeProto", function(object) object@oriented)
    setReplaceMethod("oriented", "BlockEdgeProto", function(x, 
        value) {
        x@oriented <- value
        x
    })
    if (!isGeneric("edgeWidth")) {
        if (is.function("edgeWidth")) 
            fun <- edgeWidth
        else fun <- function(object) standardGeneric("edgeWidth")
        setGeneric("edgeWidth", fun)
    }
    setMethod("edgeWidth", "EdgeProto", function(object) object@width)
    setGeneric("edgeWidth<-", function(x, value) standardGeneric("edgeWidth<-"))
    setReplaceMethod("edgeWidth", "EdgeProto", function(x, value) {
        x@width <- value
        x
    })
    if (!isGeneric("widthOfEdge")) {
        if (is.function("widthOfEdge")) 
            fun <- widthOfEdge
        else fun <- function(object) standardGeneric("widthOfEdge")
        setGeneric("widthOfEdge", fun)
    }
    setMethod("widthOfEdge", "EdgeProto", function(object) object@width)
    setGeneric("widthOfEdge<-", function(x, value) standardGeneric("widthOfEdge<-"))
    setReplaceMethod("widthOfEdge", "EdgeProto", function(x, 
        value) {
        x@width <- value
        x
    })
    if (!isGeneric("nodeTypesOfEdge")) {
        if (is.function("nodeTypesOfEdge")) 
            fun <- nodeTypesOfEdge
        else fun <- function(object) standardGeneric("nodeTypesOfEdge")
        setGeneric("nodeTypesOfEdge", fun)
    }
    setMethod("nodeTypesOfEdge", "VertexEdgeProto", function(object) rep("Vertex", 
        length(object@vertex.indices)))
    setMethod("nodeTypesOfEdge", "FactorEdgeProto", function(object) ifelse(object@vertex.indices > 
        0, "Vertex", "Factor"))
    setMethod("nodeTypesOfEdge", "BlockEdgeProto", function(object) ifelse(object@vertex.indices > 
        0, "Vertex", "Block"))
    if (!isGeneric("nodeIndicesOfEdge")) {
        if (is.function("nodeIndicesOfEdge")) 
            fun <- nodeIndicesOfEdge
        else fun <- function(object) standardGeneric("nodeIndicesOfEdge")
        setGeneric("nodeIndicesOfEdge", fun)
    }
    setMethod("nodeIndicesOfEdge", "EdgeProto", function(object) object@vertex.indices)
    setGeneric("nodeIndicesOfEdge<-", function(x, value) standardGeneric("nodeIndicesOfEdge<-"))
    setReplaceMethod("nodeIndicesOfEdge", "EdgeProto", function(x, 
        value) {
        x@vertex.indices <- value
        x
    })
    setMethod("draw", "BlockProto", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 10, color = "green", background = "white") {
        s <- w
        p <- tkcreate(canvas, "rectangle", x - s, y - s, x + 
            s, y + s, fill = color(object))
        s <- w - 2
        q <- tkcreate(canvas, "rectangle", x - s, y - s, x + 
            s, y + s, fill = background)
        return(list(dynamic = list(p), fixed = list(q)))
    })
    if (!isGeneric("Names")) {
        if (is.function("Names")) 
            fun <- Names
        else fun <- function(objects) standardGeneric("Names")
        setGeneric("Names", fun)
    }
    setMethod("Names", "list", function(objects) {
        NAMES <- lapply(objects, function(x) if (!is.null(x)) 
            name(x))
        names(NAMES) <- NULL
        return(unlist(NAMES))
    })
    setGeneric("Names<-", function(objectlist, value) standardGeneric("Names<-"))
    setReplaceMethod("Names", "list", function(objectlist, value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@name <- value[i]
        }
        else warning("Invalid list of values for names")
        objectlist
    })
    if (!isGeneric("Colors")) {
        if (is.function("Colors")) 
            fun <- Colors
        else fun <- function(objectlist) standardGeneric("Colors")
        setGeneric("Colors", fun)
    }
    setMethod("Colors", "list", function(objectlist) {
        Colors <- lapply(objectlist, function(x) if (!is.null(x)) 
            color(x))
        names(Colors) <- Names(objectlist)
        return(unlist(Colors))
    })
    setGeneric("Colors<-", function(objectlist, value) standardGeneric("Colors<-"))
    setReplaceMethod("Colors", "list", function(objectlist, value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@color <- value[i]
        }
        else warning("Invalid list of values for colors")
        objectlist
    })
    if (!isGeneric("Labels")) {
        if (is.function("Labels")) 
            fun <- Labels
        else fun <- function(objectlist) standardGeneric("Labels")
        setGeneric("Labels", fun)
    }
    setMethod("Labels", "list", function(objectlist) {
        Labels <- lapply(objectlist, function(x) if (!is.null(x)) 
            label(x))
        names(Labels) <- Names(objectlist)
        return(unlist(Labels))
    })
    setGeneric("Labels<-", function(objectlist, value) standardGeneric("Labels<-"))
    setReplaceMethod("Labels", "list", function(objectlist, value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@label <- value[i]
        }
        else warning("Invalid list of values for labels")
        objectlist
    })
    if (!isGeneric("LabelPositions")) {
        if (is.function("LabelPositions")) 
            fun <- labelPositions
        else fun <- function(objectlist) standardGeneric("LabelPositions")
        setGeneric("LabelPositions", fun)
    }
    setMethod("LabelPositions", "list", function(objectlist) {
        positions <- lapply(objectlist, function(x) if (!is.null(x)) 
            labelPosition(x))
        N.list <- unlist(lapply(positions, function(x) length(x)))
        if ((N.list[1] > 0) && all(N.list == N.list[1])) {
            positions <- matrix(unlist(positions), ncol = length(positions[[1]]), 
                byrow = TRUE)
            labels <- c("X", "Y")
            if (ncol(positions) > 2) 
                labels <- c(labels, paste("Z", 3:ncol(positions), 
                  sep = "-"))
            dimnames(positions) <- list(Names(objectlist), labels)
        }
        return(positions)
    })
    setGeneric("LabelPositions<-", function(objectlist, value) standardGeneric("LabelPositions<-"))
    setReplaceMethod("LabelPositions", "list", function(objectlist, 
        value) {
        if (length(objectlist) == nrow(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@label.position <- value[i, 
                ]
        }
        else warning("Invalid list of values for labelpositions")
        objectlist
    })
    if (!isGeneric("Positions")) {
        if (is.function("Positions")) 
            fun <- positions
        else fun <- function(objectlist) standardGeneric("Positions")
        setGeneric("Positions", fun)
    }
    setMethod("Positions", "list", function(objectlist) {
        positions <- lapply(objectlist, function(x) if (!is.null(x)) 
            position(x))
        if (class(objectlist[[1]]) == "BlockProto") {
            N <- nrow(positions[[1]])
            N.list <- unlist(lapply(positions, function(x) nrow(x)))
            if ((N > 0) && all(N.list == N)) {
                positions <- matrix(unlist(positions), ncol = 2 * 
                  N, byrow = TRUE)
                labels <- c("X", "Y", "x", "y")
                if (ncol(positions) > 2) 
                  labels <- c("X", "Y", paste("Z", 3:N, sep = "-"), 
                    "x", "y", paste("z", 3:N, sep = "-"))
                if (ncol(positions) > 2) 
                  dimnames(positions) <- list(Names(objectlist), 
                    labels)
            }
        }
        else {
            N.list <- unlist(lapply(positions, function(x) length(x)))
            if ((N.list[1] > 0) && all(N.list == N.list[1])) {
                positions <- matrix(unlist(positions), ncol = length(positions[[1]]), 
                  byrow = TRUE)
                labels <- c("X", "Y")
                if (ncol(positions) > 2) 
                  labels <- c(labels, paste("Z", 3:ncol(positions), 
                    sep = "-"))
                dimnames(positions) <- list(Names(objectlist), 
                  labels)
            }
        }
        return(positions)
    })
    setGeneric("Positions<-", function(objectlist, value) standardGeneric("Positions<-"))
    setReplaceMethod("Positions", "list", function(objectlist, 
        value) {
        if (length(objectlist) == nrow(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@position <- value[i, 
                ]
        }
        else warning("Invalid list of values for positions")
        objectlist
    })
    if (!isGeneric("Strata")) {
        if (is.function("Strata")) 
            fun <- strata
        else fun <- function(objectlist) standardGeneric("Strata")
        setGeneric("Strata", fun)
    }
    setMethod("Strata", "list", function(objectlist) {
        strata <- lapply(objectlist, function(x) if (!is.null(x)) 
            stratum(x))
        names(strata) <- Names(objectlist)
        return(unlist(strata))
    })
    setGeneric("Strata<-", function(objectlist, value) standardGeneric("Strata<-"))
    setReplaceMethod("Strata", "list", function(objectlist, value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@stratum <- value[i]
        }
        else warning("Invalid list of values for strata")
        objectlist
    })
    if (!isGeneric("NodeAncestors")) {
        if (is.function("NodeAncestors")) 
            fun <- ancestors
        else fun <- function(objectlist) standardGeneric("NodeAncestors")
        setGeneric("NodeAncestors", fun)
    }
    setMethod("NodeAncestors", "list", function(objectlist) {
        ancestors <- lapply(objectlist, function(x) if (!is.null(x)) 
            ancestors(x))
        names(ancestors) <- Names(objectlist)
        return(ancestors)
    })
    setGeneric("NodeAncestors<-", function(objectlist, value) standardGeneric("NodeAncestors<-"))
    setReplaceMethod("NodeAncestors", "list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@ancestors <- value[i]
        }
        else warning("Invalid list of values for ancestors")
        objectlist
    })
    if (!isGeneric("NodeDescendants")) {
        if (is.function("NodeDescendants")) 
            fun <- descendants
        else fun <- function(objectlist) standardGeneric("NodeDescendants")
        setGeneric("NodeDescendants", fun)
    }
    setMethod("NodeDescendants", "list", function(objectlist) {
        descendants <- lapply(objectlist, function(x) if (!is.null(x)) 
            descendants(x))
        names(descendants) <- Names(objectlist)
        return(descendants)
    })
    setGeneric("NodeDescendants<-", function(objectlist, value) standardGeneric("NodeDescendants<-"))
    setReplaceMethod("NodeDescendants", "list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@descendants <- value[i]
        }
        else warning("Invalid list of values for descendants")
        objectlist
    })
    if (!isGeneric("Indices")) {
        if (is.function("Indices")) 
            fun <- indices
        else fun <- function(objectlist) standardGeneric("Indices")
        setGeneric("Indices", fun)
    }
    setMethod("Indices", "list", function(objectlist) {
        indices <- lapply(objectlist, function(x) if (!is.null(x)) 
            index(x))
        names(indices) <- Names(objectlist)
        return(unlist(indices))
    })
    if (!isGeneric("NodeTypes")) {
        if (is.function("NodeTypes")) 
            fun <- nodeTypes
        else fun <- function(objectlist) standardGeneric("NodeTypes")
        setGeneric("NodeTypes", fun)
    }
    setMethod("NodeTypes", "list", function(objectlist) {
        indices <- lapply(objectlist, function(x) if (!is.null(x)) 
            nodeTypesOfEdge(x))
        names(indices) <- Names(objectlist)
        return(indices)
    })
    if (!isGeneric("NodeIndices")) {
        if (is.function("NodeIndices")) 
            fun <- nodeIndices
        else fun <- function(objectlist) standardGeneric("NodeIndices")
        setGeneric("NodeIndices", fun)
    }
    setMethod("NodeIndices", "list", function(objectlist) {
        indices <- lapply(objectlist, function(x) if (!is.null(x)) 
            nodeIndicesOfEdge(x))
        names(indices) <- Names(objectlist)
        return(indices)
    })
    if (!isGeneric("Widths")) {
        if (is.function("Widths")) 
            fun <- Widths
        else fun <- function(objectlist) standardGeneric("Widths")
        setGeneric("Widths", fun)
    }
    setMethod("Widths", "list", function(objectlist) {
        widths <- lapply(objectlist, function(x) if (!is.null(x)) 
            width(x))
        names(widths) <- Names(objectlist)
        return(unlist(widths))
    })
    setGeneric("Widths<-", function(objectlist, value) standardGeneric("Widths<-"))
    setReplaceMethod("Widths", "list", function(objectlist, value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@width <- value[i]
        }
        else warning("Invalid list of values for widths")
        objectlist
    })
    if (!isGeneric("Oriented")) {
        if (is.function("Oriented")) 
            fun <- oriented
        else fun <- function(objectlist) standardGeneric("Oriented")
        setGeneric("Oriented", fun)
    }
    setMethod("Oriented", "list", function(objectlist) {
        oriented <- lapply(objectlist, function(x) if (!is.null(x)) 
            oriented(x))
        names(oriented) <- Names(objectlist)
        return(unlist(oriented))
    })
    setGeneric("Oriented<-", function(objectlist, value) standardGeneric("Oriented<-"))
    setReplaceMethod("Oriented", "list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@oriented <- value[i]
        }
        else warning("Invalid list of values for oriented")
        objectlist
    })
    if (!isGeneric("Visible")) {
        if (is.function("Visible")) 
            fun <- visible
        else fun <- function(objectlist) standardGeneric("Visible")
        setGeneric("Visible", fun)
    }
    setMethod("Visible", "list", function(objectlist) {
        visible <- lapply(objectlist, function(x) if (!is.null(x)) 
            visible(x))
        names(visible) <- Names(objectlist)
        return(unlist(visible))
    })
    setGeneric("Visible<-", function(objectlist, value) standardGeneric("Visible<-"))
    setReplaceMethod("Visible", "list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@visible <- value[i]
        }
        else warning("Invalid list of values for visible")
        objectlist
    })
    setClass("defaultModelObjectProto", representation(name = "character"))
    "newDefaultModelObject" <- function(name) {
        result <- new("defaultModelObjectProto", name = name)
        return(result)
    }
    setClass("defaultTestObjectProto", representation(deviance = "numeric", 
        df = "numeric", p = "numeric"))
    "newDefaultTestObject" <- function(name) {
        df <- round(runif(1, 1, 25))
        message("Just generating a random test!!!!!")
        deviance <- rchisq(1, df)
        p <- 1 - pchisq(deviance, df)
        result <- new("defaultTestObjectProto", df = df, deviance = deviance, 
            p = p)
        return(result)
    }
}
".onLoad.dynamicGraphInterface" <-
function () 
{
    if (!isGeneric("testLabel")) {
        if (is.function("testLabel")) 
            fun <- testLabel
        else fun <- function(object) standardGeneric("testLabel")
        setGeneric("testLabel", fun)
    }
    setMethod("testLabel", "defaultTestObjectProto", function(object) format(object@p, 
        digits = 4))
    if (!isGeneric("labelOfTest")) {
        if (is.function("labelOfTest")) 
            fun <- labelOfTest
        else fun <- function(object) standardGeneric("labelOfTest")
        setGeneric("labelOfTest", fun)
    }
    setMethod("labelOfTest", "defaultTestObjectProto", function(object) format(object@p, 
        digits = 4))
    if (!isGeneric("testWidth")) {
        if (is.function("testWidth")) 
            fun <- testWidth
        else fun <- function(object) standardGeneric("testWidth")
        setGeneric("testWidth", fun)
    }
    setMethod("testWidth", "defaultTestObjectProto", function(object) round(2 + 
        5 * (1 - object@p)))
    if (!isGeneric("widthOfTest")) {
        if (is.function("widthOfTest")) 
            fun <- widthOfTest
        else fun <- function(object) standardGeneric("widthOfTest")
        setGeneric("widthOfTest", fun)
    }
    setMethod("widthOfTest", "defaultTestObjectProto", function(object) round(2 + 
        5 * (1 - object@p)))
    if (!isGeneric("testEdge")) {
        if (is.function("testEdge")) 
            fun <- testEdge
        else fun <- function(object, action, name.1, name.2, 
            ...) standardGeneric("testEdge")
        setGeneric("testEdge", fun)
    }
    setMethod("testEdge", signature(object = "defaultModelObjectProto"), 
        function(object, action, name.1, name.2, ...) {
            args <- list(...)
            from.type <- args$from.type
            to.type <- args$to.type
            f <- function(type) if (is.null(type)) 
                ""
            else paste("(", type, ")")
            message(paste("Should return an object with the edge from", 
                name.1, f(from.type), "to", name.2, f(to.type), 
                "deleted from the argument object"))
            return(newDefaultTestObject())
        })
    if (!isGeneric("modifyModel")) {
        if (is.function("modifyModel")) 
            fun <- modifyModel
        else fun <- function(object, action, name, name.1, name.2, 
            ...) standardGeneric("modifyModel")
        setGeneric("modifyModel", fun)
    }
    setMethod("modifyModel", signature(object = "defaultModelObjectProto"), 
        function(object, action, name, name.1, name.2, ...) {
            args <- list(...)
            FactorVertices <- NULL
            FactorEdges <- NULL
            f <- function(type) if (is.null(type)) 
                ""
            else paste("(", type, ")")
            if (action == "dropEdge") {
                message(paste("Should return an object with the edge from", 
                  name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                  "deleted from the argument object"))
            }
            else if (action == "addEdge") {
                message(paste("Should return an object with the edge from", 
                  name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                  "added to the argument object"))
            }
            else if (action == "dropVertex") {
                message(paste("Should return an object with the vertex", 
                  name, f(args$type), "deleted from the argument object"))
                if (!is.null(args$Arguments) && (args$index > 
                  0) && !is.null(args$Arguments$factorVertexList) && 
                  !is.null(args$Arguments$vertexList)) {
                  x <- (args$Arguments$factorVertexList)
                  factors <- lapply(x, function(i) i@vertex.indices)
                  types <- lapply(x, function(i) class(i))
                  factors <- lapply(factors, function(x) x[x != 
                    args$index])
                  if (!(is.null(factors))) {
                    result <- returnFactorVerticesAndEdges(args$Arguments$vertexList, 
                      factors, types, factorClasses = validFactorClasses())
                    FactorVertices <- result$FactorVertices
                    FactorEdges <- result$FactorEdges
                  }
                }
            }
            else if (action == "addVertex") {
                message(paste("Should return an object with the vertex", 
                  name, f(args$type), args$index, "added to the argument object"))
            }
            return(list(object = object, FactorVertices = FactorVertices, 
                FactorEdges = FactorEdges))
        })
}
