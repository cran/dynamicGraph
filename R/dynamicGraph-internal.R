".asDataFrame" <-
function (v, setRowLabels = FALSE, shortNames = TRUE, excludeSlots = FALSE) 
{
    ".removeNull" <- function(x) {
        result <- NULL
        for (i in seq(along = x)) if (!is.null(x[[i]]) && (length(x[[i]] > 
            0))) 
            result <- append(result, list(x[[i]]))
        return(result)
    }
    f <- function(name, v) {
        if (name == "class") 
            x <- lapply(v, function(i) slot(i, name))
        else x <- lapply(v, function(i) if (name %in% slotNames(i)) 
            slot(i, name))
        w <- unique(range(unlist(lapply(x, length))))
        if (length(w) > 1) 
            x <- lapply(x, function(i) c(i, rep(NA, max(w) - 
                length(i))))
        w <- max(w)
        X <- unlist(x)
        if (shortNames) {
            if (name == "vertex.indices") 
                name <- "v..i."
            if (name == "label.position") 
                name <- "l..pos."
            if (name == "position") 
                name <- "p."
        }
        if (w > 1) {
            X <- matrix(X, ncol = w, byrow = TRUE)
            dimnames(X) <- list(NULL, paste(name, 1:w, sep = "."))
        }
        else {
            X <- matrix(X, ncol = 1)
            dimnames(X) <- list(NULL, name)
        }
        names(X) <- NULL
        return(X)
    }
    names.all <- unique(sort(unlist(lapply(v, function(i) slotNames(i)))))
    names.all <- c(names.all, "class")
    names <- slotNames(v[[1]])
    names <- c(names, names.all[is.na(match(names.all, names))])
    if (is.character(excludeSlots) || excludeSlots) {
        if (is.logical(excludeSlots)) 
            excludeSlots <- c("class", "name", "label.position", 
                "descendants", "ancestors")
        if (("label" %in% excludeSlots) && setRowLabels) 
            excludeSlots <- excludeSlots[excludeSlots != "label"]
        names <- names[is.na(match(names, excludeSlots))]
    }
    Y <- data.frame(.removeNull(lapply(names, function(name) f(name, 
        v))))
    if (setRowLabels && ("label" %in% names)) {
        labels <- t(Y["label"])
        if (length(unique(match(labels, labels))) < length(labels)) 
            labels <- paste(labels, 1:length(labels), sep = ".")
        dimnames(Y)[[1]] <- labels
        if (is.character(excludeSlots) || excludeSlots) 
            Y <- Y[!(dimnames(Y)[[2]] == "label")]
    }
    return(Y)
}
".asRow" <-
function (positions) 
if (is.null(dim(positions))) return(positions) else return(t(positions))
".cliquesFromEdges" <-
function (Edges, Vertices, VisibleVertices) 
{
    require(ggm)
    e <- NodeIndices(Edges)
    if (length(e) > 0) {
        e <- lapply(e, function(egde) if (sum(abs(egde)) > 0) 
            egde)
        e <- .removeNull(e)
    }
    else e <- NULL
    factors <- NULL
    if (length(e) < 2) {
        if (length(e) == 1) 
            factors <- append(e, as.list(VisibleVertices))
        else if (length(VisibleVertices) > 0) 
            factors <- as.list(VisibleVertices)
    }
    else {
        n <- Names(Vertices)
        X <- matrix(rep(0, length(n)^2), ncol = length(n))
        lapply(e, function(i) X[i[1], i[2]] <<- 1)
        dimnames(X) <- list(n, n)
        X <- X[VisibleVertices, VisibleVertices]
        factors <- cliques(X + t(X))
    }
    return(factors)
}
".dashReplaceMethod" <-
function (x, value) 
{
    abort <- function() {
        message("Invalid DASH PATTERN, see the Tcl/tk Reference Manual;")
        value <<- x@dash
    }
    if (!is.na(as.logical(value))) {
        if (as.logical(value)) 
            value <- "."
        else value <- " "
    }
    else if (is.character(value)) {
        a <- substring(value, 1:nchar(value), 1:nchar(value))
        A <- c(".", ",", "-", "_")
        if (any(a %in% A)) {
            A <- c(".", ",", "-", "_", " ")
            if (!all(a %in% A)) 
                abort()
        }
        else {
            A <- c(paste(0:9))
            if (any(a %in% A)) {
                A <- c(paste(0:9), " ")
                if (!all(a %in% A)) 
                  abort()
            }
            else abort()
        }
    }
    else abort()
    x@dash <- value
    x
}
".emptyDgList" <-
function (type = "dg.list", n = 0) 
new(type, vector("list", n))
".emptyToNull" <-
function (x) 
{
    if ((length(x) == 1) && is.null(x[[1]])) 
        return(NULL)
    else return(x)
}
".First.lib" <-
function (lib, pkg) 
{
    .onLoad.dynamicGraph()
}
".IsEmpty" <-
function (x) 
{
    if (is.null(x) || (length(x) == 1) && is.null(x[[1]])) 
        return(TRUE)
    else return(FALSE)
}
".newDynamicGraphModelObject" <-
function (object, model = list(object), graphs = list(NULL), 
    title = "", index = 0) 
{
    return(new("DynamicGraphModel", title = title, index = index, 
        model = model, graphs = graphs))
}
".newDynamicGraphObject" <-
function (vertices, blocks = list(NULL), blockTree = list(NULL), 
    models = list(NULL), title = "") 
{
    ".nullToTree" <- function(x) if (is.null(x)) 
        list()
    else return(x)
    return(new("DynamicGraph", title = title, vertices = vertices, 
        blocks = .nullToList(blocks, type = "dg.BlockList"), 
        blockTree = .nullToTree(blockTree), models = models))
}
".nullToEmpty" <-
function (x) 
if (is.null(x)) return(numeric()) else return(x)
".nullToList" <-
function (x, type = "dg.list") 
if (is.null(x)) .emptyDgList(type) else return(x)
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
    setClass("dg.list", contains = "list", representation("list"), 
        prototype(list()))
    setClass("dg.NodeList", contains = "dg.list")
    setClass("dg.VertexList", contains = "dg.NodeList")
    setClass("dg.BlockList", contains = "dg.NodeList")
    setClass("dg.FactorVertexList", contains = "dg.NodeList")
    setClass("dg.EdgeList", contains = "dg.NodeList")
    setClass("dg.VertexEdgeList", contains = "dg.EdgeList")
    setClass("dg.BlockEdgeList", contains = "dg.EdgeList")
    setClass("dg.FactorEdgeList", contains = "dg.EdgeList")
    setClass("dg.ExtraEdgeList", contains = "dg.EdgeList")
    setClass("DynamicGraph", representation(title = "character", 
        vertices = "dg.VertexList", blocks = "dg.BlockList", 
        blockTree = "list", drawModel = "function", models = "list"))
    setClass("DynamicGraphModel", representation(title = "character", 
        index = "numeric", redrawView = "function", model = "list", 
        graphs = "list"))
    setClass("DynamicGraphView", representation(title = "character", 
        index = "numeric", update = "function", top = "list", 
        canvas = "list", viewLabel = "list", tags = "list", id = "numeric", 
        visibleVertices = "numeric", visibleBlocks = "numeric", 
        extraVertices = "dg.VertexList", vertexEdges = "dg.VertexEdgeList", 
        blockEdges = "dg.BlockEdgeList", factorVertices = "dg.FactorVertexList", 
        factorEdges = "dg.FactorEdgeList", extraEdges = "dg.ExtraEdgeList"))
    for (prototype in paste(validViewClasses()[, 2])) setClass(prototype, 
        contains = "DynamicGraphView")
    setClass("dg.Node", representation(color = "character", label = "character", 
        label.position = "numeric"), prototype(color = "black", 
        label = "Label", label.position = c(0, 0, 0)))
    setClass("dg.Vertex", contains = "dg.Node", representation(name = "character", 
        index = "numeric", position = "numeric", blockindex = "numeric", 
        stratum = "numeric"), prototype(name = "Name", index = 0, 
        position = c(0, 0, 0), blockindex = 0, stratum = 0))
    for (prototype in paste(validVertexClasses()[, 2])) setClass(prototype, 
        contains = "dg.Vertex")
    setClass("dg.TextVertex", contains = "dg.Vertex")
    setClass("dg.Block", contains = "dg.Node", representation(stratum = "numeric", 
        index = "numeric", ancestors = "numeric", descendants = "numeric", 
        position = "matrix", closed = "logical", visible = "logical"), 
        prototype(stratum = 0, index = 0, ancestors = 0, descendants = 0, 
            position = matrix(rep(0, 6), ncol = 3), closed = TRUE, 
            visible = TRUE, color = "black", label = "Label", 
            label.position = c(0, 0, 0)))
    setClass("dg.FactorVertex", contains = "dg.Vertex", representation(vertex.indices = "numeric"), 
        prototype(vertex.indices = c(0, 0)))
    for (prototype in paste(validFactorClasses()[, 2])) setClass(prototype, 
        contains = "dg.FactorVertex")
    setClass("dg.Edge", contains = "dg.Node", representation(dash = "character", 
        vertex.indices = "numeric", width = "numeric"), prototype(dash = "", 
        vertex.indices = c(0, 0), width = 2))
    setClass("dg.VertexEdge", contains = "dg.Edge", representation(oriented = "logical"), 
        prototype(oriented = FALSE))
    for (prototype in paste(validEdgeClasses()[-1, 2])) setClass(prototype, 
        contains = "dg.VertexEdge")
    setClass("dg.FactorEdge", contains = "dg.Edge", representation(), 
        prototype())
    setClass("dg.ExtraEdge", contains = "dg.Edge", representation(), 
        prototype())
    setClass("dg.BlockEdge", contains = "dg.Edge", representation(oriented = "logical"), 
        prototype(oriented = TRUE))
    if (!isGeneric("color")) {
        if (is.function("color")) 
            fun <- color
        else fun <- function(object) standardGeneric("color")
        setGeneric("color", fun)
    }
    setMethod("color", "dg.Node", function(object) object@color)
    setGeneric("color<-", function(x, value) standardGeneric("color<-"))
    setReplaceMethod("color", "dg.Node", function(x, value) {
        if (!(value %in% colors())) 
            message("Invalid color;")
        else x@color <- value
        x
    })
    if (!isGeneric("label")) {
        if (is.function("label")) 
            fun <- label
        else fun <- function(object) standardGeneric("label")
        setGeneric("label", fun)
    }
    setMethod("label", "dg.Node", function(object) object@label)
    setGeneric("label<-", function(x, value) standardGeneric("label<-"))
    setReplaceMethod("label", "dg.Node", function(x, value) {
        x@label <- value
        x
    })
    if (!isGeneric("labelPosition")) {
        if (is.function("labelPosition")) 
            fun <- labelPosition
        else fun <- function(object) standardGeneric("labelPosition")
        setGeneric("labelPosition", fun)
    }
    setMethod("labelPosition", "dg.Node", function(object) object@label.position)
    setGeneric("labelPosition<-", function(x, value) standardGeneric("labelPosition<-"))
    setReplaceMethod("labelPosition", "dg.Node", function(x, 
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
    setMethod("name", "dg.Vertex", function(object) object@name)
    setMethod("name", "dg.Edge", function(object) object@label)
    setMethod("name", "dg.Block", function(object) object@label)
    setGeneric("name<-", function(x, value) standardGeneric("name<-"))
    setReplaceMethod("name", "dg.Vertex", function(x, value) {
        x@name <- value
        x
    })
    if (!isGeneric("index")) {
        if (is.function("index")) 
            fun <- index
        else fun <- function(object) standardGeneric("index")
        setGeneric("index", fun)
    }
    setMethod("index", "dg.Vertex", function(object) object@index)
    setMethod("index", "dg.Block", function(object) abs(object@index))
    setMethod("index", "dg.FactorVertex", function(object) abs(object@index))
    setGeneric("index<-", function(x, value) standardGeneric("index<-"))
    setReplaceMethod("index", "dg.Vertex", function(x, value) {
        x@index <- value
        x
    })
    setReplaceMethod("index", "dg.Block", function(x, value) {
        x@index <- -abs(value)
        x
    })
    setReplaceMethod("index", "dg.FactorVertex", function(x, 
        value) {
        x@index <- -abs(value)
        x
    })
    if (!isGeneric("nodeIndices")) {
        if (is.function("nodeIndices")) 
            fun <- nodeIndices
        else fun <- function(object) standardGeneric("nodeIndices")
        setGeneric("nodeIndices", fun)
    }
    setMethod("nodeIndices", "dg.FactorVertex", function(object) object@vertex.indices)
    setMethod("nodeIndices", "dg.Edge", function(object) object@vertex.indices)
    setGeneric("nodeIndices<-", function(x, value) standardGeneric("nodeIndices<-"))
    setReplaceMethod("nodeIndices", "dg.FactorVertex", function(x, 
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
    setMethod("position", "dg.Vertex", function(object) object@position)
    setGeneric("position<-", function(x, value) standardGeneric("position<-"))
    setReplaceMethod("position", "dg.Vertex", function(x, value) {
        x@position <- value
        x
    })
    setMethod("position", "dg.Block", function(object) t(object@position))
    setReplaceMethod("position", "dg.Block", function(x, value) {
        x@position <- t(value)
        x
    })
    if (!isGeneric("stratum")) {
        if (is.function("stratum")) 
            fun <- stratum
        else fun <- function(object) standardGeneric("stratum")
        setGeneric("stratum", fun)
    }
    setMethod("stratum", "dg.Vertex", function(object) object@stratum)
    setGeneric("stratum<-", function(x, value) standardGeneric("stratum<-"))
    setReplaceMethod("stratum", "dg.Vertex", function(x, value) {
        x@stratum <- value
        x
    })
    setMethod("stratum", "dg.Block", function(object) object@stratum)
    setReplaceMethod("stratum", "dg.Block", function(x, value) {
        x@stratum <- value
        x
    })
    if (!isGeneric("blockindex")) {
        if (is.function("blockindex")) 
            fun <- blockindex
        else fun <- function(object) standardGeneric("blockindex")
        setGeneric("blockindex", fun)
    }
    setMethod("blockindex", "dg.Vertex", function(object) object@blockindex)
    setGeneric("blockindex<-", function(x, value) standardGeneric("blockindex<-"))
    setReplaceMethod("blockindex", "dg.Vertex", function(x, value) {
        x@blockindex <- value
        x
    })
    if (!isGeneric("ancestors")) {
        if (is.function("ancestors")) 
            fun <- ancestors
        else fun <- function(object, blockList = NULL, vertexList = NULL, 
            ...) standardGeneric("ancestors")
        setGeneric("ancestors", fun)
    }
    setMethod("ancestors", "dg.Vertex", function(object, blockList = NULL, 
        vertexList = NULL, ...) warning("Not implemented"))
    setGeneric("ancestors<-", function(x, value) standardGeneric("ancestors<-"))
    setReplaceMethod("ancestors", "dg.Vertex", function(x, value) {
        warning("Not implemented")
    })
    setMethod("ancestors", "dg.Block", function(object, blockList = NULL, 
        vertexList = NULL, ...) object@ancestors)
    setReplaceMethod("ancestors", "dg.Block", function(x, value) {
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
    setMethod("descendants", "dg.Vertex", function(object, blockList = NULL, 
        vertexList = NULL, ...) warning("Not implemented"))
    setGeneric("descendants<-", function(x, value) standardGeneric("descendants<-"))
    setReplaceMethod("descendants", "dg.Vertex", function(x, 
        value) {
        warning("Not implemented")
    })
    setMethod("descendants", "dg.Block", function(object, blockList = NULL, 
        vertexList = NULL, ...) object@descendants)
    setReplaceMethod("descendants", "dg.Block", function(x, value) {
        x@descendants <- value
        x
    })
    if (!isGeneric("closed")) {
        if (is.function("closed")) 
            fun <- closed
        else fun <- function(object) standardGeneric("closed")
        setGeneric("closed", fun)
    }
    setMethod("closed", "dg.Block", function(object) object@closed)
    setGeneric("closed<-", function(x, value) standardGeneric("closed<-"))
    setReplaceMethod("closed", "dg.Block", function(x, value) {
        x@closed <- value
        x
    })
    if (!isGeneric("visible")) {
        if (is.function("visible")) 
            fun <- visible
        else fun <- function(object) standardGeneric("visible")
        setGeneric("visible", fun)
    }
    setMethod("visible", "dg.Vertex", function(object) warning("Not implemented"))
    setGeneric("visible<-", function(x, value) standardGeneric("visible<-"))
    setReplaceMethod("visible", "dg.Vertex", function(x, value) {
        warning("Not implemented")
    })
    setMethod("visible", "dg.Block", function(object) object@visible)
    setReplaceMethod("visible", "dg.Block", function(x, value) {
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
    setMethod("draw", "dg.TextVertex", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "black", background = "white") {
        s <- w * sqrt(4/pi)/10
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object), activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.DiscreteVertex", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(4/pi)
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object), activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.OrdinalVertex", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        p <- tkcreate(canvas, "rectangle", x - w, y - w, x + 
            w, y + w, fill = color(object), activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.ContinuousVertex", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(4/pi)
        p <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = color(object), activefill = "IndianRed")
        s <- 0.6 * w * sqrt(4/pi)
        q <- tkcreate(canvas, "oval", x - s, y - s, x + s, y + 
            s, fill = background)
        return(list(dynamic = list(p), fixed = list(q)))
    })
    if (!isGeneric("propertyDialog")) {
        if (is.function("propertyDialog")) 
            fun <- propertyDialog
        else fun <- function(object, classes = NULL, title = class(object), 
            sub.title = label(object), name.object = name(object), 
            okReturn = TRUE, fixedSlots = NULL, difficultSlots = NULL, 
            top = NULL, entryWidth = 20, do.grab = FALSE) standardGeneric("propertyDialog")
        setGeneric("propertyDialog", fun)
    }
    setMethod("propertyDialog", "dg.Node", function(object, classes = NULL, 
        title = class(object), sub.title = label(object), name.object = name(object), 
        okReturn = TRUE, fixedSlots = NULL, difficultSlots = NULL, 
        top = NULL, entryWidth = 20, do.grab = FALSE) {
        .propertyDialog(object, classes = classes, title = title, 
            sub.title = sub.title, name.object = name.object, 
            okReturn = okReturn, fixedSlots = fixedSlots, difficultSlots = difficultSlots, 
            top = top, entryWidth = entryWidth, do.grab = do.grab)
    })
    if (!isGeneric("addToPopups")) {
        if (is.function("addToPopups")) 
            fun <- draw
        else fun <- function(object, type, nodePopupMenu, i, 
            updateArguments, Args, ...) standardGeneric("addToPopups")
        setGeneric("addToPopups", fun)
    }
    setMethod("addToPopups", "dg.Node", function(object, type, 
        nodePopupMenu, i, updateArguments, Args, ...) {
        if ((type == "Factor")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a factor!"), 
                command = function() {
                  str(i)
                })
        if ((type == "Vertex")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a vertex!"), 
                command = function() {
                  print(i)
                  updateArguments(NULL)
                  Arguments = Args()
                  print(Arguments$vertexList[[i]]@label)
                })
        else if ((type == "OpenBlock")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an open block!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "ClosedBlock")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a closed block!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "Edge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an edge!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "VertexEdge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a graph edge!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "FactorEdge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a factor edge!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "ExtraEdge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a extra edge!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "BlockEdge")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is a block edge!"), 
                command = function() {
                  str(i)
                })
        else if ((type == "Extra")) 
            tkadd(nodePopupMenu, "command", label = paste(" --- This is an extra!"), 
                command = function() {
                  str(i)
                })
        else tkadd(nodePopupMenu, "command", label = paste(" --- This is ??? "), 
            command = function() {
                str(i)
            })
    })
    setMethod("draw", "dg.Generator", function(object, canvas, 
        position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - 0, y + s, x + s, 
            y + 0, x + 0, y - s, x - s, y - 0, fill = color(object), 
            activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.DiscreteGenerator", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object), 
            activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.LinearGenerator", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object), 
            activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.QuadraticGenerator", function(object, 
        canvas, position, x = position[1], y = position[2], stratum = 0, 
        w = 2, color = "green", background = "white") {
        s <- w * sqrt(2)
        p <- tkcreate(canvas, "polygon", x - w, y + w, x + w, 
            y + w, x + w, y - w, x - w, y - w, fill = color(object), 
            activefill = "IndianRed")
        return(list(dynamic = list(p), fixed = NULL))
    })
    setMethod("draw", "dg.Edge", function(object, canvas, position, 
        x = lapply(position, function(e) e[1]), y = lapply(position, 
            function(e) e[2]), stratum = as.vector(rep(0, length(position)), 
            mode = "list"), w = 2, color = "green", background = "white", 
        font.edge.label = "8x16") {
        f <- function(i, j) {
            arrowhead <- "none"
            if ((class(object) == "dg.VertexEdge") || (class(object) == 
                "dg.BlockEdge")) 
                if (object@oriented) 
                  arrowhead <- "last"
                else if (stratum[[i]] == stratum[[j]]) 
                  arrowhead <- "none"
                else if (stratum[[i]] < stratum[[j]]) 
                  arrowhead <- "last"
                else arrowhead <- "first"
            if (class(object) == "dg.DoubleArrowEdge") 
                arrowhead <- "both"
            dash <- dash(object)
            if (class(object) == "dg.DashedEdge") 
                dash <- "-"
            l <- function(xi, yi, xj, yj) tkcreate(canvas, "line", 
                xi, yi, xj, yj, width = w, arrow = arrowhead, 
                dash = dash, fill = color(object), activefill = "DarkSlateGray")
            if ((class(object) == "dg.DoubleConnectedEdge") || 
                (class(object) == "dg.TripleConnectedEdge")) {
                dx <- x[[i]] - x[[j]]
                dy <- y[[i]] - y[[j]]
                ld <- sqrt(dx^2 + dy^2)
                dx <- w * dx/ld
                dy <- w * dy/ld
                line1 <- l(x[[i]] - dy - dx, y[[i]] + dx - dy, 
                  x[[j]] - dy + dx, y[[j]] + dx + dy)
                line2 <- l(x[[i]] + dy - dx, y[[i]] - dx - dy, 
                  x[[j]] + dy + dx, y[[j]] - dx + dy)
                if ((class(object) == "dg.TripleConnectedEdge")) {
                  line0 <- l(x[[i]], y[[i]], x[[j]], y[[j]])
                  lines <- list(line1, line0, line2)
                }
                else lines <- list(line1, line2)
            }
            else lines <- list(l(x[[i]], y[[i]], x[[j]], y[[j]]))
            label.position <- (position[[i]] + position[[j]])/2
            pos <- label.position + rep(0, length(label.position))
            label <- tkcreate(canvas, "text", pos[1], pos[2], 
                text = object@label, anchor = "nw", font = font.edge.label, 
                activefill = "DarkSlateGray")
            tags <- NULL
            if (class(object) == "dg.DottedEdge") {
                x. <- mean(unlist(x))
                y. <- mean(unlist(y))
                s <- w * sqrt(4/pi)
                p <- tkcreate(canvas, "oval", x. - s, y. - s, 
                  x. + s, y. + s, fill = color(object), activefill = "SeaGreen")
                tags <- list(p)
            }
            return(list(lines = lines, tags = tags, from = object@vertex.indices[i], 
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
    if (!isGeneric("width")) {
        if (is.function("width")) 
            fun <- width
        else fun <- function(object) standardGeneric("width")
        setGeneric("width", fun)
    }
    setMethod("width", "dg.Edge", function(object) object@width)
    setGeneric("width<-", function(x, value) standardGeneric("width<-"))
    setReplaceMethod("width", "dg.Edge", function(x, value) {
        x@width <- value
        x
    })
    if (!isGeneric("dash")) {
        if (is.function("dash")) 
            fun <- dash
        else fun <- function(object) standardGeneric("dash")
        setGeneric("dash", fun)
    }
    setMethod("dash", "dg.Edge", function(object) object@dash)
    setGeneric("dash<-", function(x, value) standardGeneric("dash<-"))
    setReplaceMethod("dash", "dg.Edge", function(x, value) {
        .dashReplaceMethod(x, value)
    })
    if (!isGeneric("oriented")) {
        if (is.function("oriented")) 
            fun <- oriented
        else fun <- function(object) standardGeneric("oriented")
        setGeneric("oriented", fun)
    }
    setMethod("oriented", "dg.VertexEdge", function(object) object@oriented)
    setGeneric("oriented<-", function(x, value) standardGeneric("oriented<-"))
    setReplaceMethod("oriented", "dg.VertexEdge", function(x, 
        value) {
        x@oriented <- value
        x
    })
    setMethod("oriented", "dg.BlockEdge", function(object) object@oriented)
    setReplaceMethod("oriented", "dg.BlockEdge", function(x, 
        value) {
        x@oriented <- value
        x
    })
    if (!isGeneric("nodeTypesOfEdge")) {
        if (is.function("nodeTypesOfEdge")) 
            fun <- nodeTypesOfEdge
        else fun <- function(object) standardGeneric("nodeTypesOfEdge")
        setGeneric("nodeTypesOfEdge", fun)
    }
    setMethod("nodeTypesOfEdge", "dg.VertexEdge", function(object) rep("Vertex", 
        length(object@vertex.indices)))
    setMethod("nodeTypesOfEdge", "dg.FactorEdge", function(object) ifelse(object@vertex.indices > 
        0, "Vertex", "Factor"))
    setMethod("nodeTypesOfEdge", "dg.ExtraEdge", function(object) ifelse(object@vertex.indices > 
        0, "Vertex", "Extra"))
    setMethod("nodeTypesOfEdge", "dg.BlockEdge", function(object) ifelse(object@vertex.indices > 
        0, "Vertex", "Block"))
    if (!isGeneric("nodeIndicesOfEdge")) {
        if (is.function("nodeIndicesOfEdge")) 
            fun <- nodeIndicesOfEdge
        else fun <- function(object) standardGeneric("nodeIndicesOfEdge")
        setGeneric("nodeIndicesOfEdge", fun)
    }
    setMethod("nodeIndicesOfEdge", "dg.Edge", function(object) object@vertex.indices)
    setGeneric("nodeIndicesOfEdge<-", function(x, value) standardGeneric("nodeIndicesOfEdge<-"))
    setReplaceMethod("nodeIndicesOfEdge", "dg.Edge", function(x, 
        value) {
        x@vertex.indices <- value
        x
    })
    setMethod("draw", "dg.Block", function(object, canvas, position, 
        x = position[1], y = position[2], stratum = 0, w = 10, 
        color = "green", background = "white") {
        s <- w
        p <- tkcreate(canvas, "rectangle", x - s, y - s, x + 
            s, y + s, fill = color(object), activefill = "IndianRed")
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
    setMethod("Names", "dg.list", function(objects) {
        NAMES <- lapply(objects, function(x) if (!is.null(x)) 
            name(x))
        names(NAMES) <- NULL
        return(unlist(NAMES))
    })
    setGeneric("Names<-", function(objectlist, value) standardGeneric("Names<-"))
    setReplaceMethod("Names", "dg.list", function(objectlist, 
        value) {
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
    setMethod("Colors", "dg.list", function(objectlist) {
        Colors <- lapply(objectlist, function(x) if (!is.null(x)) 
            color(x))
        names(Colors) <- Names(objectlist)
        return(unlist(Colors))
    })
    setGeneric("Colors<-", function(objectlist, value) standardGeneric("Colors<-"))
    setReplaceMethod("Colors", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@color <- value[i]
        }
        else warning("Invalid list of values for colors")
        objectlist
    })
    if (!isGeneric("Dashes")) {
        if (is.function("Dashes")) 
            fun <- Dashes
        else fun <- function(objectlist) standardGeneric("Dashes")
        setGeneric("Dashes", fun)
    }
    setMethod("Dashes", "dg.list", function(objectlist) {
        Dashes <- lapply(objectlist, function(x) if (!is.null(x)) 
            dash(x))
        names(Dashes) <- Names(objectlist)
        return(unlist(Dashes))
    })
    setGeneric("Dashes<-", function(objectlist, value) standardGeneric("Dashes<-"))
    setReplaceMethod("Dashes", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@dash <- value[i]
        }
        else warning("Invalid list of values for dashs")
        objectlist
    })
    if (!isGeneric("Labels")) {
        if (is.function("Labels")) 
            fun <- Labels
        else fun <- function(objectlist) standardGeneric("Labels")
        setGeneric("Labels", fun)
    }
    setMethod("Labels", "dg.list", function(objectlist) {
        Labels <- lapply(objectlist, function(x) if (!is.null(x)) 
            label(x))
        names(Labels) <- Names(objectlist)
        return(unlist(Labels))
    })
    setGeneric("Labels<-", function(objectlist, value) standardGeneric("Labels<-"))
    setReplaceMethod("Labels", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@label <- value[i]
        }
        else warning("Invalid list of values for labels")
        objectlist
    })
    if (!isGeneric("LabelPositions")) {
        if (is.function("LabelPositions")) 
            fun <- LabelPositions
        else fun <- function(objectlist) standardGeneric("LabelPositions")
        setGeneric("LabelPositions", fun)
    }
    setMethod("LabelPositions", "dg.list", function(objectlist) {
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
    setReplaceMethod("LabelPositions", "dg.list", function(objectlist, 
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
            fun <- Positions
        else fun <- function(objectlist) standardGeneric("Positions")
        setGeneric("Positions", fun)
    }
    setMethod("Positions", "dg.list", function(objectlist) {
        if (length(objectlist) == 0) 
            return(NULL)
        positions <- lapply(objectlist, function(x) if (!is.null(x)) 
            position(x))
        if (class(objectlist[[1]]) == "dg.Block") {
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
    setReplaceMethod("Positions", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == nrow(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@position <- value[i, 
                ]
        }
        else warning("Invalid list of values for positions")
        objectlist
    })
    if (!isGeneric("Closed")) {
        if (is.function("Closed")) 
            fun <- Closed
        else fun <- function(objectlist) standardGeneric("Closed")
        setGeneric("Closed", fun)
    }
    setMethod("Closed", "dg.list", function(objectlist) {
        closed <- lapply(objectlist, function(x) if (!is.null(x)) 
            closed(x))
        names(closed) <- Names(objectlist)
        return(unlist(closed))
    })
    setGeneric("Closed<-", function(objectlist, value) standardGeneric("Closed<-"))
    setReplaceMethod("Closed", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@closed <- value[i]
        }
        else warning("Invalid list of values for closed")
        objectlist
    })
    if (!isGeneric("Strata")) {
        if (is.function("Strata")) 
            fun <- Strata
        else fun <- function(objectlist) standardGeneric("Strata")
        setGeneric("Strata", fun)
    }
    setMethod("Strata", "dg.list", function(objectlist) {
        strata <- lapply(objectlist, function(x) if (!is.null(x)) 
            stratum(x))
        names(strata) <- Names(objectlist)
        return(unlist(strata))
    })
    setGeneric("Strata<-", function(objectlist, value) standardGeneric("Strata<-"))
    setReplaceMethod("Strata", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@stratum <- value[i]
        }
        else warning("Invalid list of values for strata")
        objectlist
    })
    if (!isGeneric("NodeAncestors")) {
        if (is.function("NodeAncestors")) 
            fun <- NodeAncestors
        else fun <- function(objectlist) standardGeneric("NodeAncestors")
        setGeneric("NodeAncestors", fun)
    }
    setMethod("NodeAncestors", "dg.list", function(objectlist) {
        ancestors <- lapply(objectlist, function(x) if (!is.null(x)) 
            ancestors(x))
        names(ancestors) <- Names(objectlist)
        return(ancestors)
    })
    setGeneric("NodeAncestors<-", function(objectlist, value) standardGeneric("NodeAncestors<-"))
    setReplaceMethod("NodeAncestors", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@ancestors <- value[i]
        }
        else warning("Invalid list of values for ancestors")
        objectlist
    })
    if (!isGeneric("NodeDescendants")) {
        if (is.function("NodeDescendants")) 
            fun <- NodeDescendants
        else fun <- function(objectlist) standardGeneric("NodeDescendants")
        setGeneric("NodeDescendants", fun)
    }
    setMethod("NodeDescendants", "dg.list", function(objectlist) {
        descendants <- lapply(objectlist, function(x) if (!is.null(x)) 
            descendants(x))
        names(descendants) <- Names(objectlist)
        return(descendants)
    })
    setGeneric("NodeDescendants<-", function(objectlist, value) standardGeneric("NodeDescendants<-"))
    setReplaceMethod("NodeDescendants", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@descendants <- value[i]
        }
        else warning("Invalid list of values for descendants")
        objectlist
    })
    if (!isGeneric("Indices")) {
        if (is.function("Indices")) 
            fun <- Indices
        else fun <- function(objectlist) standardGeneric("Indices")
        setGeneric("Indices", fun)
    }
    setMethod("Indices", "dg.list", function(objectlist) {
        indices <- lapply(objectlist, function(x) if (!is.null(x)) 
            index(x))
        names(indices) <- Names(objectlist)
        return(unlist(indices))
    })
    if (!isGeneric("Blockindices")) {
        if (is.function("Blockindices")) 
            fun <- Blockindices
        else fun <- function(objectlist) standardGeneric("Blockindices")
        setGeneric("Blockindices", fun)
    }
    setMethod("Blockindices", "dg.list", function(objectlist) {
        blockindices <- lapply(objectlist, function(x) if (!is.null(x)) 
            blockindex(x))
        names(blockindices) <- Names(objectlist)
        return(unlist(blockindices))
    })
    if (!isGeneric("NodeTypes")) {
        if (is.function("NodeTypes")) 
            fun <- NodeTypes
        else fun <- function(objectlist) standardGeneric("NodeTypes")
        setGeneric("NodeTypes", fun)
    }
    setMethod("NodeTypes", "dg.list", function(objectlist) {
        indices <- lapply(objectlist, function(x) if (!is.null(x)) 
            nodeTypesOfEdge(x))
        names(indices) <- Names(objectlist)
        return(indices)
    })
    if (!isGeneric("NodeIndices")) {
        if (is.function("NodeIndices")) 
            fun <- NodeIndices
        else fun <- function(objectlist) standardGeneric("NodeIndices")
        setGeneric("NodeIndices", fun)
    }
    setMethod("NodeIndices", "dg.list", function(objectlist) {
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
    setMethod("Widths", "dg.list", function(objectlist) {
        widths <- lapply(objectlist, function(x) if (!is.null(x)) 
            width(x))
        names(widths) <- Names(objectlist)
        return(unlist(widths))
    })
    setGeneric("Widths<-", function(objectlist, value) standardGeneric("Widths<-"))
    setReplaceMethod("Widths", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@width <- value[i]
        }
        else warning("Invalid list of values for widths")
        objectlist
    })
    if (!isGeneric("Oriented")) {
        if (is.function("Oriented")) 
            fun <- Oriented
        else fun <- function(objectlist) standardGeneric("Oriented")
        setGeneric("Oriented", fun)
    }
    setMethod("Oriented", "dg.list", function(objectlist) {
        oriented <- lapply(objectlist, function(x) if (!is.null(x)) 
            oriented(x))
        names(oriented) <- Names(objectlist)
        return(unlist(oriented))
    })
    setGeneric("Oriented<-", function(objectlist, value) standardGeneric("Oriented<-"))
    setReplaceMethod("Oriented", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@oriented <- value[i]
        }
        else warning("Invalid list of values for oriented")
        objectlist
    })
    if (!isGeneric("Visible")) {
        if (is.function("Visible")) 
            fun <- Visible
        else fun <- function(objectlist) standardGeneric("Visible")
        setGeneric("Visible", fun)
    }
    setMethod("Visible", "dg.list", function(objectlist) {
        visible <- lapply(objectlist, function(x) if (!is.null(x)) 
            visible(x))
        names(visible) <- Names(objectlist)
        return(unlist(visible))
    })
    setGeneric("Visible<-", function(objectlist, value) standardGeneric("Visible<-"))
    setReplaceMethod("Visible", "dg.list", function(objectlist, 
        value) {
        if (length(objectlist) == length(value)) {
            for (i in seq(along = objectlist)) objectlist[[i]]@visible <- value[i]
        }
        else warning("Invalid list of values for visible")
        objectlist
    })
    setClass("dg.Model", representation(name = "character", visibleVertices = "numeric", 
        visibleBlocks = "numeric", extraVertices = "dg.VertexList", 
        vertexEdges = "dg.VertexEdgeList", blockEdges = "dg.BlockEdgeList", 
        factorVertices = "dg.FactorVertexList", factorEdges = "dg.FactorEdgeList", 
        extraEdges = "dg.ExtraEdgeList"))
    "newDefaultModelObject" <- function(name) {
        result <- new("dg.Model", name = name, extraVertices = .emptyDgList("dg.VertexList"), 
            vertexEdges = .emptyDgList("dg.VertexEdgeList"), 
            blockEdges = .emptyDgList("dg.BlockEdgeList"), factorVertices = .emptyDgList("dg.FactorVertexList"), 
            factorEdges = .emptyDgList("dg.FactorEdgeList"), 
            extraEdges = .emptyDgList("dg.ExtraEdgeList"))
        return(result)
    }
    if (!isGeneric("vertexEdges")) {
        if (is.function("vertexEdges")) 
            fun <- vertexEdges
        else fun <- function(object) standardGeneric("vertexEdges")
        setGeneric("vertexEdges", fun)
    }
    setMethod("vertexEdges", "dg.Model", function(object) object@vertexEdges)
    setGeneric("vertexEdges<-", function(x, value) standardGeneric("vertexEdges<-"))
    setReplaceMethod("vertexEdges", "dg.Model", function(x, value) {
        x@vertexEdges <- value
        x
    })
    if (!isGeneric("graphComponents")) {
        if (is.function("graphComponents")) 
            fun <- graphComponents
        else fun <- function(object, viewType = NULL, ...) standardGeneric("graphComponents")
        setGeneric("graphComponents", fun)
    }
    setMethod("graphComponents", "dg.Model", function(object, 
        viewType = NULL, ...) {
        args <- list(...)
        Args <- args$Arguments
        Edges <- object@vertexEdges
        Vertices <- Args$vertexList
        VisibleVertices <- object@visibleVertices
        if (viewType == "Factor") {
            factors <- .cliquesFromEdges(Edges, Vertices, VisibleVertices)
            if (is.null(factors) || (length(factors) == 0)) {
                FactorVertices <- .emptyDgList("dg.FactorVertexList")
                FactorEdges <- .emptyDgList("dg.FactorEdgeList")
            }
            else {
                result <- returnFactorVerticesAndEdges(Vertices, 
                  factors)
                FactorVertices <- result$FactorVertices
                FactorEdges <- result$FactorEdges
            }
            list(vertexEdges = object@vertexEdges, blockEdges = object@blockEdges, 
                factorVertices = FactorVertices, factorEdges = FactorEdges, 
                visibleVertices = object@visibleVertices, visibleBlocks = object@visibleBlocks, 
                extraVertices = object@extraVertices, extraEdges = object@extraEdges)
        }
        else if (viewType == "Moral") {
            message("Moral view not implemented; ")
            list(vertexEdges = object@vertexEdges, blockEdges = .emptyDgList("dg.BlockEdgeList"), 
                factorVertices = .emptyDgList("dg.FactorVertexList"), 
                factorEdges = .emptyDgList("dg.FactorEdgeList"), 
                visibleVertices = object@visibleVertices, visibleBlocks = numeric(), 
                extraVertices = object@extraVertices, extraEdges = object@extraEdges)
        }
        else if (viewType == "Essential") {
            message("Essential view not implemented; ")
            list(vertexEdges = object@vertexEdges, blockEdges = .emptyDgList("dg.BlockEdgeList"), 
                factorVertices = .emptyDgList("dg.FactorVertexList"), 
                factorEdges = .emptyDgList("dg.FactorEdgeList"), 
                visibleVertices = object@visibleVertices, visibleBlocks = numeric(), 
                extraVertices = object@extraVertices, extraEdges = object@extraEdges)
        }
        else if (viewType == "Simple") {
            list(vertexEdges = object@vertexEdges, blockEdges = object@blockEdges, 
                factorVertices = .emptyDgList("dg.FactorVertexList"), 
                factorEdges = .emptyDgList("dg.FactorEdgeList"), 
                visibleVertices = object@visibleVertices, visibleBlocks = object@visibleBlocks, 
                extraVertices = object@extraVertices, extraEdges = object@extraEdges)
        }
        else message("View type not implemented; ")
    })
    if (!isGeneric("setGraphComponents")) {
        if (is.function("setGraphComponents")) 
            fun <- setGraphComponents
        else fun <- function(object, viewType = NULL, visibleVertices = NULL, 
            extraVertices = NULL, vertexEdges = NULL, blockEdges = NULL, 
            factorVertices = NULL, factorEdges = NULL, extraEdges = NULL, 
            ...) standardGeneric("setGraphComponents")
        setGeneric("setGraphComponents", fun)
    }
    setMethod("setGraphComponents", signature(object = "dg.Model"), 
        function(object, viewType = NULL, visibleVertices = NULL, 
            visibleBlocks = NULL, extraVertices = NULL, vertexEdges = NULL, 
            blockEdges = NULL, factorVertices = NULL, factorEdges = NULL, 
            extraEdges = NULL, ...) {
            if (!is.null(visibleVertices)) 
                object@visibleVertices <- visibleVertices
            if (!(viewType == "Moral")) 
                if (!is.null(visibleBlocks)) 
                  object@visibleBlocks <- visibleBlocks
            if (!is.null(extraVertices)) 
                object@extraVertices <- extraVertices
            if (!is.null(vertexEdges)) 
                object@vertexEdges <- vertexEdges
            if (!is.null(blockEdges)) 
                object@blockEdges <- blockEdges
            if ((viewType == "Factor")) {
                if (!is.null(factorVertices)) 
                  object@factorVertices <- factorVertices
                if (!is.null(factorEdges)) 
                  object@factorEdges <- factorEdges
            }
            return(object)
        })
    setClass("dg.Test", representation(deviance = "numeric", 
        df = "numeric", p = "numeric"))
    "newDefaultTestObject" <- function(name) {
        df <- round(runif(1, 1, 25))
        message("Just generating a random test!!!!!")
        deviance <- rchisq(1, df)
        p <- 1 - pchisq(deviance, df)
        result <- new("dg.Test", df = df, deviance = deviance, 
            p = p)
        return(result)
    }
    if (!isGeneric("asDataFrame")) {
        if (is.function("asDataFrame")) 
            fun <- asDataFrame
        else fun <- function(objectlist, setRowLabels = FALSE, 
            ...) standardGeneric("asDataFrame")
        setGeneric("asDataFrame", fun)
    }
    setMethod("asDataFrame", "dg.list", function(objectlist, 
        setRowLabels = FALSE, ...) {
        .asDataFrame(objectlist, setRowLabels, ...)
    })
    if (!isGeneric("Str")) {
        if (is.function("Str")) 
            fun <- Str
        else fun <- function(object, setRowLabels = FALSE, title = "", 
            ...) standardGeneric("Str")
        setGeneric("Str", fun)
    }
    setMethod("Str", "NULL", function(object, setRowLabels = FALSE, 
        title = "", ...) message(paste(title, "NULL", sep = ": ")))
    setMethod("Str", "integer", function(object, setRowLabels = FALSE, 
        title = "", ...) message(paste(title, paste(object, collapse = ", "), 
        sep = ": ")))
    setMethod("Str", "numeric", function(object, setRowLabels = FALSE, 
        title = "", ...) message(paste(title, paste(object, collapse = ", "), 
        sep = ": ")))
    setMethod("show", "dg.list", function(object) Str(object))
    setMethod("Str", "list", function(object, setRowLabels = TRUE, 
        title = "", ...) {
        if ((length(object) > 0) && (!is.null(object[[1]]))) 
            if ((extends(class(object[[1]]), "dg.Node")) || (extends(class(object[[1]]), 
                "dg.Vertex")) || (extends(class(object[[1]]), 
                "dg.VertexEdge"))) {
                if (class(object) == "list") 
                  message("<<<<< List-object not 'dg.list' !!! >>>>>")
                if ((extends(class(object[[1]]), "dg.Block")) && 
                  (length(object) == 2) && (is.list(object[[2]]))) {
                  .StrBlockTree(object, title)
                }
                else {
                  Y <- asDataFrame(object, setRowLabels = setRowLabels, 
                    ...)
                  if ("label" %in% dimnames(Y)[[2]]) {
                    labels <- t(Y["label"])
                    if (length(unique(match(labels, labels))) < 
                      length(labels)) 
                      labels <- paste(title, labels, 1:length(labels), 
                        sep = ".")
                    else labels <- paste(title, labels, sep = ".")
                    dimnames(Y)[[1]] <- labels
                  }
                  print(Y)
                }
            }
            else NextMethod("str", object, ...)
    })
    setMethod("show", "DynamicGraphView", function(object) Str(object))
    setMethod("Str", "DynamicGraphView", function(object, setRowLabels = FALSE, 
        title = "", m = 0, ...) {
        message(paste(rep("-", 80)))
        sub.title <- paste("<<", object@title, " | ", m, object@index, 
            ">>", collapse = " ")
        nc <- 78 - nchar(title) - nchar(sub.title)
        if (nc < 0) 
            nc <- 1
        cat(paste(title, paste(rep(" ", nc), collapse = ""), 
            sub.title, "\n", collapse = ""))
        if (!is.null(object)) {
            Str(object@visibleVertices, title = "visibleVertices", 
                ...)
            Str(object@visibleBlocks, title = "visibleBlocks", 
                ...)
            Str(object@extraVertices, title = "extraVertices", 
                ...)
            Str(object@extraEdges, title = "extraEdges", ...)
            Str(object@vertexEdges, title = "vertexEdges", ...)
            Str(object@blockEdges, title = "blockEdges", ...)
            Str(object@factorVertices, title = "factorVertices", 
                ...)
            Str(object@factorEdges, title = "factorEdges", ...)
        }
    })
    setMethod("Str", "dg.Model", function(object, setRowLabels = FALSE, 
        title = "", ...) {
        message(object@name)
    })
    setMethod("show", "DynamicGraphModel", function(object) Str(object))
    setMethod("Str", "DynamicGraphModel", function(object, setRowLabels = FALSE, 
        title = "", ...) {
        if (!is.null(object)) {
            message(paste(rep("=", 80)))
            sub.title <- paste("<<", object@title, " | ", object@index, 
                ">>", collapse = " ")
            nc <- 78 - nchar(title) - nchar(sub.title)
            if (nc < 0) 
                nc <- 1
            cat(paste(title, paste(rep(" ", nc), collapse = ""), 
                sub.title, "\n", collapse = ""))
            if (hasMethod("Str", class(object@model[[1]]))) 
                Str(object@model[[1]])
            for (i in 1:length(object@graphs)) Str(object@graphs[[i]], 
                m = object@index, title = paste(title, "; Graph: ", 
                  i, sep = " "), ...)
        }
    })
    setMethod("show", "DynamicGraph", function(object) Str(object))
    setMethod("Str", "DynamicGraph", function(object, setRowLabels = FALSE, 
        title = "", ...) {
        message(paste(rep("#", 80)))
        message(paste("<<", object@title, ">>", sep = ""))
        Str(object@vertices, title = "vertices", ...)
        Str(object@blocks, title = "blocks", ...)
        .StrBlockTree(object@blockTree, title = "blockTree")
        for (i in 1:length(object@models)) Str(object@models[[i]], 
            title = paste("Model: ", i, sep = " "), ...)
        message(paste(rep("#", 80)))
    })
}
".onLoad.dynamicGraphInterface" <-
function () 
{
    setMethod("label", "dg.Test", function(object) format(object@p, 
        digits = 4))
    setMethod("width", "dg.Test", function(object) round(2 + 
        5 * (1 - object@p)))
    if (!isGeneric("testEdge")) {
        if (is.function("testEdge")) 
            fun <- testEdge
        else fun <- function(object, action, name.1, name.2, 
            ...) standardGeneric("testEdge")
        setGeneric("testEdge", fun)
    }
    setMethod("testEdge", signature(object = "dg.Model"), function(object, 
        action, name.1, name.2, ...) {
        args <- list(...)
        from.type <- args$from.type
        to.type <- args$to.type
        f <- function(type) if (is.null(type)) 
            ""
        else paste("(", type, ")")
        message(paste("Should return an object with the edge from", 
            name.1, f(from.type), "to", name.2, f(to.type), "deleted from the argument object"))
        return(newDefaultTestObject())
    })
    if (!isGeneric("modifyModel")) {
        if (is.function("modifyModel")) 
            fun <- modifyModel
        else fun <- function(object, action, name, name.1, name.2, 
            ...) standardGeneric("modifyModel")
        setGeneric("modifyModel", fun)
    }
    setMethod("modifyModel", signature(object = "dg.Model"), 
        function(object, action, name, name.1, name.2, ...) {
            args <- list(...)
            Args <- args$Arguments
            Edges <- args$newEdges$vertexEdges
            Vertices <- Args$vertexList
            DoFactors <- FALSE
            if (!is.null(args$Arguments) && !is.null(args$Arguments$factorVertexList) && 
                (length(args$Arguments$factorVertexList) > 0) && 
                !is.null(args$Arguments$vertexList)) 
                DoFactors <- TRUE
            FactorVertices <- NULL
            FactorEdges <- NULL
            BlockEdges <- NULL
            VisibleVertices <- Args$visibleVertices
            VisibleBlocks <- Args$visibleBlocks
            ExtraVertices <- NULL
            ExtraEdges <- NULL
            f <- function(type) if (is.null(type)) 
                ""
            else paste("(", type, ")")
            g <- function(type) if (is.null(type)) 
                ""
            else type
            if (action == "dropEdge") {
                message(paste("Should return an object with the edge from", 
                  name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                  "deleted from the argument object"))
                if ((g(args$from.type) == "Factor") || (g(args$from.type) == 
                  "Factor")) 
                  return(NULL)
            }
            else if (action == "addEdge") {
                message(paste("Should return an object with the edge from", 
                  name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                  "added to the argument object"))
                if ((g(args$from.type) == "Factor") || (g(args$from.type) == 
                  "Factor")) 
                  return(NULL)
            }
            else if (action == "dropVertex") {
                message(paste("Should return an object with the vertex", 
                  name, f(args$type), "deleted from the argument object"))
                if ((g(args$type) == "Factor")) 
                  return(NULL)
                VisibleVertices <- VisibleVertices[VisibleVertices != 
                  args$index]
                if (DoFactors && (args$index > 0)) {
                  x <- (args$Arguments$factorVertexList)
                  factors <- lapply(x, function(i) i@vertex.indices)
                  types <- lapply(x, function(i) class(i))
                  factors <- lapply(factors, function(x) {
                    y <- x[x != args$index]
                    if (length(y) > 0) 
                      return(y)
                    else return(NULL)
                  })
                  if (!is.null(factors)) {
                    types <- types[unlist(lapply(factors, function(i) !is.null(i)))]
                    factors <- .removeNull(factors)
                  }
                  if (!is.null(factors)) {
                    subset <- function(x) lapply(x, function(a) any(unlist(lapply(x, 
                      function(A) all(!is.na(match(a, A))) && 
                        (length(a) < length(A))))))
                    s <- subset(factors)
                    types <- types[!unlist(s)]
                    factors <- factors[!unlist(s)]
                    if (!(is.null(factors))) {
                      result <- returnFactorVerticesAndEdges(args$Arguments$vertexList, 
                        factors, types, factorClasses = validFactorClasses())
                      FactorVertices <- result$FactorVertices
                      FactorEdges <- result$FactorEdges
                    }
                  }
                  else {
                    DoFactors <- FALSE
                    FactorVertices <- .emptyDgList("dg.FactorVertexList")
                    FactorEdges <- .emptyDgList("dg.FactorEdgeList")
                  }
                }
            }
            else if (action == "addVertex") {
                VisibleVertices <- c(VisibleVertices, args$index)
                message(paste("Should return an object with the vertex", 
                  name, f(args$type), args$index, "added to the argument object"))
                if (DoFactors && (args$index > 0)) {
                  x <- (args$Arguments$factorVertexList)
                  factors <- lapply(x, function(i) i@vertex.indices)
                  types <- lapply(x, function(i) class(i))
                  if (!is.null(factors)) 
                    factors <- .removeNull(factors)
                  if (is.null(factors)) {
                    factors <- list(args$index)
                    types <- validFactorClasses()[1, 1]
                  }
                  else {
                    n <- length(types)
                    factors <- append(factors, list(args$index))
                    types <- append(types, types[n])
                  }
                  if (!(is.null(factors))) {
                    result <- returnFactorVerticesAndEdges(args$Arguments$vertexList, 
                      factors, types, factorClasses = validFactorClasses())
                    FactorVertices <- result$FactorVertices
                    FactorEdges <- result$FactorEdges
                  }
                }
            }
            if (is.null(FactorVertices) && DoFactors && !is.null(Edges)) {
                factors <- .cliquesFromEdges(Edges, Vertices, 
                  VisibleVertices)
                if (is.null(factors) || (length(factors) == 0)) {
                  FactorVertices <- .emptyDgList("dg.FactorVertexList")
                  FactorEdges <- .emptyDgList("dg.FactorEdgeList")
                }
                else {
                  result <- returnFactorVerticesAndEdges(Vertices, 
                    factors)
                  FactorVertices <- result$FactorVertices
                  FactorEdges <- result$FactorEdges
                }
            }
            return(list(object = object, BlockEdges = BlockEdges, 
                FactorVertices = FactorVertices, FactorEdges = FactorEdges, 
                VisibleVertices = VisibleVertices, VisibleBlocks = VisibleBlocks, 
                ExtraVertices = ExtraVertices, ExtraEdges = ExtraEdges))
        })
}
".propertyDialog" <-
function (object, classes = NULL, title = class(object), sub.title = label(object), 
    name.object = name(object), okReturn = TRUE, fixedSlots = NULL, 
    difficultSlots = NULL, top = NULL, entryWidth = 20, do.grab = FALSE) 
{
    "subSelectDialog" <- function(dlg, itemNames, init = 0, title = title, 
        background = "white", subtitle = sub.title) {
        scr <- tkscrollbar(dlg, repeatinterval = 5, command = function(...) tkyview(tl, 
            ...))
        tl <- tklistbox(dlg, height = length(itemNames), selectmode = "single", 
            yscrollcommand = function(...) tkset(scr, ...), background = background)
        tkgrid(tklabel(dlg, text = subtitle), tl, scr)
        tkgrid.configure(scr, rowspan = length(itemNames), sticky = "nsw")
        for (i in (1:length(itemNames))) tkinsert(tl, "end", 
            itemNames[i])
        if (!is.null(init)) 
            tkselection.set(tl, init)
        return(tl)
    }
    "subTextDialog" <- function(dlg, subtitle, entryInit, entryWidth = 20, 
        background = "white") {
        textEntryVarTcl <- tclVar(paste(entryInit))
        textEntryWidget <- tkentry(dlg, width = paste(entryWidth), 
            textvariable = textEntryVarTcl, background = background)
        tkgrid(tklabel(dlg, text = subtitle), textEntryWidget)
        return(textEntryVarTcl)
    }
    "subListDialog" <- function(dlg, subtitle, entryInit, entryWidth = 20, 
        background = "white") {
        r <- list()
        for (i in 1:length(entryInit)) {
            t <- subTextDialog(dlg, subtitle = paste(subtitle, 
                names(entryInit[i]), i, ": "), entryInit = entryInit[i], 
                entryWidth = entryWidth, background = background)
            r <- append(r, list(t))
        }
        return(r)
    }
    "onCancel" <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
        if (!is.null(top)) 
            tkfocus(top)
    }
    "onOK" <- function() {
        "getList" <- function(t.list) {
            r <- list()
            for (i in 1:length(t.list)) {
                t <- (tclvalue(t.list[[i]]))
                r <- append(r, list(t))
            }
            return(unlist(r))
        }
        result <<- lapply(t.list, function(t) {
            if (length(t) > 1) 
                r <- getList(t)
            else r <- tclvalue(t)
        })
        if (!is.null(classes)) 
            r.class <<- as.numeric(tkcurselection(t.class)) + 
                1
        if ((length(r.class) == 0) && !(length(init.class) == 
            0)) 
            r.class <<- init.class + 1
        tkgrab.release(dlg)
        tkdestroy(dlg)
        if (!is.null(top)) 
            tkfocus(top)
    }
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    if (do.grab) 
        tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, paste(title, ": ", name.object))
    idCancel <- NULL
    r.class <- idCancel
    result <- idCancel
    tkgrid(tklabel(dlg, text = "    "))
    tkgrid(tklabel(dlg, text = "Name: "), tklabel(dlg, text = name.object))
    tkgrid(tklabel(dlg, text = "    "))
    t.list <- lapply(slotNames(object), function(slot.name) {
        slot.value <- slot(object, slot.name)
        size <- length(c(unlist(slot.value)))
        color <- "white"
        if (slot.name %in% difficultSlots) 
            color <- "grey"
        if (slot.name %in% fixedSlots) 
            color <- "DarkGrey"
        if (size > 1) 
            t <- subListDialog(dlg, subtitle = slot.name, entryInit = slot.value, 
                entryWidth = entryWidth, background = color)
        else t <- subTextDialog(dlg, subtitle = slot.name, entryInit = slot.value, 
            entryWidth = entryWidth, background = color)
        return(t)
    })
    init.class <- list()
    if (!is.null(classes)) {
        init.class <- which(class(object) == classes[, 2]) - 
            1
        if (length(init.class) == 0) 
            t.class <- subSelectDialog(dlg, class(object), init = NULL, 
                title = title, subtitle = "Class:", background = "grey")
        else {
            color <- "white"
            if ("class" %in% difficultSlots) 
                color <- "grey"
            if ("class" %in% fixedSlots) 
                color <- "DarkGrey"
            t.class <- subSelectDialog(dlg, classes[, 1], init = init.class, 
                title = title, subtitle = "Class:", background = color)
        }
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(tklabel(dlg, text = "    "))
    if (TRUE || okReturn) 
        tkgrid(tklabel(dlg, text = "    "), OK.but, Cancel.but, 
            tklabel(dlg, text = "    "))
    else tkgrid(tklabel(dlg, text = "    "), Cancel.but, tklabel(dlg, 
        text = "    "))
    tkgrid(tklabel(dlg, text = "    "))
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {
        tkgrab.release(dlg)
        if (!is.null(top)) 
            tkfocus(top)
    })
    tkwait.window(dlg)
    Result <- list()
    isCancel <- function(x) (is.null(x))
    if (!is.null(classes) && !isCancel(r.class) && !(length(init.class) == 
        0) && (init.class != r.class - 1)) {
        if ("class" %in% difficultSlots) 
            message(paste("Trying to change the difficult slot '", 
                "class", "' ; "))
        if ("class" %in% fixedSlots) 
            message(paste("Trying to change the fixed slot '", 
                "class", "' ; "))
        class(object) <- classes[r.class, 2]
        Result <- list(class = classes[r.class, 2])
    }
    if (!is.null(result) && !isCancel(result)) {
        slot.names <- slotNames(object)
        for (i in 1:length(result)) {
            slot.name <- slot.names[i]
            slot.value <- slot(object, slot.name)
            slot.class <- class(slot.value)
            r <- result[[i]]
            size <- length(c(unlist(slot.value)))
            if (size == 1) 
                class(r) <- class(slot.value)
            else if (slot.class == "matrix") {
                r <- as.numeric(r)
                dim(r) <- dim(slot.value)
            }
            else if (slot.class == "numeric") {
                r <- as.numeric(r)
            }
            else r <- r
            if (is.numeric(slot.value)) {
                if (any(is.na(r))) {
                  message("Invalid number;")
                  change <- FALSE
                }
                else {
                  diff <- r - slot.value
                  diff[r != 0] <- diff[r != 0]/r[r != 0]
                  change <- (sum(diff^2) > 0.001)
                }
            }
            else change <- !(all(r == slot.value))
            if (change) {
                if (slot.name %in% difficultSlots) 
                  message(paste("Trying to change the difficult slot '", 
                    slot.name, "' ; "))
                if (slot.name %in% fixedSlots) 
                  message(paste("Trying to change the fixed slot '", 
                    slot.name, "' ; "))
                slotname <- slot.name
                if (slot.name == "label.position") 
                  slotname <- "labelPosition"
                if (slot.name == "vertex.indices") 
                  slotname <- "nodeIndices"
                r <- slot(eval(call(paste(slotname, "<-", sep = ""), 
                  object, r)), slot.name)
                slot(object, slot.name) <<- r
                item <- list(slot(object, slot.name))
                names(item) <- slot.name
                Result <- append(Result, item)
            }
        }
    }
    return(list(object = object, values = Result))
}
".removeNull" <-
function (x) 
{
    result <- NULL
    for (i in seq(along = x)) if (!is.null(x[[i]])) 
        result <- append(result, list(x[[i]]))
    class(result) <- class(x)
    return(result)
}
".StrBlockTree" <-
function (tree, title = "blockTree") 
{
    message(title)
    "subStrBlockTree" <- function(tree, k, p, dept) {
        i <- abs(tree$block@index)
        s <- tree$block@stratum
        label <- tree$block@label
        d <- paste(tree$block@descendants, collapse = ",")
        a <- paste(tree$block@ancestors, collapse = ",")
        nc <- 10 - nchar(a)
        if (nc < 0) 
            nc <- 1
        cat(paste("|", paste(rep(".", dept), collapse = ""), 
            i, paste(rep(" ", 10 - dept), collapse = ""), "(", 
            k, "/", p, ")", ": ", label, "; Stratum: ", s, "; Ancestors: ", 
            a, paste(rep(" ", nc), collapse = ""), "; Descendants: ", 
            d, "\n", sep = ""))
        if (!is.null((tree$sub.blocks))) 
            for (j in 1:length(tree$sub.blocks)) {
                subStrBlockTree(tree$sub.blocks[[j]], j, i, dept + 
                  1)
            }
    }
    if (!.IsEmpty(tree) && (length(tree) > 0)) 
        subStrBlockTree(tree, 1, 0, 0)
    invisible()
}
