"dynamicGraphMain" <-
function (vertexList, visibleVertices = 1:length(vertexList), 
    edgeList = NULL, blockList = NULL, blockEdgeList = NULL, 
    blockTree = NULL, oriented = FALSE, factorVertexList = NULL, 
    factorEdgeList = NULL, extraList = NULL, object = NULL, objectName = NULL, 
    vertexClasses = validVertexClasses(), title = "dynamicGraph", 
    transformation = NULL, width = 400, height = 400, w = 6, 
    vertexColor = "blue", extraVertexColor = "black", edgeColor = "blue", 
    factorVertexColor = "green", factorEdgeColor = "green", blockEdgeColor = "default", 
    blockColors = NULL, background = "white", closeenough = 2, 
    drawBlockFrame = TRUE, drawBlockBackground = TRUE, UserMenus = NULL, 
    hasMethods = TRUE, enterLeaveUpdate = TRUE, namesOnEdges = TRUE, 
    updateEdgeLabels = TRUE, debug.strata = FALSE, debug.edges = FALSE, 
    debug.position = FALSE, debug.update = FALSE, ...) 
{
    colors <- c("DarkGreen", "navy", "NavyBlue", "DarkBlue", 
        "DarkRed", "MidnightBlue", "DarkSlateGray", "DarkSlateGrey", 
        "MediumBlue", "ForestGreen", "SaddleBrown", "DarkOliveGreen", 
        "firebrick", "brown", "blue", "green", "red", "DarkSlateBlue", 
        "SeaGreen", "DarkCyan", "DarkMagenta", "OliveDrab", "sienna", 
        "LimeGreen", "DimGray", "DimGrey", "maroon", "OrangeRed", 
        "DarkGoldenrod", "chocolate", "MediumSeaGreen", "DarkViolet", 
        "LawnGreen", "LightSeaGreen", "SteelBlue", "chartreuse", 
        "SpringGreen", "black", "SlateGray", "SlateGrey", "VioletRed", 
        "IndianRed", "DarkOrange", "RoyalBlue", "peru", "SlateBlue", 
        "BlueViolet", "DarkOrchid", "LightSlateGray", "LightSlateGrey", 
        "YellowGreen", "CadetBlue", "DarkTurquoise", "goldenrod", 
        "orange", "DeepPink", "tomato", "DodgerBlue", "purple", 
        "DeepSkyBlue", "coral", "gold", "DarkSeaGreen", "RosyBrown", 
        "GreenYellow", "MediumPurple", "PaleVioletRed", "DarkKhaki", 
        "MediumOrchid", "CornflowerBlue", "salmon", "LightCoral", 
        "turquoise", "LightSlateBlue", "SandyBrown", "DarkSalmon", 
        "DarkGray", "DarkGrey", "cyan", "magenta", "yellow", 
        "LightGreen", "tan", "LightSalmon", "HotPink", "burlywood", 
        "orchid", "PaleGreen", "gray", "grey", "SkyBlue", "LightGoldenrod", 
        "LightSkyBlue", "aquamarine", "LightSteelBlue", "plum", 
        "violet", "khaki", "LightBlue", "thistle", "LightPink", 
        "PowderBlue", "LightGray", "LightGrey", "PaleGoldenrod", 
        "wheat", "NavajoWhite", "pink", "PaleTurquoise", "PeachPuff", 
        "gainsboro", "moccasin", "bisque", "BlanchedAlmond", 
        "AntiqueWhite", "PapayaWhip", "MistyRose", "beige", "lavender", 
        "LemonChiffon", "linen", "cornsilk", "OldLace", "LightCyan", 
        "LightYellow", "honeydew", "WhiteSmoke", "seashell", 
        "LavenderBlush", "AliceBlue", "FloralWhite", "azure", 
        "ivory", "MintCream", "GhostWhite", "snow", "white")
    "myColor" <- function(i) colors[min(i%%137, length(colors))]
    "asRow" <- function(positions) if (is.null(dim(positions))) 
        return(positions)
    else return(t(positions))
    "selectDialog" <- function(popup, vertexNames, title = "Variable selection entry", 
        subtitle = "Select variable") {
        tkwm.deiconify(popup)
        tkgrab.set(popup)
        tkfocus(popup)
        tkwm.title(popup, title)
        scr <- tkscrollbar(popup, repeatinterval = 5, command = function(...) tkyview(tl, 
            ...))
        tl <- tklistbox(popup, height = 4, selectmode = "single", 
            yscrollcommand = function(...) tkset(scr, ...), background = "white")
        tkgrid(tklabel(popup, text = subtitle))
        tkgrid(tl, scr)
        tkgrid.configure(scr, rowspan = 4, sticky = "nsw")
        for (i in (1:length(vertexNames))) tkinsert(tl, "end", 
            vertexNames[i])
        tkselection.set(tl, 0)
        return(tl)
    }
    "newGraphLattice" <- function(vertices, blocks = list(NULL), 
        blockTree = list(NULL)) {
        return(new("GraphLatticeProto", dummyVertices = vertices, 
            blocks = blocks, blockTree = blockTree))
    }
    "newGraph" <- function(graph.lattice, vertices, graph.edges, 
        factor.edges, block.edges, Add.GraphWindow = redrawGraphWindow, 
        title = "Graph diddler", close.enough = closeenough, 
        background = "white", width = 400, height = 400) {
        top <- tktoplevel()
        canvas <- tkcanvas(top, relief = "raised", background = background, 
            closeenough = close.enough, width = width, height = height)
        tkpack(canvas)
        result <- new("CanvasProto", top = top, canvas = canvas, 
            tags = list(NULL), graphEdges = graph.edges, factorEdges = factor.edges, 
            blockEdges = block.edges, id = 0, dummyVisibleVertices = 1:length(vertices))
        tktitle(top) <- title
        return(result)
    }
    "redrawGraphWindow" <- function(graphLattice = NULL, graphWindow = NULL, 
        edgeList = NULL, blockEdgeList = NULL, factorVertexList = NULL, 
        factorEdgeList = NULL, visibleVertices = NULL, extraList = NULL, 
        object = NULL, title = "dynamicGraph", transformation = NULL, 
        width = NULL, height = NULL, w = NULL, vertexColor = NULL, 
        extraVertexColor = NULL, edgeColor = NULL, factorVertexColor = NULL, 
        factorEdgeColor = NULL, blockEdgeColor = NULL, blockColors = NULL, 
        background = NULL, initialWindow = FALSE, ...) {
        "relativePositionsCanvas" <- function(positions) {
            if (!is.null(zoomPositions)) {
                diff <- 100/(zoomPositions[, 2] - zoomPositions[, 
                  1])
                p <- (diag(diff) %*% (asRow(positions)) - 0)
            }
            else p <- asRow(positions)
            return(round(t(diag(c(width, height, rep(100, N - 
                2))/100) %*% (p + 0))))
        }
        "inversCanvasRelativePosition" <- function(positions) {
            p <- asRow(positions)
            p <- t(diag(c(100/width, 100/height, rep(1, N - 2))) %*% 
                p - 0)
            if (!is.null(zoomPositions)) {
                diff <- (zoomPositions[, 2] - zoomPositions[, 
                  1])/100
                q <- t(diag(diff) %*% t(p + 0))
                return(q)
            }
            else return(p)
        }
        "positionsCanvas" <- function(positions) {
            if (!is.null(zoomPositions)) {
                a <- zoomPositions[, 1]
                b <- zoomPositions[, 2]
                A <- matrix(rep(a, ifelse(is.null(dim(positions)), 
                  1, nrow(positions))), byrow = TRUE, ncol = N)
                diff <- 100/(b - a)
                p <- (diag(diff) %*% (asRow(positions) - t(A)) - 
                  50)
            }
            else p <- asRow(positions)
            return(round(t(diag(c(width, height, rep(100, N - 
                2))/100) %*% (p + 50))))
        }
        "inversCanvasPosition" <- function(positions) {
            p <- asRow(positions)
            p <- t(diag(c(100/width, 100/height, rep(1, N - 2))) %*% 
                p - 50)
            if (!is.null(zoomPositions)) {
                a <- zoomPositions[, 1]
                b <- zoomPositions[, 2]
                A <- matrix(rep(a, ifelse(is.null(dim(positions)), 
                  1, nrow(positions))), byrow = TRUE, ncol = N)
                diff <- (b - a)/100
                q <- t(diag(diff) %*% t(p + 50) + t(A))
                return(q)
            }
            else return(p)
        }
        "replaceXY" <- function(x, y, position) {
            position[1] <- as.numeric(x)
            position[2] <- as.numeric(y)
            return(position)
        }
        "callPopup" <- function(i, PopupMenu) {
            force(i)
            function(x, y) {
                rootx <- as.integer(tkwinfo("rootx", canvas))
                rooty <- as.integer(tkwinfo("rooty", canvas))
                xCanvas <- as.integer(x) + rootx
                yCanvas <- as.integer(y) + rooty
                .Tcl(paste("tk_popup", .Tcl.args(PopupMenu, xCanvas, 
                  yCanvas)))
            }
        }
        "remove.NULL" <- function(x) {
            result <- NULL
            for (i in seq(along = x)) if (!is.null(x[[i]])) 
                result <- append(result, list(x[[i]]))
            return(result)
        }
        "getTag" <- function(text, number) {
            tag <- paste(text, number, GraphWindow@id, sep = "-")
            if (is.null(GraphWindow@tags[[1]])) 
                GraphWindow@tags <<- list(tag)
            else GraphWindow@tags <<- append(GraphWindow@tags, 
                list(tag))
            return(tag)
        }
        "verticesUpdate" <- function(update.positions = TRUE, 
            update.label.positions = TRUE) {
            for (i in seq(along = vertexList)) {
                position <- positionsVertices[i, ]
                position(vertexList[[i]]) <<- position
                position <- positionsLabels[i, ]
                labelPosition(vertexList[[i]]) <<- position
                label(vertexList[[i]]) <<- Labels[i]
                vertexList[[i]]@name <<- namesVertices[i]
                color(vertexList[[i]]) <<- colorsVertices[i]
                stratum(vertexList[[i]]) <<- strataVertices[i]
            }
            return(vertexList)
        }
        "edgesUpdate" <- function(update.label.positions = TRUE) {
            for (i in seq(along = edgeList)) {
                position <- positionsEdgeLabels[i, ]
                edgeList[[i]]@label.position <<- position
            }
            return(edgeList)
        }
        "blockTreeUpdate" <- function(tree) {
            "subBlockTreeUpdate" <- function(tree) {
                i <- abs(tree$block@index)
                position <- positionsBlocks[i, , ]
                position(tree$block) <<- position
                position <- positionsBlockLabels[i, ]
                labelPosition(tree$block) <<- position
                tree$block@stratum <<- strataBlocks[i]
                tree$block@label <<- blockLabels[i]
                tree$block@visible <<- !closedBlock[i]
                if (!is.null((tree$sub.blocks))) 
                  for (j in 1:length(tree$sub.blocks)) subBlockTreeUpdate(tree$sub.blocks[[j]])
            }
            subBlockTreeUpdate(tree)
        }
        "blocksUpdate" <- function() {
            for (i in seq(along = blockList)) {
                position <- positionsBlocks[i, , ]
                position(blockList[[i]]) <<- position
                position <- positionsBlockLabels[i, ]
                labelPosition(blockList[[i]]) <<- position
                blockList[[i]]@stratum <<- strataBlocks[i]
                blockList[[i]]@label <<- blockLabels[i]
                blockList[[i]]@visible <<- !closedBlock[i]
            }
            if (!is.null(blockTree)) 
                blockTreeUpdate(blockTree)
        }
        "blockEdgesUpdate" <- function() {
        }
        "factorVerticesUpdate" <- function() {
            for (i in seq(along = factorVertexList)) {
                position <- positionsFactorVertices[i, ]
                position(factorVertexList[[i]]) <<- position
                position <- positionsFactorLabels[i, ]
                labelPosition(factorVertexList[[i]]) <<- position
                label(factorVertexList[[i]]) <<- factorLabels[i]
                factorVertexList[[i]]@name <<- namesFactorVertices[i]
                color(factorVertexList[[i]]) <<- colorsFactorVertices[i]
                stratum(factorVertexList[[i]]) <<- strataFactorVertices[i]
            }
            return(factorVertexList)
        }
        "factorEdgesUpdate" <- function() {
        }
        "extraVerticesUpdate" <- function() {
            for (i in seq(along = extraList)) {
                position <- positionsExtraVertices[i, ]
                position(extraList[[i]]) <<- position
                position <- positionsExtraLabels[i, ]
                labelPosition(extraList[[i]]) <<- position
                label(extraList[[i]]) <<- extraLabels[i]
                extraList[[i]]@name <<- namesExtraVertices[i]
                color(extraList[[i]]) <<- colorsExtraVertices[i]
                stratum(extraList[[i]]) <<- strataExtraVertices[i]
            }
            return(extraList)
        }
        "updateArguments" <- function(menuItem, vertices = TRUE, 
            edges = TRUE, blocks = FALSE) {
            if (vertices && (is.null(menuItem$update.vertices) || 
                menuItem$update.vertices)) {
                V <- verticesUpdate()
                V <- factorVerticesUpdate()
                V <- extraVerticesUpdate()
            }
            if (edges && (is.null(menuItem$update.edges) || menuItem$update.edges)) {
                E <- edgesUpdate()
                E <- factorEdgesUpdate()
            }
            if (blocks && (is.null(menuItem$update.blocks) || 
                menuItem$update.blocks)) 
                B <- blocksUpdate()
        }
        "returnEdges" <- function(edge.type = "graphEdge") {
            if (edge.type == "graphEdge") 
                return(GraphWindow@graphEdges)
            else if (edge.type == "factorEdge") 
                return(GraphWindow@factorEdges)
            else if (edge.type == "blockEdge") 
                return(GraphWindow@blockEdges)
        }
        "currentEdges" <- function(edge.type = "graphEdge") {
            E <- returnEdges(edge.type = edge.type)
            if (length(E) > 0) {
                E <- lapply(E, function(egde) if (sum(abs(egde@vertex.indices)) > 
                  0) 
                  egde)
                E <- remove.NULL(E)
            }
            else E <- NULL
            return(E)
        }
        "append.index.edge" <- function(e, edge.type = "graphEdge") {
            if (edge.type == "graphEdge") 
                new.edge <- returnEdgeList(list(e), vertexList, 
                  color = edgeColor, oriented = oriented)
            else if (edge.type == "factorEdge") 
                new.edge <- returnFactorEdgeList(list(e), vertexList, 
                  color = factorEdgeColor, factorVertexList)
            else if (edge.type == "blockEdge") 
                new.edge <- list()
            E <- append(returnEdges(edge.type = edge.type), new.edge)
            if (edge.type == "graphEdge") 
                GraphWindow@graphEdges <<- E
            else if (edge.type == "factorEdge") 
                GraphWindow@factorEdges <<- E
            else if (edge.type == "blockEdge") 
                GraphWindow@blockEdges <<- E
            return(E)
        }
        "append.edge" <- function(e, edge.type = "graphEdge") {
            E <- append(returnEdges(edge.type = edge.type), list(e))
            if (edge.type == "graphEdge") 
                GraphWindow@graphEdges <<- E
            else if (edge.type == "factorEdge") 
                GraphWindow@factorEdges <<- E
            else if (edge.type == "blockEdge") 
                GraphWindow@blockEdges <<- E
            return(E)
        }
        "selectCurrentEdges" <- function(omitEdges = FALSE, edge.type = "graphEdge") {
            E <- returnEdges(edge.type = edge.type)
            if (length(E) > 0) {
                j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                if (edge.type == "graphEdge") 
                  j <- j | non.graph.edge(edge.type = edge.type)
                E <- sapply(1:length(E), function(x) if (!j[x]) 
                  E[[x]])
                E <- remove.NULL(E)
            }
            else E <- NULL
            return(E)
        }
        "copyCurrentEdges" <- function(omitEdges = FALSE, edge.type = "graphEdge") {
            E <- returnEdges(edge.type = edge.type)
            if (length(E) > 0) {
                j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                if (edge.type == "graphEdge") 
                  j <- j | non.graph.edge(edge.type = edge.type)
                E <- lapply(1:length(E), function(x) if (!j[x]) 
                  E[[x]]@vertex.indices)
                E <- remove.NULL(E)
            }
            else E <- NULL
            if (edge.type == "graphEdge") 
                E <- returnEdgeList(E, vertexList, color = edgeColor, 
                  oriented = oriented)
            else if (edge.type == "factorEdge") 
                E <- returnFactorEdgeList(E, vertexList, factorVertexList, 
                  color = factorEdgeColor)
            else if (edge.type == "blockEdge") 
                E <- returnEdges(edge.type = edge.type)
            return(E)
        }
        "appendToCurrentEdges" <- function(omitEdges = FALSE, 
            new.edge = NULL, edge.type = "graphEdge") {
            E <- returnEdges(edge.type = edge.type)
            if (length(E) > 0) {
                j <- omitEdges | vertex.in.edge(0, edge.type = edge.type)
                if (edge.type == "graphEdge") 
                  j <- j | non.graph.edge(edge.type = edge.type)
                E <- lapply(1:length(E), function(x) if (!j[x]) 
                  E[[x]])
                E <- remove.NULL(E)
                edge.list <- lapply(E, function(e) e@vertex.indices)
                if (!is.null(new.edge)) 
                  edge.list <- append(edge.list, new.edge)
            }
            else edge.list <- new.edge
            if (edge.type == "graphEdge") 
                E <- returnEdgeList(edge.list, vertexList, color = edgeColor, 
                  oriented = oriented)
            else if (edge.type == "factorEdge") 
                E <- NULL
            else if (edge.type == "blockEdge") 
                E <- NULL
            return(E)
        }
        "Args" <- function(x.redrawGraphWindow = redrawGraphWindow, 
            x.graphLattice = GraphLattice, x.graphWindow = GraphWindow, 
            x.vertexList = vertexList, x.edgeList = currentEdges(edge.type = "graphEdge"), 
            x.blockList = blockList, x.blockTree = blockTree, 
            x.blockEdgeList = currentEdges(edge.type = "blockEdge"), 
            x.factorVertexList = factorVertexList, x.factorEdgeList = currentEdges(edge.type = "factorEdge"), 
            x.extraList = extraList, x.visibleVertices = visibleVertices, 
            x.object = object, x.title = title, x.transformation = transformation, 
            x.width = width, x.height = height, x.w = w, x.vertexColor = vertexColor, 
            x.extraVertexColor = extraVertexColor, x.edgeColor = edgeColor, 
            x.factorVertexColor = factorVertexColor, x.factorEdgeColor = factorEdgeColor, 
            x.blockEdgeColor = blockEdgeColor, x.blockColors = blockColors, 
            x.background = background) return(list(redrawGraphWindow = x.redrawGraphWindow, 
            graphLattice = x.graphLattice, graphWindow = x.graphWindow, 
            vertexList = x.vertexList, edgeList = x.edgeList, 
            blockList = x.blockList, blockTree = x.blockTree, 
            blockEdgeList = x.blockEdgeList, factorVertexList = x.factorVertexList, 
            factorEdgeList = x.factorEdgeList, extraList = x.extraList, 
            visibleVertices = x.visibleVertices, object = x.object, 
            title = x.title, transformation = x.transformation, 
            width = x.width, height = x.height, w = x.w, vertexColor = x.vertexColor, 
            extraVertexColor = x.vertexColor, edgeColor = x.edgeColor, 
            factorVertexColor = x.factorVertexColor, factorEdgeColor = x.factorEdgeColor, 
            blockEdgeColor = x.blockEdgeColor, blockColors = x.blockColors, 
            background = x.background))
        "which.unordered.edge" <- function(e, edge.type = "graphEdge") {
            n <- length(e)
            unlist(lapply(returnEdges(edge.type = edge.type), 
                function(i) length(e[!is.na(match(e, i@vertex.indices))]) == 
                  n))
        }
        "which.edge" <- function(e, edge.type = "graphEdge") unlist(lapply(returnEdges(edge.type = edge.type), 
            function(i) all(i@vertex.indices == e)))
        "vertex.in.edge" <- function(e, edge.type = "graphEdge") unlist(lapply(returnEdges(edge.type = edge.type), 
            function(i) is.element(e, i@vertex.indices)))
        "non.graph.edge" <- function(edge.type = "graphEdge") unlist(lapply(returnEdges(edge.type = edge.type), 
            function(i) any(i@vertex.indices <= 0)))
        "edge.vertices" <- function(i, type.negative = "Factor", 
            edge.type = "graphEdge") {
            E <- returnEdges(edge.type = edge.type)
            edge <- E[[i]]@vertex.indices
            edge.vertices <- vector("list", length(edge))
            for (j in seq(along = edge)) if (edge[j] > 0) 
                edge.vertices[[j]] <- vertexList[[edge[j]]]
            else if (type.negative == "Factor") 
                edge.vertices[[j]] <- factorVertexList[[-edge[j]]]
            else if (type.negative == "ClosedBlock") 
                edge.vertices[[j]] <- blockList[[-edge[j]]]
            return(edge.vertices)
        }
        "edge.names" <- function(i, type.negative = "Factor", 
            edge.type = "graphEdge") lapply(edge.vertices(i, 
            type.negative = type.negative, edge.type = edge.type), 
            function(v) retVertexName(v@index, vertex.type = ifelse(v@index > 
                0, "Vertex", type.negative)))
        "edge.positions" <- function(i, type.negative = "Factor", 
            edge.type = "graphEdge") lapply(edge.vertices(i, 
            type.negative = type.negative, edge.type = edge.type), 
            function(v) retVertexPos(v@index, ifelse(v@index > 
                0, "Vertex", type.negative)))
        "edge.strata" <- function(i, type.negative = "Factor", 
            edge.type = "graphEdge") lapply(edge.vertices(i, 
            type.negative = type.negative, edge.type = edge.type), 
            function(v) retStratum(v@index, vertex.type = ifelse(v@index > 
                0, "Vertex", type.negative)))
        "clearEdge" <- function(i, edge.type = "graphEdge") if (edge.type == 
            "graphEdge") 
            GraphWindow@graphEdges[[i]]@vertex.indices <<- c(0, 
                0)
        else if (edge.type == "factorEdge") 
            GraphWindow@factorEdges[[i]]@vertex.indices <<- c(0, 
                0)
        else if (edge.type == "blockEdge") 
            GraphWindow@blockEdges[[i]]@vertex.indices <<- c(0, 
                0)
        "from" <- function(i, edge.type = "graphEdge") returnEdges(edge.type = edge.type)@vertex.indices[1]
        "to" <- function(i, edge.type = "graphEdge") returnEdges(edge.type = edge.type)[[i]]@vertex.indices[2]
        "setTransformation" <- function(value = NULL) {
            if (is.null(value) == (!is.null(transformation))) {
                if (!is.null(blockList)) 
                  for (i in seq(along = blockList)) deleteBlock(i)
                transformation <<- value
                if (!is.null(blockList)) 
                  for (i in seq(along = blockList)) drawBlock(blockList[[i]], 
                    i)
            }
            else transformation <<- value
            subUpdateGraphWindow("setTransformation", all.blockframes = TRUE)
        }
        "angle" <- function(value = NULL) if (!is.null(value)) 
            Angle <<- value
        else return(Angle)
        "project" <- function(position) if (!is.null(transformation)) 
            t(transformation %*% asRow(position))
        else position
        "inversProject" <- function(position) if (!is.null(transformation)) 
            t(solve(transformation, asRow(position)))
        else position
        "applyTransformation" <- function(trans, draw.box = FALSE, 
            redraw = TRUE) {
            if (!is.null(transformation)) {
                transformation <<- transformation %*% trans
                if (redraw) 
                  subUpdateGraphWindow("applyTransformation", 
                    all.blockframes = TRUE)
            }
        }
        "sphereRand" <- function(n) {
            nx2 <- 2
            while ((nx2 >= 1)) {
                x <- 2 * runif(n) - 1
                nx2 <- sum(x^2)
            }
            return(x/sqrt(nx2))
        }
        "makeRotation" <- function(x, y, alpha = 0, use.alpha = FALSE, 
            n = length(x)) {
            if (length(x) != length(y) || !is.null(dim(x)) || 
                !is.null(dim(y))) 
                stop("Invalid arguments")
            dnrm2 <- function(x) sqrt(sum(x^2))
            rot <- diag(1, n)
            nx <- dnrm2(x)
            ny <- dnrm2(y)
            if ((nx == 0) || (ny == 0)) 
                return(rot)
            x <- x * (1/nx)
            y <- y * (1/ny)
            xy <- t(x) %*% y
            c <- ifelse(use.alpha, cos(alpha), xy)
            cc <- 1 - c^2
            s <- ifelse(use.alpha, sin(alpha), ifelse(cc > 0, 
                sqrt(cc), 0))
            cm1 <- c - 1
            y <- y - xy * x
            ny <- dnrm2(y)
            if (ny == 0) 
                return(rot)
            y <- y * (1/ny)
            a <- x * cm1 + y * s
            b <- -x * s + y * cm1
            rot <- rot + a %*% t(x) + b %*% t(y)
            return(rot)
        }
        "canvasToSphere" <- function(X) {
            rad <- 100
            pos <- inversCanvasRelativePosition(X)
            x <- pos[1]
            y <- pos[2]
            norm.2 <- x^2 + y^2
            rad.2 <- rad^2
            z <- sqrt(max(rad.2 - norm.2, 0))
            res <- c(x, y, z, rep(0, N - 3))
            if (norm.2 < rad.2) 
                return(res)
            else {
                r <- sqrt(norm.2/rad.2)
                return(res/r)
            }
        }
        "doHandRotate" <- function() {
            p <- NULL
            function(x, y) {
                tkconfigure(canvas, cursor = "watch")
                X <- replaceXY(x, y, rep(50, N))
                if (is.null(p)) 
                  p <<- canvasToSphere(X)
                else {
                  oldp <- p
                  p <<- canvasToSphere(X)
                  applyTransformation(makeRotation(oldp, p), 
                    draw.box = FALSE, redraw = TRUE)
                  tkconfigure(canvas, cursor = "arrow")
                }
            }
        }
        "rockPlot" <- function(k = 2) {
            function(x, y) {
                tkconfigure(canvas, cursor = "watch")
                print("rockPlot")
                angle <- 10
                p1 <- sphereRand(N)
                p2 <- sphereRand(N)
                for (i in 1:k) applyTransformation(makeRotation(p1, 
                  p2, alpha = angle, use.alpha = TRUE), draw.box = FALSE, 
                  redraw = TRUE)
                for (i in 1:(2 * k)) applyTransformation(makeRotation(p1, 
                  p2, alpha = -angle, use.alpha = TRUE), draw.box = FALSE, 
                  redraw = TRUE)
                for (i in 1:k) applyTransformation(makeRotation(p1, 
                  p2, alpha = angle, use.alpha = TRUE), draw.box = FALSE, 
                  redraw = TRUE)
                print("Finished rocking!")
                tkconfigure(canvas, cursor = "arrow")
            }
        }
        "vertexItem" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "ClosedBlock") 
            return(itemsClosedBlocks[[i]])
        else if (vertex.type == "Vertex") 
            return(itemsVertices[[i]])
        else if (vertex.type == "Factor") 
            return(itemsFactors[[-i]])
        else if (vertex.type == "Extra") 
            return(itemsExtras[[i]])
        "setVertexItem" <- function(i, value, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "ClosedBlock") 
            itemsClosedBlocks[[i]] <<- value
        else if (vertex.type == "Vertex") 
            itemsVertices[[i]] <<- value
        else if (vertex.type == "Factor") 
            itemsFactors[[abs(i)]] <<- value
        else if (vertex.type == "Extra") 
            itemsExtras[[i]] <<- value
        "edgeItem" <- function(i, edge.type = "graphEdge") {
            if (i > 0) 
                return(itemsEdges[[i]])
            else if (edge.type == "blockEdge") 
                return(itemsBlockEdges[[-i]])
            else return(itemsFactorEdges[[-i]])
        }
        "setEdgeItem" <- function(i, edge.type = "graphEdge", 
            edges = NULL) {
            if (i > 0) 
                itemsEdges[[i]] <<- edges
            else if (edge.type == "blockEdge") 
                itemsBlockEdges[[-i]] <<- edges
            else itemsFactorEdges[[-i]] <<- edges
        }
        "openBlockItem" <- function(i) return(itemsOpenBlocks[[i]])
        "setOpenBlockItem" <- function(i, blocks) itemsOpenBlocks[[i]] <<- blocks
        "closedBlockItem" <- function(i) return(itemsClosedBlocks[[i]])
        "setCloseVertex" <- function(i, value, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "Vertex") {
            closedVertex[i] <<- value
            if (value) {
                tkdelete(canvas, vertexItem(i)$tag)
                updateBlockEdges()
                updateCountBlockEdges <<- updateCountBlockEdgesMain
            }
            else drawVertex(i, w = w, vertexcolor = vertexColor, 
                vertex.type = "Vertex")
        }
        "setCloseBlock" <- function(i, value, update = TRUE) if (i > 
            0) {
            closedBlock[i] <<- value
            if (all(is.na(positionsClosedBlocks[i, ]))) 
                positionsClosedBlocks[i, ] <<- apply(positionsBlocks[i, 
                  , ], 1, mean)
            if (value) 
                tkdelete(canvas, openBlockItem(i)$tag)
            else tkdelete(canvas, closedBlockItem(i)$tag)
            if (update) 
                if ((updateCountBlockEdges < updateCountBlockEdgesMain)) {
                  updateBlockEdges()
                  updateCountBlockEdges <<- updateCountBlockEdgesMain
                }
        }
        "setDoubleCloseBlock" <- function(i, value, update = TRUE) if (i > 
            0) {
            doubleClosedBlock[i] <<- value
            if (value) {
                if (all(is.na(positionsClosedBlocks[i, ]))) 
                  positionsClosedBlocks[i, ] <<- apply(positionsBlocks[i, 
                    , ], 1, mean)
                if (closedBlock[i]) 
                  tkdelete(canvas, closedBlockItem(i)$tag)
                else tkdelete(canvas, openBlockItem(i)$tag)
                closedBlock[i] <<- TRUE
            }
        }
        "retStratum" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            if (vertex.type == "ClosedBlock") 
                strataBlocks[abs(i)]
            else if (vertex.type == "Vertex") 
                strataVertices[i]
            else if (vertex.type == "Factor") 
                strataFactorVertices[-i]
            else if (vertex.type == "Extra") 
                strataExtraVertices[i]
        }
        "setStratum" <- function(i, value, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            update <- FALSE
            if (vertex.type == "Vertex") {
                strataVertices[i] <<- value
                if ((value > 0) && (closedBlock[value] != closedVertex[i])) {
                  setCloseVertex(i, !closedVertex[i], vertex.type)
                  if (!closedVertex[i]) {
                    pos <- retVertexPos(i, vertex.type)
                    moveEdgesToVertex(pos, i, edge.type = "graphEdge")
                  }
                  update <- TRUE
                }
            }
            else if (vertex.type == "Factor") 
                strataFactorVertices[-i] <<- value
            else if (vertex.type == "Extra") 
                strataExtraVertices[i] <<- value
            return(update)
        }
        "updateVertexStratum" <- function(position, i) {
            currentStratum <- retStratum(i, vertex.type = "Vertex")
            update <- FALSE
            if (!is.null(blockList)) {
                setStratum(i, 0, vertex.type = "Vertex")
                for (j in seq(along = blockList)) if (inBlock(position, 
                  j)) {
                  change <- setStratum(i, j, vertex.type = "Vertex")
                  update <- update || change
                }
            }
            return(update || (currentStratum != retStratum(i, 
                vertex.type = "Vertex")))
        }
        "updateVertexStratum" <- function(position, i) {
            currentStratum <- retStratum(i, vertex.type = "Vertex")
            update <- FALSE
            if (!is.null(blockList)) {
                k <- 0
                for (j in seq(along = blockList)) if (inBlock(position, 
                  j)) {
                  k <- j
                }
                change <- setStratum(i, k, vertex.type = "Vertex")
                update <- update || change
            }
            return(update || (currentStratum != retStratum(i, 
                vertex.type = "Vertex")))
        }
        "updateVerticesStrata" <- function() {
            if (debug.update) 
                print(paste("updateVerticesStrata"))
            updateEdges <- FALSE
            if (!is.null(vertexList)) 
                for (i in seq(along = vertexList)) {
                  update <- updateVertexStratum(positionsVertices[i, 
                    ], i)
                  updateEdges <- updateEdges || update
                }
            if (updateEdges) {
                setUpdateBlockEdges("updateVerticesStrata")
            }
            return(updateEdges)
        }
        "findMove" <- function(position, dxy = rep(0, N)) return(inversProject(inversCanvasPosition(positionsCanvas(project(position)) + 
            dxy)))
        "findDifference" <- function(p1, p2) return(relativePositionsCanvas(project(inversProject(inversCanvasPosition(p1)) - 
            inversProject(inversCanvasPosition(p2)))))
        "retVertexPos" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            if (vertex.type == "ClosedBlock") 
                position <- positionsClosedBlocks[abs(i), ]
            else if (vertex.type == "Vertex") 
                if (closedVertex[i]) 
                  position <- positionsClosedBlocks[blockReferences[retStratum(i, 
                    vertex.type)], ]
                else position <- positionsVertices[i, ]
            else if (vertex.type == "Factor") 
                position <- positionsFactorVertices[-i, ]
            else if (vertex.type == "Extra") 
                position <- positionsExtraVertices[i, ]
            return(positionsCanvas(project(position)))
        }
        "setVertexPos" <- function(i, xy, dxy = rep(0, N), vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            position <- inversProject(inversCanvasPosition(xy))
            if (vertex.type == "ClosedBlock") 
                positionsClosedBlocks[i, ] <<- position
            else if (vertex.type == "Vertex") {
                positionsVertices[i, ] <<- position
                positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                  ], dxy)
            }
            else if (vertex.type == "Factor") {
                positionsFactorVertices[-i, ] <<- position
                positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                  ], dxy)
            }
            else if (vertex.type == "Extra") {
                positionsExtraVertices[i, ] <<- position
                positionsExtraLabels[i, ] <<- findMove(positionsExtraLabels[i, 
                  ], dxy)
            }
            if (vertex.type != "ClosedBlock") 
                if (updateVertexStratum(position, i)) {
                  setUpdateBlockEdges("setVertexPos")
                }
        }
        "changeVertexPos" <- function(i, dxy = rep(0, N), vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            if (vertex.type == "Vertex") {
                positionsVertices[i, ] <<- findMove(positionsVertices[i, 
                  ], dxy)
                positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                  ], dxy)
            }
            else if (vertex.type == "Factor") {
                positionsFactorVertices[-i, ] <<- findMove(positionsFactorVertices[-i, 
                  ], dxy)
                positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                  ], dxy)
            }
            else if (vertex.type == "Extra") {
                positionsExtraVertices[i, ] <<- findMove(positionsExtraVertices[i, 
                  ], dxy)
                positionsExtraLabels[i, ] <<- findMove(positionsExtraLabels[i, 
                  ], dxy)
            }
        }
        "retVertexName" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "OpenBlock") 
            blockLabels[i]
        else if (vertex.type == "ClosedBlock") 
            blockLabels[abs(i)]
        else if (vertex.type == "Vertex") 
            namesVertices[i]
        else if (vertex.type == "Factor") 
            namesFactorVertices[-i]
        else if (vertex.type == "Extra") 
            namesExtraVertices[i]
        "retVertexColor" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "ClosedBlock") 
            color(blockList[[i]])
        else if (vertex.type == "Vertex") 
            colorsVertices[i]
        else if (vertex.type == "Factor") 
            colorsFactorVertices[-i]
        else if (vertex.type == "Extra") 
            colorsExtraVertices[-i]
        "setVertexColor" <- function(i, color = retVertexColor(i, 
            vertex.type), vertex.type = ifelse(i > 0, "Vertex", 
            "Factor")) {
            items <- vertexItem(i, vertex.type)$dot$dynamic
            if (!is.null(items)) 
                if (length(items) > 0) 
                  for (k in seq(length(items))) tkitemconfigure(canvas, 
                    items[[k]], fill = color[[1]])
        }
        "retVertexLabel" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if ((vertex.type == "OpenBlock") || 
            (vertex.type == "ClosedBlock")) 
            blockLabels[i]
        else if (vertex.type == "Vertex") 
            Labels[i]
        else if (vertex.type == "Factor") 
            factorLabels[-i]
        else if (vertex.type == "Extra") 
            extraLabels[i]
        "setVertexLabel" <- function(i, label, vertex.type) if (vertex.type == 
            "ClosedBlock") {
            blockLabels[i] <<- label
            tkitemconfigure(canvas, itemsClosedBlocks[[i]]$l, 
                text = label)
        }
        else if (vertex.type == "Vertex") {
            Labels[i] <<- label
            tkitemconfigure(canvas, itemsVertices[[i]]$l, text = label)
        }
        else if (vertex.type == "Factor") {
            factorLabels[-i] <<- label
            tkitemconfigure(canvas, itemsFactors[[-i]]$l, text = label)
        }
        else if (vertex.type == "Extra") {
            extraLabels[i] <<- label
            tkitemconfigure(canvas, itemsExtras[[i]]$l, text = label)
        }
        "retLabelPos" <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            if (vertex.type == "ClosedBlock") 
                position <- positionsBlockLabels[i, ]
            else if (vertex.type == "Vertex") 
                position <- positionsLabels[i, ]
            else if (vertex.type == "Factor") 
                position <- positionsFactorLabels[-i, ]
            else if (vertex.type == "Extra") 
                position <- positionsExtraLabels[i, ]
            positionsCanvas(project(position))
        }
        "setLabelPos" <- function(i, xy, dxy = rep(0, N), vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (vertex.type == "ClosedBlock") {
            positionsBlockLabels[i, ] <<- positionsBlockLabels[i, 
                ] + inversCanvasRelativePosition(dxy)
        }
        else if (vertex.type == "Vertex") 
            positionsLabels[i, ] <<- findMove(positionsLabels[i, 
                ], dxy)
        else if (vertex.type == "Factor") 
            positionsFactorLabels[-i, ] <<- findMove(positionsFactorLabels[-i, 
                ], dxy)
        else if (vertex.type == "Extra") 
            positionsExtraLabels[i, ] <<- findMove(positionsExtraLabels[i, 
                ], dxy)
        "retEdgeLabelPos" <- function(label.number, f = 0, t = 0) relativePositionsCanvas(positionsEdgeLabels[label.number, 
            ])
        "setEdgeLabelPos" <- function(edgeNode, label.number, 
            xy, dxy = rep(0, N), f = 0, t = edgeNode$to, edge.type = edgeNode$type) {
            positionsEdgeLabels[label.number, ] <<- positionsEdgeLabels[label.number, 
                ] + inversCanvasRelativePosition(dxy)
            E <- returnEdges(edge.type = edge.type)
            labelPosition(E[[edgeNode$nr]]) <<- positionsEdgeLabels[label.number, 
                ]
        }
        "setEdgeLabel" <- function(edgeNode, label = "", i = edgeNode$label.number, 
            f = 0, t = edgeNode$to, edge.type = edgeNode$type) {
            if (debug.strata && (label != " ")) {
                text <- paste(paste(i, paste(f, t, sep = "-"), 
                  sep = "<"), label, sep = ">")
                tkitemconfigure(canvas, edgeNode$label, text = text)
                tkitemconfigure(canvas, edgeNode$label, fill = myColor(i + 
                  2))
            }
            else tkitemconfigure(canvas, edgeNode$label, text = label)
            if (edgeNode$type == "graphEdge") 
                labelOfEdge(GraphWindow@graphEdges[[edgeNode$nr]]) <<- label
            else if (edgeNode$type == "factorEdge") 
                labelOfEdge(GraphWindow@factorEdges[[edgeNode$nr]]) <<- label
            else if (edgeNode$type == "blockEdge") 
                labelOfEdge(GraphWindow@blockEdges[[edgeNode$nr]]) <<- label
        }
        "retEdgeLabel" <- function(edgeNode, i, f, t, edge.type = "graphEdge") {
            if (edgeNode$type == "graphEdge") 
                labelOfEdge(GraphWindow@graphEdges[[edgeNode$nr]])
            else if (edgeNode$type == "factorEdge") 
                labelOfEdge(GraphWindow@factorEdges[[edgeNode$nr]])
            else if (edgeNode$type == "blockEdge") 
                labelOfEdge(GraphWindow@blockEdges[[edgeNode$nr]])
        }
        "setEdgeWidth" <- function(edgeNode, width = 1, i = edgeNode$label.number, 
            f = 0, t = edgeNode$to, edge.type = edgeNode$type) {
            tkitemconfigure(canvas, edgeNode$edge, width = width)
            if (edgeNode$type == "graphEdge") 
                widthOfEdge(GraphWindow@graphEdges[[edgeNode$nr]]) <<- width
            else if (edgeNode$type == "factorEdge") 
                widthOfEdge(GraphWindow@factorEdges[[edgeNode$nr]]) <<- width
            else if (edgeNode$type == "blockEdge") 
                widthOfEdge(GraphWindow@blockEdges[[edgeNode$nr]]) <<- width
        }
        "vertexTypeOfEdge" <- function(index, edge.type = "graphEdge", 
            edgeObject = NULL) if (edge.type == "factorBlockEdge") 
            ifelse(index > 0, "Factor", "ClosedBlock")
        else ifelse(index > 0, "Vertex", ifelse(edge.type == 
            "factorEdge", "Factor", "ClosedBlock"))
        "setEdgeCoords" <- function(edgeNode, posFrom, posTo, 
            f, t, from.type = vertexTypeOfEdge(f, edgeNode$type), 
            to.type = vertexTypeOfEdge(t, edgeNode$type)) {
            stratumFrom <- retStratum(f, from.type)
            stratumTo <- retStratum(t, to.type)
            reverse <- edgeNode$reverse
            display <- TRUE
            fun <- function(i, type) {
                if (type == "ClosedBlock") {
                  if (!closedBlock[abs(i)]) 
                    display <<- FALSE
                  if (doubleClosedBlock[abs(i)]) 
                    display <<- FALSE
                }
                else if (closedVertex[abs(i)]) 
                  display <<- FALSE
            }
            fun(f, from.type)
            fun(t, to.type)
            if ((stratumFrom != 0) || (stratumTo != 0)) {
                if (stratumFrom == stratumTo) 
                  tkitemconfigure(canvas, edgeNode$edge, arrow = "none")
                else tkitemconfigure(canvas, edgeNode$edge, arrow = "last")
                reverse <- (stratumFrom > stratumTo)
            }
            if (display) {
                diff <- posTo - posFrom
                l <- sqrt(sum(diff^2))
                posTo <- posTo - diff * min(2 * w, l)/l
                posFrom <- posFrom + diff * min(2 * w, l)/l
                label <- retEdgeLabel(edgeNode, edgeNode$label.number, 
                  f, t, edge.type = edgeNode$type)
                tkitemconfigure(canvas, edgeNode$label, text = label)
            }
            else {
                posTo <- c(0, 0)
                posFrom <- c(0, 0)
                tkitemconfigure(canvas, edgeNode$label, text = "")
            }
            if (reverse) 
                tkcoords(canvas, edgeNode$edge, posTo[1], posTo[2], 
                  posFrom[1], posFrom[2])
            else tkcoords(canvas, edgeNode$edge, posFrom[1], 
                posFrom[2], posTo[1], posTo[2])
        }
        "retBlockPos" <- function(i, j) {
            position <- positionsBlocks[i, , j]
            positionsCanvas(project(position))
        }
        "changeBlockCornerPos" <- function(i, A, dxy) {
            db <- toBlockPoints(A, dxy)
            positionsBlocks[i, , 1] <<- findMove(positionsBlocks[i, 
                , 1], db[1, ])
            positionsBlocks[i, , 2] <<- findMove(positionsBlocks[i, 
                , 2], db[2, ])
        }
        "changeBlockPos" <- function(i, A, dxy) {
            positionsBlocks[i, , 1] <<- findMove(positionsBlocks[i, 
                , 1], dxy)
            positionsBlocks[i, , 2] <<- findMove(positionsBlocks[i, 
                , 2], dxy)
        }
        "inBlock" <- function(position, block) {
            if (FALSE) 
                return(FALSE)
            else {
                block.position <- t(positionsBlocks[block, , 
                  ])
                if (!all((block.position[1, ] < block.position[2, 
                  ]))) 
                  warning("Invalid block positions")
                return(all((block.position[1, ] < position) & 
                  (position < block.position[2, ])))
            }
        }
        "retBlockPoints" <- function(i, header = FALSE, box = FALSE, 
            n) {
            A <- positionsBlocks[i, , 1]
            if (header) {
                if (box) {
                  A <- A + c(1, 1, rep(0, N - 2))
                  B <- A + c(3 * n, 5, rep(0, N - 2))
                }
                else {
                  B <- positionsBlocks[i, , 2]
                  A <- A + c(1, 5, rep(0, N - 2))
                  B[1] <- B[1] - 1
                  B[2] <- A[2] + 1
                }
            }
            else B <- positionsBlocks[i, , 2]
            delta <- c(0, 0, 0)
            position <- matrix(c(c(A[1], A[2], A[3]), c(B[1], 
                B[2], B[3]) + delta, c(A[1], B[2], A[3]), c(B[1], 
                A[2], A[3]), c(A[1], A[2], B[3]) + delta, c(A[1], 
                B[2], B[3]) + delta, c(B[1], A[2], B[3]) + delta, 
                c(B[1], B[2], A[3])), ncol = 3, byrow = TRUE)
            if (N > 3) 
                for (i in 4:N) position <- cbind(position, rep(0, 
                  8))
            positionsCanvas(project(position))
        }
        "toBlockPoints" <- function(n, p) {
            result <- switch(EXPR = paste(n - 1), "0" = c(c(p[1], 
                p[2], p[3]), c(0, 0, 0)), "1" = c(c(0, 0, 0), 
                c(p[1], p[2], p[3])), "2" = c(c(p[1], 0, p[3]), 
                c(0, p[2], 0)), "3" = c(c(0, p[2], p[3]), c(p[1], 
                0, 0)), "4" = c(c(p[1], p[2], 0), c(0, 0, p[3])), 
                "5" = c(c(p[1], 0, 0), c(0, p[2], p[3])), "6" = c(c(0, 
                  p[2], 0), c(p[1], 0, p[3])), "7" = c(c(0, 0, 
                  p[3]), c(p[1], p[2], 0)))
            result <- matrix(result, ncol = 3, byrow = TRUE)
            if (N > 3) 
                for (i in 4:N) result <- cbind(result, rep(0, 
                  8))
            return(result)
        }
        "retBlockLabelPos" <- function(i) relativePositionsCanvas(positionsBlockLabels[i, 
            ])
        "addEdgePopups" <- function(canvas, edge, i, f, t, edgePopupMenu, 
            UserMenus, edge.type = "graphEdge") {
            tkadd(edgePopupMenu, "command", label = paste("Edge from", 
                retVertexLabel(f), "to", retVertexLabel(t), "(echo indices)"), 
                command = function() {
                  print("Hej from edge")
                  print(c(f, t))
                })
            tkadd(edgePopupMenu, "command", label = paste("Drop edge (Here: Slave!) [click edge]"), 
                command = function() subDropEdge(i, f, t, edge.type = edge.type, 
                  slave = TRUE))
            tkadd(edgePopupMenu, "command", label = paste("Delete all edges to/from blocks"), 
                command = function() subDropEdge(i, f, t, from.all = TRUE, 
                  to.all = TRUE, edge.type = edge.type, slave = FALSE))
            tkadd(edgePopupMenu, "command", label = paste(" - Drag edge:     Move edge with two vertices"), 
                command = function() message("Left click edge and drag edge"))
            tkadd(edgePopupMenu, "command", label = paste(" - Drag label:     Move label of edge"), 
                command = function() message("Left click edge label and drag label"))
            tkadd(edgePopupMenu, "command", label = paste("Set edge label"), 
                command = function() {
                  activateEdge(i, edge.type = edge.type)()
                  changeEdgeLabel(i, f, t, edge.type = edge.type)()
                })
            tkadd(edgePopupMenu, "command", label = paste("Compute edge label [click label]"), 
                command = function() {
                  activateEdge(i, edge.type = edge.type)()
                  computeEdgeLabel(i, f, t, FALSE, edge.type = edge.type)()
                })
            tkadd(edgePopupMenu, "command", label = paste("Force compute edge label [double click label]"), 
                command = function() {
                  activateEdge(i, edge.type = edge.type)()
                  computeEdgeLabel(i, f, t, TRUE, edge.type = edge.type)()
                })
            tkadd(edgePopupMenu, "command", label = paste("Delete label of edge [triple click label]"), 
                command = function() deleteEdgeLabel(i, f, t, 
                  edge.type = edge.type)())
            if (hasMethod("addToPopups", class(edge))) 
                addToPopups(edge, "Edge", edgePopupMenu)
            UserEdgePopup <- function(item) {
                force(item)
                force(f)
                force(t)
                force(edge.type)
                force(edge)
                function(...) {
                  updateArguments(UserMenus[[item]])
                  j <- which.unordered.edge(c(t, f), edge.type = edge.type)
                  from.type <- vertexTypeOfEdge(f, edge.type, 
                    edge)
                  to.type <- vertexTypeOfEdge(t, edge.type, edge)
                  UserMenus[[item]]$command(object, retVertexName(f, 
                    from.type), retVertexName(t, to.type), from = f, 
                    to = t, from.type = from.type, to.type = to.type, 
                    edge.index = i, which.edge = j, edge.type = edge.type, 
                    Arguments = Args())
                }
            }
            if (length(UserMenus) > 0) 
                for (item in seq(along = UserMenus)) if (names(UserMenus[item]) == 
                  "Edge") 
                  tkadd(edgePopupMenu, "command", label = UserMenus[[item]]$label, 
                    command = UserEdgePopup(item))
        }
        "setEdgePopup" <- function(canvas, edge, line, label, 
            i, f, t, UserMenus, edge.type = "graphEdge") {
            edgePopupMenu <- tkmenu(canvas, tearoff = FALSE)
            addEdgePopups(canvas, edge, i, f, t, edgePopupMenu, 
                UserMenus, edge.type)
            tkitembind(canvas, label, "<Leave>", function() tkconfigure(canvas, 
                cursor = "arrow"))
            tkitembind(canvas, label, "<Enter>", function() tkconfigure(canvas, 
                cursor = "hand1"))
            tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                cursor = "arrow"))
            tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                cursor = "tcross"))
            tkitembind(canvas, label, "<Button-1>", activateEdge(i, 
                edge.type = edge.type))
            tkitembind(canvas, label, "<B1-Motion>", moveEdgeLabel(i, 
                f, t, edge.type = edge.type))
            tkitembind(canvas, label, "<ButtonRelease-1>", computeEdgeLabel(i, 
                f, t, FALSE, edge.type = edge.type))
            tkitembind(canvas, label, "<Double-Button-1>", computeEdgeLabel(i, 
                f, t, TRUE, edge.type = edge.type))
            tkitembind(canvas, label, "<Triple-Button-1>", deleteEdgeLabel(i, 
                f, t, edge.type = edge.type))
            tkitembind(canvas, label, "<Button-3>", callPopup(i, 
                edgePopupMenu))
            tkitembind(canvas, line, "<Button-1>", activateEdge(i, 
                edge.type = edge.type))
            tkitembind(canvas, line, "<ButtonRelease-1>", deleteEdge(i, 
                f, t, edge.type = edge.type))
            tkitembind(canvas, line, "<B1-Motion>", moveEdge(i, 
                f, t, edge.type = edge.type))
            tkitembind(canvas, line, "<Button-3>", callPopup(i, 
                edgePopupMenu))
        }
        "drawEdge" <- function(edge, i, edgecolor = "black", 
            lower = FALSE, edge.type = "graphEdge") {
            tag <- getTag(edge.type, i)
            type.negative <- ifelse(edge.type == "blockEdge", 
                "ClosedBlock", "Factor")
            useMethod <- FALSE
            if (!is.null(edge)) 
                useMethod <- hasMethod("draw", class(edge))
            if (useMethod) {
                position <- edge.positions(i, type.negative = type.negative, 
                  edge.type = edge.type)
                strata <- edge.strata(i, type.negative = type.negative, 
                  edge.type = edge.type)
                x <- lapply(position, function(e) e[1])
                y <- lapply(position, function(e) e[2])
                result <- draw(edge, canvas, position, x, y, 
                  stratum = strata, w = edge@width, color = edge@color, 
                  background = background)
            }
            else {
                f <- from(i, edge.type = edge.type)
                t <- to(i, edge.type = edge.type)
                from.type <- vertexTypeOfEdge(f, edge.type, edge)
                to.type <- vertexTypeOfEdge(t, edge.type, edge)
                posFrom <- retVertexPos(f, from.type)
                posTo <- retVertexPos(t, to.type)
                stratumFrom <- retStratum(f, from.type)
                stratumTo <- retStratum(t, to.type)
                if (stratumFrom == stratumTo) 
                  arrowhead = "none"
                else if (stratumFrom < stratumTo) 
                  arrowhead = "last"
                else arrowhead = "first"
                E <- returnEdges(edge.type = edge.type)[[i]]
                line <- tkcreate(canvas, "line", posFrom[1], 
                  posFrom[2], posTo[1], posTo[2], arrow = arrowhead, 
                  width = E@width, fill = E@color)
                label.position <- (posFrom + posTo)/2
                pos <- label.position + rep(0, N)
                txt <- E@label
                label <- tkcreate(canvas, "text", pos[1], pos[2], 
                  text = txt, anchor = "nw", font = "8x16")
                result <- list(list(line = line, from = f, to = t, 
                  label = label, label.position = label.position))
            }
            for (k in 1:length(result)) {
                positionsEdgeLabels <<- rbind(positionsEdgeLabels, 
                  rep(0, N))
                tkaddtag(canvas, tag, "withtag", result[[k]]$label)
                tkaddtag(canvas, tag, "withtag", result[[k]]$line)
                f <- result[[k]]$from
                t <- result[[k]]$to
                edgeNode <- list(nr = i, type = edge.type, to = t, 
                  tag = tag, reverse = FALSE, edge = result[[k]]$line, 
                  label = result[[k]]$label, label.number = nrow(positionsEdgeLabels))
                setEdgeItem(f, edge.type = edge.type, c(edgeItem(f, 
                  edge.type = edge.type), list(edgeNode)))
                from.type <- vertexTypeOfEdge(f, edge.type, edge)
                to.type <- vertexTypeOfEdge(t, edge.type, edge)
                posFrom <- retVertexPos(f, from.type)
                posTo <- retVertexPos(t, to.type)
                setEdgeCoords(edgeNode, posFrom, posTo, f, t, 
                  from.type, to.type)
                edgeNode <- list(nr = i, type = edge.type, to = f, 
                  tag = tag, reverse = TRUE, edge = result[[k]]$line, 
                  label = result[[k]]$label, label.number = nrow(positionsEdgeLabels))
                setEdgeItem(t, edge.type = edge.type, c(edgeItem(t, 
                  edge.type = edge.type), list(edgeNode)))
                setEdgePopup(canvas, edge, result[[k]]$line, 
                  result[[k]]$label, i, f, t, UserMenus, edge.type = edge.type)
            }
        }
        tkcoordsBlock <- function(i, color = "black", lower = FALSE) {
            tkcoordsRectangleLine <- function(line, i, A, B, 
                positions, color = "black", width = 1) {
                posA <- positions[A, ]
                posB <- positions[B, ]
                tkcoords(canvas, line, posA[1], posA[2], posB[1], 
                  posB[2])
            }
            tkcoordsCornerLine <- function(line, i, A, posA, 
                posB, color = "black", width = 1) {
                diff <- posB - posA
                l <- sqrt(sum(diff^2))
                posB <- posA + diff * min(30, l)/l
                posA <- posA - diff * min(w/2, l)/l
                tkcoords(canvas, line, posA[1], posA[2], posB[1], 
                  posB[2])
            }
            tkcoordsRectangleCorner <- function(line, i, A, B, 
                C, D, positions, color = "black", width = 2) {
                posA <- positions[A, ]
                tkcoordsCornerLine(line[[1]], i, A, posA, positions[B, 
                  ], color, width)
                tkcoordsCornerLine(line[[2]], i, A, posA, positions[C, 
                  ], color, width)
                if (!is.null(transformation)) 
                  tkcoordsCornerLine(line[[3]], i, A, posA, positions[D, 
                    ], color, width)
            }
            "tkcoordsRectangle" <- function(rectangle, i, positions, 
                color = "black", width = 1) {
                line <- rectangle$Lines
                tkcoordsRectangleLine(line[[1]], i, 1, 3, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[2]], i, 4, 8, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[3]], i, 1, 4, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[4]], i, 3, 8, positions, 
                  color, width)
                if (!is.null(transformation)) {
                  tkcoordsRectangleLine(line[[5]], i, 5, 6, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[6]], i, 7, 2, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[7]], i, 5, 7, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[8]], i, 6, 2, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[9]], i, 1, 5, positions, 
                    color, width)
                  tkcoordsRectangleLine(line[[10]], i, 3, 6, 
                    positions, color, width)
                  tkcoordsRectangleLine(line[[11]], i, 4, 7, 
                    positions, color, width)
                  tkcoordsRectangleLine(line[[12]], i, 8, 2, 
                    positions, color, width)
                }
                corner <- rectangle$Corners
                tkcoordsRectangleCorner(corner[[1]], i, 1, 3, 
                  4, 5, positions, color, width + 2)
                tkcoordsRectangleCorner(corner[[2]], i, 8, 4, 
                  3, 2, positions, color, width + 2)
                tkcoordsRectangleCorner(corner[[3]], i, 4, 1, 
                  8, 7, positions, color, width + 2)
                tkcoordsRectangleCorner(corner[[4]], i, 3, 8, 
                  1, 6, positions, color, width + 2)
                if (!is.null(transformation)) {
                  tkcoordsRectangleCorner(corner[[5]], i, 5, 
                    6, 7, 1, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[6]], i, 2, 
                    7, 6, 8, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[7]], i, 7, 
                    5, 2, 4, positions, color, width + 2)
                  tkcoordsRectangleCorner(corner[[8]], i, 6, 
                    2, 5, 3, positions, color, width + 2)
                }
            }
            "tkcoordsBar" <- function(line, i, positions, color = "black", 
                width = 1) {
                tkcoordsRectangleLine(line[[1]], i, 1, 3, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[2]], i, 4, 8, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[3]], i, 1, 4, positions, 
                  color, width)
                tkcoordsRectangleLine(line[[4]], i, 3, 8, positions, 
                  color, width)
            }
            positions <- retBlockPoints(i)
            if (!is.null(openBlockItem(i)$canvas)) 
                tkcoords(canvas, openBlockItem(i)$canvas, positions[1, 
                  1], positions[1, 2], positions[8, 1], positions[8, 
                  2])
            if (!is.null(openBlockItem(i)$rectangle)) 
                tkcoordsRectangle(openBlockItem(i)$rectangle, 
                  i, positions, color = color, width = 1)
            txt <- blockList[[i]]@label
            positions <- retBlockPoints(i, header = TRUE, n = nchar(txt))
            if (!is.null(openBlockItem(i)$bar)) 
                tkcoordsBar(openBlockItem(i)$bar, i, positions, 
                  color = color, width = 1)
            pos <- retBlockPos(i, 1) + c(8, 4, 0)
            tkcoords(canvas, openBlockItem(i)$label, pos[1], 
                pos[2])
        }
        "drawBlock" <- function(block, i, color = "Grey", box = FALSE, 
            lower = TRUE) {
            "drawRectangleLine" <- function(i, A, B, positions, 
                tag, color = "black", width = 1) {
                posA <- positions[A, ]
                posB <- positions[B, ]
                line <- tkcreate(canvas, "line", posA[1], posA[2], 
                  posB[1], posB[2], width = width, fill = color)
                tkaddtag(canvas, tag, "withtag", line)
                tkitembind(canvas, line, "<B1-Motion>", moveBlockLine(i, 
                  A, B))
                tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                if ((A == 1) && (B == 3)) 
                  cursor <- "left_side"
                else if ((A == 4) && (B == 8)) 
                  cursor <- "right_side"
                else if ((A == 1) && (B == 4)) 
                  cursor <- "top_side"
                else if ((A == 3) && (B == 8)) 
                  cursor <- "bottom_side"
                else if ((A == 5) && (B == 6)) 
                  cursor <- "left_side"
                else if ((A == 7) && (B == 2)) 
                  cursor <- "right_side"
                else if ((A == 5) && (B == 7)) 
                  cursor <- "top_side"
                else if ((A == 6) && (B == 2)) 
                  cursor <- "bottom_side"
                else if ((A == 1) && (B == 5)) 
                  cursor <- "top_side"
                else if ((A == 3) && (B == 6)) 
                  cursor <- "bottom_side"
                else if ((A == 4) && (B == 7)) 
                  cursor <- "top_side"
                else if ((A == 8) && (B == 2)) 
                  cursor <- "bottom_side"
                tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                  cursor = cursor))
                return(line)
            }
            "drawCornerLine" <- function(i, A, posA, posB, tag, 
                color = "black", width = 1) {
                diff <- posB - posA
                l <- sqrt(sum(diff^2))
                posB <- posA + diff * min(25, l)/l
                posA <- posA - diff * min(w/2, l)/l
                line <- tkcreate(canvas, "line", posA[1], posA[2], 
                  posB[1], posB[2], width = width, fill = color)
                tkaddtag(canvas, tag, "withtag", line)
                tkitembind(canvas, line, "<B1-Motion>", moveBlockPoint(i, 
                  A))
                tkitembind(canvas, line, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                if ((A == 1) || (A == 5)) 
                  cursor <- "top_left_corner"
                else if ((A == 8) || (A == 2)) 
                  cursor <- "bottom_right_corner"
                else if ((A == 4) || (A == 7)) 
                  cursor <- "top_right_corner"
                else if ((A == 3) || (A == 6)) 
                  cursor <- "bottom_left_corner"
                tkitembind(canvas, line, "<Enter>", function() tkconfigure(canvas, 
                  cursor = cursor))
                return(line)
            }
            "drawRectangleCorner" <- function(i, A, B, C, D, 
                positions, tag, color = "black", width = 2) {
                posA <- positions[A, ]
                l <- vector("list", 3)
                l[[1]] <- drawCornerLine(i, A, posA, positions[B, 
                  ], tag, color, width)
                l[[2]] <- drawCornerLine(i, A, posA, positions[C, 
                  ], tag, color, width)
                if (!is.null(transformation)) 
                  l[[3]] <- drawCornerLine(i, A, posA, positions[D, 
                    ], tag, color, width)
                return(l)
            }
            "drawRectangle" <- function(i, positions, tag, color = "black", 
                width = 1) {
                l <- vector("list", 12)
                l[[1]] <- drawRectangleLine(i, 1, 3, positions, 
                  tag, color, width)
                l[[2]] <- drawRectangleLine(i, 4, 8, positions, 
                  tag, color, width)
                l[[3]] <- drawRectangleLine(i, 1, 4, positions, 
                  tag, color, width)
                l[[4]] <- drawRectangleLine(i, 3, 8, positions, 
                  tag, color, width)
                if (!is.null(transformation)) {
                  l[[5]] <- drawRectangleLine(i, 5, 6, positions, 
                    tag, color, width)
                  l[[6]] <- drawRectangleLine(i, 7, 2, positions, 
                    tag, color, width)
                  l[[7]] <- drawRectangleLine(i, 5, 7, positions, 
                    tag, color, width)
                  l[[8]] <- drawRectangleLine(i, 6, 2, positions, 
                    tag, color, width)
                  l[[9]] <- drawRectangleLine(i, 1, 5, positions, 
                    tag, color, width)
                  l[[10]] <- drawRectangleLine(i, 3, 6, positions, 
                    tag, color, width)
                  l[[11]] <- drawRectangleLine(i, 4, 7, positions, 
                    tag, color, width)
                  l[[12]] <- drawRectangleLine(i, 8, 2, positions, 
                    tag, color, width)
                }
                c <- vector("list", 8)
                c[[1]] <- drawRectangleCorner(i, 1, 3, 4, 5, 
                  positions, tag, color, width + 2)
                c[[2]] <- drawRectangleCorner(i, 8, 4, 3, 2, 
                  positions, tag, color, width + 2)
                c[[3]] <- drawRectangleCorner(i, 4, 1, 8, 7, 
                  positions, tag, color, width + 2)
                c[[4]] <- drawRectangleCorner(i, 3, 8, 1, 6, 
                  positions, tag, color, width + 2)
                if (!is.null(transformation)) {
                  c[[5]] <- drawRectangleCorner(i, 5, 6, 7, 1, 
                    positions, tag, color, width + 2)
                  c[[6]] <- drawRectangleCorner(i, 2, 7, 6, 8, 
                    positions, tag, color, width + 2)
                  c[[7]] <- drawRectangleCorner(i, 7, 5, 2, 4, 
                    positions, tag, color, width + 2)
                  c[[8]] <- drawRectangleCorner(i, 6, 2, 5, 3, 
                    positions, tag, color, width + 2)
                }
                return(list(Lines = l, Corners = c))
            }
            "drawBar" <- function(i, positions, tag, box = FALSE, 
                color = "black", width = 1) {
                l <- vector("list", 4)
                l[[1]] <- drawRectangleLine(i, 1, 3, positions, 
                  tag, color, width)
                l[[2]] <- drawRectangleLine(i, 4, 8, positions, 
                  tag, color, width)
                l[[3]] <- drawRectangleLine(i, 1, 4, positions, 
                  tag, color, width)
                l[[4]] <- drawRectangleLine(i, 3, 8, positions, 
                  tag, color, width)
                return(l)
            }
            tag <- getTag("block", i)
            positions <- retBlockPoints(i)
            posA <- positions[1, ]
            posB <- positions[8, ]
            popupitems <- NULL
            blockcanvas <- NULL
            if (drawBlockBackground) 
                if (is.null(transformation)) {
                  blockcanvas <- tkcreate(canvas, "rectangle", 
                    posA[1], posA[2], posB[1], posB[2], fill = color(block))
                  tkaddtag(canvas, tag, "withtag", blockcanvas)
                  popupitems <- append(popupitems, list(blockcanvas))
                }
            if (drawBlockFrame) 
                Rectangle <- drawRectangle(i, positions, tag, 
                  color = color, width = 1)
            else Rectangle <- NULL
            txt <- blockLabels[i]
            positions <- retBlockPoints(i, header = TRUE, box = box, 
                n = nchar(txt))
            if (drawBlockFrame) {
                Bar <- drawBar(i, positions, tag, box = box, 
                  color = color, width = 2)
                popupitems <- append(popupitems, Bar)
            }
            else Bar <- NULL
            posA <- retBlockPos(i, 1)
            pos <- posA + c(8, 4, 0)
            label <- tkcreate(canvas, "text", pos[1], pos[2], 
                text = txt, anchor = "nw", font = "10x20")
            setOpenBlockItem(i, list(tag = tag, rectangle = Rectangle, 
                canvas = blockcanvas, bar = Bar, label = label, 
                block = i))
            setNodePopup(canvas, blockList[[i]], tag, popupitems, 
                label, i, "OpenBlock", UserMenus)
        }
        addNodePopups <- function(canvas, vertex, i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor"), nodePopupMenu, UserNodePopupItems) {
            label <- retVertexLabel(i, vertex.type)
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Vertex", 
                  label, "(echo index)"), command = function() print(paste("Hej from vertex", 
                  label, "with index", i)))
            else if ((vertex.type == "OpenBlock") || (vertex.type == 
                "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Block", 
                  label, "(echo index)"), command = function() print(paste("Hej from block", 
                  label, "with index", i)))
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Highlight for adding edge [ Click vertex ]"), 
                  command = function() {
                    subActivateVertex(i, color = "green", vertex.type = vertex.type)
                    message("Click the other vertex")
                  })
            else if ((vertex.type == "OpenBlock")) {
            }
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Highlight block for adding edges [ Click block ]"), 
                  command = function() {
                    subActivateVertex(i, color = "green", vertex.type = "ClosedBlock")
                    message("Click vertex or block")
                  })
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Adding edge after highlight (Here: Slave!) [ Click vertex ]"), 
                  command = newEdge(i, vertex.type = "Vertex", 
                    slave = TRUE))
            else if ((vertex.type == "OpenBlock")) {
            }
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Adds edges from/to block after highlight (Here: Slaves!) [ Click block ]"), 
                  command = newEdge(i, vertex.type = "ClosedBlock", 
                    slave = TRUE))
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste(" - Drag vertex: Move vertex"), 
                  command = function() {
                  })
            else if ((vertex.type == "OpenBlock")) {
                tkadd(nodePopupMenu, "command", label = paste(" - Drag block head:    Move block"), 
                  command = function() {
                  })
                tkadd(nodePopupMenu, "command", label = paste(" - Drag block corner:  Resize block"), 
                  command = function() {
                  })
            }
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste(" - Drag block:  Move minimized block"), 
                  command = function() {
                  })
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste(" - Drag label:    Move vertex label"), 
                  command = function() {
                  })
            else if ((vertex.type == "OpenBlock")) {
            }
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste(" - Drag label:   Move label of minimized block"), 
                  command = function() {
                  })
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Delete vertex [ Double click vertex ]"), 
                  command = function() {
                  })
            else if ((vertex.type == "OpenBlock")) {
                tkadd(nodePopupMenu, "command", label = paste("Minimize block [ Double click block head ]"), 
                  command = function() {
                    closeBlock(i)()
                  })
                tkadd(nodePopupMenu, "command", label = paste("Maximize block"), 
                  command = function() {
                    zoomPositions <- positionsBlocks[i, , ]
                    zoomPositions[, 1] <- zoomPositions[, 1] - 
                      2
                    zoomPositions[, 2] <- zoomPositions[, 2] + 
                      2
                    zoomPositions <<- zoomPositions
                    subUpdateGraphWindow("Maximize", updateAllVertices = TRUE, 
                      all.blockframes = TRUE)
                  })
                tkadd(nodePopupMenu, "command", label = paste("Redraw full graph"), 
                  command = function() {
                    zoomPositions <<- NULL
                    subUpdateGraphWindow("Redraw", updateAllVertices = TRUE, 
                      all.blockframes = TRUE)
                  })
            }
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Open block [ Double click minimized block ]"), 
                  command = function() {
                    openBlock(i)()
                  })
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Drop vertex   (Here: Slave!)"), 
                  command = function() subDropVertex(i, vertex.type = vertex.type, 
                    slave = TRUE))
            else if ((vertex.type == "OpenBlock")) {
            }
            else if ((vertex.type == "ClosedBlock")) {
            }
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Change label [ Double click label ]"), 
                  command = changeVertexLabel(i, vertex.type = vertex.type))
            else if ((vertex.type == "OpenBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Change label of block"), 
                  command = changeVertexLabel(i, vertex.type = "ClosedBlock"))
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Change label [ Double click label of minimized block ]"), 
                  command = changeVertexLabel(i, vertex.type = "ClosedBlock"))
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) 
                tkadd(nodePopupMenu, "command", label = paste("Delete vertex label"), 
                  command = deleteVertexLabel(i, vertex.type = "Vertex"))
            else if ((vertex.type == "OpenBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Delete label of block"), 
                  command = deleteVertexLabel(i, vertex.type = "ClosedBlock"))
            else if ((vertex.type == "ClosedBlock")) 
                tkadd(nodePopupMenu, "command", label = paste("Delete label of block"), 
                  command = deleteVertexLabel(i, vertex.type = "ClosedBlock"))
            if ((vertex.type == "Vertex") || (vertex.type == 
                "Factor")) {
            }
            else if ((vertex.type == "OpenBlock")) {
            }
            else if ((vertex.type == "ClosedBlock")) {
            }
            if (hasMethod("addToPopups", class(vertex))) 
                addToPopups(vertex, vertex.type, nodePopupMenu)
            NodePopup <- function(UserNodePopupItems, item, vertex.type) {
                force(UserNodePopupItems)
                force(item)
                force(vertex.type)
                function(...) {
                  updateArguments(UserNodePopupItems[[item]], 
                    blocks = TRUE)
                  UserNodePopupItems[[item]]$command(object, 
                    retVertexName(i, vertex.type), type = vertex.type, 
                    index = i, Arguments = Args())
                }
            }
            if (length(UserNodePopupItems) > 0) 
                for (item in seq(along = UserNodePopupItems)) if ((names(UserNodePopupItems[item]) == 
                  vertex.type)) 
                  tkadd(nodePopupMenu, "command", label = UserNodePopupItems[[item]]$label, 
                    command = NodePopup(UserNodePopupItems, item, 
                      vertex.type))
        }
        setNodePopup <- function(canvas, vertex, tag, result, 
            label, i, vertex.type, UserNodePopupItems) {
            f <- function(item, i, label = FALSE) {
                tkitembind(canvas, item, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                if (label) {
                  if (vertex.type == "ClosedBlock") 
                    tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                      cursor = "diamond_cross"))
                  else tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                    cursor = "hand2"))
                }
                else if (vertex.type == "ClosedBlock") 
                  tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                    cursor = "cross"))
                else tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                  cursor = "crosshair"))
                if (label) 
                  tkitembind(canvas, item, "<Button-1>", newEdge(i, 
                    vertex.type, slave = FALSE))
                else tkitembind(canvas, item, "<Button-1>", newEdge(i, 
                  vertex.type, slave = FALSE))
                if (label) 
                  tkitembind(canvas, item, "<B1-Motion>", moveVertexLabel(i, 
                    vertex.type))
                else tkitembind(canvas, item, "<B1-Motion>", 
                  moveVertex(i, vertex.type))
                if (label) 
                  tkitembind(canvas, item, "<Double-Button-1>", 
                    changeVertexLabel(i, vertex.type))
                else if (vertex.type == "ClosedBlock") 
                  tkitembind(canvas, item, "<Double-Button-1>", 
                    openBlock(i))
                else tkitembind(canvas, item, "<Double-Button-1>", 
                  undisplayVertex(i, vertex.type, slave = FALSE))
                if (label) 
                  tkitembind(canvas, item, "<Triple-Button-1>", 
                    deleteVertexLabel(i, vertex.type))
                tkitembind(canvas, item, "<Button-3>", callPopup(i, 
                  nodePopupMenu))
                tkaddtag(canvas, tag, "withtag", item)
            }
            blockitembind <- function(item, label = FALSE) {
                tkitembind(canvas, item, "<Leave>", function() tkconfigure(canvas, 
                  cursor = "arrow"))
                if (label) 
                  tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                    cursor = "diamond_cross"))
                else tkitembind(canvas, item, "<Enter>", function() tkconfigure(canvas, 
                  cursor = "cross_reverse"))
                tkitembind(canvas, item, "<B1-Motion>", moveBlock(i, 
                  1))
                tkitembind(canvas, item, "<Double-Button-1>", 
                  closeBlock(i))
                tkitembind(canvas, item, "<Button-3>", callPopup(i, 
                  nodePopupMenu))
                tkaddtag(canvas, tag, "withtag", item)
            }
            nodePopupMenu <- tkmenu(canvas, tearoff = FALSE)
            addNodePopups(canvas, vertex, i, vertex.type, nodePopupMenu, 
                UserNodePopupItems)
            if (vertex.type == "OpenBlock") {
                blockitembind(label, label = TRUE)
                if (!is.null(result)) 
                  if (length(result) > 0) 
                    for (k in seq(length(result))) blockitembind(result[[k]])
            }
            else {
                f(label, i, TRUE)
                if (!is.null(result$dynamic)) 
                  if (length(result$dynamic) > 0) 
                    for (k in seq(length(result$dynamic))) f(result$dynamic[[k]], 
                      i, FALSE)
                if (!is.null(result$fixed)) 
                  if (length(result$fixed) > 0) 
                    for (k in seq(length(result$fixed))) f(result$fixed[[k]], 
                      i, FALSE)
            }
        }
        subDrawVertex <- function(vertex, i, w = w, vertexcolor = vertexcolor, 
            vertex.type = ifelse(i > 0, "Vertex", "Factor")) {
            tag <- getTag(vertex.type, i)
            pos <- retVertexPos(i, vertex.type)
            if (hasMethod("draw", class(vertex))) 
                dot <- draw(vertex, canvas, pos, x = pos[1], 
                  y = pos[2], stratum = retStratum(i, vertex.type = vertex.type), 
                  w = w, color = vertexcolor, background = background)
            else {
                s <- w * sqrt(4/pi)
                p <- tkcreate(canvas, "oval", pos[1] - s, pos[2] - 
                  s, pos[1] + s, pos[2] + s, fill = vertexcolor)
                dot <- list(dynamic = list(p), fixed = NULL)
            }
            label <- tkcreate(canvas, "text", pos[1] + w, pos[2], 
                text = retVertexLabel(i, vertex.type), anchor = "nw", 
                font = "10x20")
            if (debug.strata && (vertex.type != "Factor") && 
                (vertex.type != "Extra")) {
                strata <- retStratum(i, vertex.type)
                color <- myColor(strata)
                numbers <- tkcreate(canvas, "text", pos[1] - 
                  4 * w, pos[2] - 4 * w, text = paste(i, strata, 
                  sep = "."), fill = color, anchor = "nw", font = "12x30")
                tkaddtag(canvas, tag, "withtag", numbers)
            }
            else numbers <- NULL
            if (vertex.type != "OpenBlock") 
                setNodePopup(canvas, vertex, tag, dot, label, 
                  i, vertex.type, UserMenus)
            else setNodePopup(canvas, vertex, tag, dot, label, 
                i, vertex.type, UserMenus)
            return(list(tag = tag, dot = dot, label = label, 
                numbers = numbers))
        }
        drawVertex <- function(i, w = w, vertexcolor = vertexcolor, 
            vertex.type = ifelse(i > 0, "Vertex", "Factor")) {
            if (vertex.type == "ClosedBlock") 
                itemsClosedBlocks[[i]] <<- subDrawVertex(blockList[[i]], 
                  i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type)
            else if (vertex.type == "Vertex") 
                itemsVertices[[i]] <<- subDrawVertex(vertexList[[i]], 
                  i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type)
            else if (vertex.type == "Factor") 
                itemsFactors[[-i]] <<- subDrawVertex(factorVertexList[[-i]], 
                  i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type)
            else if (vertex.type == "Extra") 
                itemsExtras[[i]] <<- subDrawVertex(extraList[[i]], 
                  i, w = w, vertexcolor = vertexcolor, vertex.type = vertex.type)
        }
        setActivatedVertex <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) activatedNode <<- list(number = i, 
            vertex.type = vertex.type)
        retActivatedVertex <- function() return(activatedNode[[1]])
        retActivatedVertexVertex.Type <- function() return(activatedNode[[2]])
        deActivateVertex <- function(i, color = retVertexColor(i, 
            vertex.type), vertex.type = ifelse(i > 0, "Vertex", 
            "Factor")) {
            if ((retActivatedVertex() == i) && (retActivatedVertexVertex.Type() == 
                vertex.type)) {
                setActivatedVertex(0, "Null")
                setVertexColor(i, color = color, vertex.type = vertex.type)
                return(TRUE)
            }
            else return(FALSE)
        }
        subActivateVertex <- function(i, color = "green", vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) if (!deActivateVertex(i, 
            "cyan", vertex.type)) 
            if (retActivatedVertex() == 0) {
                setActivatedVertex(i, vertex.type)
                setVertexColor(i, color = color, vertex.type = vertex.type)
                return(TRUE)
            }
            else return(FALSE)
        else return(TRUE)
        activateVertex <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            force(i)
            function(...) subActivateVertex(i, color = "green", 
                vertex.type = vertex.type)
        }
        activateEdge <- function(i, edge.type = "graphEdge") {
            force(i)
            force(edge.type)
            function(...) {
                activatedEdge <<- i
            }
        }
        retActivatedEdge <- function(edge.type = "graphEdge") return(activatedEdge)
        subUpdateGraphWindow <- function(txt = "", updateAllVertices = FALSE, 
            raiseEdges = FALSE, updateEdges = FALSE, all.blockframes = FALSE, 
            blockframes = NULL) {
            if (debug.update) 
                print(paste("subUpdateGraphWindow:", txt, " (0)"))
            if (debug.update) 
                print(paste("subUpdateGraphWindow: Blocks (1-1)"))
            if (!is.null(blockList)) 
                for (i in seq(along = blockList)) {
                  if (closedBlock[i]) {
                    pos <- retVertexPos(i, "ClosedBlock")
                    update.edges <- updateEdges || is.element(i, 
                      blockframes)
                    if (!is.null(itemsClosedBlocks[[i]])) {
                      pos <- retVertexPos(i, "ClosedBlock")
                      xy <- tkcoords(canvas, itemsClosedBlocks[[i]]$tag)
                      xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                        byrow = 2), 2, mean)
                      if (!any(is.nan(xy))) {
                        dxy <- findDifference(pos, c(xy, rep(0, 
                          N - 2)))
                        if ((sum(dxy[1:2]^2)) > 0) 
                          update.edges <- TRUE
                        tkmove(canvas, itemsClosedBlocks[[i]]$tag, 
                          dxy[1], dxy[2])
                      }
                      posLabel <- pos + retBlockLabelPos(i)
                      tkcoords(canvas, itemsClosedBlocks[[i]]$label, 
                        posLabel[1], posLabel[2])
                      tkitemconfigure(canvas, itemsClosedBlocks[[i]]$label, 
                        text = retVertexLabel(i, vertex.type = "ClosedBlock"))
                    }
                    if (debug.update) 
                      print(paste("subUpdateGraphWindow BlockEdges (1-2):", 
                        i))
                    if (!is.null(blockEdgeList)) 
                      if ((update.edges || raiseEdges) && (length(itemsBlockEdges[[i]]) > 
                        0)) 
                        for (e in itemsBlockEdges[[i]]) if (!(is.null(e))) 
                          if (TRUE) {
                            from.type <- vertexTypeOfEdge(-i, 
                              e$type)
                            to.type <- vertexTypeOfEdge(e$to, 
                              e$type)
                            posTo <- retVertexPos(e$to, to.type)
                            setEdgeCoords(e, pos, posTo, i, e$to, 
                              from.type, to.type)
                            l <- sqrt(sum((posTo - pos)^2))
                            if (l < 1) 
                              setEdgeLabel(e, " ", e$label.number, 
                                f = i)
                            else {
                              posLabel <- (pos + posTo)/2 + retEdgeLabelPos(e$label.number, 
                                i, e$to)
                              tkcoords(canvas, e$label, posLabel[1], 
                                posLabel[2])
                            }
                          }
                  }
                  else if (all.blockframes || (is.element(i, 
                    blockframes))) {
                    if (debug.update) 
                      print(paste("subUpdateGraphWindow Blockframe (1-3):", 
                        i))
                    tkitemconfigure(canvas, itemsOpenBlocks[[i]]$label, 
                      text = retVertexLabel(i, vertex.type = "ClosedBlock"))
                    tkcoordsBlock(i, lower = FALSE)
                  }
                }
            if (debug.update) 
                print(paste("subUpdateGraphWindow: Extras (3)"))
            if (!is.null(itemsExtras)) 
                for (i in seq(along = itemsExtras)) if (!is.null(itemsExtras[[i]]) && 
                  !is.null(itemsExtras[[i]][[1]])) {
                  if (updateAllVertices) {
                    tkdelete(canvas, vertexItem(i, vertex.type = "Extra")$tag)
                    drawVertex(i, w = w, vertexcolor = vertexColor, 
                      vertex.type = "Extra")
                  }
                  else {
                    pos <- retVertexPos(i, "Extra")
                    xy <- tkcoords(canvas, itemsExtras[[i]]$tag)
                    xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                      byrow = 2), 2, mean)
                    dxy <- findDifference(pos, c(xy, rep(0, N - 
                      2)))
                    tkmove(canvas, itemsExtras[[i]]$tag, dxy[1], 
                      dxy[2])
                    posLabel <- retLabelPos(i, vertex.type = "Extra")
                    xyl <- as.numeric(tkcoords(canvas, itemsExtras[[i]]$l))
                    tkitemconfigure(canvas, itemsExtras[[i]]$l, 
                      text = retVertexLabel(i, vertex.type = "Extra"))
                    dxy <- findDifference(posLabel, c(xyl, rep(0, 
                      N - 2)))
                    tkmove(canvas, itemsExtras[[i]]$l, dxy[1], 
                      dxy[2])
                  }
                }
            if (debug.update) 
                print(paste("subUpdateGraphWindow: Factors (4)"))
            if (!is.null(itemsFactors)) 
                for (i in seq(along = itemsFactors)) if (!is.null(itemsFactors[[i]]) && 
                  !is.null(itemsFactors[[i]][[1]])) {
                  vertex.indices <- factorVertexList[[i]]@vertex.indices
                  setFactorVertexPosition(i, vertex.indices)
                }
            if (debug.update) 
                print(paste("subUpdateGraphWindow: Vertices (5-1)"))
            for (i in seq(along = itemsVertices)) if (!is.null(itemsVertices[[i]]) && 
                !is.null(itemsVertices[[i]][[1]])) {
                pos <- retVertexPos(i, "Vertex")
                update.edges <- updateEdges || updateAllVertices
                if (!closedVertex[i]) {
                  if (updateAllVertices) {
                    tkdelete(canvas, vertexItem(i)$tag)
                    drawVertex(i, w = w, vertexcolor = vertexColor, 
                      vertex.type = "Vertex")
                  }
                  else {
                    xy <- tkcoords(canvas, itemsVertices[[i]]$tag)
                    xy <- apply(matrix(as.numeric(xy), ncol = 2, 
                      byrow = 2), 2, mean)
                    dxy <- findDifference(pos, c(xy, rep(0, N - 
                      2)))
                    if (debug.update) 
                      print(paste("subUpdateGraphWindow: Vertices (5-2)", 
                        i, paste("", dxy, collapse = ""), sum(dxy^2), 
                        sum(dxy[1:2]^2)))
                    if ((sum(dxy[1:2]^2)) > 0) 
                      update.edges <- TRUE
                    tkmove(canvas, itemsVertices[[i]]$tag, dxy[1], 
                      dxy[2])
                    posLabel <- retLabelPos(i, vertex.type = "Vertex")
                    xyl <- as.numeric(tkcoords(canvas, itemsVertices[[i]]$l))
                    tkitemconfigure(canvas, itemsVertices[[i]]$l, 
                      text = retVertexLabel(i, vertex.type = "Vertex"))
                    dxy <- findDifference(posLabel, c(xyl, rep(0, 
                      N - 2)))
                    tkmove(canvas, itemsVertices[[i]]$l, dxy[1], 
                      dxy[2])
                    if (debug.strata) {
                      strata <- retStratum(i, vertex.type = "Vertex")
                      color <- myColor(strata)
                      tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                        text = paste(i, strata, sep = "."))
                      tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                        fill = color)
                    }
                  }
                }
                else update.edges <- TRUE
                if (debug.update) 
                  print(paste("subUpdateGraphWindow: Edges (5-2)", 
                    i, update.edges))
                if ((update.edges || raiseEdges) && (length(itemsEdges[[i]]) > 
                  0)) 
                  for (e in itemsEdges[[i]]) if (!(is.null(e))) 
                    if (TRUE) {
                      from.type <- vertexTypeOfEdge(i, e$type)
                      to.type <- vertexTypeOfEdge(e$to, e$type)
                      posTo <- retVertexPos(e$to, to.type)
                      setEdgeCoords(e, pos, posTo, i, e$to, from.type, 
                        to.type)
                      l <- sqrt(sum((posTo - pos)^2))
                      if (l < 1) 
                        setEdgeLabel(e, " ", e$label.number, 
                          f = i)
                      else {
                        tkitemraise(canvas, e$edge)
                        tkitemraise(canvas, e$label)
                        posLabel <- (pos + posTo)/2 + retEdgeLabelPos(e$label.number, 
                          i, e$to)
                        tkcoords(canvas, e$label, posLabel[1], 
                          posLabel[2])
                      }
                    }
            }
            if (debug.update) 
                print(paste("subUpdateGraphWindow: Exit"))
        }
        setUpdateVertices <- function(txt = "") {
            updateCountVerticesMain <<- updateCountVerticesMain + 
                1
            updateCountVertices <<- updateCountVerticesMain
        }
        setUpdateLabels <- function(txt = "") {
            updateCountLabelsMain <<- updateCountLabelsMain + 
                1
            updateCountLabels <<- updateCountLabelsMain
        }
        setUpdateBlocks <- function(txt = "") {
            updateCountBlocksMain <<- updateCountBlocksMain + 
                1
            updateCountBlocks <<- updateCountBlocksMain
        }
        setUpdateBlockEdges <- function(txt = "", local = TRUE) {
            updateCountBlockEdgesMain <<- updateCountBlockEdgesMain + 
                1
            if (!local) 
                updateCountBlockEdges <<- updateCountBlockEdgesMain
        }
        setUpdateAll <- function(txt = "") {
            updateCountVerticesMain <<- updateCountVerticesMain + 
                1
            updateCountLabelsMain <<- updateCountLabelsMain + 
                1
            updateCountBlocksMain <<- updateCountBlocksMain + 
                1
            updateCountBlockEdgesMain <<- updateCountBlockEdgesMain + 
                1
            updateCountVertices <<- updateCountVerticesMain
            updateCountLabels <<- updateCountLabelsMain
            updateCountBlocks <<- updateCountBlocksMain
            updateCountBlockEdges <<- updateCountBlockEdgesMain
        }
        updatePositions <- function(txt = "") {
            force(txt)
            function(...) {
                if (debug.update) 
                  print(paste("updatePositions:", txt))
                n <- length(vertexList)
                m <- length(itemsVertices)
                if (n > m) 
                  for (i in seq(n - m)) {
                    closedVertex <<- c(closedVertex, FALSE)
                    itemsVertices <<- append(itemsVertices, list(NULL))
                    itemsEdges <<- append(itemsEdges, list(NULL))
                  }
                all.blockframes <- (updateCountBlocks < updateCountBlocksMain)
                updateEdges <- (updateCountBlockEdges < updateCountBlockEdgesMain)
                if (updateCountBlockEdges < updateCountBlockEdgesMain) 
                  updateVerticesStrata()
                if ((updateCountVertices < updateCountVerticesMain) || 
                  (updateCountLabels < updateCountLabelsMain) || 
                  updateEdges || all.blockframes) 
                  subUpdateGraphWindow(txt, all.blockframes = all.blockframes)
                if (updateEdges) 
                  updateBlockEdges()
                updateCountVertices <<- updateCountVerticesMain
                updateCountLabels <<- updateCountLabelsMain
                updateCountBlocks <<- updateCountBlocksMain
                updateCountBlockEdges <<- updateCountBlockEdgesMain
            }
        }
        tkdeleteRectangleCorner <- function(line) {
            tkdelete(canvas, line[[1]])
            tkdelete(canvas, line[[2]])
            if (!is.null(transformation)) 
                tkdelete(canvas, line[[3]])
        }
        tkdeleteRectangle <- function(rectangle) {
            line <- rectangle$Lines
            tkdelete(canvas, line[[1]])
            tkdelete(canvas, line[[2]])
            tkdelete(canvas, line[[3]])
            tkdelete(canvas, line[[4]])
            if (!is.null(transformation)) {
                tkdelete(canvas, line[[5]])
                tkdelete(canvas, line[[6]])
                tkdelete(canvas, line[[7]])
                tkdelete(canvas, line[[8]])
                tkdelete(canvas, line[[9]])
                tkdelete(canvas, line[[10]])
                tkdelete(canvas, line[[11]])
                tkdelete(canvas, line[[12]])
            }
            corner <- rectangle$Corners
            tkdeleteRectangleCorner(corner[[1]])
            tkdeleteRectangleCorner(corner[[2]])
            tkdeleteRectangleCorner(corner[[3]])
            tkdeleteRectangleCorner(corner[[4]])
            if (!is.null(transformation)) {
                tkdeleteRectangleCorner(corner[[5]])
                tkdeleteRectangleCorner(corner[[6]])
                tkdeleteRectangleCorner(corner[[7]])
                tkdeleteRectangleCorner(corner[[8]])
            }
        }
        tkdeleteBar <- function(line) {
            tkdelete(canvas, line[[1]])
            tkdelete(canvas, line[[2]])
            tkdelete(canvas, line[[3]])
            tkdelete(canvas, line[[4]])
        }
        deleteBlock <- function(i) {
            tkdeleteRectangle(openBlockItem(i)$rectangle)
            tkdeleteBar(openBlockItem(i)$bar)
            tkdelete(canvas, openBlockItem(i)$label)
            if (!is.null(openBlockItem(i)$canvas)) 
                tkdelete(canvas, openBlockItem(i)$canvas)
        }
        subSubDeleteEdge <- function(i, f, t, edge.type = "graphEdge") {
            if (debug.position) 
                print(paste("subSubDeleteEdge", i, f, t, edge.type))
            E <- returnEdges(edge.type = edge.type)[[i]]
            delete <- function(k, edges) {
                edges <- edgeItem(k, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) 
                    if ((e$nr == i) && (e$type == edge.type)) 
                      if (e$to > k) {
                        tkdelete(canvas, e$edge)
                        tkdelete(canvas, e$label)
                      }
            }
            for (j in E@vertex.indices) delete(j, edgeItem(j, 
                edge.type = edge.type))
            remove <- function(k, edges) if (length(edges) > 
                0) {
                result <- NULL
                for (e in edges) if (!(is.null(e))) 
                  if (!((e$nr == i) && (e$type == edge.type))) 
                    result <- c(result, list(e))
                if (is.null(result)) 
                  setEdgeItem(k, edge.type = edge.type, list(NULL))
                else setEdgeItem(k, edge.type = edge.type, result)
            }
            for (j in E@vertex.indices) remove(j, edgeItem(j, 
                edge.type = edge.type))
        }
        subSubUndisplayFactorVertex <- function(i, edge.type = "factorEdge") {
            edges <- edgeItem(i, edge.type = edge.type)
            if (length(edges) > 0) 
                for (e in edges) if (!(is.null(e))) {
                  subSubDeleteEdge(e$nr, i, e$to, edge.type = edge.type)
                  clearEdge(e$nr, edge.type = edge.type)
                }
            visibleVertices <<- visibleVertices[visibleVertices != 
                i]
            tkdelete(canvas, vertexItem(i)$tag)
            setVertexItem(i, list(NULL))
        }
        clearFactorEdges <- function() {
            for (f in seq(along = itemsFactors)) subSubUndisplayFactorVertex(-f)
        }
        subSubUndisplayVertex <- function(i, edge.type = "graphEdge") {
            edges <- edgeItem(i, edge.type = edge.type)
            if (length(edges) > 0) 
                for (e in edges) if (!(is.null(e))) 
                  if ((e$type == edge.type)) {
                    subSubDeleteEdge(e$nr, i, e$to, edge.type = edge.type)
                    clearEdge(e$nr, edge.type = edge.type)
                  }
            visibleVertices <<- visibleVertices[visibleVertices != 
                i]
            tkdelete(canvas, vertexItem(i)$tag)
            setVertexItem(i, list(NULL))
        }
        update.edge.labels <- function() {
            subUpdateEdgeLabels <- function(itemsNodes, edge.type = "graphEdge") for (f in seq(along = itemsNodes)) if (!is.null(itemsNodes[[f]]) && 
                !is.null(itemsNodes[[f]][[1]])) {
                if (edge.type != "graphEdge") 
                  f <- -f
                edges <- edgeItem(f, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) 
                    if (e$to < f) {
                      if (namesOnEdges) {
                        vertexnames <- c(retVertexName(f, vertexTypeOfEdge(f, 
                          e$type)), retVertexName(e$to, vertexTypeOfEdge(e$to, 
                          e$type)))
                        if (e$reverse) 
                          vertexnames <- rev(vertexnames)
                        label <- paste(vertexnames, collapse = "~")
                      }
                      else label <- ""
                      setEdgeLabel(e, label, e$label.number, 
                        f = f)
                      setEdgeWidth(e, 2, e$label.number, f = f)
                    }
            }
            if (updateEdgeLabels) {
                subUpdateEdgeLabels(itemsVertices, edge.type = "graphEdge")
                subUpdateEdgeLabels(itemsFactors, edge.type = "factorEdge")
                subUpdateEdgeLabels(itemsClosedBlocks, edge.type = "blockEdge")
            }
        }
        updateBlockEdges <- function() {
            if (debug.update) 
                print(paste("updateBlockEdges"))
            Edges <- selectCurrentEdges(omitEdges = FALSE, edge.type = "graphEdge")
            edge.list <- lapply(Edges, function(i) i@vertex.indices)
            verticesUpdate()
            NewBlockEdges <- returnBlockEdgeList(edge.list, vertexList, 
                blockList, color = blockEdgeColor, oriented = oriented)
            new.list <- lapply(NewBlockEdges, function(i) {
                x <- i@vertex.indices
                names(x) <- NULL
                x
            })
            old.list <- lapply(returnEdges(edge.type = "blockEdge"), 
                function(i) {
                  x <- i@vertex.indices
                  names(x) <- NULL
                  x
                })
            match.old.new <- match(old.list, new.list)
            for (i in seq(along = match.old.new)) if (is.na(match.old.new[i])) 
                if (all(abs(old.list[[i]]) > 0)) {
                  subSubDeleteEdge(i, old.list[[i]][1], old.list[[i]][2], 
                    edge.type = "blockEdge")
                  clearEdge(i, edge.type = "blockEdge")
                }
            match.new.old <- match(new.list, old.list)
            for (i in seq(along = match.new.old)) if (is.na(match.new.old[i])) {
                E <- append.edge(NewBlockEdges[[i]], edge.type = "blockEdge")
                drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                  edge.type = "blockEdge")
            }
        }
        deleteAllEdgeLabels <- function() {
            function(...) {
                g <- function(items, edge.type = "graphEdge") for (f in seq(along = items)) if (!is.null(items[[f]]) && 
                  !is.null(items[[f]][[1]])) {
                  ff <- ifelse(edge.type == "graphEdge", f, -f)
                  edges <- edgeItem(ff, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if ((e$to < f) || ((ff < 0) && (f < e$to))) {
                        setEdgeLabel(e, "", e$label.number, f = ff)
                        setEdgeWidth(e, 2, e$label.number, f = ff)
                      }
                }
                g(itemsVertices, edge.type = "graphEdge")
                g(itemsClosedBlocks, edge.type = "blockEdge")
                g(itemsFactors, edge.type = "factorEdge")
            }
        }
        moveEdgesToVertex <- function(X, v, edge.type = "graphEdge") {
            edges <- edgeItem(v, edge.type = edge.type)
            if (length(edges) > 0) 
                for (e in edges) if (!(is.null(e))) {
                  from.type <- vertexTypeOfEdge(v, e$type)
                  to.type <- vertexTypeOfEdge(e$to, e$type)
                  posTo <- retVertexPos(e$to, to.type)
                  setEdgeCoords(e, X, posTo, v, e$to, from.type, 
                    to.type)
                  pos <- (X + posTo)/2 + retEdgeLabelPos(e$label.number, 
                    v, e$to)
                  tkcoords(canvas, e$label, pos[1], pos[2])
                }
        }
        moveVerticesInBlock <- function(i, dxy, move.vertices = TRUE) {
            for (v in seq(along = vertexList)) if (is.element(v, 
                visibleVertices)) 
                if ((blockReferences[retStratum(v, vertex.type = "Vertex")] == 
                  i)) {
                  if (move.vertices) {
                    tkmove(canvas, vertexItem(v)$tag, dxy[1], 
                      dxy[2])
                    changeVertexPos(v, dxy)
                  }
                  pos <- retVertexPos(v, "Vertex")
                  moveEdgesToVertex(pos, v, edge.type = "graphEdge")
                }
        }
        subMoveVertex <- function(i, vertex.type, posFrom, posTo) {
            dxy <- findDifference(posTo, posFrom)
            tag <- vertexItem(i, vertex.type)$tag
            tkmove(canvas, tag, dxy[1], dxy[2])
            tkitemraise(canvas, tag)
            if (debug.strata && (vertex.type != "Factor") && 
                (vertex.type != "Extra") && (vertex.type != "ClosedBlock")) {
                strata <- retStratum(i, vertex.type)
                color <- myColor(strata)
                tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                  text = paste(i, strata, sep = "."))
                tkitemconfigure(canvas, itemsVertices[[i]]$numbers, 
                  fill = color)
            }
            setVertexPos(i, posTo, dxy, vertex.type)
            if (vertex.type == "ClosedBlock") {
                moveVerticesInBlock(i, dxy, move.vertices = FALSE)
                moveEdgesToVertex(posTo, -i, edge.type = "blockEdge")
            }
            else if (vertex.type == "Vertex") {
                if (closedVertex[i]) 
                  posTo <- retVertexPos(i, "Vertex")
                moveEdgesToVertex(posTo, i, edge.type = "graphEdge")
            }
            else if (vertex.type != "Extra") {
                if (closedVertex[-i]) 
                  posTo <- retVertexPos(i, vertex.type)
                moveEdgesToVertex(posTo, i, edge.type = "graphEdge")
            }
        }
        setFactorVertexPosition <- function(i, vertex.indices) {
            posFrom <- retVertexPos(-i, "Factor")
            positions <- NULL
            for (j in seq(along = vertex.indices)) if (vertex.indices[j] > 
                0) 
                positions <- rbind(positions, positionsVertices[vertex.indices[j], 
                  ])
            position <- apply(positions, 2, mean)
            posTo <- positionsCanvas(project(position))
            subMoveVertex(-i, "Factor", posFrom, posTo)
        }
        moveFactorVertex <- function(vertex) {
            if (!is.null(itemsFactors)) 
                for (i in seq(along = itemsFactors)) if (!is.null(itemsFactors[[i]]) && 
                  !is.null(itemsFactors[[i]][[1]])) {
                  vertex.indices <- factorVertexList[[i]]@vertex.indices
                  if (is.element(vertex, vertex.indices)) 
                    setFactorVertexPosition(i, vertex.indices)
                }
        }
        moveVertex <- function(i, vertex.type = ifelse(i > 0, 
            "Vertex", "Factor")) {
            force(i)
            force(vertex.type)
            function(x, y) {
                deActivateVertex(i, retVertexColor(i, vertex.type), 
                  vertex.type)
                posFrom <- retVertexPos(i, vertex.type)
                posTo <- replaceXY(x, y, posFrom)
                subMoveVertex(i, vertex.type, posFrom, posTo)
                moveFactorVertex(i)
                setUpdateVertices("")
            }
        }
        moveVertexLabel <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            force(i)
            force(vertex.type)
            function(x, y) {
                deActivateVertex(i, retVertexColor(i, vertex.type), 
                  vertex.type)
                if (vertex.type == "ClosedBlock") {
                  posFrom <- retVertexPos(i, vertex.type) + retBlockLabelPos(i)
                  X <- replaceXY(x, y, posFrom)
                  dxy <- findDifference(X, posFrom)
                  tkcoords(canvas, vertexItem(i, vertex.type)$label, 
                    x, y)
                  setLabelPos(i, X, dxy, vertex.type)
                }
                else {
                  posFrom <- retLabelPos(i, vertex.type)
                  X <- replaceXY(x, y, posFrom)
                  dxy <- findDifference(X, posFrom)
                  tkmove(canvas, vertexItem(i, vertex.type)$label, 
                    dxy[1], dxy[2])
                  setLabelPos(i, X, dxy, vertex.type)
                }
                setUpdateLabels("")
            }
        }
        changeVertexLabel <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            force(i)
            force(vertex.type)
            function(...) {
                ReturnVal <- modalDialog("Label Entry", "Enter new label", 
                  retVertexLabel(i, vertex.type), GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                setVertexLabel(i, ReturnVal, vertex.type)
                setUpdateLabels("")
            }
        }
        deleteVertexLabel <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor")) {
            force(i)
            force(vertex.type)
            function(...) {
                setVertexLabel(i, "", vertex.type)
                setUpdateLabels("")
            }
        }
        changeEdgeLabel <- function(i, f, t, edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(edge.type)
            function(...) {
                if (retActivatedEdge() == i) {
                  edges <- edgeItem(f, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if (e$nr == i) 
                        if (e$to == t) {
                          label <- paste(edge.names(i, edge.type = edge.type), 
                            collapse = "%")
                          ReturnVal <- modalDialog("Edge label Entry", 
                            "Enter new label", label, GraphWindow@top)
                          if (ReturnVal == "ID_CANCEL") 
                            return()
                          setEdgeLabel(e, ReturnVal, e$label.number, 
                            f = f)
                        }
                }
            }
        }
        subDropVertex <- function(i, vertex.type = "Vertex", 
            slave = TRUE) {
            if (debug.position) 
                print(paste("subDropVertex", i, vertex.type))
            tkconfigure(canvas, cursor = "watch")
            graphEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                edge.type = "graphEdge"), edge.type = "graphEdge")
            factorEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                edge.type = "factorEdge"), edge.type = "factorEdge")
            blockEdges <- copyCurrentEdges(omitEdges = vertex.in.edge(i, 
                edge.type = "blockEdge"), edge.type = "blockEdge")
            if (vertex.type == "Vertex") {
                newEdges <- list(graphEdges = graphEdges, factorEdges = factorEdges, 
                  blockEdges = blockEdges)
            }
            else if (vertex.type == "Factor") 
                newEdges <- list(graphEdges = graphEdges, factorEdges = factorEdges, 
                  blockEdges = NULL)
            else if (vertex.type == "closedBlock") 
                newEdges <- list(graphEdges = graphEdges, factorEdges = NULL, 
                  blockEdges = NULL)
            R <- NULL
            if (is.null(object)) 
                R <- TRUE
            Arguments <- Args()
            if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                class(object)))) 
                R <- modifyModel(object, action = "dropVertex", 
                  name = retVertexName(i, vertex.type), index = i, 
                  type = vertex.type, newEdges = newEdges, visibleVertices = visibleVertices[visibleVertices != 
                    i], Arguments = Arguments)
            if (!is.null(R) && !is.null(R$object)) {
                object <<- R$object
                if (!is.null(objectName)) 
                  assign(objectName, object, pos = 1)
            }
            if (!is.null(R)) 
                if (slave) {
                  if (is.null(R$newEdges$graphEdges)) 
                    Edges <- newEdges$graphEdges
                  else Edges <- R$newEdges$graphEdges
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = NULL, edgeList = Edges, blockEdgeList = blockEdgeList, 
                    factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                    visibleVertices = visibleVertices[visibleVertices != 
                      i], extraList = R$ExtraVertices, object = R$object, 
                    title = "Slave window", Arguments = Arguments)
                }
                else {
                  subSubUndisplayVertex(i)
                  updateBlockEdges()
                  clearFactorEdges()
                  update.edge.labels()
                  if (!is.null(R$FactorVertices) && !is.null(R$FactorEdges)) 
                    drawFactors(R$FactorEdges, R$FactorVertices)
                }
            else {
                message("Null result in dropVertex")
            }
            tkconfigure(canvas, cursor = "arrow")
        }
        undisplayVertex <- function(i, vertex.type = ifelse(i > 
            0, "Vertex", "Factor"), slave = TRUE) {
            force(i)
            force(slave)
            function(...) {
                deActivateVertex(i, retVertexColor(i, vertex.type), 
                  vertex.type)
                subDropVertex(i, vertex.type, slave)
            }
        }
        moveEdge <- function(i, f, t, edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(edge.type)
            function(x, y) {
                activateEdge(0, edge.type = edge.type)()
                from.type <- vertexTypeOfEdge(f, edge.type)
                to.type <- vertexTypeOfEdge(t, edge.type)
                posFrom <- retVertexPos(f, from.type)
                posTo <- retVertexPos(t, to.type)
                pos <- (posFrom + posTo)/2
                X <- replaceXY(x, y, pos)
                dxy <- findDifference(X, pos)
                if (sum(dxy^2) < 0.25^2 * sum((posFrom - posTo)^2)) {
                  fun <- function(ii, vertex.type, position) {
                    if (vertex.type == "ClosedBlock") {
                      if (closedBlock[-ii]) 
                        tkmove(canvas, vertexItem(-ii, vertex.type = vertex.type)$tag, 
                          dxy[1], dxy[2])
                      setVertexPos(-ii, position + dxy, dxy, 
                        vertex.type = vertex.type)
                    }
                    else if (!closedVertex[ii]) {
                      tkmove(canvas, vertexItem(ii)$tag, dxy[1], 
                        dxy[2])
                      setVertexPos(ii, position + dxy, dxy, vertex.type = vertex.type)
                    }
                    edges <- edgeItem(ii, edge.type = edge.type)
                    if (length(edges) > 0) 
                      for (e in edges) if (!(is.null(e))) {
                        from.type <- vertexTypeOfEdge(ii, e$type)
                        to.type <- vertexTypeOfEdge(e$to, e$type)
                        pos <- retVertexPos(e$to, to.type)
                        setEdgeCoords(e, position, pos, ii, e$to, 
                          from.type, to.type)
                        pos <- (pos + position)/2 + retEdgeLabelPos(e$label.number, 
                          ii, e$to)
                        tkcoords(canvas, e$label, pos[1], pos[2])
                      }
                  }
                  fun(f, from.type, posFrom)
                  fun(t, to.type, posTo)
                }
                setUpdateVertices("")
            }
        }
        moveEdgeLabel <- function(i, f, t, edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(edge.type)
            function(x, y) {
                activateEdge(0, edge.type = edge.type)()
                edges <- edgeItem(f, edge.type = edge.type)
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) 
                    if (e$nr == i) 
                      if ((e$to == t)) 
                        if (!(e$reverse) || !oriented) {
                          from.type <- vertexTypeOfEdge(f, edge.type)
                          to.type <- vertexTypeOfEdge(t, edge.type)
                          pos <- (retVertexPos(t, to.type) + 
                            retVertexPos(f, from.type))/2
                          pos <- pos + retEdgeLabelPos(e$label.number, 
                            f, e$to)
                          X <- replaceXY(x, y, pos)
                          dxy <- findDifference(X, pos)
                          tkcoords(canvas, e$label, x, y)
                          setEdgeLabelPos(e, e$label.number, 
                            X, dxy, f = f)
                        }
            }
        }
        subSubAddEdge <- function(f, t, from.type, to.type, edge.type = "graphEdge", 
            slave = TRUE) {
            fun <- function(result, ff, tt, edge.type = "graphEdge") {
                if (!any(which.edge(c(ff, tt), edge.type = edge.type)) && 
                  (!any(which.edge(c(tt, ff), edge.type = edge.type)) || 
                    oriented)) 
                  return(append(result, list(c(ff, tt))))
                else return(result)
            }
            if (debug.position) 
                print(paste("subSubAddEdge", f, t, from.type, 
                  to.type, edge.type))
            result <- NULL
            redraw <- FALSE
            if (edge.type == "graphEdge") {
                result <- fun(result, f, t, edge.type = "graphEdge")
            }
            else if (edge.type == "extraEdge") {
                message("No edges to extra vertices!")
            }
            else if (edge.type == "factorEdge") {
                message("Resulting edges should be returned from modifyModel!")
                redraw <- TRUE
            }
            else if (edge.type == "factorBlockEdge") {
                message("Resulting edges should be returned from modifyModel!")
                message("Factoredges are only draw between vertices and factors!")
                redraw <- TRUE
            }
            else if (edge.type == "blockEdge") {
                from.stratum <- f
                from.stratum <- c(from.stratum, blockList[[from.stratum]]@descendants)
                to.stratum <- t
                to.stratum <- c(to.stratum, blockList[[to.stratum]]@descendants)
                if (from.type == "Vertex") {
                  for (w in seq(along = vertexList)) if (is.element(w, 
                    visibleVertices)) 
                    if (is.element(retStratum(w, vertex.type = "Vertex"), 
                      to.stratum)) 
                      result <- fun(result, f, w, edge.type = "graphEdge")
                }
                else if (to.type == "Vertex") {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if (is.element(retStratum(v, vertex.type = "Vertex"), 
                      from.stratum)) 
                      result <- fun(result, v, t, edge.type = "graphEdge")
                }
                else {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if (is.element(retStratum(v, vertex.type = "Vertex"), 
                      from.stratum)) 
                      for (w in seq(along = vertexList)) if (is.element(w, 
                        visibleVertices)) 
                        if (is.element(retStratum(w, vertex.type = "Vertex"), 
                          to.stratum)) 
                          result <- fun(result, v, w, edge.type = "graphEdge")
                }
            }
            if (redraw || !is.null(result)) {
                graphEdges <- appendToCurrentEdges(omitEdges = FALSE, 
                  new.edge = result, edge.type = "graphEdge")
                newEdges <- list(graphEdges = graphEdges, factorEdges = NULL, 
                  blockEdges = NULL)
                R <- NULL
                if (is.null(object)) 
                  R <- TRUE
                Arguments <- Args()
                if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                  class(object)))) 
                  R <- modifyModel(object, action = "addEdge", 
                    name.1 = retVertexName(f, from.type), name.2 = retVertexName(t, 
                      from.type), from = f, to = t, from.type = from.type, 
                    to.type = to.type, newEdges = newEdges, Arguments = Arguments)
                if (!is.null(R) && !is.null(R$object)) {
                  object <<- R$object
                  if (!is.null(objectName)) 
                    assign(objectName, object, pos = 1)
                }
                if (!is.null(R)) {
                  if (slave) {
                    message("redrawGraphWindow in addEdge")
                    if (is.null(R$edgeList)) 
                      if (is.null(R$newEdges$graphEdges)) 
                        Edges <- newEdges$graphEdges
                      else Edges <- R$newEdges$graphEdges
                    else Edges <- R$edgeList
                    redrawGraphWindow(graphLattice = GraphLattice, 
                      graphWindow = NULL, edgeList = Edges, blockEdgeList = blockEdgeList, 
                      factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                      visibleVertices = visibleVertices, extraList = R$ExtraVertices, 
                      object = R$object, title = "Slave window", 
                      Arguments = Arguments)
                  }
                  else if (redraw) {
                    redrawGraphWindow(graphLattice = GraphLattice, 
                      graphWindow = GraphWindow, edgeList = R$edgeList, 
                      blockEdgeList = blockEdgeList, factorVertexList = R$FactorVertices, 
                      factorEdgeList = R$FactorEdges, visibleVertices = visibleVertices, 
                      extraList = R$ExtraVertices, object = R$object, 
                      title = title, Arguments = Arguments)
                  }
                  else {
                    for (i in seq(along = result)) {
                      E <- append.index.edge(result[[i]], edge.type = "graphEdge")
                      drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                        edge.type = "graphEdge")
                    }
                    activateEdge(0, edge.type = edge.type)()
                    setActivatedVertex(0, "Vertex")
                    updateBlockEdges()
                    clearFactorEdges()
                    update.edge.labels()
                    if (!is.null(R$FactorVertices) && !is.null(R$FactorEdges)) 
                      drawFactors(R$FactorEdges, R$FactorVertices)
                  }
                }
                else {
                  message("Null result in addEdge")
                }
            }
        }
        subAddEdge <- function(f, t, from.type, to.type, edge.type = "graphEdge", 
            slave = TRUE) {
            tkconfigure(canvas, cursor = "watch")
            if (as.logical(debug.edges) && (as.numeric(debug.edges) > 
                1)) 
                print(paste("subAddEdge", f, t, from.type, to.type))
            if ((from.type != "Extra") && (to.type != "Extra")) 
                subSubAddEdge(f, t, from.type, to.type, edge.type = edge.type, 
                  slave)
            setVertexColor(f, color = retVertexColor(i, from.type), 
                from.type)
            tkconfigure(canvas, cursor = "arrow")
        }
        newEdge <- function(i, vertex.type = ifelse(i > 0, "Vertex", 
            "Factor"), slave = TRUE) {
            force(i)
            force(slave)
            force(vertex.type)
            function(...) {
                from.type <- retActivatedVertexVertex.Type()
                f <- retActivatedVertex()
                t <- i
                if (as.logical(debug.edges) && (as.numeric(debug.edges) > 
                  1)) 
                  print(paste("newEdge", f, t, from.type, vertex.type))
                if (!subActivateVertex(i, "green", vertex.type)) 
                  if ((f != 0) && !((f == i) && (from.type == 
                    vertex.type))) {
                    edge.type <- "graphEdge"
                    if ((vertex.type == "Factor") || (from.type == 
                      "Factor")) 
                      edge.type <- "factorEdge"
                    if ((vertex.type == "ClosedBlock") || (from.type == 
                      "ClosedBlock")) 
                      if ((edge.type == "factorEdge")) 
                        edge.type <- "factorBlockEdge"
                      else edge.type <- "blockEdge"
                    if ((vertex.type == "Extra") || (from.type == 
                      "Extra")) 
                      edge.type <- "extraEdge"
                    subAddEdge(f, t, from.type, vertex.type, 
                      edge.type = edge.type, slave)
                  }
            }
        }
        addLastEdge <- function(vertex.type = ifelse(i > 0, "Vertex", 
            "Factor")) {
            function() {
                n <- length(vertexList)
                f <- retActivatedVertex()
                if ((f > 0) && (retActivatedVertexVertex.Type() == 
                  "Vertex")) 
                  setVertexColor(f, color = retVertexColor(i, 
                    vertex.type), vertex.type)
                setActivatedVertex(n - 1, vertex.type)
                newEdge(n, vertex.type = "Vertex", slave = FALSE)()
            }
        }
        subSubDropEdge <- function(i, f, t, from.type = vertexTypeOfEdge(f, 
            edge.type), to.type = vertexTypeOfEdge(t, edge.type), 
            edge.type = "graphEdge", slave = TRUE) {
            if (as.logical(debug.edges) && (as.numeric(debug.edges) > 
                1)) 
                print(paste("subSubDropEdge", f, t, from.type, 
                  to.type, edge.type))
            j.g <- FALSE
            redraw <- FALSE
            if (edge.type == "graphEdge") {
                j.g <- which.unordered.edge(c(t, f), edge.type = edge.type)
                graphEdges <- selectCurrentEdges(omitEdges = j.g, 
                  edge.type = edge.type)
                newEdges <- list(graphEdges = graphEdges, factorEdges = NULL, 
                  blockEdges = NULL)
            }
            else if (edge.type == "factorEdge") {
                message("Resulting edges should be returned from modifyModel!")
                redraw <- TRUE
                newEdges <- list(graphEdges = NULL, factorEdges = NULL, 
                  blockEdges = NULL)
            }
            else if (edge.type == "factorBlockEdge") {
                message("Not possible: Factoredges are only draw between vertices and factors!")
                newEdges <- list(graphEdges = NULL, factorEdges = NULL, 
                  blockEdges = NULL)
            }
            else if (edge.type == "blockEdge") {
                from.stratum <- -f
                if (from.stratum > 0) 
                  from.stratum <- c(from.stratum, blockList[[from.stratum]]@descendants)
                to.stratum <- -t
                if (to.stratum > 0) 
                  to.stratum <- c(to.stratum, blockList[[to.stratum]]@descendants)
                if (from.type == "Vertex") {
                  for (w in seq(along = vertexList)) if (is.element(w, 
                    visibleVertices)) 
                    if (is.element(retStratum(w, vertex.type = "Vertex"), 
                      to.stratum)) 
                      j.g <- j.g | which.unordered.edge(c(f, 
                        w), edge.type = "graphEdge")
                }
                else if (to.type == "Vertex") {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if (is.element(retStratum(v, vertex.type = "Vertex"), 
                      from.stratum)) 
                      j.g <- j.g | which.unordered.edge(c(v, 
                        t), edge.type = "graphEdge")
                }
                else {
                  for (v in seq(along = vertexList)) if (is.element(v, 
                    visibleVertices)) 
                    if (is.element(retStratum(v, vertex.type = "Vertex"), 
                      from.stratum)) 
                      for (w in seq(along = vertexList)) if (is.element(w, 
                        visibleVertices)) 
                        if (is.element(retStratum(w, vertex.type = "Vertex"), 
                          to.stratum)) 
                          if (v != w) 
                            j.g <- j.g | which.unordered.edge(c(v, 
                              w), edge.type = "graphEdge")
                }
                graphEdges <- selectCurrentEdges(omitEdges = j.g, 
                  edge.type = "graphEdge")
                newEdges <- list(graphEdges = graphEdges, factorEdges = NULL, 
                  blockEdges = NULL)
            }
            R <- NULL
            if (is.null(object)) 
                R <- TRUE
            Arguments <- Args()
            if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                class(object)))) 
                R <- modifyModel(object, action = "dropEdge", 
                  name.1 = retVertexName(f, from.type), name.2 = retVertexName(t, 
                    to.type), from = f, to = t, from.type = from.type, 
                  to.type = to.type, edge.index = i, newEdges = newEdges, 
                  Arguments = Arguments)
            if (!is.null(R) && !is.null(R$object)) {
                object <<- R$object
                if (!is.null(objectName)) 
                  assign(objectName, object, pos = 1)
            }
            if (!is.null(R)) 
                if (slave) {
                  message("redrawGraphWindow in dropEdge")
                  if (is.null(R$edgeList)) 
                    if (is.null(R$newEdges$graphEdges)) 
                      Edges <- newEdges$graphEdges
                    else Edges <- R$newEdges$graphEdges
                  else Edges <- R$edgeList
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = NULL, edgeList = Edges, blockEdgeList = blockEdgeList, 
                    factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                    visibleVertices = visibleVertices, extraList = R$ExtraVertices, 
                    object = R$object, title = "Slave window", 
                    Arguments = Arguments)
                }
                else if (redraw) {
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = GraphWindow, edgeList = R$edgeList, 
                    blockEdgeList = blockEdgeList, factorVertexList = R$FactorVertices, 
                    factorEdgeList = R$FactorEdges, visibleVertices = visibleVertices, 
                    extraList = R$ExtraVertices, object = R$object, 
                    title = title, Arguments = Arguments)
                }
                else {
                  subSubDeleteEdge(i, f, t, edge.type = edge.type)
                  clearEdge(i, edge.type = edge.type)
                  if (edge.type == "blockEdge") {
                    for (k in seq(along = j.g)) if (j.g[k]) {
                      subSubDeleteEdge(k, f, t, edge.type = "graphEdge")
                      clearEdge(k, edge.type = "graphEdge")
                    }
                  }
                  activateEdge(0, edge.type = edge.type)()
                  updateBlockEdges()
                  clearFactorEdges()
                  update.edge.labels()
                  if (!is.null(R$FactorVertices) && !is.null(R$FactorEdges)) 
                    drawFactors(R$FactorEdges, R$FactorVertices)
                }
            else message("Null result in dropEdge")
        }
        subDropEdge <- function(i, f, t, from.type = vertexTypeOfEdge(f, 
            edge.type), to.type = vertexTypeOfEdge(t, edge.type), 
            from.all = FALSE, to.all = FALSE, edge.type = "graphEdge", 
            slave = TRUE) {
            tkconfigure(canvas, cursor = "watch")
            if (as.logical(debug.edges) && (as.numeric(debug.edges) > 
                1)) 
                print(paste("subDropEdge", f, t, from.type, to.type, 
                  from.all, to.all, edge.type, slave))
            if (from.type == "ClosedBlock") 
                from.stratum <- f
            else from.stratum <- retStratum(f, vertex.type = from.type)
            if (to.type == "ClosedBlock") 
                to.stratum <- t
            else to.stratum <- retStratum(t, vertex.type = to.type)
            if (from.all && ((from.type == "ClosedBlock") || 
                ((from.type == "Vertex")))) {
                for (v in seq(along = vertexList)) if (is.element(v, 
                  visibleVertices)) 
                  if ((retStratum(v, vertex.type = "Vertex") == 
                    from.stratum)) 
                    subDropEdge(NULL, v, t, "Vertex", to.type, 
                      FALSE, to.all, edge.type = edge.type, slave = slave)
            }
            else if (to.all && ((to.type == "ClosedBlock") || 
                ((to.type == "Vertex")))) {
                for (w in seq(along = vertexList)) if (is.element(w, 
                  visibleVertices)) 
                  if ((retStratum(w, vertex.type = "Vertex") == 
                    to.stratum)) 
                    subDropEdge(NULL, f, w, "Vertex", "Vertex", 
                      FALSE, FALSE, edge.type = edge.type, slave = slave)
            }
            else {
                j <- which.unordered.edge(c(t, f), edge.type = edge.type)
                if (any(j)) {
                  i <- (1:(length(j)))[j]
                  for (j in seq(along = i)) subSubDropEdge(i[j], 
                    f, t, edge.type = edge.type, slave = slave)
                }
            }
            tkconfigure(canvas, cursor = "arrow")
        }
        deleteEdge <- function(i, f, t, edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(edge.type)
            function(...) {
                if (as.logical(debug.edges) && (as.numeric(debug.edges) > 
                  1)) 
                  print(paste("deleteEdge", i, f, t, edge.type))
                if (retActivatedEdge() == i) 
                  subDropEdge(i, f, t, from.all = FALSE, to.all = FALSE, 
                    edge.type = edge.type, slave = FALSE)
            }
        }
        computeEdgeLabel <- function(i, f, t, force = FALSE, 
            edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(force)
            force(edge.type)
            function(...) {
                if (retActivatedEdge() == i) {
                  tkconfigure(canvas, cursor = "watch")
                  edges <- edgeItem(f, edge.type = edge.type)
                  if (length(edges) > 0) 
                    for (e in edges) if (!(is.null(e))) 
                      if (e$nr == i) 
                        if (e$to == t) 
                          if (!is.null(object) && (hasMethods || 
                            hasMethod("testEdge", class(object)))) {
                            from.type <- vertexTypeOfEdge(f, 
                              edge.type)
                            to.type <- vertexTypeOfEdge(t, edge.type)
                            R <- testEdge(object, action = "remove", 
                              name.1 = retVertexName(f, from.type), 
                              name.2 = retVertexName(t, to.type), 
                              from = f, to = t, from.type = from.type, 
                              to.type = to.type, edge.index = i, 
                              force = force, Arguments = Args())
                            if (!is.null(R)) {
                              if ((hasMethods || hasMethod("labelOfTest", 
                                class(R)))) 
                                setEdgeLabel(e, testLabel(R), 
                                  e$label.number, f = f)
                              if ((hasMethods || hasMethod("widthOfTest", 
                                class(R)))) 
                                setEdgeWidth(e, testWidth(R), 
                                  e$label.number, f = f)
                              activateEdge(0, edge.type = edge.type)()
                            }
                          }
                  tkconfigure(canvas, cursor = "arrow")
                }
            }
        }
        deleteEdgeLabel <- function(i, f, t, edge.type = "graphEdge") {
            force(i)
            force(f)
            force(t)
            force(edge.type)
            function(...) {
                edges <- edgeItem(f, edge.type = edge.type)
                E <- returnEdges(edge.type = edge.type)[[i]]
                if (length(edges) > 0) 
                  for (e in edges) if (!(is.null(e))) 
                    if (e$nr == i) {
                      setEdgeLabel(e, "", e$label.number, f = f)
                      setEdgeWidth(e, E@width, e$label.number, 
                        f = f)
                    }
            }
        }
        moveBlockPoint <- function(i, A) {
            force(i)
            force(A)
            function(x, y) {
                posTo <- retBlockPoints(i)[A, ]
                X <- replaceXY(x, y, posTo)
                dxy <- findDifference(X, posTo)
                changeBlockCornerPos(i, A, dxy)
                tkcoordsBlock(i, color = "black", lower = FALSE)
                if (updateVerticesStrata()) 
                  setUpdateBlockEdges("moveBlockPoint")
                subUpdateGraphWindow("moveBlockPoint", blockframes = 0)
                setUpdateBlocks("")
            }
        }
        moveBlockLine <- function(i, A, B) {
            force(i)
            force(A)
            force(B)
            function(x, y) {
                if ((A == 1) && (B == 3)) 
                  direction <- 1
                else if ((A == 4) && (B == 8)) 
                  direction <- 1
                else if ((A == 1) && (B == 4)) 
                  direction <- 2
                else if ((A == 3) && (B == 8)) 
                  direction <- 2
                else if ((A == 5) && (B == 6)) 
                  direction <- 1
                else if ((A == 7) && (B == 2)) 
                  direction <- 1
                else if ((A == 5) && (B == 7)) 
                  direction <- 2
                else if ((A == 6) && (B == 2)) 
                  direction <- 2
                else if ((A == 1) && (B == 5)) 
                  direction <- 3
                else if ((A == 3) && (B == 6)) 
                  direction <- 3
                else if ((A == 4) && (B == 7)) 
                  direction <- 3
                else if ((A == 8) && (B == 2)) 
                  direction <- 3
                posA <- retBlockPoints(i)[A, ]
                posB <- retBlockPoints(i)[B, ]
                X <- replaceXY(x, y, (posA + posB)/2)
                Y <- X
                Y[direction] <- (posA[direction] + posB[direction])/2
                dxy <- findDifference(X, Y)
                changeBlockCornerPos(i, A, dxy)
                tkcoordsBlock(i, color = "black", lower = FALSE)
                if (updateVerticesStrata()) 
                  setUpdateBlockEdges("moveBlockLine")
                subUpdateGraphWindow("moveBlockLine", blockframes = 0)
                setUpdateBlocks("")
            }
        }
        moveBlock <- function(i, A) {
            force(i)
            force(A)
            Y <- NULL
            function(x, y) {
                posTo <- retBlockPoints(i)[A, ]
                if (is.null(Y)) 
                  Y <<- replaceXY(x, y, posTo)
                else {
                  X <- replaceXY(x, y, posTo)
                  dxy <- findDifference(X, Y)
                  Y <<- X
                  changeBlockPos(i, A, dxy)
                  moveVerticesInBlock(i, dxy)
                  for (j in blockList[[i]]@descendants) if ((j != 
                    0) && (j != i)) {
                    changeBlockPos(j, A, dxy)
                    moveVerticesInBlock(j, dxy)
                    tkcoordsBlock(j, color = "black", lower = FALSE)
                  }
                  if (updateVerticesStrata()) 
                    setUpdateBlockEdges("moveBlock")
                  subUpdateGraphWindow("moveBlock", blockframes = 0)
                  tkcoordsBlock(i, color = "black", lower = FALSE)
                  setUpdateBlocks("")
                }
            }
        }
        doubleCloseBlock <- function(i, ancestor, update = TRUE) {
            blockReferences[i] <<- ancestor
            for (v in seq(along = vertexList)) if (is.element(v, 
                visibleVertices)) 
                if (retStratum(v, vertex.type = "Vertex") == 
                  i) 
                  setCloseVertex(v, TRUE, "Vertex")
            for (j in blockList[[i]]@descendants) if ((j != 0)) 
                doubleCloseBlock(j, ancestor, update = FALSE)
            setDoubleCloseBlock(i, TRUE, update = update)
        }
        closeBlock <- function(i, update = TRUE) {
            force(i)
            function(...) {
                for (v in seq(along = vertexList)) if (is.element(v, 
                  visibleVertices)) 
                  if (retStratum(v, vertex.type = "Vertex") == 
                    i) 
                    setCloseVertex(v, TRUE, "Vertex")
                for (j in blockList[[i]]@descendants) if ((j != 
                  i) && (j != 0) && !doubleClosedBlock[j]) 
                  doubleCloseBlock(j, i, update = FALSE)
                setCloseBlock(i, TRUE, update = update)
                drawVertex(i, w = 10, vertexcolor = "Black", 
                  vertex.type = "ClosedBlock")
                if (update) 
                  subUpdateGraphWindow("closeBlock", blockframes = i, 
                    updateEdges = TRUE)
            }
        }
        openBlock <- function(i, update = TRUE) {
            force(i)
            function(...) {
                setDoubleCloseBlock(i, FALSE, update = FALSE)
                blockReferences[i] <<- i
                setCloseBlock(i, FALSE, update = update)
                vertex.type <- "ClosedBlock"
                deActivateVertex(i, retVertexColor(i, vertex.type), 
                  vertex.type)
                drawBlock(blockList[[i]], i)
                for (v in seq(along = vertexList)) if (is.element(v, 
                  visibleVertices)) 
                  if (retStratum(v, vertex.type = "Vertex") == 
                    i) 
                    setCloseVertex(v, FALSE, "Vertex")
                for (j in blockList[[i]]@descendants) if ((j != 
                  0) && closedBlock[j]) 
                  openBlock(j, update = FALSE)()
                if (update) 
                  subUpdateGraphWindow("openBlock", raiseEdges = TRUE, 
                    updateEdges = TRUE)
            }
        }
        subNewVertex <- function(position, get.name = TRUE, get.vertex.type = TRUE, 
            get.how.to.compute = TRUE) {
            n <- length(vertexList) + 1
            if (get.name) {
                ReturnVal <- modalDialog("Name Entry", "Enter name of new variable", 
                  paste("V", n, sep = ""), GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
            }
            else ReturnVal <- paste("V", n, sep = "")
            vertextypes <- vertexClasses[, 1]
            vertextypes <- paste(vertextypes)
            vertex.type <- vertextypes[1]
            if (get.vertex.type) {
                popup <- tktoplevel()
                tkfocus(popup)
                tl <- selectDialog(popup, vertextypes, title = "Variable vertex.type selection entry", 
                  subtitle = "Select vertex.type")
                OnOK <- function() {
                  i <- as.numeric(tkcurselection(tl)) + 1
                  vertex.type <<- vertextypes[i]
                  tkdestroy(popup)
                }
                OK.but <- tkbutton(popup, text = "   OK   ", 
                  command = OnOK)
                tkgrid(OK.but)
                tkwait.window(popup)
            }
            vertex <- newVertex(ReturnVal, ReturnVal, vertex.type, 
                n, position, color = vertexColor, stratum = 0, 
                vertexClasses = vertexClasses)
            if (get.how.to.compute) {
                Expression <- modalDialog("Expression Entry", 
                  "Enter expression for computing new variable", 
                  paste("object <<- object"), GraphWindow@top)
                if (Expression == "ID_CANCEL") 
                  return()
                else eval(parse(text = Expression))
            }
            else Expression <- ReturnVal
            vertexList <<- append(vertexList, list(vertex))
            positionsVertices <<- rbind(positionsVertices, position(vertex))
            positionsLabels <<- rbind(positionsLabels, position(vertex))
            Labels <<- c(Labels, Expression)
            closedVertex <<- c(closedVertex, FALSE)
            colorsVertices <<- c(colorsVertices, vertexColor)
            strataVertices <<- c(strataVertices, 0)
            namesVertices <<- c(namesVertices, ReturnVal)
            itemsVertices <<- append(itemsVertices, list(NULL))
            itemsEdges <<- append(itemsEdges, list(NULL))
        }
        subAddVertex <- function(index, vertex.type = "Vertex", 
            slave = TRUE) {
            tkconfigure(canvas, cursor = "watch")
            newEdges <- list(graphEdges = appendToCurrentEdges(omitEdges = FALSE, 
                new.edge = NULL, edge.type = "graphEdge"), factorEdges = returnEdges(edge.type = "factorEdge"), 
                blockEdges = returnEdges(edge.type = "blockEdge"))
            R <- NULL
            if (is.null(object)) 
                R <- TRUE
            Arguments <- Args()
            if (!is.null(object) && (hasMethods || hasMethod("modifyModel", 
                class(object)))) 
                R <- modifyModel(object, action = "addVertex", 
                  name = retVertexName(index, vertex.type), index = index, 
                  type = vertex.type, newEdges = newEdges, visibleVertices = c(visibleVertices, 
                    index), Arguments = Arguments)
            if (!is.null(R) && !is.null(R$object)) {
                object <<- R$object
                if (!is.null(objectName)) 
                  assign(objectName, object, pos = 1)
            }
            if (!is.null(R)) 
                if (slave) {
                  if (is.null(R$newEdges$graphEdges)) 
                    Edges <- newEdges$graphEdges
                  else Edges <- R$newEdges$graphEdges
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = NULL, edgeList = Edges, blockEdgeList = blockEdgeList, 
                    factorVertexList = R$FactorVertices, factorEdgeList = R$FactorEdges, 
                    visibleVertices = c(index, visibleVertices), 
                    extraList = R$ExtraVertices, object = R$object, 
                    title = "Slave window", Arguments = Arguments)
                }
                else {
                  drawVertex(index, w = w, vertexcolor = vertexColor, 
                    vertex.type = "Vertex")
                  visibleVertices <<- c(visibleVertices, index)
                  clearFactorEdges()
                  update.edge.labels()
                  if (!is.null(R$FactorVertices) && !is.null(R$FactorEdges)) 
                    drawFactors(R$FactorEdges, R$FactorVertices)
                }
            else {
                message("Null result in addVertex")
            }
            tkconfigure(canvas, cursor = "arrow")
        }
        createNewVertex <- function() {
            function(x, y) {
                X <- replaceXY(x, y, rep(50, N))
                position <- c(inversProject(inversCanvasPosition(X)))
                subNewVertex(position, get.name = FALSE, get.vertex.type = FALSE, 
                  get.how.to.compute = FALSE)
                n <- length(vertexList)
                subAddVertex(n, slave = FALSE)
            }
        }
        configure <- function() {
            function(x, y, ...) {
                x <- as.numeric(x)
                y <- as.numeric(y)
            }
        }
        initFactorVariables <- function(factorVertexList) {
            if (length(factorVertexList) > 0) {
                itemsFactors <<- vector("list", length(factorVertexList))
                itemsFactorEdges <<- vector("list", length(factorVertexList))
                namesFactorVertices <<- Names(factorVertexList)
                positionsFactorVertices <<- Positions(factorVertexList)
                positionsFactorLabels <<- positionsFactorVertices
                positionsFactorLabels[, 1] <<- positionsFactorLabels[, 
                  1] + 0.1 * w
                factorLabels <<- Labels(factorVertexList)
                colorsFactorVertices <<- Colors(factorVertexList)
                strataFactorVertices <<- rep(0, length(factorVertexList))
                if (!is.matrix(positionsFactorVertices)) 
                  warning("Positions of factor-vertices should have same number of coordinates")
                else if (!(dim(positionsFactorVertices)[2] == 
                  dim(positionsFactorVertices)[2])) 
                  warning("Factor-vertices should have same number of coordinates as vertices")
            }
        }
        initExtraVariables <- function(extraList) {
            if (length(extraList) > 0) {
                itemsExtras <<- vector("list", length(extraList))
                itemsExtraEdges <<- vector("list", length(extraList))
                namesExtraVertices <<- Names(extraList)
                positionsExtraVertices <<- Positions(extraList)
                positionsExtraLabels <<- positionsExtraVertices
                positionsExtraLabels[, 1] <<- positionsExtraLabels[, 
                  1] + 0.1 * w
                extraLabels <<- Labels(extraList)
                colorsExtraVertices <<- Colors(extraList)
                strataExtraVertices <<- rep(0, length(extraList))
                if (!is.matrix(positionsExtraVertices)) 
                  warning("Positions of extra-vertices should have same number of coordinates")
                else if (!(dim(positionsExtraVertices)[2] == 
                  dim(positionsExtraVertices)[2])) 
                  warning("Extra-vertices should have same number of coordinates as vertices")
            }
        }
        drawFactors <- function(X.FactorEdges, X.FactorVertices) {
            factorEdgeList <<- X.FactorEdges
            factorVertexList <<- X.FactorVertices
            initFactorVariables(factorVertexList)
            for (i in seq(along = factorEdgeList)) {
                f <- factorEdgeList[[i]]@vertex.indices[1]
                t <- factorEdgeList[[i]]@vertex.indices[2]
                E <- append.index.edge(c(f, t), edge.type = "factorEdge")
                drawEdge(E[[length(E)]], length(E), lower = TRUE, 
                  edge.type = "factorEdge")
            }
            if (length(factorVertexList) > 0) 
                for (i in seq(along = factorVertexList)) drawVertex(-i, 
                  w = w, vertexcolor = vertexColor, vertex.type = "Factor")
        }
        drawExtras <- function(X.ExtraVertices) {
            extraList <<- X.ExtraVertices
            initExtraVariables(extraList)
            if (length(extraList) > 0) 
                for (i in seq(along = extraList)) drawVertex(i, 
                  w = w, vertexcolor = vertexColor, vertex.type = "Extra")
        }
        setMainMenu <- function() {
            topMenu <- tkmenu(GraphWindow@top)
            tkconfigure(GraphWindow@top, menu = topMenu)
            fileMenu <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(fileMenu, "command", label = "Save as Postscript ... ", 
                command = function() {
                  fileName <- tclvalue(tkgetSaveFile(initialfile = "dynamicGraph.ps", 
                    filetypes = "{{Postsript Files} {.ps}}"))
                  if (!nchar(fileName)) 
                    tkmessageBox(message = "No file was selected!")
                  else tkpostscript(canvas, file = fileName, 
                    width = width, height = height)
                })
            tkadd(fileMenu, "command", label = "Quit", command = function() tkdestroy(GraphWindow@top))
            tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
            userMenu <- tkmenu(topMenu, tearoff = FALSE)
            UserMainMenu <- function(i) {
                force(i)
                function(...) {
                  updateArguments(UserMenus[[i]], blocks = TRUE)
                  arguments <- Args()
                  UserMenus[[i]]$command(object, Arguments = arguments)
                }
            }
            if (length(UserMenus) > 0) 
                for (i in seq(along = UserMenus)) if (names(UserMenus[i]) == 
                  "MainUser") 
                  tkadd(userMenu, "command", label = UserMenus[[i]]$label, 
                    command = UserMainMenu(i))
            tkadd(topMenu, "cascade", label = "User", menu = userMenu)
            variableMenu <- tkmenu(topMenu, tearoff = FALSE)
            selectVariableDialog <- function() {
                vertexlabels <- Labels[visibleVertices]
                popup <- tktoplevel()
                tl <- selectDialog(popup, vertexlabels)
                OnOK <- function() {
                  i <- as.numeric(tkcurselection(tl)) + 1
                  variableChoice <- vertexlabels[i]
                  j <- (1:length(Labels))[Labels == variableChoice]
                  tkdestroy(popup)
                  subActivateVertex(j, color = "green", vertex.type = "Vertex")
                  msg <- paste("Click other vertex to add edge to ", 
                    variableChoice, sep = "")
                  tkmessageBox(message = msg)
                }
                OK.but <- tkbutton(popup, text = "   OK   ", 
                  command = OnOK)
                tkgrid(OK.but)
            }
            tkadd(variableMenu, "command", label = "Highlight vertex (for adding edge)", 
                command = function() selectVariableDialog())
            selectOtherDialog <- function(slave = TRUE) {
                "not.in" <- function(x, l = max(x)) {
                  y <- rep(TRUE, l)
                  y[x] <- FALSE
                  return((1:l)[y])
                }
                vertexnames <- Names(vertexList[not.in(visibleVertices, 
                  length(vertexList))])
                popup <- tktoplevel()
                tl <- selectDialog(popup, vertexnames)
                OnOK <- function() {
                  vertex <- as.numeric(tkcurselection(tl)) + 
                    1
                  variableChoice <- vertexnames[vertex]
                  index <- nameToVertexIndex(variableChoice, 
                    vertexList)
                  tkdestroy(popup)
                  if (length(vertexnames) > 0) 
                    subAddVertex(index, slave = slave)
                }
                OK.but <- tkbutton(popup, text = "   OK   ", 
                  command = OnOK)
                tkgrid(OK.but)
            }
            tkadd(variableMenu, "command", label = "Select vertex among variables not displayed (slave)", 
                command = function() selectOtherDialog())
            tkadd(variableMenu, "command", label = "Select vertex among variables not displayed (here)", 
                command = function() selectOtherDialog(slave = FALSE))
            tkadd(variableMenu, "command", label = "Create new variable (not displayed before selected)", 
                command = function() subNewVertex(rep(0, N)))
            tkadd(topMenu, "cascade", label = "Variables", menu = variableMenu)
            edgesMenu <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(edgesMenu, "command", label = " ... ", command = function() {
            })
            tkadd(edgesMenu, "command", label = "Delete all edge labels", 
                command = function() {
                  deleteAllEdgeLabels()()
                })
            tkadd(topMenu, "cascade", label = "Edges", menu = edgesMenu)
            generatorsMenu <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(generatorsMenu, "command", label = " ... e.i. factors ", 
                command = function() {
                })
            tkadd(topMenu, "cascade", label = "Generators", menu = generatorsMenu)
            blocksMenu <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(blocksMenu, "command", label = " ... ", command = function() {
            })
            tkadd(blocksMenu, "command", label = "Add block ", 
                command = function() {
                  message("Not implemented.")
                })
            tkadd(topMenu, "cascade", label = "Blocks", menu = blocksMenu)
            graphMenu <- tkmenu(topMenu, tearoff = FALSE)
            tkadd(graphMenu, "command", label = "Copy: Make slave window", 
                command = function() {
                  edgeList <- currentEdges(edge.type = "graphEdge")
                  blockEdgeList <- currentEdges(edge.type = "blockEdge")
                  factorEdgeList <- currentEdges(edge.type = "factorEdge")
                  Arguments <- Args()
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = NULL, edgeList = edgeList, 
                    blockEdgeList = blockEdgeList, factorVertexList = factorVertexList, 
                    factorEdgeList = factorEdgeList, visibleVertices = visibleVertices, 
                    extraList = extraList, object = object, title = "Slave window", 
                    Arguments = Arguments)
                })
            tkadd(graphMenu, "command", label = "Redraw graph window", 
                command = function() {
                  Arguments <- Args()
                  redrawGraphWindow(graphLattice = GraphLattice, 
                    graphWindow = GraphWindow, Arguments = Arguments)
                })
            tkadd(graphMenu, "command", label = "Update graph window", 
                command = function() {
                  subUpdateGraphWindow("Update from main menu", 
                    updateAllVertices = TRUE, raiseEdges = TRUE, 
                    updateEdges = TRUE, all.blockframes = TRUE)
                })
            tkadd(graphMenu, "command", label = "Reset (enable) transformation", 
                command = function() setTransformation(diag(1, 
                  N)))
            tkadd(graphMenu, "command", label = "Disable rotation", 
                command = function() setTransformation(NULL))
            tkadd(topMenu, "cascade", label = "Graph", menu = graphMenu)
            exportMenu <- tkmenu(topMenu, tearoff = FALSE)
            latticeExport <- function() {
                ReturnVal <- modalDialog("Lattice Name Entry", 
                  "Enter name for lattice", "Lattice", GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                GraphLattice@dummyVertices <- verticesUpdate()
                assign(ReturnVal, GraphLattice, pos = 1)
            }
            tkadd(graphMenu, "command", label = "Assign GraphLattice in .GlobalEnv", 
                command = function() latticeExport())
            tkadd(exportMenu, "command", label = "Assign GraphLattice in .GlobalEnv", 
                command = function() latticeExport())
            graphWindowExport <- function() {
                ReturnVal <- modalDialog("GraphWindow Name Entry", 
                  "Enter name for graphWindow", "GraphWindow", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                assign(ReturnVal, GraphWindow, pos = 1)
            }
            tkadd(graphMenu, "command", label = "Assign GraphWindow in .GlobalEnv", 
                command = function() graphWindowExport())
            tkadd(exportMenu, "command", label = "Assign GraphWindow in .GlobalEnv", 
                command = function() graphWindowExport())
            objectExport <- function() {
                ReturnVal <- modalDialog("Object Name Entry", 
                  "Enter name for object", "Object", GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                assign(ReturnVal, object, pos = 1)
            }
            tkadd(exportMenu, "command", label = "Assign Object in .GlobalEnv", 
                command = function() objectExport())
            tkadd(graphMenu, "command", label = "Assign Object in .GlobalEnv", 
                command = function() objectExport())
            transformationExport <- function() {
                ReturnVal <- modalDialog("Transformation Name Entry", 
                  "Enter name for transformation", "Transformation", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                assign(ReturnVal, transformation, pos = 1)
            }
            tkadd(exportMenu, "command", label = "Assign Transformation in .GlobalEnv", 
                command = function() transformationExport())
            tkadd(graphMenu, "command", label = "Assign Transformation in .GlobalEnv", 
                command = function() transformationExport())
            verticesExport <- function() {
                ReturnVal <- modalDialog("Vertices Name Entry", 
                  "Enter name for vertices", "Vertices", GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                vertices <- verticesUpdate()
                assign(ReturnVal, vertexList, pos = 1)
            }
            tkadd(variableMenu, "command", label = "Assign Vertices in .GlobalEnv", 
                command = function() verticesExport())
            tkadd(exportMenu, "command", label = "Assign Vertices in .GlobalEnv", 
                command = function() verticesExport())
            edgesExport <- function() {
                ReturnVal <- modalDialog("Edges Name Entry", 
                  "Enter name for Edges", "Edges", GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                edgesUpdate()
                assign(ReturnVal, edgeList, pos = 1)
            }
            tkadd(edgesMenu, "command", label = "Assign Edges in .GlobalEnv", 
                command = function() edgesExport())
            tkadd(exportMenu, "command", label = "Assign Edges in .GlobalEnv", 
                command = function() edgesExport())
            extraVerticesExport <- function() {
                ReturnVal <- modalDialog("ExtraVertices Name Entry", 
                  "Enter name for the extravertices", "ExtraVertices", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                extraVerticesUpdate()
                assign(ReturnVal, extraList, pos = 1)
            }
            tkadd(variableMenu, "command", label = "Assign ExtraVertices in .GlobalEnv", 
                command = function() extraVerticesExport())
            tkadd(exportMenu, "command", label = "Assign ExtraVertices in .GlobalEnv", 
                command = function() extraVerticesExport())
            factorVerticesExport <- function() {
                ReturnVal <- modalDialog("FactorVertices Name Entry", 
                  "Enter name for the factorvertices", "FactorVertices", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                factorVerticesUpdate()
                assign(ReturnVal, factorVertexList, pos = 1)
            }
            tkadd(generatorsMenu, "command", label = "Assign FactorVertices in .GlobalEnv", 
                command = function() factorVerticesExport())
            tkadd(exportMenu, "command", label = "Assign FactorVertices in .GlobalEnv", 
                command = function() factorVerticesExport())
            factorEdgesExport <- function() {
                ReturnVal <- modalDialog("FactorEdges Name Entry", 
                  "Enter name for the factoredges", "FactorEdges", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                factorEdgesUpdate()
                assign(ReturnVal, factorEdgeList, pos = 1)
            }
            tkadd(generatorsMenu, "command", label = "Assign FactorEdges in .GlobalEnv", 
                command = function() factorEdgesExport())
            tkadd(exportMenu, "command", label = "Assign FactorEdges in .GlobalEnv", 
                command = function() factorEdgesExport())
            blockListExport <- function() {
                ReturnVal <- modalDialog("Blocklist Name Entry", 
                  "Enter name for the blocklist", "BlockList", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                blocksUpdate()
                assign(ReturnVal, blockList, pos = 1)
            }
            tkadd(blocksMenu, "command", label = "Assign BlockList in .GlobalEnv", 
                command = function() blockListExport())
            tkadd(exportMenu, "command", label = "Assign BlockList in .GlobalEnv", 
                command = function() blockListExport())
            blockTreeExport <- function() {
                ReturnVal <- modalDialog("Blocktree Name Entry", 
                  "Enter name for the blocktree", "BlockTree", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                blocksUpdate()
                assign(ReturnVal, blockTree, pos = 1)
            }
            tkadd(blocksMenu, "command", label = "Assign BlockTree in .GlobalEnv", 
                command = function() blockTreeExport())
            tkadd(exportMenu, "command", label = "Assign BlockTree in .GlobalEnv", 
                command = function() blockTreeExport())
            blockEdgesExport <- function() {
                ReturnVal <- modalDialog("BlockEdges Name Entry", 
                  "Enter name for the blockedges", "BlockEdges", 
                  GraphWindow@top)
                if (ReturnVal == "ID_CANCEL") 
                  return()
                blockEdgesUpdate()
                assign(ReturnVal, blockEdgeList, pos = 1)
            }
            tkadd(blocksMenu, "command", label = "Assign BlockEdges in .GlobalEnv", 
                command = function() blockEdgesExport())
            tkadd(exportMenu, "command", label = "Assign BlockEdges in .GlobalEnv", 
                command = function() blockEdgesExport())
            tkadd(topMenu, "cascade", label = "Export", menu = exportMenu)
        }
        updateCountVertices <- updateCountVerticesMain
        updateCountLabels <- updateCountLabelsMain
        updateCountBlocks <- updateCountBlocksMain
        updateCountBlockEdges <- updateCountBlockEdgesMain
        zoomPositions <- NULL
        args <- list(...)
        Arguments <- args$Arguments
        if (!is.null(Arguments)) {
            if (is.null(graphLattice) && !is.null(Arguments$graphLattice)) 
                graphLattice <- Arguments$graphLattice
            if (is.null(edgeList) && !is.null(Arguments$edgeList)) 
                edgeList <- Arguments$edgeList
            if (is.null(factorVertexList) && !is.null(Arguments$factorVertexList)) 
                factorVertexList <- Arguments$factorVertexList
            if (is.null(factorEdgeList) && !is.null(Arguments$factorEdgeList)) 
                factorEdgeList <- Arguments$factorEdgeList
            if (is.null(blockEdgeList) && !is.null(Arguments$blockEdgeList)) 
                blockEdgeList <- Arguments$blockEdgeList
            if (is.null(visibleVertices) && !is.null(Arguments$visibleVertices)) 
                visibleVertices <- Arguments$visibleVertices
            if (is.null(extraList) && !is.null(Arguments$extraList)) 
                extraList <- Arguments$extraList
            if (is.null(object) && !is.null(Arguments$object)) 
                object <- Arguments$object
            if (is.null(title) && !is.null(Arguments$title)) 
                title <- Arguments$title
            if (is.null(transformation) && !is.null(Arguments$transformation)) 
                transformation <- Arguments$transformation
            if (is.null(width) && !is.null(Arguments$width)) 
                width <- Arguments$width
            if (is.null(height) && !is.null(Arguments$height)) 
                height <- Arguments$height
            if (is.null(w) && !is.null(Arguments$w)) 
                w <- Arguments$w
            if (is.null(vertexColor) && !is.null(Arguments$vertexColor)) 
                vertexColor <- Arguments$vertexColor
            if (is.null(extraVertexColor) && !is.null(Arguments$extraVertexColor)) 
                extraVertexColor <- Arguments$extraVertexColor
            if (is.null(edgeColor) && !is.null(Arguments$edgeColor)) 
                edgeColor <- Arguments$edgeColor
            if (is.null(factorVertexColor) && !is.null(Arguments$factorVertexColor)) 
                factorVertexColor <- Arguments$factorVertexColor
            if (is.null(factorEdgeColor) && !is.null(Arguments$factorEdgeColor)) 
                factorEdgeColor <- Arguments$factorEdgeColor
            if (is.null(blockEdgeColor) && !is.null(Arguments$blockEdgeColor)) 
                blockEdgeColor <- Arguments$blockEdgeColor
            if (is.null(blockColors) && !is.null(Arguments$blockColors)) 
                blockColors <- Arguments$blockColors
            if (is.null(background) && !is.null(Arguments$background)) 
                background <- Arguments$background
        }
        if (is.null(visibleVertices)) 
            visibleVertices <- 1:length(vertexList)
        if (is.null(width)) 
            width <- 400
        if (is.null(height)) 
            height <- 400
        if (is.null(w)) 
            w <- 6
        if (is.null(vertexColor)) 
            vertexColor <- "blue"
        if (is.null(extraVertexColor)) 
            extraVertexColor <- "black"
        if (is.null(edgeColor)) 
            edgeColor <- "blue"
        if (is.null(factorVertexColor)) 
            factorVertexColor <- "green"
        if (is.null(factorEdgeColor)) 
            factorEdgeColor <- "green"
        if (is.null(blockEdgeColor)) 
            blockEdgeColor <- "default"
        if (is.null(blockColors)) 
            blockColors <- NULL
        if (is.null(background)) 
            background <- "white"
        if (is.null(edgeList)) 
            edgeList <- vector("list", length = 0)
        if (is.null(factorEdgeList)) 
            factorEdgeList <- vector("list", length = 0)
        if (is.null(blockEdgeList)) 
            blockEdgeList <- vector("list", length = 0)
        if (is.null(graphWindow)) {
            ArgWindow <- FALSE
            GraphWindow <- newGraph(graphLattice, vertexList, 
                edgeList, factorEdgeList, blockEdgeList, redrawGraphWindow, 
                background = background, title = title, width = width, 
                height = height)
        }
        else {
            ArgWindow <- TRUE
            GraphWindow <- graphWindow
            GraphWindow@graphEdges <- edgeList
            GraphWindow@factorEdges <- factorEdgeList
            GraphWindow@blockEdges <- blockEdgeList
            for (i in seq(along = GraphWindow@tags)) tkdelete(GraphWindow@canvas, 
                GraphWindow@tags[[i]])
            GraphWindow@tags <- list(NULL)
            GraphWindow@id <- GraphWindow@id + 1
        }
        activatedNode <- list(number = 0, vertex.type = "Null")
        activatedEdge <- 0
        itemsFactors <- NULL
        itemsFactorEdges <- NULL
        namesFactorVertices <- NULL
        positionsFactorVertices <- NULL
        positionsFactorLabels <- NULL
        factorLabels <- NULL
        colorsFactorVertices <- NULL
        strataFactorVertices <- NULL
        itemsExtras <- NULL
        itemsExtraEdges <- NULL
        namesExtraVertices <- NULL
        positionsExtraVertices <- NULL
        positionsExtraLabels <- NULL
        extraLabels <- NULL
        colorsExtraVertices <- NULL
        strataExtraVertices <- NULL
        Angle <- 10
        canvas <- GraphWindow@canvas
        itemsEdges <- vector("list", length(vertexList))
        itemsVertices <- vector("list", length(vertexList))
        itemsOpenBlocks <- vector("list", length(blockList))
        itemsClosedBlocks <- vector("list", length(blockList))
        positionsEdgeLabels <- NULL
        closedVertex <- rep(FALSE, length(vertexList))
        closedBlock <- rep(FALSE, length(blockList))
        doubleClosedBlock <- rep(FALSE, length(blockList))
        if (!is.null(blockList)) {
            itemsBlockEdges <- vector("list", length(blockEdgeList))
        }
        initFactorVariables(factorVertexList)
        initExtraVariables(extraList)
        if (!is.null(blockList)) 
            for (i in seq(along = blockList)) drawBlock(blockList[[i]], 
                i)
        for (i in seq(along = edgeList)) drawEdge(edgeList[[i]], 
            i, edge.type = "graphEdge")
        for (i in seq(along = factorEdgeList)) drawEdge(factorEdgeList[[i]], 
            i, edge.type = "factorEdge")
        for (i in seq(along = blockEdgeList)) drawEdge(blockEdgeList[[i]], 
            i, edge.type = "blockEdge")
        for (i in seq(along = vertexList)) if (is.element(i, 
            visibleVertices)) 
            drawVertex(i, w = w, vertexcolor = vertexColor, vertex.type = "Vertex")
        if (length(factorVertexList) > 0) 
            for (i in seq(along = factorVertexList)) drawVertex(-i, 
                w = w, vertexcolor = vertexColor, vertex.type = "Factor")
        if (length(extraList) > 0) 
            for (i in seq(along = extraList)) drawVertex(i, w = w, 
                vertexcolor = vertexColor, vertex.type = "Extra")
        if (initialWindow) 
            update.edge.labels()
        setMainMenu()
        tkbind(canvas, "<B3-Motion>", doHandRotate())
        tkbind(canvas, "<Insert>", createNewVertex())
        tkbind(canvas, "<Button-2>", createNewVertex())
        tkbind(canvas, "<Double-Button-2>", addLastEdge())
        tkbind(canvas, "<Configure>", configure())
        if (enterLeaveUpdate) {
            tkbind(canvas, "<Enter>", updatePositions("Enter"))
            tkbind(canvas, "<Leave>", updatePositions("Leave"))
        }
        GraphLattice@graphs <<- append(list(GraphWindow), GraphLattice@graphs)
        return(GraphLattice)
    }
    updateCountVerticesMain <- 0
    updateCountLabelsMain <- 0
    updateCountBlocksMain <- 0
    updateCountBlockEdgesMain <- 0
    GraphLattice <- newGraphLattice(vertexList)
    namesVertices <- Names(vertexList)
    positionsVertices <- Positions(vertexList)
    if (is.matrix(positionsVertices)) {
        positionsLabels <- positionsVertices
        positionsLabels[, 1] <- positionsLabels[, 1] + 0.1 * 
            w
        Labels <- Labels(vertexList)
        colorsVertices <- Colors(vertexList)
        strataVertices <- Strata(vertexList)
        N <- ncol(positionsVertices)
        if (is.null(blockList) && !is.null(blockTree)) 
            blockList <- blockTreeToList(blockTree)
        positionsBlock <- NULL
        positionsBlockLabels <- NULL
        positionsClosedBlocks <- NULL
        blockLabels <- NULL
        strataBlocks <- NULL
        if (!is.null(blockList)) {
            positionsBlocks <- Positions(blockList)
            d <- dim(positionsBlocks)
            positionsBlocks <- array(positionsBlocks, dim = c(d[1], 
                d[2]/2, 2))
            positionsClosedBlocks <- matrix(rep(NA, N * length(blockList)), 
                ncol = N)
            positionsClosedBlocks <- apply(positionsBlocks, c(1, 
                2), mean)
            blockReferences <- 1:length(blockList)
            positionsBlockLabels <- matrix(rep(0, N * length(blockList)), 
                ncol = N)
            blockLabels <- Labels(blockList)
            strataBlocks <- Strata(blockList)
            itemsBlockEdges <- vector("list", length(blockEdgeList))
        }
        Result <- redrawGraphWindow(graphLattice = GraphLattice, 
            graphWindow = NULL, edgeList = edgeList, blockEdgeList = blockEdgeList, 
            factorVertexList, factorEdgeList, visibleVertices = visibleVertices, 
            extraList = extraList, object = object, title = title, 
            transformation = NULL, width = width, height = height, 
            w = w, vertexColor = vertexColor, extraVertexColor = extraVertexColor, 
            edgeColor = edgeColor, factorVertexColor = factorVertexColor, 
            factorEdgeColor = factorEdgeColor, blockEdgeColor = blockEdgeColor, 
            blockColors = blockColors, background = background, 
            initialWindow = TRUE)
    }
    else {
        Result <- NULL
        warning("Positions of vertices should have same number of coordinates")
    }
    return(Result)
}
