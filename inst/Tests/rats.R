
rats.data <- read.table("rats.txt", header = TRUE)

rats.data; dimnames(rats.data); class(rats.data); dim(rats.data)

library(CoCoCg)

rats <- makeCoCoCg(title = "Source: Morrison (1976). Multivariate Statistical Methods, McGraw-Hill. a: Sex; b: Treatment; x: Wt loss 1; y: Wt loss 2")

result <- enterDataFrame(rats.data, to.factor = 1:2)

enterModel(".;")

showTable("observed", ":a:b")
showTable("canonical", ":a:b:x:y", matrix = TRUE)

showOptions("specification")

exportCoCo("rats.xpt")

endCoCo();

quitCoCo()
