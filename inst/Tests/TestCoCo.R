
require(tcltk)

require(dynamicGraph)

require(CoCo)

source("usermenus.R")

data(Reinis);                                                    
fullModel <- makeModel(enterModel("*", object = Reinis));
fullGraph <- dynamic.Graph(fullModel, UserMenus = Menus);
backward(recursive = TRUE, follow = TRUE, object = Reinis);
# eh(object = Reinis);
lastModel <- makeModel("last");
backwardGraph <- dynamic.Graph(lastModel, UserMenus = Menus);

