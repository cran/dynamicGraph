
require(tcltk)

require(dynamicGraph)

require(CoCo)

source("usermenus.R")

data(Reinis);                                                    
fullModel <- makeModel(enterModel("*", data = Reinis));
fullGraph <- dynamic.Graph(fullModel, UserMenus = Menus);
backward(recursive = TRUE, follow = TRUE);
# eh();
lastModel <- makeModel("last");
backwardGraph <- dynamic.Graph(lastModel, UserMenus = Menus);

