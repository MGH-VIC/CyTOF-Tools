
#Load Data for Monocle
CD2 = DataLoad(pattern = "\\.csv$",subsample=TRUE,fraction=0.01,number=NULL,by='PatientId',Monocle = TRUE)
Pat25546_2 = CD2[['25546']]
Pat25546_2@phenoData@data$Size_Factor = 1
test = reduceDimension(Pat25546_2,max_components = 2,reduction_method = 'DDRTree')
Pat25546_2 <- orderCells(test)
plot_cell_trajectory(Pat25546_2)