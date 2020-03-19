#Single-Cell Trajectory Analysis
#Functions for Running Single-Cell Trajectory Algorithms
#Triplets Melanoma
#Joshua Hess

#Create Function for running SCORPIUS up to trajectory inference
RunSCORPIUS = function(CD,distance="euclidean",dim=2,show_by = 'GroupId',export_graph=FALSE,
                       export_heat=FALSE,var_imp=FALSE,threads=35,num_perm=0,ntrees=10000){
  #This function will take the loaded data that you created with DataLoad 
  #function and will run scorpius over each element in that list
  
  #Get names of the CD object
  CD_names = list(names(CD))
  
  out = lapply(seq_along(CD),function(i,CD_object,names){
    
    #Get the CD object from your list
    x = CD[[i]]
    #Get name of list element
    tmp_name = CD_names[[1]][i]
    print(paste("Working on ","tmp_name",sep = ""))
    #xtract expression data
    expression = as.data.frame(x['Expression'])
    #Extract group data
    group_name = as.factor(x$Info[,show_by])
    #Reduce dimensionality
    space = reduce_dimensionality(expression, dist = distance, ndim = dim)
    #Trajectory inference
    traj = infer_trajectory(space)
    #Check to see if we are exporting things
    if(export_graph){
      print('Exporting Dimension Reduction Plot...')
      #Produce dimension reduction plot
      jpeg(paste(tmp_name,'_DimRed.jpeg',sep = ""))
      draw_trajectory_plot(space, progression_group = group_name, contour = TRUE,path=traj$path)
      dev.off()
      print('Finished Exporting Plot')
    }
    if(export_heat){
      print('Exporting Heatmap for Trajectory...')
      #Produce Trajectory Heatmap
      jpeg(paste(tmp_name,'_heatmap.jpeg',sep = ""),width = 5500,height = 2000,res = 300)
      draw_trajectory_heatmap(expression, traj$time, group_name, show_labels_row = TRUE,show_labels_col = FALSE)
      dev.off()
      print('Finished Exporting Heatmap')
    }
    if(var_imp){
      print('Calculating Random Forest Feature Importances...')
      #Get marker importance through trajectory with random forest classifier
      gimp = gene_importances(expression, traj$time, num_permutations = num_perm, num_threads = threads,ntree = ntrees)
      #Export results
      write.csv(gimp,paste(tmp_name,"_Marker_Importance.csv",sep = ""))
      print('Finished Calculation')
      #Add random forest to returned list
      x[["VariableImp"]] = gimp
    }
    x[["Space"]] = space
    x[["Trajectory"]] = traj
    #Return our object
    return(x)
  },CD_object = CD, names= CD_names)
  return(out)
}





