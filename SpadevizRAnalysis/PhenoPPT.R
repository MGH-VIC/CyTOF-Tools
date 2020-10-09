#Functions for running correlation between two clusterings
#Joshua Hess

#Check for missing packages and install if needed
list.of.packages <- c("officer","magrittr","magick","DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#load the packages
require('officer')
require(magrittr)
require("magick")
require("DT")


GeneratePPT = function(ImportDataMaster_list,ppt_name){
  # Function for generating a powerpoint for scatterplots combined with parallel coordinate plots
  
  #Get the directory so we can export the powerpoint
  dir <- getwd()
  doc <- read_pptx() 
  #Get the names of the first two experiments (Index comes from the list returned by FilterCorrResults function)
  Grp1_name = ImportDataMaster_list[["ExpName"]]
  #Get the directory for the cluster PCP
  Grp1_PCP = ImportDataMaster_list[["FilesPCP"]]
  #Get the directory for the cluster scatterplots
  Grp1_Scat = ImportDataMaster_list[["ScatterPlotFiles"]]
  #Get the number of rows in the datatable
  data = ImportDataMaster_list[["ClusterAbundances"]]
  num = nrow(data)
  #Set index
  rownumber = 0
  
  #Iterate through the rows of the filtered corr results
  for(i in 1:num){
    #Get the cluster number from experiment 1
    Grp1_cluster_number = gsub(paste("Cluster ",sep="_"),"",rownames(data)[i])
    #Get a name for this cluster and its location in the PCP files
    Grp1_cluster_file = paste(dir,"/",Grp1_PCP,"/Cluster ", Grp1_cluster_number, ".jpeg", sep = "")
    #Get a name for this cluster and its location in the Scatter plot files
    Grp1_scatter_file = paste(dir,"/",Grp1_Scat, "/Cluster ", Grp1_cluster_number, ".jpeg", sep = "")
    
    #Read the group 1 cluster PCP
    Grp1_cluster_file_magick = image_read(Grp1_cluster_file)
    #image_info(Grp1_cluster_file_magick)
    #Grp1_cluster_file_magick = image_crop(Grp1_cluster_file_magick, "2000x900")
    image_write(Grp1_cluster_file_magick, path = "Grp1_cluster_file.jpeg", format = "jpeg")
    #Read the group 1 scatter plot
    Grp1_scatter_file_magick = image_read(Grp1_scatter_file)
    #image_info(Grp1_cluster_file_magick)
    image_write(Grp1_scatter_file_magick, path = "Grp1_scatter_file.jpeg", format = "jpeg")
    
    #Get the file for these temporary images
    Grp1_cluster_file = paste(dir, "/Grp1_cluster_file.jpeg", sep = "")
    #Get the file for these temporary images
    Grp1_scatter_file = paste(dir, "/Grp1_scatter_file.jpeg", sep = "")
    
    #Create slide title
    slide_title = paste(rownames(data)[i])
    
    doc <- doc %>%
      add_slide(layout = "Two Content", master = "Office Theme") %>%
      #ph_with_text(type = "title", str = slide_title) %>%
      #ph_with_img(type = "body", str = "body (index 1) is text", index = 1) %>% 
      # ph_with_img(type = "body", index = 1, src = Grp1_cluster_file, height = 3.5, width = 4.58 ) %>%
      # ph_with_img(type = "body", index = 2, src = Grp2_cluster_file, height = 3.5, width = 4.58 )
        #ph_with(value = Grp1_cluster_file, height = 3.4, width = 5.1, left = 0.1, top = 0) %>%
        ph_with(value = external_img(Grp1_cluster_file),location = ph_location(left = 0.1, top = 0,
                                                                 width = 5.1, height = 3.4)) %>%
      #ph_with_img_at(src = Grp2_cluster_file, height = 3.4, width = 5.1, left = 0.1, top = 3.4)%>%
      
        ph_with(value = external_img(Grp1_scatter_file), location = ph_location(left = 5.32, top = 0.55,
                                                                width = 2, height = 2))
     # ph_with_img_at(src = Grp2_scatter_file, height = 2, width = 2, left = 5.32, top = 3.75)
    #Remove the temporary files
    file.remove(Grp1_cluster_file, Grp1_scatter_file)
    #Print the index
    print(i)
  }
  #Print out the ppt document
  print(doc, target = ppt_name)
}













#




