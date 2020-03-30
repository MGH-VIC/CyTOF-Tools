#Cluster Matching script
#Joshua Hess


#Check for missing packages and install if needed
# list.of.packages <- c("devtools","Rcpp","biclust","diptest","evtree","ggdendro","ggfortify","ggplot2","gplots","gdata","ggrepel",
#                       "ggRandomForests","gridExtra","gtable","gtools","igraph","MASS","packcircles","plyr","randomForestSRC",
#                       "reshape2","pheatmap","readxl","raster","openxlsx","bindrcpp","stringi","statmod")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
#Check for github packages
require('devtools')
# list_git = c("tchitchek-lab/SPADEVizR")
# new.packages <- list_git[!(list_git %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install_github(new.packages)
# #Check for Biomanager packages
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# list_bio = c("FlowSOM","flowCore","edgeR")
# new.packages <- list_bio[!(list_bio %in% installed.packages()[,"Package"])]
# if(length(new.packages)){
#   for (pack in new.packages){
#     biocLite("flowCore", suppressUpdates = TRUE)
#   }
# }


require(dplyr)
require(openxlsx)
require(tidyverse)
require('Rcpp')
require('biclust')
require('data.table')
require('diptest')
require('evtree')
require('ggdendro')
require("ggfortify")
require('ggplot2')
require('gplots')
require('gdata')
require('ggrepel')
require('ggRandomForests')
require('gridExtra')
require('gtable')
require('gtools')
require('igraph')
require('MASS')
require('packcircles')
require('plyr')
require("randomForestSRC")
require('reshape2')
require('pheatmap')
require('readxl')
require("raster")
require('openxlsx')
require("FlowSOM")
require('Rcpp')
require("SPADEVizR")
require(statmod)
require("edgeR")
require(RColorBrewer)
require(stringr)
require(tidyverse)

#Import custom modules
source("utils.R") #Sources utils function for phenoviewer_modified


#Modified phenoviewer script for creating parallel coordinates plots
phenoViewer_modified <- function(Results,
                                 samples        = NULL,
                                 clusters       = NULL,
                                 markers        = NULL,
                                 show.mean      = "both",
                                 show.on_device = TRUE,
                                 sort.markers   = TRUE) {
  
  ### when testing the function, use the parameters inside the function and test line by line of code. Use statement below to test the function above
  # Results=results
  # samples        = NULL
  # clusters       = "Cluster 10"
  # markers        = NULL
  # show.mean      = "only"
  # show.on_device = TRUE
  # sort.markers   = TRUE
  
  if (is.null(Results)) {
    stop("Error in phenoViewer: 'Results' parameter can not be NULL")
  } else if (class(Results)[1] != "Results") {
    stop("Error in phenoViewer: 'Results' parameter must be a 'Results' object")
  }
  
  if(length(Results@marker.names) == 0){
    stop("Error in phenoViewer: 'Results' object must contain phenotypes")
  }
  
  if (is.null(samples)) {
    samples     <- Results@sample.names
    data        <- Results@cluster.phenotypes
    cluster.abundances <- Results@cluster.abundances
  } else if (!all(samples %in% Results@sample.names)) {
    stop("Error in phenoViewer: 'samples' parameter must contains only samples names\n Unknown sample names: ",
         paste(setdiff(unique(samples), Results@sample.names), collapse = " "))
  } else {
    data               <- subset(Results@cluster.phenotypes, sample %in% samples, drop = FALSE)
    cluster.abundances <- Results@cluster.abundances[, samples, drop = FALSE]
  }
  
  data <- stats::na.omit(data)
  
  if (is.null(clusters)) {
    stop("Error in phenoViewer: 'clusters' parameter is required")
  } else if (all(clusters %in% Results@cluster.names)) {
    if (typeof(clusters) != "character") {
      stop("Error in phenoViewer: 'clusters' parameter must be a character vector")
    }
    clusters        <- unique(clusters)
    clusters.select <- data[, "cluster"] %in% clusters
    data            <- data[clusters.select,]
    cluster.abundances     <- cluster.abundances[clusters,]
  } else {
    stop("Error in phenoViewer:\nUnknown clusters : ", paste(setdiff(unique(clusters), Results@cluster.names), collapse = " "))
  }
  
  data <- plyr::ddply(data, c("sample"), function(df) {
    apply(df[, 3:ncol(df)], 2, mean, na.rm = TRUE)
  }) 
  
  if (is.null(markers)) {
    markers <- Results@marker.names
  } else if (all(markers %in% Results@marker.names)) {
    markers <- unique(markers)
    data <- data[, c("sample", markers)]
  } else {
    stop("Error in phenoViewer: Unknown markers :", paste(setdiff(unique(markers), Results@marker.names), collapse = " "))
  }
  
  if (show.mean != "none" && show.mean != "both" && show.mean != "only") {
    stop("Error in phenoViewer: 'show.mean' parameter must contain only one of these : 'none', 'both' or 'only'")
  }
  
  if (!is.logical(show.on_device)) { stop("Error in phenoViewer: 'show.on_device' parameter must be a logical") }
  
  data           <- reshape2::melt(data, id = c("sample"), stringsAsFactors = FALSE)
  colnames(data) <- c("samples", "marker", "value")
  
  names.palette  <- unique(Results@cluster.phenotypes$sample)
  palette        <- ggcolors(length(names.palette))
  names(palette) <- names.palette
  
  assignments <- Results@assignments
  
  if (!is.null(assignments)) {
    
    order       <- unique(assignments$bc)
    assignments <- assignments[samples, , drop = FALSE]
    data$bc <- assignments[data$samples, "bc"]
    order       <- intersect(order, unique(assignments$bc))
    data$bc <- factor(data$bc, levels = order)
    
    names.palette  <- unique(assignments$bc)
    palette        <- ggcolors(length(names.palette))
    names(palette) <- names.palette
    
  } else if (is.element("bc", colnames(assignments))) {
    warning("Warning in phenoViewer: 'assignments' slot do not contain the column 'bc' in the provided 'Results' object. Consequently, the samples names will be used in remplacement")
  } else {
    warning("Warning in phenoViewer: 'assignments' slot in the provided 'Results' object is absent. Consequently, the samples names will be used in remplacement")
  }
  
  if(sort.markers==TRUE){
    clustering.markers  <- Results@clustering.markers
    ordered.markers     <- c(gtools::mixedsort(clustering.markers),gtools::mixedsort(setdiff(Results@marker.names, clustering.markers)))
    bold.markers        <- ifelse(is.element(ordered.markers, clustering.markers), "bold", "plain")
    colored.markers     <- ifelse(is.element(ordered.markers, clustering.markers), "blue", "black")
    data$marker         <- factor(data$marker, levels = ordered.markers, ordered = TRUE)
  }else{
    clustering.markers  <- Results@clustering.markers
    ordered.markers     <- markers
    bold.markers        <- ifelse(is.element(ordered.markers, clustering.markers), "bold", "plain")
    colored.markers     <- ifelse(is.element(ordered.markers, clustering.markers), "blue", "black")
    data$marker         <- factor(data$marker, levels = ordered.markers, ordered = TRUE)
  }
  
  for (i in seq_len(nrow(data))) {
    data[i, "lower.bound"] <- Results@bounds[1, as.character(data[i, "marker"])]
    data[i, "upper.bound"] <- Results@bounds[2, as.character(data[i, "marker"])]
  }
  
  cells.number <- sum(colSums(cluster.abundances))
  
  title    <- paste("Pheno Viewer - cluster: ", paste0(clusters, collapse = ", "), " (", format(cells.number, big.mark = " "), " cells)", sep = "")
  bounds   <- as.numeric(row.names(Results@bounds))
  subtitle <- paste0("Grey ribbon displays from ", (bounds[1] * 100), "% to ", (bounds[2] * 100), "% percentiles of the range expression")
  
  max.value <- -1
  min.value <- -1
  
  max.value <- max(c(data$value, data$upper.bound), na.rm = TRUE)
  min.value <- min(c(data$value, data$lower.bound), na.rm = TRUE)
  
  max.value <-  max.value * (1 + sign(max.value) * 0.1)
  min.value <-  min.value * (1 - sign(min.value) * 0.1)
  
  
  means <- plyr::ddply(data,
                       c("marker"),
                       function(df){mean(df$value, na.rm = TRUE)})
  colnames(means) <- c("marker", "means")
  
  data_means <- data.frame(marker = 0, means= 0, clusters = 0)
  tmp_clusters<- unique(Results@cluster.phenotypes$cluster) ###### make sure the clusters input here is equal names with the phenotype and abundance sheets ###
  for(i in tmp_clusters){
    tmp_data<- Results@cluster.phenotypes
    tmp_clusters.select <- tmp_data[, "cluster"] %in% i
    tmp_data <- tmp_data[tmp_clusters.select,]
    
    tmp_data <- plyr::ddply(tmp_data, c("sample"), function(df) {
      apply(df[, 3:ncol(df)], 2, mean, na.rm = TRUE)
    }) 
    
    tmp_data           <- reshape2::melt(tmp_data, id = c("sample"), stringsAsFactors = FALSE)
    colnames(tmp_data) <- c("samples", "marker", "value")
    
    tmp_means <- plyr::ddply(tmp_data,
                             c("marker"),
                             function(df){mean(df$value, na.rm = TRUE)})
    colnames(tmp_means) <- c("marker", "means")
    tmp_means$clusters = i
    
    data_means = rbind(data_means, tmp_means)
  }
  data_means = data_means[-1, ]
  # data_means$marker = substr(data_means$marker, 2, 100000)
  #data_means = data_means[order(data_means$marker, decreasing = TRUE), ]
  plot <- ggplot2::ggplot(data = data_means) +
    ggplot2::ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), ""))))
  
  plot  <- plot + ggplot2::geom_line(ggplot2::aes_string(x = "marker", y = "means", group = "clusters"),
                                     size = 0.5, #changes size of background lines
                                     alpha = 1,
                                     color = "#CCCCCC")+ 
    ggplot2::scale_y_continuous(limits = c(min.value, max.value), breaks = round(seq(0, max.value, by = 1), 0)) +
    ggplot2::theme_bw()
  
  plot <- plot + ggplot2::geom_line(data  = means,
                                    ggplot2::aes_string(x = "marker", y = "means", group = 1),
                                    #group = 1,
                                    linetype = "solid",
                                    size  = 1,
                                    color = "#FF6666") 
  
  plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, face = bold.markers, color = colored.markers)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 6),
                   legend.key  = ggplot2::element_blank(),
                   plot.title  = ggplot2::element_text(hjust=0.5)) +
    ggplot2::xlab("markers") +
    ggplot2::ylab("marker expressions") +
    ggplot2::guides(col = ggplot2::guide_legend(ncol = 1))
  
  
  grid::grid.draw(plot)
  invisible(plot)
}


ParallelCoordinatePlots = function(ImportDataMaster_list,out_dir){
  #Function for exporting parallel coordinate plots from imported
  #spadevizR function
  
  #Get the current working directory
  PrimaryDirectory <- getwd()
  #Create directory
  dir.create(out_dir, showWarnings = FALSE)
  #Move to directory
  setwd(out_dir)
  
  #Export cluster images
  for(i in 1:nrow(ImportDataMaster_list[["ClusterAbundances"]])){
    jpeg(paste(rownames(ImportDataMaster_list[["ClusterAbundances"]])[i], ".jpeg", sep = ""),
         width=2000,
         height=1500, 
         res = 300)
    phenoViewer_modified(ImportDataMaster_list[["ImportedData"]], clusters = rownames(ImportDataMaster_list[["ClusterAbundances"]])[i])
    dev.off()
  }
  #Change back to primary directory
  setwd(PrimaryDirectory)
  #Add the names of the parallel coordinate plot directory to this list
  ImportDataMaster_list[["FilesPCP"]] = out_dir
  #Return the output directory
  return(ImportDataMaster_list)
}












#
