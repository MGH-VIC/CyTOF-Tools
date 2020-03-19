WaterfallPlot = function(MLobject, plot_colors){
  
  #Get the dataframe for the waterfall plot from MLobject
  if(class(MLobject) == "list"){
    print("Detected an ML object")
    #Access the list and get the second object(Plotting object)
    data = MLobject[[2]]
  }
  #Order the waterfall plot data
  data = data[order(-data[,1]),,drop = FALSE]
  
  #create new column for names
  data$Group <- ifelse(CoefficientsLDA$`Coefficients of Linear Discriminants` < 0,
                       levels(input_to_lda$Group)[1],
                       levels(input_to_lda$Group)[2])
  
  #Plot coefficients in waterfall plot
  tmp_values = CoefficientsLDA$`Coefficients of Linear Discriminants`
  features = rownames(CoefficientsLDA)
  features = factor(features, levels = features[order(tmp_values)])
  axval = (max(abs(tmp_values)) + 0.03)
  
  plot_coef = ggplot(CoefficientsLDA, aes(x=features, y=tmp_values)) + 
    geom_bar(stat='identity', aes(fill=Group), width=.6)  +
    scale_fill_manual(name="Group", 
                      labels = c(levels(input_to_lda$Group)[1],levels(input_to_lda$Group)[2]), 
                      values = plot_colors
    ) + 
    labs(subtitle="", title= "") +
    theme(text = element_text(size=0.5),                         
          axis.text.x = element_text(face="bold", color="black", size=2.5, angle=0),
          axis.text.y = element_text(face="bold", color="black", size=2.5, angle=0),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.15),
          panel.grid = element_line(colour = "grey", size = 0.1),
          plot.margin = unit(c(2,2,2,2), "mm"),
          axis.title.y = element_text(angle = 90, vjust = 1, hjust = 0.5, size = 4),
          axis.title.x = element_text(angle = 0, vjust = -0.3, hjust = 0.5, size = 4),
          plot.title = element_text(vjust=3, hjust = 0),
          plot.subtitle = element_text(vjust = 3),
          legend.title = element_text(size=2), 
          legend.text = element_text(size=2),
          legend.key.size = unit(0.3,"line"),
          legend.margin = ggplot2::margin(t=-0.5, unit = "mm")) +
    labs(x = "Phenotypic Class", y = expression(paste("LDA Coeffiecient"))) + 
    scale_y_continuous(limits = c(-axval, axval)) +
    coord_flip()
  ggsave(paste(day,levels(input_to_lda$Group)[1],
               levels(input_to_lda$Group)[2],
               "coeff_plot.jpeg",sep="_"),plot = plot_coef, units = "mm", width = 50, height = 50)
}

