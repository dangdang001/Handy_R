# Function to make the forest plot 

#input_data:   original data set containing columns named as:
#              Parameter, class, Ratio, LowerCL, UpperCL, ProbChiSq, num
#outFigure:    destination of output figures - full path, extension could be ".jpg" or ".tiff"
#neg_x:        space length for variables and levels names
#width, height: output figure size
#resolution:   output figure resolution
#total_size:   size of all texts in the output figure
#comments:     text to be shown at the bottom right of the figure
#ratios:       "Odds ratio" or "Hazard ratio"
#arrow:        upper limit of the displayed confidence intervals

if (!("metafor" %in% rownames(installed.packages()))) install.packages('metafor');
library("metafor")

Forest_plot <- function(input_data, outFigure, neg_x = 6,
                        width = 2, height = 1.8, resolution = 600, total_size = height / 2,
                        tck_pos = 0.01, add_pos = 3, 
                        comment = "", ratios, arrow = 10,p_pos=-3,max_show=arrow,com_pos=1) {
  
  data <- input_data[,c("Parameter","class","Ratio","LowerCL", "UpperCL","ProbChiSq")]
  data$Parameter <- as.character(data$Parameter)
  data$class <- as.character(data$class)
  data$ProbChiSq <- as.character(data$ProbChiSq)
  
  Bind <- NULL
  for(i in 1:dim(data)[1]){
    if (i==1 & data[i,"Parameter"]!=data[i,"class"]) {        
      Bind<-rbind(data.frame(data[1,],dx=2),data.frame(data[1,],dx=1))        
    } else {
      
      if (i==1 & data[i,"Parameter"]==data[i,"class"]) {
        Bind<-rbind(data.frame(data[1,],dx=1))
      }
      if (data[i,"Parameter"]!=data[i-1,"Parameter"] & data[i,"Parameter"]!=data[i,"class"]) {
        Bind<-rbind(Bind,data.frame(data[i,],dx=2),data.frame(data[i,],dx=1))
      } else {
        Bind<-rbind(Bind,data.frame(data[i,],dx=1))
      }
    }
  }
  
  Bind<-as.data.frame(Bind)
  Bind[Bind[,"dx"]==2,"class"]<-Bind[Bind[,"dx"]==2,"Parameter"]
  
  Bind$Num<-1:nrow(Bind)
  Bind$locat<-max(Bind$Num+2)-Bind$Num
  
  data <- Bind[Bind$dx == 1, ]
  rown <- data$locat;
  
  range<-c(floor(min(data$LowerCL)*2)/2,ceiling(max(data$UpperCL)*2)/2)
  ORrange<-unique(sort(c(seq(range[1],range[2],(range[2]-range[1])/5),1)))
  
  if (substr(outFigure, nchar(outFigure), nchar(outFigure)) == "f") {
    tiff(filename = outFigure, width = width, height = height, units = "in", res = resolution);
  } else if (substr(outFigure, nchar(outFigure), nchar(outFigure)) == "g") {
    jpeg(filename = outFigure, width = width, height = height, units = "in", res = resolution);
  }
  
  par(mar = c(5.1, 4.1, 2.1, 2) / 3 * total_size)
  
  cex_title = 2.0 / 9 * total_size;
  cex_text = 1.9 / 9 * total_size;
  a = forest(x = data$Ratio, ci.lb = data$LowerCL, ci.ub = data$UpperCL, 
             refline = 1, alim = c(-neg_x, arrow),
             lwd = 0.2 * total_size, tck = -0.01, mgp = c(0, -tck_pos, 0), 
             xlim = c(-neg_x, arrow), ylim = c(-1, max(data$locat + 4)), 
             at = c(0:max_show), ilab.xpos = -0.01, cex = cex_text,
             xlab = '', slab = data$class, rows = rown, efac = 0,
             psize = 0.03 / (data$UpperCL - data$LowerCL) * 7 * total_size);
  
  
  par(font = 4)
  text(-neg_x, Bind[Bind$dx == 2, "locat"], pos = 4, Bind[Bind$dx == 2, "Parameter"],
       cex = cex_text)
  par(font = 1)
  text(-neg_x / p_pos, Bind[Bind$dx == 1, "locat"], pos = 4, Bind[Bind$dx == 1, "ProbChiSq"],
       cex = cex_text)
  
  par(font = 2)
  text(arrow + add_pos, max(data$locat + 3), pos = 2, paste(ratios, "[95% CI]"), cex = cex_title)
  text(-neg_x, max(data$locat + 3), pos = 4, "Variables", cex = cex_title)
  text(-neg_x / p_pos, max(data$locat + 3), pos = 4, "P-values", cex = cex_title)
  
  par(font = 1, xpd = NA)
  text(1 + 0.3 / total_size, 0, paste(ratios, "smaller"), pos = 2, cex = cex_title)
  text(1 - 0.3 / total_size, 0, paste(ratios, "bigger"), pos = 4, cex = cex_title)
  
  par(font = 3)
  text(-neg_x * com_pos, -4, pos = 4, cex = cex_title * 0.9, comment);
  
  dev.off();
}
