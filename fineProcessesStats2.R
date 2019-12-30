## Statistical analysis for fine processes after protease

#install missing libraries
requiredPackages = c("base", "readbulk", "dplyr", "plyr", "ggplot2", "gridExtra", "ggpubr", "rstudioapi", "svDialogs", "stringi")
for(i in requiredPackages){
  if(!require(i,character.only = TRUE)) install.packages(i)
  library(i,character.only = TRUE)
}

# no protease
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data no protease directory", label = "Select", path = NULL)
noProtData <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
noProtData$File <- NULL
# protease
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data protease directory", label = "Select", path = NULL)
protData <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
protData$File <- NULL
# virtual protease parameter 1
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data virtual protease directory", label = "Select", path = NULL)
prot1Data <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
prot1Data$File <- NULL
# virtual protease parameter 2
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data virtual protease directory", label = "Select", path = NULL)
prot2Data <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
prot2Data$File <- NULL

outDir <- selectDirectory(caption = "Select AstroDot fine processes output directory", label = "Select", path = NULL)

  # Diameters
  diameters <- as.data.frame(cbind(noprot = noProtData$Diameters, prot = protData$Diameters, vprot1 = prot1Data$Diameters, vprot2 = prot2Data$Diameters))
  legend <- c("no prot", "prot", "vprotx2", "vprotx3")  
  histo <- list()
    for (i in colnames(diameters)) {
      histo[[i]] <- ggplot(diameters, aes_string(diameters[[i]]), color="black") +
        theme_bw(base_size = 16) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSe) +
        labs(x = "diameters (log)", y = "density") +
        scale_x_log10() + ylim(0, 25) +
        geom_vline(aes(xintercept = mean(diameters[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
    }
  plot_grid(plotlist = histo, labels = legend, ncol = 2, nrow = 2, label_x = 0.55, label_y = 0.95, label_size = 12)
 
  
    
    for (i in colnames(diameters)) {
        histo <- ggplot(diameters, aes(diameters[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "diameters (log)", y = "density") +
        scale_x_log10() + ylim(0, 25) +
        geom_vline(aes(xintercept = mean(diameters[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
        ggsave(paste0(outDir,"astroDiameters_",i,"_Plot.pdf"), plot = histo, path = dataDir,
           units="mm",width=250, height = 250, dpi=300)
    }
    
    # branchs number
    branchs <- as.data.frame(cbind(noprot = noProtData$Branchs, prot = protData$Branchs, vprot1 = prot1Data$Branchs, vprot2 = prot2Data$Branchs))
    
    for (i in colnames(branchs)) {
      histo <- ggplot(branchs, aes(branchs[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "branch number (log)", y = "density") +
        scale_x_log10() + ylim(0, 25) +
        geom_vline(aes(xintercept = mean(branchs[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"branch_number",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # normalized branchs number
    norm_branchs <- as.data.frame(cbind(noprot = noProtData$BranchsNormVol, prot = protData$BranchsNormVol, vprot1 = prot1Data$BranchsNormVol, vprot2 = prot2Data$BranchsNormVol))
    
    for (i in colnames(norm_branchs)) {
      histo <- ggplot(norm_branchs, aes(norm_branchs[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "normalized branch number (log)", y = "density") +
        scale_x_log10() + ylim(0, 3) +
        geom_vline(aes(xintercept = mean(norm_branchs[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"norbranch_number",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # branchs length
    branch_lengths <- as.data.frame(cbind(noprot = noProtData$Lengths, prot = protData$Lengths, vprot1 = prot1Data$Lengths, vprot2 = prot2Data$Lengths))
    
    for (i in colnames(branch_lengths)) {
      histo <- ggplot(branch_lengths, aes(branch_lengths[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "branch lengths (log)", y = "density") +
        scale_x_log10() + ylim(0, 6) +
        geom_vline(aes(xintercept = mean(branch_lengths[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"branchlengths",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # normalized branchs length
    norm_branch_lengths <- as.data.frame(cbind(noprot = noProtData$LengthNormVol, prot = protData$LengthNormVol, vprot1 = prot1Data$LengthNormVol, vprot2 = prot2Data$LengthNormVol))
    
    for (i in colnames(norm_branch_lengths)) {
      histo <- ggplot(norm_branch_lengths, aes(norm_branch_lengths[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "normalized branch lengths (log)", y = "density") +
        scale_x_log10() +
        geom_vline(aes(xintercept = mean(norm_branch_lengths[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"normalized_branchlengths",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # end points
    endpoints <- as.data.frame(cbind(noprot = noProtData$EndPoints, prot = protData$EndPoints, vprot1 = prot1Data$EndPoints, vprot2 = prot2Data$EndPoints))
    
    for (i in colnames(endpoints)) {
      histo <- ggplot(endpoints, aes(endpoints[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "endpoints (log)", y = "density") +
        scale_x_log10() + ylim(0, 15) +
        geom_vline(aes(xintercept = mean(endpoints[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"endpoints",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # normalized endpoints
    norm_endpoints <- as.data.frame(cbind(noprot = noProtData$EndPointsNormVol, prot = protData$EndPointsNormVol, vprot1 = prot1Data$EndPointsNormVol, vprot2 = prot2Data$EndPointsNormVol))
    
    for (i in colnames(norm_endpoints)) {
      histo <- ggplot(norm_endpoints, aes(norm_endpoints[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "normalized endpoints (log)", y = "density") +
        scale_x_log10() + ylim(0, 2.5) +
        geom_vline(aes(xintercept = mean(norm_endpoints[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"normalized_endpoints",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
    # junctions
    junctions <- as.data.frame(cbind(noprot = noProtData$Junctions, prot = protData$Junctions, vprot1 = prot1Data$Junctions, vprot2 = prot2Data$Junctions))
    
    for (i in colnames(junctions)) {
      histo <- ggplot(junctions, aes(junctions[[i]]), color="black", fill="white") + scale_x_log10() +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "Junctions (log)", y = "density") +
        scale_x_log10() + ylim(0, 6) +
        geom_vline(aes(xintercept = mean(junctions[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"junctions",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
       
    }
    
    # normalized junctions
    norm_junctions <- as.data.frame(cbind(noprot = noProtData$JunctionsNormVol, prot = protData$JunctionsNormVol, vprot1 = prot1Data$JunctionsNormVol, vprot2 = prot2Data$JunctionsNormVol))
    
    for (i in colnames(norm_junctions)) {
      histo <- ggplot(norm_junctions, aes(norm_junctions[[i]]), color="black", fill="white") +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue"),
              axis.title.x = element_text(face="bold"),
              legend.title = element_text(face="bold")) +
        geom_histogram(aes(y= ..density..), position="identity", alpha=0.5, bins = 30) +
        geom_density(alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
        labs(x = "normalized junctions (log)", y = "density") +
        scale_x_log10() + ylim(0, 1.5) +
        geom_vline(aes(xintercept = mean(norm_junctions[[i]], na.rm = TRUE)), color='blue', linetype='dashed', size=0.5)
      ggsave(paste0(outDir,"normalized_junctions",i,"_Plot.pdf"), plot = histo, path = dataDir,
             units="mm",width=250, height = 250, dpi=300)
        
    }
    
  ## Statistics tests
   
    
    dataStats <- data.frame(matrix(ncol = 6, nrow = 9))
    row_names = c("diameters", "branchs", "norm branchs", "branch lengths", "norm branch lengths", "endpoints",
                                          "norm endpoints", "junctions", "norm junctions") 
    col_names = c("Wilcoxon no protease/protease", "Wilcoxon no protease/v protease x2", "Wilcoxon no protease/v protease x3",
                  "Spearman no protease/protease", "Spearman no protease/v protease x2", "Spearman no protease/v protease x3")
    colnames(dataStats) <- col_names
    rownames(dataStats) <- row_names
    
    n = 1
    for (i in colnames(diameters)) {
      if (i != "noprot") {
         # wilcoxon
        dataStats["diameters", n] <- wilcox.test(diameters$noprot, diameters[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["diameters", n+3] <- cor.test(diameters$noprot, diameters[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(branchs)) {
      if (i != "noprot") {
        dataStats["branchs", n] <- wilcox.test(branchs$noprot, branchs[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["branchs", n+3] <- cor.test(branchs$noprot, branchs[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(norm_branchs)) {
      if (i != "noprot") {
        dataStats["norm branchs", n] <- wilcox.test(norm_branchs$noprot, norm_branchs[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["norm branchs", n+3] <- cor.test(norm_branchs$noprot, norm_branchs[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    
    n = 1
    for (i in colnames(branch_lengths)) {
      if (i != "noprot") {
        dataStats["branch lengths", n] <- wilcox.test(branch_lengths$noprot, branch_lengths[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["branch lengths", n+3] <- cor.test(branch_lengths$noprot, branch_lengths[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(norm_branch_lengths)) {
      if (i != "noprot") {
        dataStats["norm branch lengths", n] <- wilcox.test(norm_branch_lengths$noprot, norm_branch_lengths[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["norm branch lengths", n+3] <- cor.test(norm_branch_lengths$noprot, norm_branch_lengths[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(endpoints)) {
      if (i != "noprot") {
        dataStats["endpoints", n] <- wilcox.test(endpoints$noprot, endpoints[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["endpoints", n+3] <- cor.test(endpoints$noprot, endpoints[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(norm_endpoints)) {
      if (i != "noprot") {
        dataStats["norm endpoints", n] <- wilcox.test(norm_endpoints$noprot, norm_endpoints[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["norm endpoints", n+3] <- cor.test(norm_endpoints$noprot, norm_endpoints[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(junctions)) {
      if (i != "noprot") {
        dataStats["junctions", n] <- wilcox.test(junctions$noprot, junctions[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["junctions", n+3] <- cor.test(junctions$noprot, junctions[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
    n = 1
    for (i in colnames(norm_junctions)) {
      if (i != "noprot") {
        dataStats["norm junctions", n] <- wilcox.test(norm_junctions$noprot, norm_junctions[[i]], paired=TRUE)$p.value
        # Spearman
        dataStats["norm junctions", n+3] <- cor.test(norm_junctions$noprot, norm_junctions[[i]], method= "spearman")$p.value
        n <- n + 1
      }
    }
   
    write.csv(dataStats, file = paste0(outDir,"astroFineProcessStats.csv"))

    
    