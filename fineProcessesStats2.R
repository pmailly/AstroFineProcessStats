## Statistical analysis for fine processes after protease

#install missing libraries
requiredPackages = c("base", "readbulk", "dplyr", "plyr", "ggplot2", "ggpubr", "rstudioapi", "svDialogs", "stringi", "cowplot")
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
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data virtual protease directory 1", label = "Select", path = NULL)
prot1Data <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
prot1Data$File <- NULL
# virtual protease parameter 2
dataDir <- selectDirectory(caption = "Select AstroDot fine processes data virtual protease directory 2", label = "Select", path = NULL)
prot2Data <- read_bulk(dataDir, subdirectories = FALSE, extension = ".csv", fun = read.delim, stringsAsFactors = TRUE)
# remove file column
prot2Data$File <- NULL

outDir <- selectDirectory(caption = "Select AstroDot fine processes output directory", label = "Select", path = NULL)

legend <- c("no prot", "prot", "vprotx2", "vprotx4")

#Diameters
diameters <- as.data.frame(cbind(noprot = noProtData$Diameters, prot = protData$Diameters, vprot1 = prot1Data$Diameters, vprot2 = prot2Data$Diameters))
## multi plots
histos <- list()
xLimMax <- max(sapply(diameters, max, na.rm = TRUE))
for (i in colnames(diameters)) {
  meanValue <- mean(diameters[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g", meanValue)), x=0.60,  y=0.80, hjust=0,
                              gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] <- ggplot(diameters, aes_string(diameters[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..),alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "diameters (µm)", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroDiameters_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)

# branchs number
branchs <- as.data.frame(cbind(noprot = noProtData$Branchs, prot = protData$Branchs, vprot1 = prot1Data$Branchs, vprot2 = prot2Data$Branchs))
## multi plots
histos <- list()
xLimMax <- max(sapply(branchs, max, na.rm = TRUE))
for (i in colnames(branchs)) {
  meanValue <- mean(branchs[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g", meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(branchs, aes_string(branchs[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..),alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "branchs number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroBranchs_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)

# normalized branchs number
norm_branchs <- as.data.frame(cbind(noprot = noProtData$BranchsNormVol, prot = protData$BranchsNormVol, vprot1 = prot1Data$BranchsNormVol, vprot2 = prot2Data$BranchsNormVol))
# multi plots
histos <- list()
xLimMax <- max(sapply(norm_branchs, max, na.rm = TRUE))
for (i in colnames(norm_branchs)) {
  meanValue <- mean(norm_branchs[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g", meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(norm_branchs, aes_string(norm_branchs[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "normalized branchs number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroNormBranchs_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)


# branchs length
branch_lengths <- as.data.frame(cbind(noprot = noProtData$Lengths, prot = protData$Lengths, vprot1 = prot1Data$Lengths, vprot2 = prot2Data$Lengths))
# multiplots
histos <- list()
xLimMax <- max(sapply(branch_lengths, max, na.rm = TRUE))
for (i in colnames(branch_lengths)) {
  meanValue <- mean(branch_lengths[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(branch_lengths, aes_string(branch_lengths[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..),alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "branchs length (µm)", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroBranchLengths_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)


# normalized branchs length
norm_branch_lengths <- as.data.frame(cbind(noprot = noProtData$LengthNormVol, prot = protData$LengthNormVol, vprot1 = prot1Data$LengthNormVol, vprot2 = prot2Data$LengthNormVol))
# multi plots
histos <- list()
xLimMax <- max(sapply(norm_branch_lengths, max, na.rm = TRUE))
for (i in colnames(norm_branch_lengths)) {
  meanValue <- mean(norm_branch_lengths[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(norm_branch_lengths, aes_string(norm_branch_lengths[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "normalized branchs length (µm)", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroNormBranchLengths_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)



# end points
endpoints <- as.data.frame(cbind(noprot = noProtData$EndPoints, prot = protData$EndPoints, vprot1 = prot1Data$EndPoints, vprot2 = prot2Data$EndPoints))
# multi plots
histos <- list()
xLimMax <- max(sapply(endpoints, max, na.rm = TRUE))
for (i in colnames(endpoints)) {
  meanValue <- mean(endpoints[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(endpoints, aes_string(endpoints[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "endpoints number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroEndpoints_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)


# normalized endpoints
norm_endpoints <- as.data.frame(cbind(noprot = noProtData$EndPointsNormVol, prot = protData$EndPointsNormVol, vprot1 = prot1Data$EndPointsNormVol, vprot2 = prot2Data$EndPointsNormVol))
# multi plots
histos <- list()
xLimMax <- max(sapply(norm_endpoints, max, na.rm = TRUE))
for (i in colnames(norm_endpoints)) {
  meanValue <- mean(norm_endpoints[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(norm_endpoints, aes_string(norm_endpoints[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "normalized endpoints number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroNormEndpoints_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)


# junctions
junctions <- as.data.frame(cbind(noprot = noProtData$Junctions, prot = protData$Junctions, vprot1 = prot1Data$Junctions, vprot2 = prot2Data$Junctions))
# multi plots
histos <- list()
xLimMax <- max(sapply(junctions, max, na.rm = TRUE))
for (i in colnames(junctions)) {
  meanValue <- mean(junctions[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(junctions, aes_string(junctions[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "junctions number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroJunctions_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)


# normalized junctions
norm_junctions <- as.data.frame(cbind(noprot = noProtData$JunctionsNormVol, prot = protData$JunctionsNormVol, vprot1 = prot1Data$JunctionsNormVol, vprot2 = prot2Data$JunctionsNormVol))
# multi plots
histos <- list()
xLimMax <- max(sapply(norm_junctions, max, na.rm = TRUE))
for (i in colnames(norm_junctions)) {
  meanValue <- mean(norm_junctions[[i]], na.rm = TRUE)
  grob <- grobTree(textGrob(paste0("Mean = ", sprintf("%0.3g",meanValue)), x=0.60,  y=0.80, hjust=0,
                            gp=gpar(col="blue", fontsize=12, fontface="italic")))
  histos[[i]] = ggplot(norm_junctions, aes_string(norm_junctions[[i]]), color="black", fill="white") +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, size=20, face = "bold", color = "blue")) +
    geom_histogram(aes(y= ..ncount..), position="identity", alpha=0.5, bins = 30) +
    geom_density(aes(y=..scaled..), alpha=.2, color="#FF6666", fill="#FF6666", show.legend = FALSE) +
    labs(x = "normalized junctions number", y = "density") +
    xlim(0, xLimMax) +
    geom_vline(aes(xintercept = meanValue), color='blue', linetype='dashed', size=0.5) +
    annotation_custom(grob)
}
plot_list <- plot_grid(plotlist = histos, labels = legend, ncol = 2, nrow = 2, label_x = 0.45, label_y = 0.95, label_size = 12)
ggsave("astroNormJunctions_Plot.pdf", plot = plot_list, path = outDir, units="mm",width=250, height = 250, dpi=300)



## Statistics tests

dataStats <- data.frame(matrix(ncol = 6, nrow = 9))
row_names = c("diameters", "branchs", "norm branchs", "branch lengths", "norm branch lengths", "end points",
              "norm end points", "junctions", "norm junctions") 
col_names = c("Wilcoxon no protease/protease", "Wilcoxon no protease/v protease x2", "Wilcoxon no protease/v protease x3",
              "Spearman no protease/protease", "Spearman no protease/v protease x2", "Spearman no protease/v protease x3")
colnames(dataStats) <- col_names
rownames(dataStats) <- row_names

n = 1
for (i in colnames(diameters)) {
  if (i != "noprot") {
    # wilcoxon
    dataStats[1, n] <- wilcox.test(diameters$noprot, diameters[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[1, n+3] <- cor.test(diameters$noprot, diameters[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(branchs)) {
  if (i != "noprot") {
    dataStats[2, n] <- wilcox.test(branchs$noprot, branchs[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[2, n+3] <- cor.test(branchs$noprot, branchs[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(norm_branchs)) {
  if (i != "noprot") {
    dataStats[3, n] <- wilcox.test(norm_branchs$noprot, norm_branchs[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[3, n+3] <- cor.test(norm_branchs$noprot, norm_branchs[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}

n = 1
for (i in colnames(branch_lengths)) {
  if (i != "noprot") {
    dataStats[4, n] <- wilcox.test(branch_lengths$noprot, branch_lengths[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[4, n+3] <- cor.test(branch_lengths$noprot, branch_lengths[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(norm_branch_lengths)) {
  if (i != "noprot") {
    dataStats[5, n] <- wilcox.test(norm_branch_lengths$noprot, norm_branch_lengths[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[5, n+3] <- cor.test(norm_branch_lengths$noprot, norm_branch_lengths[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(endpoints)) {
  if (i != "noprot") {
    dataStats[6, n] <- wilcox.test(endpoints$noprot, endpoints[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[6, n+3] <- cor.test(endpoints$noprot, endpoints[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(norm_endpoints)) {
  if (i != "noprot") {
    dataStats[7, n] <- wilcox.test(norm_endpoints$noprot, norm_endpoints[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[7, n+3] <- cor.test(norm_endpoints$noprot, norm_endpoints[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(junctions)) {
  if (i != "noprot") {
    dataStats[8, n] <- wilcox.test(junctions$noprot, junctions[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[8, n+3] <- cor.test(junctions$noprot, junctions[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}
n = 1
for (i in colnames(norm_junctions)) {
  if (i != "noprot") {
    dataStats[9, n] <- wilcox.test(norm_junctions$noprot, norm_junctions[[i]], paired=TRUE)$p.value
    # Spearman
    dataStats[9, n+3] <- cor.test(norm_junctions$noprot, norm_junctions[[i]], method= "spearman")$p.value
    n <- n + 1
  }
}

write.csv(dataStats, file = file.path(outDir,"astroFineProcessStats.csv", fsep = .Platform$file.sep))


