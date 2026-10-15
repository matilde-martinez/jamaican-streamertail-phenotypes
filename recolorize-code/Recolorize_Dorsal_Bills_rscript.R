### Recolorize on Smithsonian & LSU DORSAL Bills ### 



### Load libraries and read in images

library(recolorize)
### path to images
imgs <- list.files(path = "C:/Users/matil/OneDrive/Desktop/dorsal recolorize/transparent dorsal bills", full.names = TRUE)
#Make an empty list for storing the recolorize object:
rc_list <- vector("list", length(imgs))
#add informative names based on photo names
names(rc_list) <- gsub("-A.png","",gsub("C:/Users/matil/OneDrive/Desktop/dorsal recolorize/transparent dorsal bills/BEAK-", "",imgs))




### Run initial clustering

#for every image, run the same recolorize2 function to fit a recolorize object
for (i in 1:length(imgs)) {
  rc_list[[i]] <- recolorize2(imgs[[i]], bins = 3, cutoff = 0, plotting = TRUE)
  png(filename = paste(names(rc_list[i]), "INITIAL_AND_RECLUSTERED.png", sep = "-"), width = 1259, height = 778, res = 172)
  recolorize2(imgs[[i]], bins = 3, cutoff = 0, plotting = TRUE)
  dev.off()
}
saveRDS(rc_list, file = "C:/Users/matil/OneDrive/Desktop/dorsal recolorize/dorsal_rc_list.rds")



### Collate colors and come up with a reduced set of colors that generally describe the colors found across all photos

rc_list <- readRDS("C:/Users/matil/OneDrive/Desktop/dorsal recolorize/dorsal_rc_list.rds")

#get a dataframe of all colors found
all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
#get a dataframe for cluster sizes (as a proportion of their original image):
all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes))
#plotting colors using hclust to get grouping list:
par(mar = rep(2, 4))
cluster_list <- hclust_color(all_palettes, n_final = 4)
#make an empty matrix for storing the new palette:
streamer_palette <- matrix(NA, ncol=3, nrow = length(cluster_list))

#For every color in cluster_list...
for (i in 1:length(cluster_list)) {
  #get the center indices
  idx <- cluster_list[[i]]
  #get the average value for each channel, using cluster size to get a weighted average
  ctr <- apply(all_palettes, 2,
               function(j) weighted.mean(j[idx],
                                         w = all_sizes[idx]))
  #store in the palette matrix
  streamer_palette[i, ] <- ctr
}

#checking that this set of reduced colors seems reasonable
par(mar = rep(0, 4))
plotColorPalette(streamer_palette)




### Re-map to each image to this reduced and shared set of colors

#Mapping every image to the same set of n colors:
impose_list <- lapply(imgs, function(i) imposeColors(i, streamer_palette, adjust_centers = FALSE, plotting = TRUE))
saveRDS(impose_list, file = "C:/Users/matil/OneDrive/Desktop/dorsal recolorize/dorsal_impose_list.rds")



### Check out the results and quantify the proportion of black versus red 

#Look at the palettes for these individuals
layout(matrix(1:length(impose_list), nrow=99))
par(mar = rep(0,4))
for (i in 1:length(impose_list)) {
  plotColorPalette(impose_list[[i]]$centers, impose_list[[i]]$sizes)
}

#we can see from the palettes that the first and fourth colors are black and grey and can be combined together in calculations as 'black'
#we can also see that the second and third colors are red and pink and can be combined together as "red"

#open empty vector to hold results
red.prop<-c()
#calculate red proportion in each sample:
for (i in 1:length(impose_list)){
  red.prop[i]<-sum(impose_list[[i]]$sizes[2],impose_list[[i]]$sizes[3])/sum(impose_list[[i]]$sizes)
}

#plot each sample recolored and show calculated red proportion
layout(matrix(1:2, nrow=1))
for (i in 1:length(impose_list)){
  plotImageArray(impose_list[[i]]$original_img)
  text(x = .5, y = 0, label = paste("original image\n",names(rc_list)[i]), col = "black", font = 2)
  plotImageArray(recoloredImage(impose_list[[i]]))
  text(x = .5, y = 0, label = paste("recolorized\nproportion red = ",round(red.prop[i],3)), col = "black", font = 2)
}

  
for (i in 1:length(impose_list)) {
  png(filename = paste(names(rc_list[i]), "RED_PROP.png", sep = "-"), width = 1259, height = 900, res = 172)
    layout(matrix(1:2, nrow=1))
    plotImageArray(impose_list[[i]]$original_img)
    text(x = .5, y = 0, label = paste("original image\n",names(rc_list)[i]), col = "black", font = 2)
    plotImageArray(recoloredImage(impose_list[[i]]))
    text(x = .5, y = 0, label = paste("recolorized\nproportion red = ",round(red.prop[i],3)), col = "black", font = 2)
  dev.off()
}



#save red proportion in each sample as a dataframe
dorsal.red.prop.df<-data.frame(sample=names(rc_list),
                        prop.red=red.prop)

#export dataframe with relevant info saved
write.csv(dorsal.red.prop.df, "C:/Users/matil/OneDrive/Desktop/dorsal recolorize/updated.dorsal.red.prop.df.csv")

