### Recolorize on Smithsonian & LSU VENTRAL Bills ### 



### Load libraries and read in images

library(recolorize)
### path to images
ventral_imgs <- list.files(path = "C:/Users/matil/OneDrive/Desktop/ventral recolorize/transparent ventral bills", full.names = TRUE)
#Make an empty list for storing the recolorize object:
ventral_rc_list <- vector("list", length(ventral_imgs))
#add informative names based on photo names
names(ventral_rc_list) <- gsub("-A.png","",gsub("C:/Users/matil/OneDrive/Desktop/ventral recolorize/transparent ventral bills/BEAK-", "", ventral_imgs))



### Run initial clustering

#for every image, run the same recolorize2 function to fit a recolorize object
for (i in 1:length(ventral_imgs)) {
  ventral_rc_list[[i]] <- recolorize2(ventral_imgs[[i]], bins = 3, cutoff = 0, plotting = TRUE)
  png(filename = paste(names(ventral_rc_list[i]), "INITIAL_AND_RECLUSTERED.png", sep = "-"), width = 1259, height = 778, res = 172)
  recolorize2(ventral_imgs[[i]], bins = 3, cutoff = 0, plotting = TRUE)
  dev.off()
}
saveRDS(ventral_rc_list, file = "C:/Users/matil/OneDrive/Desktop/ventral recolorize/ventral_rc_list_UPDATED.rds")



### Collate colors and come up with a reduced set of colors that generally describe the colors found across all photos

ventral_rc_list <- readRDS("C:/Users/matil/OneDrive/Desktop/ventral recolorize/ventral_rc_list_UPDATED.rds")


#get a dataframe of all colors found
v_all_palettes <- do.call(rbind, lapply(ventral_rc_list, function(i) i$centers))
#get a dataframe for cluster sizes (as a proportion of their original image):
v_all_sizes <- do.call(c, lapply(ventral_rc_list, function(i) i$sizes))
#plotting colors using hclust to get grouping list:
par(mar = rep(2, 4))
v_cluster_list <- hclust_color(v_all_palettes, n_final = 4)
#make an empty matrix for storing the new palette:
v_streamer_palette <- matrix(NA, ncol=3, nrow = length(v_cluster_list))

#For every color in cluster_list...
for (i in 1:length(v_cluster_list)) {
  #get the center indices
  v_idx <- v_cluster_list[[i]]
  #get the average value for each channel, using cluster size to get a weighted average
  v_ctr <- apply(v_all_palettes, 2,
               function(j) weighted.mean(j[v_idx],
                                         w = v_all_sizes[v_idx]))
  #store in the palette matrix
  v_streamer_palette[i, ] <- v_ctr
}

#checking that this set of reduced colors seems reasonable
par(mar = rep(0, 4))
plotColorPalette(v_streamer_palette)




### Re-map to each image to this reduced and shared set of colors

#Mapping every image to the same set of n colors:
ventral_impose_list <- lapply(ventral_imgs, function(i) imposeColors(i, v_streamer_palette, adjust_centers = FALSE, plotting = TRUE))
saveRDS(ventral_impose_list, file = "C:/Users/matil/OneDrive/Desktop/ventral recolorize/ventral_impose_list.rds")



FOUR_COLOR_ventral_impose_list <- lapply(ventral_imgs, function(i) imposeColors(i, v_streamer_palette, adjust_centers = FALSE, plotting = TRUE))
saveRDS(FOUR_COLOR_ventral_impose_list, file = "C:/Users/matil/OneDrive/Desktop/ventral recolorize/FOUR_COLOR_ventral_impose_list.rds")
FOUR_COLOR_ventral_impose_list <- readRDS("C:/Users/matil/OneDrive/Desktop/ventral recolorize/FOUR_COLOR_ventral_impose_list.rds")

THREE_COLOR_ventral_impose_list <- lapply(ventral_imgs, function(i) imposeColors(i, v_streamer_palette, adjust_centers = FALSE, plotting = TRUE))
saveRDS(THREE_COLOR_ventral_impose_list, file = "C:/Users/matil/OneDrive/Desktop/ventral recolorize/THREE_COLOR_ventral_impose_list.rds")


ventral_impose_list <- readRDS("C:/Users/matil/OneDrive/Desktop/ventral recolorize/ventral_impose_list.rds")
THREE_COLOR_ventral_impose_list <- readRDS("C:/Users/matil/OneDrive/Desktop/ventral recolorize/THREE_COLOR_ventral_impose_list.rds")
FIVE_COLOR_ventral_impose_list <- readRDS("C:/Users/matil/OneDrive/Desktop/ventral recolorize/FIVE_COLOR_ventral_impose_list.rds")


### Check out the results and quantify the proportion of black versus red 

#Look at the palettes for these individuals
layout(matrix(1:length(ventral_impose_list), nrow=99))
par(mar = rep(0,4))
for (i in 1:length(ventral_impose_list)) {
  plotColorPalette(ventral_impose_list[[i]]$centers, ventral_impose_list[[i]]$sizes)
}

#we can see from the palettes that the first and fourth colors are black and grey and can be combined together in calculations as 'black'
#we can also see that the second and third colors are red and pink and can be combined together as "red"

#open empty vector to hold results
ventral.red.prop<-c()

#calculate red proportion in each sample:
for (i in 1:length(ventral_impose_list)){
  ventral.red.prop[i]<-sum(ventral_impose_list[[i]]$sizes[2],ventral_impose_list[[i]]$sizes[3])/sum(ventral_impose_list[[i]]$sizes)
}

four.color.ventral.red.prop <- c()
for (i in 1:length(FOUR_COLOR_ventral_impose_list)){
  four.color.ventral.red.prop[i]<-sum(FOUR_COLOR_ventral_impose_list[[i]]$sizes[3], FOUR_COLOR_ventral_impose_list[[i]]$sizes[4])/sum(FOUR_COLOR_ventral_impose_list[[i]]$sizes)
}


#plot each sample recolored and show calculated red proportion
layout(matrix(1:2, nrow=1))
for (i in 1:length(ventral_impose_list)){
  plotImageArray(ventral_impose_list[[i]]$original_img)
  text(x = .5, y = 0, label = paste("original image\n",names(ventral_rc_list)[i]), col = "black", font = 2)
  plotImageArray(recoloredImage(ventral_impose_list[[i]]))
  text(x = .5, y = 0, label = paste("recolorized\nproportion red = ",round(ventral.red.prop[i],3)), col = "black", font = 2)
}


for (i in 1:length(ventral_impose_list)){
  png(filename = paste(names(ventral_rc_list[i]), "RED_PROP.png", sep = "-"), width = 1259, height = 900, res = 172)
    layout(matrix(1:2, nrow=1))
    plotImageArray(ventral_impose_list[[i]]$original_img)
    text(x = .5, y = 0, label = paste("original image\n",names(ventral_rc_list)[i]), col = "black", font = 2)
    plotImageArray(recoloredImage(ventral_impose_list[[i]]))
    text(x = .5, y = 0, label = paste("recolorized\nproportion red = ",round(ventral.red.prop[i],3)), col = "black", font = 2)
  dev.off()
}





for (i in 162){
  png(filename = paste("FOUR-COLOR", names(ventral_rc_list[i]), "RED_PROP.png", sep = "-"), width = 1259, height = 900, res = 172)
  layout(matrix(1:2, nrow=1))
  plotImageArray(FOUR_COLOR_ventral_impose_list[[i]]$original_img)
  text(x = .5, y = 0, label = paste("original image\n",names(ventral_rc_list)[i]), col = "black", font = 2)
  plotImageArray(recoloredImage(FOUR_COLOR_ventral_impose_list[[i]]))
  text(x = .5, y = 0, label = paste("recolorized\nproportion red = ",round(four.color.ventral.red.prop[i],3)), col = "black", font = 2)
  dev.off()
}






#save red proportion in each sample as a dataframe
four.color.ventral.red.prop.df<-data.frame(sample=names(ventral_rc_list),
                               prop.red=four.color.ventral.red.prop)

#export dataframe with relevant info saved
write.csv(four.color.ventral.red.prop.df, "C:/Users/matil/OneDrive/Desktop/ventral recolorize/four.color.ventral.red.prop.df.csv")

