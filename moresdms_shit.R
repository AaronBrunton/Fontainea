library(dismo)
## Loading required package: raster
library(maptools)
library(raster)

data(wrld_simpl)

predo <- BClim

# file <- read_csv("sdm_dat2.csv")
# fona <- read.table(file,  header=TRUE,  sep=',')

file <- "rfdat2_biorem.csv"
fona <- read.table(file,  header=TRUE,  sep=',')
# we do not need the lat long first column
fona <- fona[,2:3]

head(fona)

plot(wrld_simpl, xlim=c(152.242576,153.680991), ylim=c(-27.834608, -28.882318), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# add the points
points(fona$lon, fona$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(fona$lon, fona$lat, col='red', cex=0.75)


plot(wrld_simpl, add=T, border='blue', lwd=2)



# first layer of the RasterStack
plot(predo, 1)
# note the "add=TRUE" argument with plot
plot(e, add=TRUE)
# with the points function, "add" is implicit
points(fona, col='blue')



presvals <- extract(predo, fona)
set.seed(0)
backgr <- randomPoints(predo, 500)
absvals <- extract(predo, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
head(sdmdata)
#sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])

####
preds <- (fa_df2)
fona <- read.table("rfdat2_biorem.csv",  header=TRUE,  sep=',')
fona <- fona[,-1]
presvals <- extract(preds, fona)
set.seed(0)
backgr <- randomPoints(predictors, 300)



rfdat2 <- read.csv(file = "rfdat2_biorem.csv")
backgr <- randomPoints(rfdat2, 300)
absvals <- extract(predictors, backgr)

#with bio variables dropped based on VICor threshold
rf4 <- rfdat2 %>%
  mutate_at(c(4:11), funs(c(scale(.))))%>% 
  dplyr::select(-longitude, -latitude) %>% 
  mutate_if(is.character, as.factor)