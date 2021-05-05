# applying masks for debris cover
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

tmp_msks = paste0(tmpDir(), "masks")
dir.create(tmp_msks)

LSmask <- function(landsat_bands, TM=TRUE){
  # define thresholds
  
  # distinguish bands TM/OLI
  if(TM){
    cat("\n...creating masks for TM/ETM+ imagery \n") # ls_band_designations("TM")
    blue_band = 1; green_band = 2; red_band = 3;NIR_band = 4; SWIR_band = 5; TH_band = 6
  }else{
    cat("...creating masks for OLI imagery ") # ls_band_designations("OLI")
    blue_band = 2; green_band = 3; red_band = 4
    NIR_band = 5; SWIR_band = 6; TH_band = 7  #(TH is band 10 but in 7)
  }
  # mask RATIO
  ratioMethod(landsat_bands, threshold = 1.5, NIR_band, SWIR_band)
  # mask RGI
  glacierPolyR(landsat_bands)
  # shadow mask
  # makeShade()
  # cloud mask 
  # cloudScore()
  # classify
  simpleClass(landsat_bands)
}

# use overlay and save with filename option
ratioMethod <- function(landsat_bands, threshold, NIR_band, SWIR_band){
  # ratio method after Paul et al., 2004 and Hall 1998?
  raster::overlay(landsat_bands[[c(NIR_band,SWIR_band)]], fun= function(x,y) x/y > threshold,
                          filename=paste0(tmp_msks,"/ratio.grd"))
  cat("\n file 'ratio.grd' created\n")
}

# create glacier mask
glacierPolyR <- function(r){
  # r is raster object 
  # rgeos::gIsValid(sf::st_read("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp"))) # self-intersection
  library(dplyr)
  strt <- Sys.time()
  raster::shapefile('/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp') %>%
    spTransform(raster::crs(r)) %>%
    rgeos::gBuffer(byid=TRUE, width=0) %>%
    raster::crop(r) %>%
    sf::st_as_sf() %>%
    fasterize::fasterize(.,r[[1]]) %>%
    raster::writeRaster(.,filename=paste0(tmp_msks,"/glac.grd"), overwrite=TRUE)
  print(Sys.time() -strt)
  cat("\n file 'glac.grd' created\n")
}

# later try to figure out a good way to determine shadow/cloud


cloudScore <- function(){
  # after Housman 2018
  cat("\n cloudScore() has not been created...\n")
}

makeShade <- function(){
  # detect shadows (dark pixels) <10% (~25)
  # doesn't work great
  cat("\n makShade() is not complete!...\n")
  mx_name = paste0(tmp_msks,"/maxVIS.grd")
  calc(landsat_bands[[green_band:red_band]],max,na.rm=T,
             filename=mx_name, overwrite=TRUE)
  # mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0)
  raster::writeRaster(mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0) >=25.5,
                      filename=paste0(tmp_msks,"/shade.grd"), overwrite=TRUE)
  
  # raster::writeRaster(raster::raster(mx_name) >=25.5,
  #              filename=paste0(tmp_msks,"/shade.grd"), overwrite=TRUE)
}



# r2 = mask(landsat_bands[[2]],landsat_bands[[2]]!=0,maskvalue=0)
dem0 <- get_srtm30(lstk[[2]], full_extent = TRUE, mask_to=TRUE) # call function
dem = mask(crop(demL,landsat_bands),landsat_bands)
sslope = raster::terrain(demL,opt='slope',unit='degrees', neighbors=8)
aasp = raster::terrain(demL,opt='aspect',unit='degrees', neighbors=8)
#
pelv = demL*pisc
pelv[pelv==0] = NA
pstat = quantile(values(pelv),na.rm=T)[4]
#

simpleClass0 <- function(){
  g = raster(paste0(tmp_msks,"/glac.grd"))
  r = raster(paste0(tmp_msks,"/ratio.grd"))
  pisc = raster(paste0(tmp_msks,"/glac.grd"))==1 & raster(paste0(tmp_msks,"/ratio.grd"))==1
  writeRaster(pisc,
              filename=paste0(tmp_msks,"/pisc.grd"))
  writeRaster(g==1 & r!=1 & sslope<=40 & pelv<pstat,
              filename=paste0(tmp_msks,"/debris.grd"))
  writeRaster(( (raster(paste0(tmp_msks,"/glac.grd"))==1) & (raster(paste0(tmp_msks,"/ratio.grd"))==1) ),
              filename=paste0(tmp_msks,"/pisc.grd"), overwrite=TRUE)
}

simpleClass <- function(r){
  # change class name to reflect date
  strt <- Sys.time()
  pisc = raster(paste0(tmp_msks,"/glac.grd"))==1 & raster(paste0(tmp_msks,"/ratio.grd"))==1
  pisc[raster(paste0(tmp_msks,"/glac.grd"))==1 & raster(paste0(tmp_msks,"/ratio.grd"))!=1] = 2
  # correct for high cloud values
  cat("\nmust correct for cloudiness\n")
  writeRaster(pisc,filename=paste0(tmp_msks,"/class0.grd"), overwrite=TRUE)
  print(Sys.time() -strt)
  cat("\n file 'class0.grd' created\n")
}

# TO DO!
# 1. get rid of 0 vals in class0.grd
# 2. why is overlap strange?
# 3. observe specral differenceces and set cloud correction!
# 4. must also correct for some shadow!


plot(raster(paste0(tmp_msks,"/class0.grd")))
#
plotRGB(landsat_bands,3,2,1)
plot(raster(paste0(tmp_msks,"/class0.grd")),add=T,col=ggplot2::alpha(c(NA,'red'),0.5))









# OLD SRTM CODE!
## add a get DEM and get shp options (FUNCTION)

print(paste("...retrieving", t,"of",length(tile_names), "SRTM files."))
temp <- tempfile(pattern = tile_names[t], fileext = ".zip")
GET(srtm_list[t],authenticate("matthew.olson@geog.utah.edu", "Televator9", type = "basic"),
    write_disk(temp))
# save to tempdir
out <- unzip(temp, exdir = tempdir())
if(loadAll_tiles){assign(tile_names[t],raster(out))}
file.remove(temp) # remove .zip

# merge tiles
files = list.files(tempdir(),full.names = T,pattern = ".hgt")
rl <- lapply(files, raster)
print("Generating mosaic...")
rmosaic <- do.call(merge, c(rl, tolerance = 1))
rmosaic <- mask(crop(rmosaic,shape),shape)

if(del_files){files = list.files(tempdir(), full.names = T,pattern="^N");file.remove(files)}








# PLOTS
plot(raster(mx_name), breaks=c(0,25,50,100,150,200,255), col=terrain.colors(6))
plot(raster(mx_name) > 20)
#
plotRGB(landsat_bands,3,2,1)
plot(raster(paste0(tmp_msks,"/shade.grd")),add=T,col=ggplot2::alpha(c('red',NA),0.5))







# observe some points
# xym = as.matrix(data.frame(x = c(76.33396,76.38849,76.40904,76.20908,76.20117),
#                            y= c(36.21360,36.16503,36.15800,36.20081,36.04552)))
# pts = xym
nfeat= 5
pts = click(n=nfeat)
em = extract(landsat_bands,pts) # columns represent spectral bands | rows represent end member class
rownames(em) <- paste0("feature", 1:nfeat)
colnames(em) <- paste0("band",c(1:nfeat,nlayers(landsat_bands)))
head(em)
#
library(ggplot2)
require(reshape2)
require(dplyr)
melt(em) %>% mutate(bn = as.numeric(substr(Var2,5,5))) %>% 
  ggplot(aes(x=bn,y=value,colour=Var1)) + xlab("Landsat band") +
  ylab("Radiance (DN)") +
  geom_point() + geom_line()