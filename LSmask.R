# applying masks for debris cover
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dplyr)
# tmp_msks = paste0(tmpDir(), "masks")
# dir.create(tmp_msks)

LSmask <- function(landsat_bands, tile_roi, tile_name, TM=TRUE){
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
  ratioMethod(landsat_bands, threshold = 1.5, NIR_band, SWIR_band, tile_roi)
  # mask RGI
  glacierPolyR(landsat_bands,tile_name, tile_roi)
  # shadow mask
  # makeShade()
  # cloud mask 
  # cloudScore()
  # classify
  simpleClass(landsat_bands, green_band,tile_name)
  unlink(paste0(tmp_msks,"/ratio.grd"))
}

# use overlay and save with filename option
ratioMethod <- function(landsat_bands, threshold, NIR_band, SWIR_band, tile_roi){
  # ratio method after Paul et al., 2004 and Hall 1998?
  raster::overlay(landsat_bands[[c(NIR_band,SWIR_band)]], fun= function(x,y) x/y > threshold) %>%
  raster::mask(., tile_roi) %>%
    raster::writeRaster(.,paste0(tmp_msks,"/ratio.grd"),overwrite=TRUE)
  # raster::overlay(landsat_bands[[c(NIR_band,SWIR_band)]], fun= function(x,y) x/y > threshold,
  #                 filename=paste0(tmp_msks,"/ratio.grd"), overwrite=TRUE)
  cat("\n file 'ratio.grd' created\n")
}

# create glacier mask
glacierPolyR <- function(r, tile_name, tile_roi){
  # r is raster object 
  # rgeos::gIsValid(sf::st_read("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp"))) # self-intersection
  # if(file.exists(paste0(tmp_msks,"/",tile_name,"_glac.grd"))){
  if(file.exists(file.path(tmp_msks,PR,"_gpoly.rds"))){
    print("glacier mask exists",q=F)
    read_rds(path = file.path(tmp_msks,PR, "gpoly.rds")) %>%
      raster::crop(r) %>%
      fasterize::fasterize(.,r[[1]]) %>%
      raster::mask(., tile_roi) %>%
      raster::writeRaster(.,paste0(tmp_msks,"/glac.grd"),overwrite=TRUE)
  }else{
    strt <- Sys.time()
    cat("\nTransforming RGI polygons\n")
    raster::shapefile('/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp') %>%
      spTransform(raster::crs(tile_roi)) %>%
      rgeos::gBuffer(byid=TRUE, width=0) %>%
      raster::crop(buffer(tile_roi,20000)) %>%
      sf::st_as_sf() %>%
      write_rds(., path = file.path(tmp_msks,PR,"_gpoly.rds"))
    print(Sys.time() -strt)
    cat("\n file 'gpoly.rds' created\n")
    
  }
}

# create glacier mask
glacierPolyR0 <- function(r,tile_name, tile_roi){
  # r is raster object 
  # rgeos::gIsValid(sf::st_read("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp"))) # self-intersection
  # if(file.exists(paste0(tmp_msks,"/",tile_name,"_glac.grd"))){
  if(file.exists(paste0(tmp_msks,"/glac0.grd"))){
    print("glacier mask exists",q=F)
  }else{
    strt <- Sys.time()
    cat("\nTransforming RGI polygons\n")
    raster::shapefile('/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp') %>%
      spTransform(raster::crs(r)) %>%
      rgeos::gBuffer(byid=TRUE, width=0) %>%
      raster::crop(r) %>%
      sf::st_as_sf() %>%
      fasterize::fasterize(.,r[[1]]) %>%
      raster::mask(., tile_roi) %>%
      raster::writeRaster(.,paste0(tmp_msks,"/glac.grd"),overwrite=TRUE)
    print(Sys.time() -strt)
    cat("\n file 'glac.grd' created\n")
    
  }
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
  stop()
  mx_name = paste0(tmp_msks,"/maxVIS.grd")
  calc(landsat_bands[[green_band:red_band]],max,na.rm=T,
             filename=mx_name, overwrite=TRUE)
  # mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0)
  raster::writeRaster(mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0) >=25.5,
                      filename=paste0(tmp_msks,"/shade.grd"), overwrite=TRUE)
  
  # raster::writeRaster(raster::raster(mx_name) >=25.5,
  #              filename=paste0(tmp_msks,"/shade.grd"), overwrite=TRUE)
}


# classificcation with slope & elevation correcction
simpleClass0 <- function(){
  # terrain
  sslope = raster::terrain(dem,opt='slope',unit='degrees', neighbors=8)
  aasp = raster::terrain(dem,opt='aspect',unit='degrees', neighbors=8)
  pelv = dem*pisc
  pelv[pelv==0] = NA
  pstat = quantile(values(pelv),na.rm=T)[4]
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

simpleClass <- function(r, green_band, tile_name){
  # change class name to reflect date
  strt <- Sys.time()
  pisc = raster(paste0(tmp_msks,"/glac.grd"))==1 & raster(paste0(tmp_msks,"/ratio.grd"))==1
  pisc[raster(paste0(tmp_msks,"/glac.grd"))==1 & raster(paste0(tmp_msks,"/ratio.grd"))!=1] = 2
  cat("\n...revert 0 vals -> NA\n")
  pisc <- reclassify(pisc, cbind(0, NA), right=FALSE)
  pisc[pisc==2 & (r[[green_band]]>150)]=NA # correct for high cloud values
  writeRaster(pisc,filename=paste0(tmp_msks,"/",tile_name,"_class0.grd"), overwrite=TRUE)
  # # save image w/ slope and elv correction (PSTAT ERROR)
  # if(!exists(pstat)){
  #   sslope = raster::terrain(raster::raster(paste0(tmp_msks,"/dem.grd")),opt='slope',unit='degrees', neighbors=8)
  #   pstat = quantile(values(raster::raster(paste0(tmp_msks,"/dem.grd"))[pisc==1]),na.rm=T)[4]
  #   Error in h(simpleError(msg, call)) : 
  #     # error in evaluating the argument 'x' in selecting a method for function 'quantile': 
  #     # unable to find an inherited method for function ‘values’ for signature ‘"numeric"’
  # }
  # pisc[pisc==2 & (sslope<=40 | raster::raster(paste0(tmp_msks,"/dem.grd"))>pstat)]=1
  # writeRaster(pisc,paste0(tmp_msks,"/",tile_name,"_class1.grd"))
  print(Sys.time() -strt)
  cat("\n file 'class0.grd' created\n")
  # cat("\n file 'class0.grd' & 'class1.grd' created\n")
}

# TO DO!
# 2. why is overlap strange?
# 3. observe specral differenceces and set cloud correction!
# 4. must also correct for some shadow!

stop()
plot(raster(paste0(tmp_msks,"/class0.grd")),col=c("deepskyblue3","orange3"))
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


temp <- tempfile(pattern = paste0(tile_name,"_lbands_"), fileext = ".grd")





# # PLOTS
# plot(raster(mx_name), breaks=c(0,25,50,100,150,200,255), col=terrain.colors(6))
# plot(raster(mx_name) > 20)
# #
# plotRGB(landsat_bands,3,2,1)
# plot(raster(paste0(tmp_msks,"/shade.grd")),add=T,col=ggplot2::alpha(c('red',NA),0.5))
# 
# 





# observe some points
# xym = as.matrix(data.frame(x = c(76.33396,76.38849,76.40904,76.20908,76.20117),
#                            y= c(36.21360,36.16503,36.15800,36.20081,36.04552)))
# pts = xym
nfeat= 6
pts = click(n=nfeat)
em = extract(landsat_bands,pts) # columns represent spectral bands | rows represent end member class
rownames(em) <- paste0("feature", 1:nfeat)
colnames(em) <- paste0("band",1:nlayers(landsat_bands))
# colnames(em) <- paste0("band",c(1:nfeat,nlayers(landsat_bands)))
head(em)
#
library(ggplot2)
require(reshape2)
require(dplyr)
melt(em) %>% mutate(bn = as.numeric(substr(Var2,5,5))) %>% 
  ggplot(aes(x=bn,y=value,colour=Var1)) + xlab("Landsat band") +
  ylab("Radiance (DN)") +
  geom_point() + geom_line()

df0 = as.data.frame(landsat_bands)


