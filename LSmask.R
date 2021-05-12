# Simple method for classifying supra-glacial debris
# 2021-05-10
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dplyr)

# apply all masks
LSmask <- function(landsat_bands, tile_roi, tile_name, TM=TRUE){
  # distinguish bands TM/OLI
  if(TRUE){
    # cat("\n...creating masks for TM/ETM+ imagery \n") # ls_band_designations("TM")
    blue_band = 1; green_band = 2; red_band = 3;NIR_band = 4; SWIR_band = 5; TH_band = 6
  }else{
    # cat("...creating masks for OLI imagery ") # ls_band_designations("OLI")
    blue_band = 2; green_band = 3; red_band = 4; NIR_band = 5; SWIR_band = 6; TH_band = 7  #(TH is band 10 but in 7)
  }
  # mask RATIO
  ratioMethod(landsat_bands, threshold = 1.5, NIR_band, SWIR_band, tile_roi)
  # mask RGI
  glacierPolyR(landsat_bands,tile_name, tile_roi)
  # makeShade()
  # cloudScore()
  simpleClass(landsat_bands, green_band,tile_name, tile_roi,LS7=TM)
}

# ratio method after Paul et al., 2004 and Hall 1998?
ratioMethod <- function(landsat_bands, threshold, NIR_band, SWIR_band, tile_roi){
  raster::overlay(landsat_bands[[c(NIR_band,SWIR_band)]], fun= function(x,y) x/y > threshold) %>%
    raster::crop(.,tile_roi) %>%
    raster::mask(.,tile_roi) %>%
    saveRDS(.,file.path(save_folder,"ratio.rds"))
  cat("\n file 'ratio.grd' created\n")
}

# glacier mask from RGI 6.0
glacierPolyR <- function(r, tile_name, tile_roi){
  if(file.exists(file.path(save_folder,paste0(PR,"_gpoly.rds")))){
    # cat("\n.glacier polygon mask exists.\n")
    readRDS(file.path(save_folder,paste0(PR, "_gpoly.rds"))) %>%
      fasterize::fasterize(.,r[[1]]) %>%
      raster::crop(tile_roi) %>%
      raster::mask(., tile_roi) %>%
      saveRDS(., file.path(save_folder,"glac.rds"))
  }else{
    cat("\nTransforming RGI polygons for tile",PR,"\n")
    rgi1 = raster::shapefile(rgi_path) 
    rgi1 = rgi1 %>%
      spTransform(raster::crs(tile_roi)) %>%
      rgeos::gBuffer(byid=TRUE, width=0)
    rgi1 = rgi1 %>% raster::crop(buffer(tile_roi,10000)) %>%
      sf::st_as_sf()
    saveRDS(rgi1, file.path(save_folder,paste0(PR,"_gpoly.rds")))
    rgi1 %>%
      fasterize::fasterize(.,r[[1]]) %>%
      raster::crop(tile_roi) %>%
      raster::mask(., tile_roi) %>%
      saveRDS(., file.path(save_folder,"glac.rds"))
    rm(rgi1)
    cat("\n file 'gpoly.rds' created\n")
  }
}


cloudScore <- function(){
  # after Housman 2018
  cat("\n cloudScore() has not been created...\n")
  stop()
}

makeShade <- function(){
  # detect shadows (dark pixels) <10% (~25)
  # doesn't work!
  cat("\n makeShade() is not complete!...\n")
  stop()
  mx_name = paste0(save_folder,"/maxVIS.grd")
  calc(landsat_bands[[green_band:red_band]],max,na.rm=T,
             filename=mx_name, overwrite=TRUE)
  # mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0)
  raster::writeRaster(mask(raster(mx_name),raster(mx_name)!=0,maskvalue=0) >=25.5,
                      filename=paste0(save_folder,"/shade.grd"), overwrite=TRUE)
  # raster::writeRaster(raster::raster(mx_name) >=25.5,filename=paste0(save_folder,"/shade.grd"), overwrite=TRUE)
}

# simple classification
simpleClass <- function(r, green_band, tile_name, tile_roi,LS7){
  # strt <- Sys.time()
  pisc = readRDS(file.path(save_folder,"glac.rds"))==1 & readRDS(file.path(save_folder,"ratio.rds"))==1
  pisc[readRDS(file.path(save_folder,"glac.rds"))==1 & readRDS(file.path(save_folder,"ratio.rds"))!=1] = 2
  pisc <- reclassify(pisc, cbind(0, NA), right=FALSE)
  r = mask(crop(r[[green_band]], tile_roi),tile_roi)
  if(LS7){mnum=150}else{mnum=35400}
  pisc[pisc==2 & (r>mnum)]=NA
  # pisc[pisc==2 & (r>150)]=NA # correct for high cloud values
  # saveRDS(pisc, file.path(save_folder,paste0(tile_name,"_class0.rds"))) # not transferable between sessions
  raster::writeRaster(pisc,file.path(save_folder,paste0(tile_name,"_class0.grd")))
  # print(Sys.time() -strt)
  unlink(file.path(save_folder,"ratio.rds"))
  unlink(file.path(save_folder,"glac.rds"))
  cat("\n file 'class0.grd' created for",tile_name,"\n")
}

# NOT USED - classification with slope & elevation correcction
simpleClass0 <- function(){
  # terrain
  sslope = raster::terrain(dem,opt='slope',unit='degrees', neighbors=8)
  aasp = raster::terrain(dem,opt='aspect',unit='degrees', neighbors=8)
  pelv = dem*pisc
  pelv[pelv==0] = NA
  pstat = quantile(values(pelv),na.rm=T)[4]
  g = raster(paste0(save_folder,"/glac.grd"))
  r = raster(paste0(save_folder,"/ratio.grd"))
  pisc = raster(paste0(save_folder,"/glac.grd"))==1 & raster(paste0(save_folder,"/ratio.grd"))==1
  writeRaster(pisc,
              filename=paste0(save_folder,"/pisc.grd"))
  writeRaster(g==1 & r!=1 & sslope<=40 & pelv<pstat,
              filename=paste0(save_folder,"/debris.grd"))
  writeRaster(( (raster(paste0(save_folder,"/glac.grd"))==1) & (raster(paste0(save_folder,"/ratio.grd"))==1) ),
              filename=paste0(save_folder,"/pisc.grd"), overwrite=TRUE)
}

# NOT USED
simpleClass1 <- function(r, green_band, tile_name, tile_roi){
  # change class name to reflect date
  strt <- Sys.time()
  pisc = raster(paste0(save_folder,"/glac.grd"))==1 & raster(paste0(save_folder,"/ratio.grd"))==1
  pisc[raster(paste0(save_folder,"/glac.grd"))==1 & raster(paste0(save_folder,"/ratio.grd"))!=1] = 2
  cat("\n...revert 0 vals -> NA\n")
  pisc <- reclassify(pisc, cbind(0, NA), right=FALSE)
  r = mask(crop(r[[green_band]], tile_roi),tile_roi)
  pisc[pisc==2 & (r>150)]=NA # correct for high cloud values
  writeRaster(pisc,filename=file.path(save_folder,paste0(tile_name,"_class0.grd")))
  # # save image w/ slope and elv correction (PSTAT ERROR)
  # if(!exists(pstat)){
  #   sslope = raster::terrain(raster::raster(paste0(save_folder,"/dem.grd")),opt='slope',unit='degrees', neighbors=8)
  #   pstat = quantile(values(raster::raster(paste0(save_folder,"/dem.grd"))[pisc==1]),na.rm=T)[4]
  #   Error in h(simpleError(msg, call)) : 
  #     # error in evaluating the argument 'x' in selecting a method for function 'quantile': 
  #     # unable to find an inherited method for function ‘values’ for signature ‘"numeric"’
  # }
  # pisc[pisc==2 & (sslope<=40 | raster::raster(paste0(save_folder,"/dem.grd"))>pstat)]=1
  # writeRaster(pisc,paste0(save_folder,"/",tile_name,"_class1.grd"))
  cat("\n file 'class0.grd' created\n")
  print(Sys.time() -strt)
}



stop()
plot(raster(file.path(save_folder,"148035_20190901_class0.grd")),col=c("deepskyblue3","orange3"))
plot(raster(file.path(save_folder,"148035_20000803_class0.grd")),col=c("deepskyblue3","orange3"))

#
plotRGB(landsat_bands,3,2,1)
plot(raster(paste0(save_folder,"/class0.grd")),add=T,col=ggplot2::alpha(c(NA,'red'),0.5))


savename = "basic_class_wclouds2"
png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/",savename,".png"),width=12.5,height=9,unit='in',res=300)
plotRGB(landsat_bands,5,2,1)
plot(pisc,add=T,col=ggplot2::alpha(c("blue","red"),0.5))
dev.off()
# zoom

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


