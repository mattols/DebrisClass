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
  ratio_mask <- ratioMethod(landsat_bands, threshold = 1.5, NIR_band, SWIR_band)
  # mask RGI
  glacier_mask <- fasterize::fasterize(sf_glaciers,landsat_bands[[1]])
  # shadow mask
  shadow_mask <- makeShade()
  # cloud mask 
  cloud_mask = cloudScore()
}

# use overlay and save with filename option
ratioMethod <- function(landsat_bands, threshold, NIR_band, SWIR_band){
  # ratio method after Paul et al., 2004 and Hall 1998?
  raster::overlay(landsat_bands[[c(NIR_band,SWIR_band)]], fun= function(x,y) x/y > threshold,
                          filename=paste0(tmp_msks,"/ratio.grd"))
}

cloudScore <- function(){
  # after Housman 2018
  
}

makeShade <- function(){
  # after 
  m <- c(0,shade_threshold,1,shade_threshold, 255, NA)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rc <- raster::reclassify(, rclmat) # NIR_band
  # inverse VALUES!
  # shadow_mask <- raster::calc(sum(rc,na.rm=T), function(x){x[x>0]<-1;x[x==0]<-NA; return(x)})
  raster::calc(calc(landsat_bands[[blue_band:NIR_band]],na.rm=T), function(x){x[x>0]<-NA;x[x==0]<-1; return(x)})
  
}


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