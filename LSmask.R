# applying masks for debris cover
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

LSmask <- function(landsat_bands, NIR_band=4, SWIR_band=5){
  # mask RATIO
  ratio_mask = landsat_bands[[NIR_band]]/landsat_bands[[SWIR_band]]
  ratio_mask <- raster::calc(ratio_mask, function(x){x[x<=selected_masks["ratio"]]<-NA;x[!is.na(x)]<-1; return(x)})
  # mask RGI
  glacier_mask <- fasterize::fasterize(sf_glaciers,landsat_bands[[1]])
  # shadow mask
  shadow_mask <- makeShade()
  # cloud mask 
  cloud_mask = cloudScore()
}

cloudScore <- function(){
  # after Housman 2018
  
}

makeShade <- function(){
  # after 
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
LSmask <- function(landsat_bands, NIR_band=4, SWIR_band=5){
  # mask RATIO
  ratio_mask = landsat_bands[[NIR_band]]/landsat_bands[[SWIR_band]]
  ratio_mask <- raster::calc(ratio_mask, function(x){x[x<=selected_masks["ratio"]]<-NA;x[!is.na(x)]<-1; return(x)})
  # mask RGI
  glacier_mask <- fasterize::fasterize(sf_glaciers,landsat_bands[[1]])
  # shadow mask
  shadow_mask <- makeShade()
  # cloud mask 
  cloud_mask = cloudScore()
}

cloudScore <- function(){
  # after Housman 2018
  
}

makeShade <- function(){
  
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

