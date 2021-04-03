# read in Landsat data
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # TEST
# other data (replace with function)
demL  <- raster::raster("/Users/mattolson/projects/Debris_snow_class/data/test/dem/srtm_148035.tif")
glac <- rgdal::readOGR("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
sf_glaciers <- sf::st_read("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
#
# test load
PR = 148035
folder_path = "/Users/mattolson/data/Landsat/148035/L05_148035_1980_2011_C1"
tile_list = list.files(folder_path, full.names = T)
tile_path = tile_list[1]
# # #

# filter T1 data
FolderLS <- function(folder_path){
  tile_list = list.files(folder_path, full.names = T)
  # LSread
  
  # save output
}

# Load tile (set for L05)
LSread <- function(tile_path, TM=TRUE){
  # load bands and mtl
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[[:digit:]]",band_paths,value=TRUE)
  landsat_bands <- raster::brick(lapply(band_select, raster))
  names(landsat_bands) = gsub(paste0(".*(B.+).TIF"),'\\1',band_select)
  # meta (for shadows?)
  metaData <<- RStoolbox::readMeta(band_paths[which(grepl("MTL",band_paths))])
  cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  return(landsat_bands)
}

# NEW LOAD BASED ON SENSOR
if (LS7){
  ls6band <- raster::raster(current_tile[which(grepl("B6.*2",current_tile))])
  landsat_bands <- raster::stack(current_tile[which(grepl("B[1-5]",current_tile))])
  ls6band <- raster::resample(ls6band,landsat_bands[[1]])
  landsat_bands <- stack(landsat_bands,ls6band)
  cat("Loading L07 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  }else{
  if(LS8){
    landsat_1 <- raster::stack(current_tile[which(grepl("B[1]",current_tile))])
    landsat_bands <- raster::stack(current_tile[which(grepl("B[2-6]",current_tile))])
    landsat_bands <- stack(landsat_1[[1]],landsat_bands,landsat_1[[2]])
    cat("Loading L08 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  }else{
    # must be L05
    landsat_bands <- raster::stack(current_tile[which(grepl("B[[:digit:]]",current_tile))])
    cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
    
  }
metaData <<- RStoolbox::readMeta(current_tile[which(grepl("MTL",current_tile))])
}

# can weight all images by highest TIR (Thermal values) = better quality


landsat_bands = LSread(tile_path)


