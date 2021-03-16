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
FolderLS <- function(){
  
}

# Load tile (set for L05)
LSread <- function(tile_path){
  # load bands and mtl
  band_paths = list.files(tile_path, full.names = T)
  band_select = grepl("B[[:digit:]]",band_paths,value=TRUE)
  landsat_bands <- raster::stack(band_select)
  names(landsat_bands) = gsub(paste0(".*(B.+).TIF"),'\\1',band_select)
  # meta (for shadows?)
  metaData <<- RStoolbox::readMeta(band_paths[which(grepl("MTL",band_paths))])
  cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  return(landsat_bands)
}

# can weight all images by highest TIR (Thermal values) = better quality

