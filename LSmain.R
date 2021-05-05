# Landsat debris change classification
# 2020-05-04
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# package req. "raster" "sp" "rgdal" "sf" "RGISTools" "fasterize"

# # # TEST
# other data (replace with function)
demL  <- raster::raster("/Users/mattolson/projects/Debris_snow_class/data/test/dem/srtm_148035.tif")
glac <- rgdal::readOGR("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
sf_glaciers <- sf::st_read("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
#
# test 1 scene
PR = 148035
folder_path = "/Users/mattolson/data/Landsat/148035/L05_148035_1980_2011_C1"
tile_list = list.files(folder_path, full.names = T)
tile_path = tile_list[1]
# # #

# call all (create function to call all)
source('~/projects/Debris_snow_class/src/srtm_download1.R') # rewrite!
source('~/src/DebrisClass/LSread.R')
source('~/src/DebrisClass/LSmask.R')

##### single scene
dirpath
landsat_bands = LSread(tile_path)

####



# save path

# landsat scenes



