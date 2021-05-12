# Landsat debris-cover change classification
# 2021-05-10
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# package req. "raster" "sp" "rgdal" "sf" "RGISTools" "fasterize"

# call functions
source('~/src/DebrisClass/LSread.R')
source('~/src/DebrisClass/LSmask.R')

# single scene test
# multiple scenes
PR = 148035
folder_path = "/Users/mattolson/data/Landsat/148035/L07_148035_19992002_AugOct"
FolderLS(folder_path,PR, LS7 = TRUE, savepath="~/data/Landsat/results") # works
folder_path = "/Users/mattolson/data/Landsat/148035/L08_148035_20162020_AugOct/"
FolderLS(folder_path,PR, LS7 = FALSE, LS8 = TRUE, savepath="~/data/Landsat/results")
#
PR = 140041
folder_path = "/Users/mattolson/data/Landsat/140041/L07_140041_19992002_AugOct"
FolderLS(folder_path,PR, LS7 = TRUE, savepath="~/data/Landsat/results") # works
folder_path = "/Users/mattolson/data/Landsat/140041/L08_140041_20162020_AugOct"
FolderLS(folder_path,PR, LS7 = FALSE, LS8 = TRUE, savepath="~/data/Landsat/results")

# for all tiles
landpath = "/Users/mattolson/data/Landsat"
savepath = "~/data/Landsat/results"
FolderTiles(landpath, savepath)

# success!
# 0.) add aggregate function (time period) ->save
# 1.) add change function for tile ->save
# 2.) obtain DEM for change ->save
# 3.) extract dhdt for tile ->save

# read in and unlink landsat_bands when done!

# clear all data
unlink(tmp_msks, recursive=TRUE)
unlink(tmpDir(), recursive=TRUE)
rm(list=ls())
gc()

# get rid of all but one (llandsat)
tmp_ls = list.files(tmpDir(),full.names = TRUE)
fd = paste0(strsplit(basename(filename(landsat_bands)),"\\.")[[1]][1],".*")
del_ls = tmp_ls[!grepl(fd,tmp_ls)]
unlink(del_ls)


# other data (replace with function)
demL  <- raster::raster("/Users/mattolson/projects/Debris_snow_class/data/test/dem/srtm_148035.tif")
glac <- rgdal::readOGR("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
sf_glaciers <- sf::st_read("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")

