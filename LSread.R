# working with Landsat TM/ETM+/OLI
# 2021-05-10
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# define paths
lstile_path = "/Users/mattolson/projects/Debris_snow_class/data/test/LanTiles/WRS2_descending_0/WRS2_descending.shp"
rgi_path = '/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp'

# base path for tiles
FolderTiles <- function(pathLSt, savepath){
  strt <- Sys.time()
  tile_list = list.files(pathLSt, full.names = T)
  for (t in tile_list){
    cat("\n> Starting", match(t,tile_list),"of",length(tile_list),"\n")
    PR = basename(t)
    LS7 #obtain from name
    LS8
    # classify all images
    FolderLS(t,PR, LS7 = FALSE, LS8 = TRUE, savepath)
    # create aggregate images
    stop()
    LSdebris_change()
    # change...
  }
}
# 1.) all class0
# 2.) glacier outline
# 3.) aggregate classification
# 4.) change in debris
# 5.) dhdt
# 6.) dem file



# for all images in tile
FolderLS <- function(folder_path, PR, LS7=FALSE, LS8=FALSE, savepath){
  strt <- Sys.time()
  image_list = list.files(folder_path, full.names = T)
  # mask to roi
  tile_roi = Ldt_tile(PR,lstile_path)
  for (im in image_list){
    cat("\n  ...classifying image", match(im,image_list),"of",length(image_list),"\n")
    # im = image_list[1]
    tile_name = paste0(PR,"_",gsub(paste0(".*",PR,"_(.+)_2.*"),'\\1',im))
    tmp_msks <<- file.path(savepath, PR)
    if(!file.exists(tmp_msks)){
      dir.create(tmp_msks)
    }
    # LSread
    landsat_bands = LSread0(im, LS8)
    # generate DEM
    # LSdem(tile_roi) # 8 mins
    # create masks & classification
    LSmask(landsat_bands,tile_roi, tile_name,TM=TRUE) # !TM=LS7 because simple read makes ETM/TM/OLI bands the same
    # output automatically saved
    unlink(tmpDir(), recursive=TRUE)
  }
  cat("\nClassification for tile",PR,"COMPLETE\n")
  print(Sys.time() -strt)
}

# Simple load for bands 1-5 only
LSread0 <- function(tile_path,tile_roi, LS8=FALSE){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  if(LS8){bds="2-6"}else{bds = "1-5"}
  band_select = grep(paste0("B[",bds,"]"),band_paths,value=TRUE)
  landsat_bands <- raster::brick(lapply(band_select, raster))
  # landsat_bands <- mask(landsat_bands,tile_roi)
  print(Sys.time() -strt)
  return(landsat_bands)
}

# Load tile (for Landsat TM)
LSreadTM <- function(tile_path, TM=TRUE, revertNA=TRUE, synchronise=TRUE){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[[:digit:]]",band_paths,value=TRUE)
  landsat_bands <- raster::brick(lapply(band_select, raster)) # faster | landsat_bands <- raster::stack(band_select)
  names(landsat_bands) = gsub(paste0(".*(B.+).TIF"),'\\1',band_select)
  # meta (for shadows?)
  metaData <<- RStoolbox::readMeta(band_paths[which(grepl("MTL",band_paths))])
  cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  # clean and synchronize
  if(revertNA){ # change all 0 values -> NA
    cat("\n synchronising... [estimated ~4.5 mins]\n")
    landsat_bands <- reclassify(landsat_bands, cbind(0, NA), right=FALSE) # ~1.6 mins
    # landsat_bands = mask(landsat_bands,landsat_bands!=0,maskvalue=0) # 3.9 mins
  }
  if(synchronise){
    landsat_bands <- synchroniseNA(landsat_bands) # 2.9 minslstk = synchroniseNA(landstk) # 2.9 mins
  }
  print(Sys.time() -strt)
  return(landsat_bands)
}

## NEW LOAD BASED ON SENSOR (NOT NEEDED - Only read in a few bands)
# storage hog 
LSread2 <- function(tile_path, LS7=FALSE, LS8=FALSE, revertNA=TRUE, synchronise=TRUE){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[[:digit:]]",band_paths,value=TRUE)
  # insert L07 & L08 code
  if (LS7){
    ls6band <- raster::raster(band_select[which(grepl("B6.*2",band_select))])
    landsat_bands <- raster::stack(band_select[which(grepl("B[1-5]",band_select))])
    cat("\n ...resampling ETM+ Thermal Band (6)\n")
    ls6band <- raster::resample(ls6band,landsat_bands[[1]])
    landsat_bands <- raster::brick(as.list(landsat_bands,ls6band))
    rm(ls6band)
    cat("Loading L07 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  }else{
    if(LS8){
      landsat_1 <- raster::stack(band_select[which(grepl("B[1]",band_select))])
      landsat_bands <- raster::stack(band_select[which(grepl("B[2-6]",band_select))])
      landsat_bands <- stack(landsat_1[[1]],landsat_bands,landsat_1[[2]])
      cat("Loading L08 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
    }else{
      # must be L05
      landsat_bands <- raster::stack(band_select[which(grepl("B[[:digit:]]",band_select))])
      cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
    }
    metaData <<- RStoolbox::readMeta(band_select[which(grepl("MTL",band_select))])
  }
  # clean and synchronize
  if(revertNA){ # change all 0 values -> NA
    cat("\n synchronising... [estimated ~4.5 mins]\n")
    landsat_bands <- reclassify(landsat_bands, cbind(0, NA), right=FALSE) # ~1.6 mins
    # landsat_bands = mask(landsat_bands,landsat_bands!=0,maskvalue=0) # 3.9 mins
  }
  if(synchronise){
    landsat_bands <- synchroniseNA(landsat_bands) # 2.9 minslstk = synchroniseNA(landstk) # 2.9 mins
  }
  print(Sys.time() -strt)
  return(landsat_bands)
}

# can weight all images by highest TIR (Thermal values) = better quality

# synchronize function
# https://stackoverflow.com/questions/23909929/synchronise-na-among-layers-of-a-raster-stack
synchroniseNA <- function(x){
  if(canProcessInMemory(x, n = 2))
  {
    val <- getValues(x)
    NA.pos <- unique(which(is.na(val), arr.ind = T)[, 1])
    val[NA.pos, ] <- NA
    x <- setValues(x, val)
    return(x)
  } else
  {
    x <- mask(x, calc(x, fun = sum))
    return(x)
  }
}

# download and resample SRTM DEM for tile (based on referenec raster)
LSdem <- function(ref_rast, tile_roi){
  # rewrite srtm code (possibly use NASAdem)
  if(file.path(tmp_msks,paste0(PR,"_dem.rds"))){
    print("dem exists!",q=F)
  }else{
    source('~/projects/Debris_snow_class/src/srtm_download1.R') # rewrite!
    dem0 <- get_srtm30(ref_rast, full_extent = TRUE, mask_to=TRUE) # call function
    dem = mask(crop(dem0,tile_roi),tile_roi)
    raster::writeRaster(dem,filename=file.path(tmp_msks,paste0(PR,"_dem.rds")), overwrite=TRUE)
    # return(dem)
  }
}

# call landsat tile shape
Ldt_tile <- function(PR,lstile_path,dest_crs="+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs",centroid_only=FALSE){
  Ldesc = rgdal::readOGR(lstile_path)
  tile_find = which(Ldesc$PATH == substr(PR,1,3) & 
                      Ldesc$ROW == as.character(as.numeric(substr(PR,4,6))))
  Ldt = Ldesc[tile_find,]
  if(as.character(crs(Ldt))!=dest_crs){
    Ldt = sp::spTransform(Ldt,crs(dest_crs))  
    # Ldt = fasterize::fasterize()
  }
  if(centroid_only){
    Ldt_centroid = coordinates(Ldt)
    Ldt = SpatialPoints(Ldt_centroid,ccrs(dest_crs))
  }
  return(Ldt)
}
  
# make composite
time_average <- function(class_stk, before=NULL,after=NULL, month_select=NULL,
                         by_month=TRUE,aggmethod = "mean"){
  # need: class_stk, logic dates (t1, t2), aggmethod = c("max","mean","modal")
  # return: monthly averages for time period
  #         annual aves. / fall aves
  # dates = c(as.Date(names(class_stk), "X%Y_%m_%d"))
  # DATES (FOR NEPAL)
  strt=Sys.time()
  if(PR==140041){
    dates = c(as.Date(names(class_stk)[1:4], "X%Y%m%d"),
              as.Date(names(class_stk)[5:82], "X%Y_%m_%d"))
  }else{
    dates = as.Date(names(class_stk), "X%Y_%m_%d")
  }
  
  yd = format(dates, "%Y");md = as.numeric(format(dates, "%m"))
  yix = match(yd,yd)
  # TIMEPERIOD
  if (is.null(after)){
    if (is.null(before)){
      stop("must specify time period: 'before' or 'after'")
    }else{tidx = which(as.numeric(yd) <= before)}
  }else{
    if (is.null(before)){tidx = which(as.numeric(yd) >= after)
    }else{tidx = which(as.numeric(yd) <= before & as.numeric(yd) >= after)} # both
  }
  # months
  if(!is.null(month_select)){
    date_months = format(dates[tidx], "%m")
    if(length(month_select)>3){stop("Too many months selected (3 allowed)")}
    if(length(month_select)>1){
      if(length(month_select)>2){
        tidx = tidx[which(date_months==month_select[1] | date_months==month_select[2] | date_months==month_select[3])]
      }else{
        tidx = tidx[which(date_months==month_select[1] | date_months==month_select[2])]
      }
    }else{
      tidx = tidx[which(date_months==month_select)]
    }
  }
  # !!! could also select based on julian day
  #
  # select subset
  t1stk = class_stk[[tidx]]
  md1 = md[tidx]
  lsm = unique(md1)
  lsmonth = sort(lsm)
  # # # # # # # # # # # # # # # # #
  # # correct (snow vs ice) ! ! ! ! ! ! ! ! ! ! ! (change or include earlier)
  # print("...rearranging values",quote=F)
  # t1stk[t1stk==2]=4;t1stk[t1stk==1]=2;t1stk[t1stk==4]=1
  # # # # # # # # # # # # # # # # #
  if(by_month){
    cat("\nmerging by month: (",month_select,")\n",as.character(dates[tidx]), '\n > length:',length(tidx),'\n')
    # Monthly aggregate
    t1mo = stackApply(t1stk, indices = md1, aggmethod)
    # reorder
    t1mo = t1mo[[match(lsmonth,lsm)]]
  }else{
    cat("\nmerging all dates | for months: (",month_select,")\n",as.character(dates[tidx]), '\nlength:',length(tidx),'\n')
    # Aggregate all
    t1mo = stackApply(t1stk,indices=1,aggmethod)
  }
  #
  # mean
  if(aggmethod=="mean"){
    if(max(values(class_stk0[[1]]),na.rm=T)>2){
      m <- c( 0,   1.5, 1,  # at least 50% required
              1.51, 2.5, 2, 
              2.51, 3,   3)
    }else{
      m <- c( 0,   1.5, 1,  # at least 50% required
              1.51, 2.5, 2, 
              2.51, 3,   3)
    }
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    t1mo <- reclassify(t1mo, rclmat)
  }
  print(Sys.time() - strt)
  return(t1mo)
}

LSdebris_change <- function(class_path){
  
  class_Nstk
  # best to select dates from folder first
  
  # compare
  t1merge = time_average(class_Nstk, before=1995, aggmethod = "mean",
                         month_select = c("08","09"), by_month = FALSE)
  t2merge = time_average(class_Nstk, before=2002, after=1998, aggmethod = "mean",
                         month_select = c("08","09"), by_month = FALSE)
  t3merge = time_average(class_Nstk, after=2005, aggmethod = "mean",
                         month_select = c("08","09"), by_month = FALSE)
  # plot
  dm1 = t1merge-(t2merge*4)
  dm2 = t1merge-(t3merge*4)
  plot_change(dm1,simple=TRUE)
  plot_change(dm2,simple=TRUE)
}




####################################################################################
## FUNCTION 
# plot difference
plot_change <- function(t1, t2, n_class = c(2,3), savename=NULL, simple=FALSE,...){
  # 
  #
  # create difference map
  dmap = t1-(t2*4)
  # levels(as.factor(values(dmap)))
  # type
  if(n_class==2){ # "-7" "-6" "-3" "-2" (t1(1,2) - t2(4,8))
    # -7: 1-8 (pisc->debris)
    # -6: 2-8 (debris)
    # -3: 1-4 (pisc)
    # -2: 2-4 (debris->pisc)
    chlabels = rev(c(bquote(Delta~italic(pisc)%->%italic(debris)), # gain debris
                     bquote(italic(persistent~debris)), # no change
                     bquote(italic(persistent~pisc)), # no change
                     bquote(Delta~italic(debris)%->%italic(pisc)))) # lose debris
    # colors/labels
    if(simple){
      new_labs = c("Pisc","Debris","Pisc gain","Debris gain")
    }else{new_labs = c(chlabels[2],chlabels[3],chlabels[1],chlabels[4])}
    # new_col = c("#F0027F","#386CB0","#BF5B17", "greenyellow")
    # lab_col = c("#386CB0", "#BF5B17", "#F0027F", "greenyellow")
    new_col = c("cyan","#386CB0","#BF5B17", "violet")
    lab_col = c("#386CB0", "#BF5B17", "cyan", "violet")
  }else{
    ## 3-class
    # presets
    chlabels = c(bquote(Delta~italic(ice)%->%italic(debris)), # debris is growing
                 bquote(Delta~italic(snow)%->%italic(debris)), # debris rapidly growing (or incorrect)
                 bquote(italic(persistent~debris)), # no change
                 bquote(Delta~italic(ice)%->%italic(snow)), # snow cover growing
                 bquote(italic(persistent~snow)), # no change
                 bquote(Delta~italic(debris)%->%italic(snow)), # ?
                 bquote(italic(persistent~ice)), # no change
                 bquote(Delta~italic(snow)%->%italic(ice)), # less snow
                 bquote(Delta~italic(debris)%->%italic(ice))) # less debris?
    # reorder
    if(simple){
      new_labs = c("debris-cover","ice","snow","growing debris","shrinking debris","shrinking snow","growing snow")
      new_col = c("black","white","#BF5B17","#F0027F","#BEAED4","white","#386CB0","cyan","greenyellow")
      lab_col = c("#BF5B17", "#BEAED4", "#386CB0", "black", "greenyellow","#F0027F","cyan")
    }else{
      # new_labs = c(chlabels[3],chlabels[7],chlabels[5],chlabels[1],chlabels[2],chlabels[6],chlabels[9],chlabels[4],chlabels[8])
      # new_col = c("black","#666666","#BF5B17","#F0027F","#386CB0","#FFFF99","#BEAED4","cyan","greenyellow")
      # # lab_col = c("#BF5B17", "#BEAED4", "#386CB0", "black", "#666666","#FFFF99","#7FC97F","#F0027F","cyan")
      # lab_col = c("#BF5B17", "#BEAED4", "#386CB0", "black", "#666666","#FFFF99","greenyellow","#F0027F","cyan")
      # 
      new_labs = c(chlabels[3],chlabels[5],chlabels[7],chlabels[1],chlabels[2],chlabels[6],chlabels[9],chlabels[4],chlabels[8])
      new_col = c("black","#666666","#BF5B17","#F0027F","#BEAED4","#FFFF99","#386CB0","cyan","greenyellow")
      # lab_col = c("#BF5B17", "#BEAED4", "#386CB0", "black", "#666666","#FFFF99","#7FC97F","#F0027F","cyan")
      lab_col = c("#BF5B17", "#386CB0", "#BEAED4", "black", "#666666","#FFFF99","greenyellow","#F0027F","cyan")
      
    }
  }
  # title
  if(!exists("main_title")){
    main_title = "Glacier surface change Aug-Oct  [1991-96 vs. 2008-11]"
  }
  if(!is.null(savename)){
    png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_figs/",savename,".png"),width=11.5,height=9,unit='in',res=300)
    plot(dmap, col=new_col, legend=F, main=main_title)
    legend("topright",fill=lab_col,cex=0.9,
           legend=sapply(new_labs, as.expression))
    legend("bottomright","(Landsat TM)",bty='n',cex=0.8)
    dev.off()
  }else{
    plot(dmap, col=new_col, legend=F, main=main_title)
    legend("topright",fill=lab_col,cex=0.9,
           legend=sapply(new_labs, as.expression))
    legend("bottomright","(Landsat TM)",bty='n',cex=0.8)
    
  }
  if(TRUE){
    if(n_class==3){stop("not written")}
    dmap2 = dmap
    m <- c( -6.5, -5.5, 1,  #Debris
            -3.5, -2.5, 2, #PISC
            -2.5, -1.5, 3, # PISC Growth
            -7.5, -6.5, 4) # Debris Growth
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    dmap2 <- reclassify(dmap2, rclmat)
    # -7: 1-8 (pisc->debris)
    # -6: 2-8 (debris)
    # -3: 1-4 (pisc)
    # -2: 2-4 (debris->pisc)
  }
  return(dmap2)
}

