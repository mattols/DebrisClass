
source('~/projects/Debris_snow_class/src/srtm_download1.R')
d3 <- get_srtm30(ch2, full_extent = TRUE, mask_to=TRUE)

dhdt = raster("/Users/mattolson/data/Shean/shean_hma_glacier_ASTER_WV_2000-2018_dhdt.tif")

plotRGB(ls2,3,2,1)
plot(dcn, col=ggplot2::alpha(lab_col,0.5), add=T,legend=F)

# crop
ls2 = crop(landsat_bands, class_stk0)
dcn = crop(dchange2, class_stk0)

ls3 = projectRaster(ls2, crs=crs(dhdt))
dcn3 = projectRaster(dcn, crs=crs(dhdt),method = "ngb")

gg = readRDS(file.path(save_folder,"148035_gpoly.rds"))
gg3 = sf::st_transform(gg,crs=crs(dhdt))
gg3 = sf::st_crop(gg3,dcn3)

dh3 = crop(dhdt,dcn3)
dh3[(dh3<0.2)&(dh3>-0.2)] = NA

png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/HiMat_Olson.png"),width=11.5,height=9,unit='in',res=300)
plotRGB(ls3,3,2,1)
plot(dcn3, col=ggplot2::alpha(lab_col,0.6), add=T,legend=F)
# contour(dh3, levels = seq(-17,27,1),labcex=1,add=T)

# legend("topright",fill=lab_col,cex=0.9,legend=sapply(new_labs, as.expression))
dev.off()


g1dem = mask(crop(projectRaster(d3,g1dh,method = 'ngb'),gg3[37,]),gg3[37,])
d1dem = projectRaster(g1dem,g1dh,method = 'ngb')

# g1dem = (g1dem - min(getValues(g1dem),na.rm=T)) / (max(getValues(g1dem),na.rm=T) - min(getValues(g1dem),na.rm=T))
plot(g1dem)
g1dh = mask(crop(dh3,gg3[37,]),gg3[37,])
g1ch = mask(crop(projectRaster(dcn3,g1dh,method = 'ngb'),gg3[37,]),gg3[37,])

st0 = stack(g1dem, g1dh,g1ch)
df0 = as.data.frame(st0)
names(df0) = c("e","dh","ch")
df0 = df0[complete.cases(df0),]

plot(smooth.spline(df0$dh~df0$e),type="l",lwd=2,col='firebrick',ylab="dh/dt (m a-1)",xlab='Elevation (m)')

new_labs = c("Snow/ice","Debris","Debris gain", "Snow/ice gain")
lab_col = c("#386CB0", "#BF5B17", "violet","cyan")
lab_col2 = c("#BF5B17", "violet",  "#386CB0", "cyan")


png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/HiMat_Olson_dh2.png"),height=11.5,width=5,unit='in',res=300)
df0 %>% mutate(ch = recode(ch, "1" = "Snow/ice", "2" = "Debris", "3" = "Debris gain", "4" = "Snow/ice gain")) %>%
  mutate(surface_change = as.factor(ch)) %>%
  ggplot(aes(x=e,y=dh,colour=surface_change)) + geom_smooth() + theme_bw() + 
  geom_hline(yintercept = 0, "-")  + facet_wrap(~surface_change, ncol=1) +#facet_grid(.~ch) + 
  theme(strip.text.x = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none") +
  scale_color_manual(values=lab_col2) + ylab(bquote("dh/dt meters"~a^-1~"2000-2018")) +
  xlab("Elevation (m)")
dev.off()

# plot area and change


ls2 = crop(landsat_bands, class_stk0N)
ls2 = projectRaster(ls2[[1]],crs=crs(dhdt))

# NEPAL
fpath4 = "/Users/mattolson/projects/Debris_snow_class/results/simple_results/L05_Psimple140041.tif"
load("/Users/mattolson/projects/Debris_snow_class/results/simple_results/L05Pnames_140041.RData" )
class_stk0N <- raster::stack(fpath4)
names(class_stk0N) = L05Pnames_140041

t1m_nep = time_average(class_stk0N, before=1996, aggmethod = "mean",
                       month_select = c("08","09","10"), by_month = FALSE)
t3m_nep = time_average(class_stk0N, after=2007, aggmethod = "mean",
                       month_select = c("08","09","10"), by_month = FALSE)
m <- c( -7,3,
        -6,2,
        -3,1,
        -2,4)
rclmat <- matrix(m, ncol=2, byrow=TRUE)
ch = reclassify(t1m_nep-(t3m_nep*4), rclmat)

r2 = class_stk0N[[1]]
r3 = projectRaster(r2,crs=crs(dhdt))
dh3 = crop(dhdt,r3)
ch2 = projectRaster(ch,crs=crs(dhdt),method = 'ngb')
r.to.poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(ch2), 
                                         as_points = FALSE, merge = TRUE))

# PLOT NEPAL subregion dhdt
png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/Nepal_dhdt_class2_6.png"),height=7.8,width=5,unit='in',res=300)
par(mfrow=c(2,1))
par(mar=c(1,3,1,2))
par(oma=c(3,0,0,0))
plot(dh3,col=(RColorBrewer::brewer.pal(8,"RdYlBu")),cex.axis=1.2,xaxt='n', breaks=c(-20,-10,-5,-1,0,1,5,10,20),
     legend.args = list(text = bquote('dh/dt (m '~a^-1~")"), side = 3, 
                        font = 2, line = 1, cex = 1,adj=0.25))
plot(r.to.poly,add=T)

plot(ls2,col=blues9, cex.axis=1.2,legend=F)
plot(ch2, add=T,col = ggplot2::alpha(lab_col, 0.7),legend=F)
dhn = dh3;dhn[dhn>=0]= NA
# contour(dhn,add=T, levels=seq(-20,0,1), labels=seq(-20,0,4),labcex=3, col='firebrick', lwd=0.7)
legend("topright",fill= ggplot2::alpha(lab_col, 0.7),cex=0.6,
       legend=sapply(new_labs, as.expression))
# legend("bottomleft","negative dh/dt",col= "firebrick",cex=0.5,lwd=0.8)
dev.off()
par(mfrow=c(1,1))

# plot elevation compare for full scene
g1dem = mask(crop(resample(d3,dh3,method = 'ngb'),dh3),dh3)
g1ch = mask(crop(resample(ch2,dh3,method = 'ngb'),dh3),dh3)

st0 = stack(g1dem, dh3,g1ch)
df0 = as.data.frame(st0)
names(df0) = c("e","dh","ch")
df0 = df0[complete.cases(df0),]

png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/Nepal_dh_elv.png"),height=11.5,width=5,unit='in',res=300)
df0 %>% mutate(ch = recode(ch, "1" = "Snow/ice", "2" = "Debris", "3" = "Debris gain", "4" = "Snow/ice gain")) %>%
  mutate(surface_change = as.factor(ch)) %>%
  ggplot(aes(x=e,y=dh,colour=surface_change)) + geom_smooth() + theme_bw() + 
  geom_hline(yintercept = 0, "-")  + facet_wrap(~surface_change, ncol=1) +#facet_grid(.~ch) + 
  theme(strip.text.x = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none") +
  scale_color_manual(values=lab_col2) + ylab(bquote("dh/dt m"~a^-1~"2000-2018")) +
  xlab("Elevation (m)")
dev.off()

df1=df0 %>% mutate(ch = recode(ch, "1" = "Snow/ice", "2" = "Debris", "3" = "Debris gain", "4" = "Snow/ice gain")) %>%
  mutate(surface_change = as.factor(ch)) %>% mutate(e = round_any(e,100)) %>%
  group_by(surface_change, e) %>%
  dplyr::summarise(dh_m = mean(dh), dh_sd = sd(dh), n = n()) %>%
  mutate(n = n/100)

ggplot(df1) + 
  geom_line(aes(x = e, y = dh_m, colour='firebrick', size = 1, group = 1))  + 
  geom_errorbar(aes(x = e, y = dh_m,ymin=dh_m-dh_sd, ymax=dh_m+dh_sd), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~surface_change, ncol=1)

# barplot and lines for each type
png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/Nepal_dh_bar6.png"),height=11.5,width=5,unit='in',res=300)
ggplot(df1) + 
  geom_col(aes(x = e, y = n, fill=surface_change), size = 1) + 
  geom_line(aes(x = e, y = (dh_m+1)*200), colour='firebrick', size = 1.2, group = 1) + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none") + 
  scale_fill_manual(values=lab_col2) + 
  ylab(bquote(bold("Pixel count"~italic("(hundreds)")))) + #ylab("Pixel count") +
  xlab("Elevation (m)") +
  scale_y_continuous(sec.axis = sec_axis(~((./200)-1), name = bquote(bold("dh/dt m"~a^-1~"2000-2018")))) +
  geom_hline(yintercept = 1*200, linetype="dashed",col='black',size=0.6)  + 
  facet_wrap(~surface_change, ncol=1) # scales="free_y"
dev.off()

# with error bars
ggplot(df1) + 
  geom_col(aes(x = e, y = n, fill=surface_change), size = 1) + 
  geom_line(aes(x = e, y = (dh_m+1)*200), colour='firebrick', size = 1.2, group = 1) + 
  geom_errorbar(aes(x = e, y = (dh_m+1)*200, ymin=((dh_m-dh_sd)+1)*200, ymax=((dh_m+dh_sd)+1)*200), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none") + 
  scale_fill_manual(values=lab_col2) + 
  ylab(bquote(bold("Pixel count"~italic("(hundreds)")))) + #ylab("Pixel count") +
  xlab("Elevation (m)") +
  scale_y_continuous(sec.axis = sec_axis(~((./200)-1), name = bquote(bold("dh/dt m"~a^-1~"2000-2018")))) +
  geom_hline(yintercept = 1*200, linetype="dashed",col='black',size=0.6)  + 
  facet_wrap(~surface_change, ncol=1) # scales="free_y"

# switch axes
ggplot(df1) + 
  geom_line(aes(x = e, y = dh_m), colour='firebrick', size = 1.2, group = 1) + 
  geom_errorbar(aes(x = e, y = dh_m, ymin=dh_m-dh_sd, ymax=dh_m+dh_sd), width=.2,
                position=position_dodge(.9)) +
  geom_col(aes(x = e, y = (n/400), fill=surface_change), size = 1) + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position = "none") + 
  scale_fill_manual(values=lab_col2) + 
  ylab(bquote(bold("Pixel count"~italic("(hundreds)")))) + #ylab("Pixel count") +
  xlab("Elevation (m)") +
  scale_y_continuous(sec.axis = sec_axis(~(.*400), name = bquote(bold("dh/dt m"~a^-1~"2000-2018")))) +
  geom_hline(yintercept = 1, linetype="dashed",col='black',size=0.6)  + 
  facet_wrap(~surface_change, ncol=1)

df1 %>% ungroup() %>% group_by(surface_change) %>% select(n) %>%
  dplyr::summarise(n = sum(n))






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ASSESS ACCURACY




# align extent
fls = list.files(save_folder,pattern = "*class0.grd", full.names = TRUE)
rl <- lapply(fls, raster)

lapply(rl, extent)

# p2 = buffer(p,width=1e4)

for (i in 1:length(rl)){ rl[[i]] = extend(rl[[i]], p2)}
rl2 = lapply(1:length(rl), function(x)  rl[[x]] = crop(rl[[x]], p2))
s0 <- stack(rl2)

# aggMethods
#mode
t1mo = stackApply(s0[[2:8]],indices=1,modal)
#mean
t1me = stackApply(s0[[2:8]],indices=1,mean)
m <- c( 0,   1.5, 1,  # at least 50% required
        1.51, 2, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
t1me <- reclassify(t1me, rclmat)
#sum
t1ms = stackApply(s0[[2:8]],indices=1,sum)
t1mf = sum(!is.na(s0[[2:8]]))
# second time period
t2mo = stackApply(s0[[9:12]],indices=1,modal)
t2me = stackApply(s0[[9:12]],indices=1,mean)
t2ms = stackApply(s0[[9:12]],indices=1,sum)
t2mf = sum(!is.na(s0[[9:12]]))
# change map
m <- c( -7,3,
        -6,2,
        -3,1,
        -2,4)
rclmat <- matrix(m, ncol=2, byrow=TRUE)
tch = reclassify(t1mo-(t2mo*4), rclmat)
plot(tch)

# bqb
bqls = grep(paste0("BQA"),list.files(flls[tl], full.names = T),value=TRUE)
bqb <- raster::brick(lapply(bqls, raster))
bqb = crop(bqb,p2)
levels(as.factor(getValues(bqb)))
plotRGB(ls0,3,2,1)
plot(bqb==760, add=T, col=ggplot2::alpha(c(NA,"red"),0.5))
plot(bqb==764, add=T, col=ggplot2::alpha(c(NA,"blue"),0.5))
plotRGB(ls0,3,2,1)
plot(bqb==752, add=T, col=ggplot2::alpha(c(NA,"red"),0.5))
plot(bqb==756, add=T, col=ggplot2::alpha(c(NA,"blue"),0.5))
# 760 and 764 seem to be best for snow
# 752 gets cloud edges and 

# BQA
bls = unlist(lapply(1:length(flls), function(x) grep(paste0("BQA"),list.files(flls[x], full.names = T),value=TRUE)))
brl <- lapply(bls, raster)
bl2 = lapply(1:length(brl), function(x)  brl[[x]] = crop(brl[[x]], p2))
b0 <- stack(bl2)
bd = calc(b0==752,sum)
plot(bd)
plot(g,add=T,col=NA)
plot(bd>2)
plot(g,add=T,col=NA)

# snow/ice
ssl =  c(1696, 1700, 1704,1708, 1728, 1732, 1736, 1740)
bs = calc(b0 %in% ssl,sum)
plot(bs>0)
plot(g,add=T,col=NA)

# make simple classification based only on bqa info
clb = bs>0 & gf==1
clb[bd>2 & gf==1]=2
plot(clb)
clb[clb==0]=NA

# how good?
plot(t1mo-clb)
# +1 = t1 snow / clb debris
# -1 = t1 debris / clb snow
plotRGB(ls0,3,2,1)
plot(t1mo-clb, add=T, col=ggplot2::alpha(c("red",NA, "blue"),0.4),legend=F)
plotRGB(ls0,3,2,1)
plot(clb, add=T, col=ggplot2::alpha(c("blue","red"),0.4),legend=F)
plotRGB(ls0,3,2,1)
plot(t1mo, add=T, col=ggplot2::alpha(c("blue","red"),0.4),legend=F)
plotRGB(ls0,3,2,1)
plot(t1me, add=T, col=ggplot2::alpha(c("blue","red"),0.4),legend=F)

# using the bqa bands seems to do just as well as the mode
t1 = stackApply(stack(t1me,t1mo,clb), indices=1,modal)
plot(t1,col=c("blue","red"))

# align brick
# make a matrix out of it, each column represents a raster, rows the values
extent_list<-lapply(lapply(rl, extent), as.matrix)
matrix_extent<-matrix(unlist(extent_list), ncol=length(extent_list))
rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")

# create an extent with the extrem values of your extent
best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
                    min(matrix_extent[2,]), max(matrix_extent[4,]))

# the range of your extent in degrees
ranges<-apply(as.matrix(best_extent), 1, diff)
# the resolution of your raster (pick one) or add a desired resolution
reso<-res(raster(rl[[1]]))
# deviding the range by your desired resolution gives you the number of rows and columns
nrow_ncol<-ranges/reso

# create your raster with the following
s<-raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=raster(rl[[1]])@crs)

# use this raster to reproject your original raster (since your using the same crs,
# resample should work fine
r1<-resample(r1, s, method="ngb")


# need to extend???
lapply(1:8,function(x) extent(tile_roi)==extent(rl[[x]]))

lapply(1:8,function(x) extent(p2)==extent(rl[[x]]))


# test Everest
r = raster(file.path(save_folder,"140041_class1.grd"))
e <- extent( c(417000, 530000, 3050000, 3140000) )
p2 <- as(e, 'SpatialPolygons')
crs(p2) <- crs(r)
rp = crop(r,p2)
plot(rp)


flls = list.files("/Users/mattolson/data/Landsat/140041/L07_140041_19992002_AugOct/", full.names = TRUE)
dfth = data.frame(x=1:length(flls),med=NA,mean=NA,mean3=NA)
for (i in 1:length(flls)){
  print(paste(i, "of",length((flls))))
  band_paths = list.files(flls[i], full.names = T)
  band_select3 = grep(paste0("B[3]"),band_paths,value=TRUE)
  band_select = grep(paste0("B[6]"),band_paths,value=TRUE)[[2]]
  lsTH <- raster::raster(band_select)
  ls3 <- raster::raster(band_select3)
  dfth$med[i] = quantile(getValues(lsTH))[3]
  dfth$mean[i] = mean(getValues(lsTH),na.rm=T)
  dfth$mean3[i] = mean(getValues(ls3),na.rm=T)
  if (i==1){
    lstk = lsTH
    ls3k = ls3
  }else{
    lstk = stack(crop(lstk,lsTH), crop(lsTH, lstk))
    ls3k = stack(crop(ls3k,ls3), crop(ls3, ls3k))
  }
}
dfth$date = as.Date(gsub(paste0(".*",PR,"_(.+)_2.*"),'\\1',flls),"%Y%m%d")
head(dfth)  
plot(mean~date, data=dfth, ylim=c(80,125),type='b')
points(med~date,data=dfth,col='red', type='b')
match(sort(dfth$mean),dfth$mean)
match(sort(dfth$mean,decreasing=TRUE),dfth$mean) # lowest
plot(lstk[[5]], main=dfth$date[5])
#
tl = 5 # 5, 1, 7
bansls = grep(paste0("B[1-5]"),list.files(flls[tl], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls0 = crop(landsat_bands,p2)
plotRGB(ls0,3,2,1)

ratio = (ls0[[4]]/ls0[[5]]) > 1.5
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
clss = ratio==1 & gf==1
clss[ratio!=1 & gf==1]=2
clss[is.na(gf)]=NA
plot(clss)

plotRGB(ls0,3,2,1)
plot(clss, add=T, col=ggplot2::alpha(c("blue","red"),0.5))
#

for (i in 1:length(flls)){
  # tl = match(sort(dfth$mean,decreasing=TRUE),dfth$mean)[i]
  bansls = grep(paste0("B[1-5]"),list.files(flls[i], full.names = T),value=TRUE)
  landsat_bands <- raster::brick(lapply(bansls, raster))
  ls2 = crop(landsat_bands,p2)
  ratio = (ls2[[4]]/ls2[[5]]) > 1.5
  gf = fasterize::fasterize(g,ratio)
  clss0 = ratio==1 & gf==1
  clss0[ratio!=1 & gf==1]=2
  clss0[is.na(gf)]=NA
  if (i==1){
    lst = clss0
  }else{
    lst = stack(crop(lst,clss0), crop(clss0, lst))
  }
}
names(lst) = gsub(paste0(".*",PR,"_(.+)_2.*"),'\\1',flls)


tmp_ls = list.files(tmpDir(),full.names = TRUE)
fd = list(paste0(strsplit(basename(filename(ls0)),"\\.")[[1]][1],".*"),
          paste0(strsplit(basename(filename(ls2[[1]])),"\\.")[[1]][1],".*"),
          paste0(strsplit(basename(filename(bqb)),"\\.")[[1]][1],".*"),
          paste0(strsplit(basename(filename(bqb2)),"\\.")[[1]][1],".*"))

del_ls = tmp_ls[!grepl(fd[[1]],tmp_ls)]
del_ls = del_ls[!grepl(fd[[2]],tmp_ls)]
del_ls = del_ls[!grepl(fd[[3]],tmp_ls)]
del_ls = del_ls[!grepl(fd[[4]],tmp_ls)]
unlink(del_ls)








# # # # # # # # #
# CLOSER

r = raster(file.path(save_folder,"140041_class1.grd"))
e <- extent( c(457000, 490000, 3088000, 3120000) )
p2 <- as(e, 'SpatialPolygons')
crs(p2) <- crs(r)

#
tl = 5 # 5, 1, 7
bansls = grep(paste0("B[1-5]"),list.files(flls[tl], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls0 = crop(landsat_bands,p2)
plotRGB(ls0,3,2,1)

ratio = (ls0[[4]]/ls0[[5]]) > 1.5
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
clss = ratio==1 & gf==1
clss[ratio!=1 & gf==1]=2
clss[is.na(gf)]=NA
plot(clss)

plotRGB(ls0,3,2,1)
plot(clss, add=T, col=ggplot2::alpha(c("blue","red"),0.5))
#

ndsi = overlay(ls0[[c(2,5)]] , fun=function(a,b) (a-b) / (a+b)) >= 0.4
clss2 = ndsi==1 & gf==1
clss2[ndsi!=1 & gf==1]=2
clss2[is.na(gf)]=NA
plot(clss2)
plotRGB(ls0,3,2,1)
plot(clss2, add=T, col=ggplot2::alpha(c("blue","red"),0.5))
#

# BQA
bls = unlist(lapply(1:length(flls), function(x) grep(paste0("BQA"),list.files(flls[x], full.names = T),value=TRUE)))
brl <- lapply(bls, raster)
bl2 = lapply(1:length(brl), function(x)  brl[[x]] = crop(brl[[x]], p2))
b0 <- stack(bl2)
bd = calc(b0==752,sum)
plot(bd)
plot(g,add=T,col=NA)
plot(bd>2)
plot(g,add=T,col=NA)

# snow/ice
ssl =  c(1696, 1700, 1704,1708, 1728, 1732, 1736, 1740)
bs = calc(b0 %in% ssl,sum)











e <- extent( c(417000, 530000, 3050000, 3140000) )
p2 <- as(e, 'SpatialPolygons')
crs(p2) <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"




 # # # # # # # # COMPARISON
sp = grep(list.files(path="/Users/mattolson/data/Landsat/140041/Compare7_8/",full.names = TRUE), pattern='tar', invert=TRUE, value=TRUE)
# 2014
bansls = grep(paste0("B[2-6]"),list.files(sp[1], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls814 = crop(landsat_bands,p2)
bansls = grep(paste0("B[1-5]"),list.files(sp[3], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls714 = crop(landsat_bands,p2)

# classification
ratio = (ls814[[4]]/ls814[[5]]) > 1.5
ratio2 = (ls814[[3]]/ls814[[5]]) > 2
ndsi =  overlay(ls814[[3]],ls814[[5]], fun=function(x,y) (x-y)/(x+y)) > 0.4
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
cl814 = ratio==1 & gf==1
cl814[ratio!=1 & gf==1]=2
cl814[is.na(gf)]=NA
plot(cl814)
ratio = (ls714[[4]]/ls714[[5]]) > 1.5
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
cl714 = ratio==1 & gf==1
cl714[ratio!=1 & gf==1]=2
cl714[is.na(gf)]=NA
plot(cl714)

plot(cl814 - cl714)

plotRGB(ls814,3,2,1)
plot(cl814, add=T, col=ggplot2::alpha(c("blue","red"),0.5))

plotRGB(ls714,3,2,1)
plot(cl814-cl714, add=T, col=ggplot2::alpha(c("blue",NA,"red"),0.5))
#
# TO DO
# mask for clouds with each
# edge effects on sensors
# make misclassifications into points and observe spectra

cl814 - cl714
#  1 l8 debris - l7 pisc
# -1 l8 pisc - l7 debris

# BQA comparison
bls = unlist(lapply(1:length(sp), function(x) grep(paste0("BQA"),list.files(sp[x], full.names = T),value=TRUE)))
brl <- lapply(bls, raster)
bl2 = lapply(1:length(brl), function(x)  brl[[x]] = crop(brl[[x]], p2))
b0 <- stack(bl2)
b0
plotRGB(ls814,3,2,1)
l8cloud = c(2800,2804,2808,2812,6896,6900,6904,6908)
l8cirrus = c(6816,6820,6824,6828,6848,6852,6856,6860,6896,6900,6904,6908,7072,7072,7076,7080,7084,7104,7108,7112,7116,7844,7848,7852,7872,7876,7880,7884)
l8pisc = c(3744,3748,3752,3756,3776,3784,3788,7840,7844,7848,7852,7876,7880,7884)
l8clshade = c(2976,2980,2984,2988,3008,3012,3016,3020,7072,7076,7080,7084,7104,7108,7112,7116)
plot(b0[[1]] %in% l8cloud)
plot(b0[[1]] %in% l8pisc)
plot(b0[[1]] %in% l8cirrus)
plot(b0[[1]] %in% l8clshade)

# 2800 & 6896 have most
plot(b0[[1]]==2800, add=T, col=ggplot2::alpha(c(NA,"red"),0.5))
# band 9
bansls = grep(paste0("B9"),list.files(sp[1], full.names = T),value=TRUE)
landsat_bands <- raster::raster(bansls)
ls_cirr814 = crop(landsat_bands,p2)
plotRGB(ls814,3,2,1)
plot(ls_cirr814>12000, add=T, col=ggplot2::alpha(c(NA,"blue"),0.5)) # or 15000
# maybe
plotRGB(ls814,3,2,1)
plot((b0[[1]] ==2800 & lsfull814[[7]]>10000) | b0[[1]] ==6896, col=ggplot2::alpha(c(NA,"red"),0.5),add=T)
# loudmsk
l814cloud = !( (b0[[1]]==2800 & lsfull814[[7]]>10000) | b0[[1]]==6896 )
#
`%!in%` = Negate(`%in%`)





# BETTER CLASSIFICATION
bansls = c(grep(paste0("B[2-6]"),list.files(sp[1], full.names = T),value=TRUE),grep(paste0("B9"),list.files(sp[1], full.names = T),value=TRUE))
landsat_bands <- raster::brick(lapply(bansls, raster))
ls814 = crop(landsat_bands,p2)
# class (L8)
ratio = (ls814[[4]]/ls814[[5]]) > 1.5
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
cl814 = ratio==1 & gf==1  & !( (b0[[1]]==2800 & ls814[[7]]>10000) | b0[[1]]==6896 )
cl814[ratio!=1 & gf==1  & !( (b0[[1]]==2800 & ls814[[7]]>10000) | b0[[1]]==6896 )]=2
cl814[is.na(gf)]=NA
cl814[cl814==0]=NA
plot(cl814, col=c('blue',"orange"))
cl814 = filter_pixels(cl814,9)
# READ L7
bansls = grep(paste0("B[1-5]"),list.files(sp[3], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls714 = crop(landsat_bands,p2)
# class (L7)
ratio = (ls714[[4]]/ls714[[5]]) > 1.5
g = readRDS(file.path(save_folder,"140041_gpoly.rds"))
gf = fasterize::fasterize(g,ratio)
cl714 = ratio==1 & gf==1  & !( b0[[3]]==760 | b0[[3]]==762 )
cl714[ratio!=1 & gf==1 & !( b0[[3]]==760 | b0[[3]]==762 ) ]=2
cl714[is.na(gf)]=NA
cl714[cl714==0]=NA
plot(cl714, col=c('blue',"orange"))
# clump (get rid of small groups)
cl714 = filter_pixels(cl714,9)
# BQA
ssl =  c(1696, 1700, 1704,1708, 1728, 1732, 1736, 1740)
bs = calc(b0[[3]] %in% ssl,sum)
clb = bs>0 & gf==1
clb[b0[[3]]==752 & gf==1]=2
clb[clb==0]=NA
# possibly combine?
stackApply(stack(cl714,cl714,clb),indices=1, modal)


plot(cl814-cl714, col=c("blue",NA,"red"))
# compare
plotRGB(ls814,3,2,1)
plot(cl814-cl714, add=T, col=ggplot2::alpha(c("blue",NA,"red"),0.5))

# create confusion matrix
tcon = table(data.frame(l8_scene = getValues(cl814), l7_scene = getValues(cl714))) # as n
tcon
tcon2 = round( (table(data.frame(l8_scene = getValues(cl814), l7_scene = getValues(cl714)))/sum(tcon) *100) , 2) # as %
tcon2
# Accuracy ( assuming L8 is true) # Precision ( assuming L8 is true)
cat(" Final (Pisc/Debris) accuracy ~", round( ( (tcon[1,1]+tcon[2,2])/(tcon[1,1]+tcon[2,2]+tcon[2,1]+tcon[1,2]) * 100 ) ,2), "% \n",
    "Snow precision   ~", round( ( tcon[1,1]/(tcon[1,1]+tcon[1,2]) * 100 ) ,2), "% \n",
    "Debris precision ~", round( ( tcon[2,2]/(tcon[2,2]+tcon[2,1]) * 100 ) ,2), "% \n   (assuming OLI image as True)") # debris
# pretty good, issues associated with clouds
ratio = (ls814[[4]]/ls814[[5]]) > 1.5
ratio2 = (ls714[[4]]/ls714[[5]]) > 1.5
plot(calc(stack(ratio,ratio2*4), sum),col=c(NA,'red','orange','blue'))
# blue (5) - both pisc
# orange (4) - only Landsat 7 saw snow
# red (1) only landsat 8 saw snow

tratcon = table(data.frame(l8_scene = getValues(ratio), l7_scene = getValues(ratio2))) # as n
tratcon
# Accuracy ( assuming L8 is true) # Precision ( assuming L8 is true)
cat(" Ratio accuracy ~", round( ( (tratcon[1,1]+tratcon[2,2])/(tratcon[1,1]+tratcon[2,2]+tratcon[2,1]+tratcon[1,2]) * 100 ) ,2), "% \n",
    "Ratio precision ~", round( ( tratcon[2,2]/(tratcon[2,2]+tratcon[2,1]) * 100 ) ,2), "% \n   (assuming OLI image as True)") # debris





# # # ##

# 2015
bansls = grep(paste0("B[2-6]"),list.files(sp[2], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls815 = crop(landsat_bands,p2)
bansls = grep(paste0("B[1-5]"),list.files(sp[4], full.names = T),value=TRUE)
landsat_bands <- raster::brick(lapply(bansls, raster))
ls715 = crop(landsat_bands,p2)




filter_pixels <- function(r, min_size){
  # expects classes 1 & 2 only
  rc_pisc <- clump(r==1, directions = 8) 
  f<-as.data.frame(freq(rc_pisc))
  excludeID <- f$value[which(f$count <= min_size)]
  rc_pisc[rc_pisc %in% excludeID] <- NA
  # debris
  rc_deb <- clump(r==2, directions = 8) 
  f<-as.data.frame(freq(rc_deb))
  excludeID <- f$value[which(f$count <= min_size)]
  rc_deb[rc_deb %in% excludeID] <- NA
  rc_deb[!is.na(rc_pisc)] <- 1
  r[is.na(rc_deb)] = NA
  return(r)
}

# https://gis.stackexchange.com/questions/130993/remove-clumps-of-pixels-in-r
# rc_pisc <- clump(cl714==1, directions = 8) 
# f<-freq(rc_pisc)
# f<-as.data.frame(f)
# excludeID <- f$value[which(f$count <= 9)]
# rc_pisc[rc_pisc %in% excludeID] <- NA
# # debris
# rc_deb <- clump(cl714==2, directions = 8) 
# f<-freq(rc_deb)
# f<-as.data.frame(f)
# excludeID <- f$value[which(f$count <= 9)]
# rc_deb[rc_deb %in% excludeID] <- NA
# rc_deb[!is.na(rc_pisc)] <- 1
# # mask
# cl714[is.na(rc_deb)] = NA













