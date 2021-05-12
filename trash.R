
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
  facet_wrap(~surface_change, ncol=1)

png(paste0("/Users/mattolson/projects/Debris_snow_class/results/simple_new/Nepal_dh_bar3.png"),height=11.5,width=5,unit='in',res=300)
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
  facet_wrap(~surface_change, ncol=1,scales="free_y") # scales="free_y"
dev.off()

