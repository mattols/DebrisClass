
source('~/projects/Debris_snow_class/src/srtm_download1.R')
d3 <- get_srtm30(dcn3, full_extent = TRUE, mask_to=TRUE)

dhdt = raster("/Users/mattolson/data/Shean/shean_hma_glacier_ASTER_WV_2000-2018_dhdt.tif")

plotRGB(ls2,3,2,1)
plot(dcn, col=ggplot2::alpha(lab_col,0.5), add=T,legend=F)

ls2 = crop(landsat_bands2, dsurf)
dcn = crop(dchange2, dsurf)

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

getValues(g1dh[g1dem<=0.2])



