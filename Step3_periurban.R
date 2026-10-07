###characterizatio of the urabn -rural transition landscape in 2023


library(terra)
library(sf)
library(ggplot2)
library(colorspace)
library(ggalt)
library(ggnewscale)
x23<-rast("../data/lulc_Kabul_2023.tiff")[[1]]

##building density information

x23df<-as.data.frame(x23, xy=T, na.rm=F)
colnames(x23df)[3]<-"LULC"
x23df<-x23df[which(complete.cases(x23df)==T),]
x23df$built<-ifelse(x23df$LULC%in%seq(1,4),1,NA)

x23sf<-st_as_sf(x23df, coords = c("x","y"), remove = F)
st_crs(x23sf)<-crs(x23)

bd<-x23sf[which(x23sf$LULC%in%seq(1,3)),]

nbd<-x23sf[which(!(x23sf$LULC%in%seq(1,3))),]

inst<-read_sf("../data/shp/institutional.shp")
inst<-st_transform(inst,crs(x23) )

inst$x<-st_coordinates(inst)[,1]
inst$y<-st_coordinates(inst)[,2]
n6<-nbd[which(nbd$LULC==6),]
n9<-nbd[which(nbd$LULC==9),]
n10<-nbd[which(nbd$LULC==10),]
nsch<-inst[which(inst$FolderPath=="school"),]
nuni<-inst[which(inst$FolderPath=="university"),]
nfood<-inst[which(inst$FolderPath%in%c("supermarket","food shop")),]
nlib<-inst[which(inst$FolderPath=="library"),]
nmos<-inst[which(inst$FolderPath=="mosque"),]
npol<-inst[which(inst$FolderPath=="police station"),]
ngym<-inst[which(inst$FolderPath=="gym"),]




for (i in 1:nrow(bd)) {
  #show(i)
  ds6<-st_distance(bd[i,],n6[st_nearest_feature(bd[i,],n6),])
  ds9<-st_distance(bd[i,],n9[st_nearest_feature(bd[i,],n9),])#min(st_distance(bd[i,],nbd[which(nbd$LULC==9),]))
  ds10<-st_distance(bd[i,],n10[st_nearest_feature(bd[i,],n10),])#min(st_distance(bd[i,],nbd[which(nbd$LULC==10),]))
  ds_sch<-st_distance(bd[i,],nsch[st_nearest_feature(bd[i,],nsch),])
  ds_uni<-st_distance(bd[i,],nuni[st_nearest_feature(bd[i,],nuni),])#min(st_distance(bd[i,],inst[which(inst$FolderPath=="university"),]))
  ds_food<-st_distance(bd[i,],nfood[st_nearest_feature(bd[i,],nfood),])#min(st_distance(bd[i,],inst[which(inst$FolderPath%in%c("supermarket","food shop")),]))
  ds_lib<-st_distance(bd[i,],nlib[st_nearest_feature(bd[i,],nlib),])#min(st_distance(bd[i,],inst[which(inst$FolderPath=="library"),]))
  ds_mos<-st_distance(bd[i,],nmos[st_nearest_feature(bd[i,],nmos),])#min(st_distance(bd[i,],inst[which(inst$FolderPath=="mosque"),]))
  ds_pol<-st_distance(bd[i,],npol[st_nearest_feature(bd[i,],npol),])#min(st_distance(bd[i,],inst[which(inst$FolderPath=="police station"),]))  
  ds_gym<-st_distance(bd[i,],ngym[st_nearest_feature(bd[i,],ngym),])#min(st_distance(bd[i,],inst[which(inst$FolderPath=="police station"),]))  
  
  bd[i,c(6:15)]<-c(ds6, ds9, ds10, ds_sch, ds_uni, ds_food, ds_lib, ds_mos, ds_pol, ds_gym)
}
colnames(bd)[c(6:15)]<-c("ds6","ds9","ds10","ds_sch","ds_uni","ds_food","ds_lib", "ds_mos", "ds_pol", "ds_gym")
ins<-apply(bd[,c(9:15)],1,function(x){mean(x, na.rm=T)})


mn_nat<-function(x, output){A<-as.numeric(x[6]); B<-as.numeric(x[7]); C<-as.numeric(x[8]); return(mean(c(A,B,C)))}
bd$nat<-apply(bd,1,mn_nat)

mn_inst<-function(x, output){A<-as.numeric(x[6]); B<-as.numeric(x[7]); C<-as.numeric(x[8]);
D<-as.numeric(x[9]);E<-as.numeric(x[10]);F1<-as.numeric(x[11]);
G<-as.numeric(x[12]);return(mean(c(A,B,C,D,E,F1,G)))}
bd$inst<-apply(bd,1,mn_inst)

cityC<-st_as_sf(data.frame(y=34.532801372206364, x=69.16559783007249), coords=c("x","y"))
st_crs(cityC)<-4326
cityC<-st_transform(cityC,crs(x23) )
bd$Cdist<-as.numeric(st_distance(bd,cityC)[,1])

un<-unique(bd$LULC)
for ( i in 1:length (un)){
  show(i)
  ss<-bd[which(bd$LULC==un[i]),]
  Q90<-as.numeric(quantile(ss$Cdist, probs=0.98))
  #Q20<-as.numeric(quantile(ss$Cdist, probs=0.10))
  ss$LULC2<-ifelse(ss$Cdist>Q90 ,NA, ss$LULC)
  if (i==1){bd2<-ss}else{bd2<-rbind(bd2,ss)}
}
bd2$x<-st_coordinates(bd)[,1]
bd2$y<-st_coordinates(bd)[,2]
bd2$sumdist<-bd2$nat+bd2$inst
kabul_10k<-read_sf("../data/shp/kabul_buffer10k.shp")
kabul_10k<-st_transform(kabul_10k, 32642)
kabul_10k2<-st_buffer(kabul_10k,-400)


bd2<-st_buffer(bd2,50)
tt<-st_intersects(bd, kabul_10k2)
bd2$tt<-unlist(replace(tt, !sapply(tt, length),0))
bd2$tt<-as.numeric(bd2$tt)

#bd2<-bd2[-which(bd2$LULC2==1 & bd2$inst>20000),]

ggplot(data=bd2[which(bd2$tt=="1" & !(is.na(bd2$LULC2))),], aes(x=inst/1000, y=nat/1000))+#bd2$tt=="1" &
  geom_point(aes(color=as.factor(LULC2)), size=0.8)+#Cdist/1000
  #geom_smooth(formula=y~s(x, k=5), method="gam", level=0.99999, color="red2", fill="red2")+
  #scale_color_brewer(palette = "RdYlBu", labels=c("Apartment blocks", "Residential area", "Informal settlements"))+
  scale_color_manual(values = c("brown3", "darkorange3", "yellow3"), labels=c("Apartment blocks", "Residential area", "Informal settlements"))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "grey18"))+
  labs(x="Mean distance from institutional infrastructures (Km)",y= "Mean distance from natural infrastructures (Km)",
       color=element_blank())+
  theme(
    panel.grid.major = element_line(colour = "grey39", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    #legend.position = "inside",
    legend.title.position = "top", legend.title = element_text(size=20, vjust = .5, hjust = .5),
    legend.position = "top",
    #legend.position.inside = c(0.2,1),
    axis.text=element_text(size=20), #change font size of axis text
    axis.title.x=element_text(size=20, margin = unit(c(1,0,0,0), "lines")),
    axis.title.y=element_text(size=20, margin = unit(c(0,1,0,0), "lines")),
    #legend.direction = "vertical",
    legend.key = element_blank(),
    legend.text = element_text(size=23),
    legend.key.spacing.y = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=8)))


ggsave("../results/distances.png",width = 34,height = 18,units = "cm", dpi=300)