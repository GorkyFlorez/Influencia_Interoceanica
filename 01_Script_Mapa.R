library(sf)
library(ggplot2)
library(cartomisc)
library(ggspatial)
library(raster)
Via   = read_sf("Shp/Via.geojson")
Re   = read_sf("Shp/Red_vial.geojson")
CPP   = read_sf("Shp/CP.geojson")
MD   = read_sf("Shp/MDD.geojson")

Vias <- st_transform(Via  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Red <- st_transform(Re   ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
CP  <- st_transform(CPP  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
MDD <- st_transform(MD ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAme.geojson")  
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

Vias_bufe<- Vias %>% 
  st_buffer(units::set_units(10, km))


Vias_merge =st_union(Vias_bufe)  
  
MDDgra  =ggplot()+
  geom_sf(data = MDD, fill="black", color="black")+
  theme_void()
MDDgra
MDDgra.grob  <- ggplotGrob(MDDgra) 

SurAmericaa=  ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.6)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA))+
  annotation_custom(MDDgra.grob, xmin = -50, xmax = -40, ymin =-55, ymax=-40)+
  geom_segment(aes(x=-70, xend=-49,y=-14, yend=-50), 
               linetype = "dashed", color = "black", size = 0.6)+
  geom_segment(aes(x=-70, xend=-46,y=-14, yend=-43), 
               linetype = "dashed", color = "black", size = 0.6)+
  annotate(geom = "text", x = -46, y = -38, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 2, family="serif", color = 
             "black",  fontface="italic")
SurAmericaa
SurAmericaa.grob  <- ggplotGrob(SurAmericaa)  

dd_gg=ggplot()+
  geom_sf(data = MDD , fill=NA, color="gray60")+
  geom_sf(data = Red , color="gray")+
  geom_sf(data = Vias , color="red", size=1.5)+
  geom_sf(data = CP , color="black", fill="green", pch = 21)+
  geom_sf(data = Vias_merge , fill="#ffb703", alpha=0.09, color="#ffb703")+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "black",
                                     fill = NA))+
  annotation_scale()+
  annotation_north_arrow(location="tl",which_north="true",
                         style=north_arrow_fancy_orienteering ())
Secc =ggplot()+
  geom_sf(data = MDD , fill=NA, color="gray60")+
  geom_sf(data = Red , color="gray")+
  geom_sf(data = Vias , color="red", size=1.5)+
  geom_sf(data = CP , color="black", fill="green", pch = 21)+
  geom_sf(data = Vias_merge , fill="#ffb703", alpha=0.09, color="#ffb703")+
  coord_sf(xlim = c(-70.5, -68.8), ylim = c(-13.5 ,-10.5)) +
  theme_void()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "black",
                                     fill = NA))



# Mapa final
library(cowplot)
Final =ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(SurAmericaa.grob, width = 8, height = 8,x = 0.0001, y = 12.3)+
  draw_plot(Secc,             width = 8, height = 9,x = 0.0001, y = 3)+
  draw_plot(dd_gg, width =21, height = 21,x = 7, y = 0.001)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "white",
                                     fill = NA))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo                       Gorky Florez Castillo                        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = 18, y = 19, hjust = 0, vjust = 1,
           label = "Mapa de la influencia de la Carretera Interoceánica \n               suroeste de la Amazonia",
           size = 3, family = "Tahoma", color = "black", , face = "bold")



Final
ggsave(plot=Final,"Mapas/Mapa de Ubicacion Kotsimba.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1500)


