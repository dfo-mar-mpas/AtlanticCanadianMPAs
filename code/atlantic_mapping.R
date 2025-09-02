## Set up map for this overview of Canadian MPAs

#Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggspatial)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load the Canadian MPA Network Shapefiles and planning regions extracted from the Canadian Database of Protected and Conserved Areas
#August 2025 - copied from Resilient_Canadian_Network respository 01_CPCAD_extraction.R

cons_df <- read_sf("data/cpcad_complete.shp")%>%
           st_transform(CanProj)


planning_region_df <- read_sf("data/canadian_planning_regions.shp")%>%
                      st_transform(CanProj)


#extract the Atlantic regions

atlantic_regions <- c("Newfoundland-Labrador Shelves","Scotian Shelf","Gulf of Saint Lawrence")

atlantic_df <- cons_df%>%
              filter(bioregion %in% atlantic_regions)

atlantic_bioregions <- planning_region_df%>%
                       rename(bioregion = region)%>% #consistent with conservation areas
                       filter(bioregion %in% atlantic_regions)

#Basemap 

basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(name_en,geometry)%>%
  st_as_sf()%>%
  st_union()%>%
  st_transform(latlong)%>%
  st_as_sf()%>%
  mutate(country="Canada")%>%
  rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="US"),
        ne_states(country = "Greenland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Greenland"),
        ne_states(country = "Iceland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Iceland"))%>%
  st_transform(CanProj)

plot_region <- atlantic_bioregions%>%
               st_bbox()

#make the plot

p1 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country=="Canada"),fill="grey60")+
  geom_sf(data=atlantic_bioregions,fill=NA)+
  geom_sf(data=atlantic_df,aes(fill=bioregion))+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  annotation_scale(location="br")+
  scale_fill_manual(
    values = c(
      "Scotian Shelf" = "#c6dbef",
      "Gulf of Saint Lawrence" = "#6baed6",
      "Newfoundland-Labrador Shelves" = "#08519c"
    )
  )+
  labs(fill="")+
  theme(legend.position = "inside",
        legend.position.inside = c(0.76,0.92),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=8.5))

ggsave("output/atlantic_canadian_CAs.png",p1,height=8,width=6,units="in",dpi=300)
