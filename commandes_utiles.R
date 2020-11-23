
# import des librairies ---------------------------------------------------
library(stats)
library(sf)
library(mapview) 
library(ggmap)
library(grid)
library(gridExtra)
library(plm)
#library(OpenStreetMap)
library(rgdal)
library("ggspatial")
library("ggrepel")
#library(raster)
library(tidyverse)
library(mapview) 
library(grid)
library(gridExtra)
library(openxlsx)
library(corrplot)
library(dplyr)
library(raster)
library(RColorBrewer)

# chargement des shapefiles --------------------------------------------------

# pour charger un shapefile
Retention_Eau_Bassin_Versant<-st_read("./Data/Retention_Eau_Bassin_Versant/Retention_m3_WS.shp")

# pour enlever des colonnes de la table d'attribut (si on veut)
bassin_versant <-Retention_Eau_Bassin_Versant [,-c(1,9:34)]


# exemple de calcul -------------------------------------------------------

difference_entre_deux_scenarios <- Retention_Eau_Bassin_Versant %>% 
  mutate(diff=(Retentio21-Retentio20)/Retentio20) %>%  #fait le calcul
  dplyr::select(-c(2:34)) #pour enlever les colonnes 2 à 34 (optionnel, c'ets juste pour faire le ménage)



# tracer les cartes -------------------------------------------------------


#pour tracer la carte
mapview(difference_entre_deux_scenarios,zcol='diff')

#autre possibilité
plot(difference_entre_deux_scenarios)

#encore une autre (un peu plus élaborée)
ggplot()+
  geom_sf(data=difference_entre_deux_scenarios,aes(fill=diff)) +
  scale_fill_distiller(type = "div", 
                       #palette='RdBu', 
                       palette='Spectral', 
                       limit =  max(abs(difference_entre_deux_scenarios$diff)) * c(-1, 1),
                       #limit =  c(-50,50)
                       na.value = NA
  )+
  xlab("") + ylab("") +
  annotation_scale(location = "bl", width_hint = 0.25,style='ticks') +
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,#
                         height = unit(0.3, "in"),
                         width = unit(0.3, "in")) +
  theme(panel.background = element_rect(fill = 'white'),axis.ticks = element_blank(),axis.text = element_blank())+
  coord_sf(xlim = c(582746.4, 747756.9), ylim = c(6773208, 6909306))+
  labs(fill = "runoff\nretention")





