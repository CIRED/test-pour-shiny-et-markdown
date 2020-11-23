
# import des librairies ---------------------------------------------------
library(stats)
library(sf)
library(readxl)
library(stringr)
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
Recharge_Locale_Bassin_Versant<-st_read("./Data/Recharge_Locale_Bassin_Versant/recharge_locale_m3_WS.shp")

# carte de recharge des nappes pour un scénario-------------------------------------------------------

# si on veut calculer la différemncve entre 2 scénarios
difference_entre_deux_scenarios <- Recharge_Locale_Bassin_Versant %>% 
  mutate(diff=(L_INSEE_ha-L_mos_2017)/L_mos_2017*100) %>%  #fait le calcul
  dplyr::select(-c(2:34)) #pour enlever les colonnes 2 à 34 (optionnel, c'ets juste pour faire le ménage)

#carte de recharge des nappes pour un scénario
mapi<-ggplot()+
  geom_sf(data=difference_entre_deux_scenarios,aes(fill=diff)) +
  scale_fill_distiller(type = "div", 
                       palette='RdBu',direction =-1, #pour que rouge = negatif et bleu = positif
                       #palette='Spectral', 
                       limit =  max(abs(difference_entre_deux_scenarios$diff),na.rm = TRUE) * c(-1, 1),
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
  labs(fill = "Variation (%)")+
  ggtitle("Service de recharge des nappes",subtitle='INSEE haut habitat collectif vs. 2017')

#pour afficher la carte à l'écran (optionnel)
plot(mapi)

#pour enregistrer la carte
ggsave(paste0("Output/recharge_locale.png"), plot=mapi,width=198,height=140,units ='mm')


# carte de recharge des nappes pour tous les scénarios ---------------------------------------------------------------

#en fait on va faire un truc plus compliqué, on va faire ça d'un coup pour tous les scénarios
difference_entre_all_scenarios <- Recharge_Locale_Bassin_Versant %>% 
  pivot_longer(-c(1:11,28,35),names_to = "scenario",values_to = "value")  %>% #on utilise la puissance de R...
  mutate(diff_with_2017=(value-L_mos_2017)/L_mos_2017*100) %>%  #on calcule la différence avec 2017
  st_as_sf(., sf_column_name = "geometry") #Hack : les calculs d'avant enlevent le fait que R voit l'objet comme une carte, du coup on le lui redit

mapi<-ggplot()+
  geom_sf(data=difference_entre_all_scenarios,aes(fill=diff_with_2017)) +
  scale_fill_distiller(type = "div",
                       palette='RdBu',direction =-1, #pour que rouge = negatif et bleu = positif
                       #palette='Spectral',
                       limit =  max(abs(difference_entre_all_scenarios$diff_with_2017),na.rm = TRUE) * c(-1, 1),
                       #limit =  c(-50,50)
                       na.value = NA)+
  facet_wrap('scenario')+
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
  labs(fill = "Variation (%)")+
  ggtitle("Service de recharge des nappes")

#pour afficher la carte à l'écran (optionnel et long ici)
#plot(mapi)

#pour enregistrer la carte
ggsave(paste0("Output/recharge_locale_all_scenarios.png"), plot=mapi,width=594,height=420,units ='mm')


# carte de flood retention pour tous les scénarios ---------------------------------------------------------------


#idem
difference_entre_all_scenarios <- Retention_Eau_Bassin_Versant %>% 
  pivot_longer(-c(1:11,28,35),names_to = "scenario",values_to = "value")  %>% 
  mutate(diff_with_2017=(value-Retentio16)/Retentio16*100) %>%  #je crois que 2017 c'est Retentio16 ?
  st_as_sf(., sf_column_name = "geometry") 

mapi<-ggplot()+
  geom_sf(data=difference_entre_all_scenarios,aes(fill=diff_with_2017)) +
  scale_fill_distiller(type = "div",
                       palette='RdBu',direction =-1, #pour que rouge = negatif et bleu = positif
                       #palette='Spectral',
                       limit =  max(abs(difference_entre_all_scenarios$diff_with_2017),na.rm = TRUE) * c(-1, 1),
                       #limit =  c(-50,50)
                       na.value = NA)+
  facet_wrap('scenario')+
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
  labs(fill = "Variation (%)")+
  ggtitle("Service de retention d'eau")

#pour afficher la carte à l'écran (optionnel et long ici)
#plot(mapi)

#pour enregistrer la carte
ggsave(paste0("Output/retention_eau_all_scenarios.png"), plot=mapi,width=594,height=420,units ='mm')


# courbes -----------------------------------------------------------------

result_simulations <- read_excel("./Data/Evo_SE_SLUM_Pieto_mini.xlsx")

result_simulations <-result_simulations %>% 
  filter(! (Services %in% c('Retention N',
                            'Retention P',
                            'Recharge Locale Pondere WS',
                            'Urban Flooding Pondere WS',
                            'Rapport nojardin jardin'))) %>% 
  mutate(scenario_type=case_when(
    Scenarios %in% c('Densi_jardin_hab_collectif','Densi_jardin_rural',
                     'Densi_lotissements','Forets', 'Pietonnisation_jardins',
                     'Vegetalisation_bois_forets','Vegetalisation_parcs_jardins',
                     'Pacte_Vert') ~ 'Changement des parcs',
    Scenarios %in% c('SDRIF_hab_collectif','SDRIF_lotissements','SDRIF_rural') ~ 'SDRIF',
    grepl('INSEE', Scenarios, fixed = TRUE)  ~ 'INSEE',
    TRUE ~ 'Actual data'
  )) %>% 
  mutate(year_vincent=case_when(
    scenario_type == 'Changement des parcs' ~ 2020,
    scenario_type == 'SDRIF' ~ 2030,
    scenario_type == 'INSEE' ~ 2050,
    TRUE ~ year
  )) %>% 
  mutate(is_actual_data=ifelse(scenario_type=='Actual data','yes',Scenarios))

ploti<-result_simulations %>% 
  mutate(nom_et_unit=paste0(Services,"\n(",str_wrap(Unité,width = 40),")")) %>% 
  ggplot(data=.)+
  geom_point(aes(x=as.numeric(year_vincent),y= SE_absolu,color=scenario_type))+
  geom_line(aes(x=as.numeric(year_vincent),y= SE_absolu,group=is_actual_data),linetype="dotdash")+
  facet_wrap('nom_et_unit',scales = "free")+
  theme_grey(base_size=11)+
  theme(axis.ticks = element_blank(),legend.position="bottom",strip.text.x = element_text(size = 8))+
  ggtitle('Vue globale des services')+
  labs(x = "Year",y = "",color="Type de scénario") 

ggsave(paste0("Output/vue_globale.png"), plot=ploti,width=297,height=210,units ='mm')
