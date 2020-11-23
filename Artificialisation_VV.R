### Import des librairies ###################################################################################
library(stats)
library(readxl)
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
setwd("/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/Artificialisation/")
#setwd(this.dir)
### Chargement du MOS et s?lection des espaces naturels et ouvert avec exclusion terrains vacants ######################################

##### selection pour 1982 MOS des espaces naturels
#evolumos.complet<-readOGR(dsn="../Occupation_sol_admin/MOS_2017_81postes/MOS_2017_shp/shp")
#head(evolumos.complet)
#evolumos.non_spatial <- evolumos.complet %>%
#	as.data.frame(.) %>% #on enleve la partie spatiale
#  rename( MOS1982_81=MOS82, #on renomme le MOS 81 postes
#          MOS1987_81= MOS87,
#          MOS1990_81= MOS90,
#          MOS1994_81= MOS94,
#          MOS1999_81= MOS99,
#          MOS2003_81= mos2003,
#          MOS2008_81= MOS2008,
#          MOS2012_81= MOS2012,
#          MOS2017_81= MOS2017,
#  ) 
#save(evolumos.non_spatial,file="evolumos_non_spatial.rda")
#load("evolumos_non_spatial.rda")
#mos.allege <- evolumos.non_spatial %>%
#  + select(c(OBJECTID,INSEE,FID_commun,Shape_Area, DEP,MOS1982_11,MOS1987_11,MOS1990_11,MOS1994_11,MOS1999_11,MOS2003_11,MOS2008_11,MOS2012_11,MOS2017_11))   %>%
#  + group_by(INSEE,DEP, MOS1982_11, MOS1987_11, MOS1990_11, MOS1994_11, MOS1999_11, MOS2003_11, MOS2008_11, MOS2012_11,MOS2017_11) %>%
#  + summarise(Area=sum(Shape_Area))
#save(mos.allege,file="mos_allege.rda")

load("mos_allege.rda")
mos.allege2<-mos.allege %>% 
  pivot_longer(c(`MOS1982_11`,`MOS1987_11`,`MOS1990_11`,`MOS1994_11`,`MOS1999_11`,`MOS2003_11`,`MOS2008_11`,`MOS2012_11`,`MOS2017_11`),
               names_to = "annee", values_to = "code_MOS")

#variation totale par commune
classification<-mos.allege2 %>% 
  mutate(categorie=case_when(
    code_MOS %in% c('1','2','4') ~ "Natural",
    code_MOS %in% c('3','5') ~ "Agric",
    TRUE ~ "urban"
  )) %>% 
  group_by(INSEE,annee,categorie) %>% 
  summarise(Area=sum(Area)) %>% 
  ungroup() %>% 
  group_by(INSEE,annee) %>% 
  mutate(area_commune=sum(Area)) %>% 
  mutate(pourcent=Area/area_commune)

#calcul des flux
classification2<-mos.allege2 %>% 
  ungroup() %>% 
  group_by(INSEE,annee) %>% 
  mutate(area_commune=sum(Area)) %>% 
  mutate(pourcent=Area/area_commune) %>% 
  ungroup() %>% 
  mutate(annee=case_when(
      annee == 'MOS1982_11' ~ "avant",
      annee == 'MOS2017_11' ~ "apres",
      TRUE ~ "reste"
      )) %>% 
  filter(annee %in% c('avant','apres')) %>% 
  mutate(categorie=case_when(
    code_MOS %in% c('1','2','4') ~ "Natural",
    code_MOS %in% c('3','5') ~ "Agric",
    TRUE ~ "Urban"
  )) %>% 
  select(-'code_MOS') %>% 
  pivot_wider(names_from = annee, values_from = categorie) 

classification3<-classification2 %>% 
  mutate(flux=case_when(
    (avant=='Urban') & (apres=='Urban') ~ 'Urban_Urban',
    (avant=='Urban') & (apres=='Agric') ~ 'Urban_Agric',
    (avant=='Urban') & (apres=='Natural') ~ 'Urban_Natural',
    (avant=='Natural') & (apres=='Urban') ~ 'Natural_Urban',
    (avant=='Natural') & (apres=='Agric') ~ 'Natural_Agric',
    (avant=='Natural') & (apres=='Natural') ~ 'Natural_Natural',
    (avant=='Agric') & (apres=='Urban') ~ 'Agric_Urban',
    (avant=='Agric') & (apres=='Agric') ~ 'Agric_Agric',
    (avant=='Agric') & (apres=='Natural') ~ 'Agric_Natural',
  )
  ) 

#calcule la surface de la commune
surf_commune<-classification3 %>% 
  ungroup() %>% 
  group_by(INSEE) %>% 
  summarise(area_commune=mean(area_commune))

classification4<-classification3%>% 
  select(-c('avant','apres')) %>% 
  group_by(INSEE,flux) %>% 
  summarise(Area=sum(Area)) %>% 
  pivot_wider(names_from = 'flux', values_from = 'Area',values_fill = list(Area = 0)) 
  

fond_carte<-st_read('../Occupation_sol_admin/communes_Idf_bis/COMMUNE.shp')%>% 
  select(c(1,4,12))
#on ajoute les arrondissements
fond_carte<-st_read('../Occupation_sol_admin/communes_Idf_bis/ARRONDISSEMENT.shp') %>% 
  select(c(1,4,5))%>% 
  rbind(.,fond_carte) %>% 
  filter('CODE_INSEE'!='75056')
fond_carte<-fond_carte %>% 
  merge(.,classification4,by.x='CODE_INSEE',by.y='INSEE')
mapview(fond_carte,zcol='Agric_Natural')
mapview(fond_carte,zcol='Urban_Natural')
mapview(fond_carte,zcol='Natural_Urban')
mapview(fond_carte,zcol='Urban_Agric')

result_invest<-read.xlsx('../correlation/SE_corr.xlsx') %>% 
  merge(.,surf_commune,by.x='INSEE',by.y='INSEE')

combine<-fond_carte %>% 
  merge(.,result_invest,by.x='CODE_INSEE',by.y='INSEE')

combine %>% 
  mutate(pourcent=var_Carbone_per.comm/area_commune) %>% 
mapview(.,zcol='pourcent')

test<-combine 
st_geometry(test) <- NULL

test %>% 
  select(seq(3,11))%>% 
  cor(., method = c("spearman")) %>% 
  corrplot

regress<-combine %>% 
  mutate(pourcent=var_Carbone_per.comm/area_commune) %>% 
lm(var_Carbone_per.comm~
   Urban_Agric+
   Urban_Natural+
     Urban_Urban+
   Natural_Urban+
   Natural_Agric+
   Natural_Natural+
   Agric_Urban+
   #Agric_Agric+
   Agric_Natural
   ,.) 
summary(regress)

# 4 categories ------------------------------------------------------------

load("mos_allege.rda")
mos.allege2<-mos.allege %>% 
  pivot_longer(c(`MOS1982_11`,`MOS1987_11`,`MOS1990_11`,`MOS1994_11`,`MOS1999_11`,`MOS2003_11`,`MOS2008_11`,`MOS2012_11`,`MOS2017_11`),
               names_to = "annee", values_to = "code_MOS")


#calcul des flux
classification4<-mos.allege2 %>% 
  ungroup() %>% 
  group_by(INSEE,annee) %>% 
  mutate(area_commune=sum(Area)) %>% 
  mutate(pourcent=Area/area_commune) %>% 
  ungroup() %>% 
  mutate(annee=case_when(
    annee == 'MOS2012_11' ~ "avant",
    annee == 'MOS2017_11' ~ "apres",
    TRUE ~ "reste"
  )) %>% 
  filter(annee %in% c('avant','apres')) %>% 
  mutate(categorie=case_when(
    code_MOS %in% c('1','4','2') ~ "Natural",
    code_MOS %in% c('3') ~ "Agric",
    code_MOS %in% c('5') ~ "Semi-Natural",
    TRUE ~ "Urban"
  )) %>% 
  select(-'code_MOS') %>% 
  pivot_wider(names_from = annee, values_from = categorie) %>% 
  mutate(flux=paste(avant,apres,sep=" --> ")) %>% 
  mutate(flux=ifelse(
   avant==apres,avant,flux 
  )) %>% 
  select(-c('avant','apres')) %>% 
  group_by(INSEE,flux) %>% 
  summarise(Area=sum(Area)) %>% 
  #summarise(Area=sum(pourcent)) %>% 
  pivot_wider(names_from = 'flux', values_from = 'Area',values_fill = list(Area = 0)) %>% 
  ungroup()

classification4_pourcent<-classification4 %>% 
  ungroup() %>% 
  merge(.,surf_commune,by.x='INSEE',by.y='INSEE') %>% 
  pivot_longer(-c('INSEE','area_commune') ) %>% 
  mutate(value=value/area_commune) %>%  #pour avoir des pourcentages
  pivot_wider(names_from = name, values_from = value)   %>% 
  select(-'area_commune')

test<-classification4_pourcent %>%
 select(-c('INSEE'))%>% 
  cor(., method = c("spearman")) %>% 
  corrplot

result_invest<-read.xlsx('../correlation/SE_corr.xlsx') %>% 
  merge(.,surf_commune,by.x='INSEE',by.y='INSEE')


### var_Carbone_per.comm
combine<- result_invest %>%  
  select(INSEE,var_Carbone_per.comm)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE) %>% 
  select(-Urban,-Agric,-Natural,-'Semi-Natural')

regress<-combine %>% 
  lm(var_Carbone_per.comm~.,.) 
summary(regress)

### recre_I1_pourcent
combine<- result_invest %>%  
  select(INSEE,recre_I1_pourcent)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(recre_I1_pourcent~.,.) 
summary(regress)

### recre_I2_pourcent
combine<- result_invest %>%  
  select(INSEE,recre_I2_pourcent)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(recre_I2_pourcent~.,.) 
summary(regress)

### diff_grandes.cultures
combine<- result_invest %>%  
  select(INSEE,diff_grandes.cultures)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(diff_grandes.cultures~.,.) 
summary(regress)

### diff_autrescultures
combine<- result_invest %>%  
  select(INSEE,diff_autrescultures)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(diff_grandes.cultures~.,.) 
summary(regress)
  
# 3 categories bis ------------------------------------------------------------

load("mos_allege.rda")
mos.allege2<-mos.allege %>% 
  pivot_longer(c(`MOS1982_11`,`MOS1987_11`,`MOS1990_11`,`MOS1994_11`,`MOS1999_11`,`MOS2003_11`,`MOS2008_11`,`MOS2012_11`,`MOS2017_11`),
               names_to = "annee", values_to = "code_MOS")


#calcul des flux
classification4<-mos.allege2 %>% 
  ungroup() %>% 
  group_by(INSEE,annee) %>% 
  mutate(area_commune=sum(Area)) %>% 
  mutate(pourcent=Area/area_commune) %>% 
  ungroup() %>% 
  mutate(annee=case_when(
    annee == 'MOS1982_11' ~ "avant",
    annee == 'MOS2017_11' ~ "apres",
    TRUE ~ "reste"
  )) %>% 
  filter(annee %in% c('avant','apres')) %>% 
  mutate(categorie=case_when(
    code_MOS %in% c('1','4','2') ~ "Natural",
    code_MOS %in% c('3') ~ "Agric",
    TRUE ~ "Urban"
  )) %>% 
  select(-'code_MOS') %>% 
  pivot_wider(names_from = annee, values_from = categorie) %>% 
  mutate(flux=paste(avant,apres,sep=" --> ")) %>% 
  mutate(flux=ifelse(
    avant==apres,avant,flux 
  )) %>% 
  select(-c('avant','apres')) %>% 
  group_by(INSEE,flux) %>% 
  summarise(Area=sum(Area)) %>% 
  #summarise(Area=sum(pourcent)) %>% 
  pivot_wider(names_from = 'flux', values_from = 'Area',values_fill = list(Area = 0)) %>% 
  ungroup()

classification4_pourcent<-classification4 %>% 
  ungroup() %>% 
  merge(.,surf_commune,by.x='INSEE',by.y='INSEE') %>% 
  pivot_longer(-c('INSEE','area_commune') ) %>% 
  mutate(value=value/area_commune) %>%  #pour avoir des pourcentages
  pivot_wider(names_from = name, values_from = value)   %>% 
  select(-'area_commune')

test<-classification4_pourcent %>%
  select(-c('INSEE'))%>% 
  cor(., method = c("spearman")) %>% 
  corrplot

### var_Carbone_per.comm
combine<- result_invest %>%  
  select(INSEE,var_Carbone_per.comm)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(var_Carbone_per.comm~.,.) 
summary(regress)


### patrimoine_naturel
combine<- result_invest %>%  
  select(INSEE,patrimoine_naturel)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(patrimoine_naturel~.,.) 
summary(regress)



# calcul stockage carbone -------------------------------------------------

load("evolumos_non_spatial.rda")
land_carbon<-read.csv('../2- Carbon Storage/1- Données/carbon_pools_81p.csv',sep=';')

carbon_stock<-evolumos.non_spatial %>% 
  select(OBJECTID,INSEE,FID_commun,Shape_Area,c(ends_with("_81"))) %>% 
  pivot_longer(ends_with("_81")) %>% 
  merge(.,land_carbon,by.x='value',by.y='lucode') 

carbon_stock2<-carbon_stock %>% 
  mutate(carbon_content=(c_soil+c_above+c_below+c_dead)*Shape_Area/1000) %>% 
  rename(code_MOS=value)
      
  
total_an<-carbon_stock2 %>% 
  group_by(name) %>% 
  summarise(total=sum(carbon_content))%>% 
  #extract(name,"annee","MOS*_81")
  separate (name, into = c("annee","rien"),sep="_") %>% 
  separate (annee, into = c("rien","annee"),sep="MOS") %>% 
  select((-1)) %>% 
  mutate(annee=as.numeric(annee)) %>% 
  plot('annee','total')

moyenne_groupe<-carbon_stock2 %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    TRUE ~ "Urban"
  )) %>% 
  select(-'code_MOS') %>% 
  separate (name, into = c("annee","rien"),sep="_") %>% 
  separate (annee, into = c("rien","annee"),sep="MOS") %>% 
  select((-rien)) %>% 
  mutate(annee=as.numeric(annee)) 

moyenne_groupe2_cate<-moyenne_groupe %>% 
  select(OBJECTID,annee,categorie) %>% 
  group_by(OBJECTID)%>% 
  arrange(OBJECTID,annee) %>% 
  pivot_wider(names_from = annee, values_from = categorie)

###
moyenne_groupe2_cate2<-moyenne_groupe2_cate
  for (index in seq(2,dim(moyenne_groupe2_cate2)[2]-1)){
    nom_avant<-names(moyenne_groupe2_cate2)[index]
    nom_apres<-names(moyenne_groupe2_cate2)[index+1]
    moyenne_groupe2_cate2<-moyenne_groupe2_cate2 %>% 
      rename(avant=names(moyenne_groupe2_cate)[index],
             apres=names(moyenne_groupe2_cate)[index+1]) %>% 
      mutate(flux=paste(avant,apres,sep=" --> ")) %>% 
      mutate(flux=ifelse(
        index==apres,avant,flux)) 
    #pour renommer les colonnes
    names(moyenne_groupe2_cate2)[match(c('avant','apres'), names(moyenne_groupe2_cate2))]<-c(nom_avant,nom_apres)
    names(moyenne_groupe2_cate2)[match(c('flux'), names(moyenne_groupe2_cate2))]<-paste0('flux_cate_',nom_avant)
    
  }

  moyenne_groupe2_content<-moyenne_groupe %>% 
  select(OBJECTID,annee,carbon_content) %>% 
  group_by(OBJECTID)%>% 
  arrange(OBJECTID,annee) %>% 
  pivot_wider(names_from = annee, values_from = carbon_content)

moyenne_groupe2_content2<-moyenne_groupe2_content
for (index in seq(2,dim(moyenne_groupe2_content2)[2]-1)){
  nom_avant<-names(moyenne_groupe2_content2)[index]
  nom_apres<-names(moyenne_groupe2_content2)[index+1]
  moyenne_groupe2_content2<-moyenne_groupe2_content2 %>% 
    rename(avant=names(moyenne_groupe2_content)[index],
           apres=names(moyenne_groupe2_content)[index+1]) %>% 
    #mutate(flux=(apres-avant)/(as.numeric(nom_apres)-as.numeric(nom_avant))) 
  mutate(flux=(apres-avant)) 
  #pour renommer les colonnes
  names(moyenne_groupe2_content2)[match(c('avant','apres'), names(moyenne_groupe2_content2))]<-c(nom_avant,nom_apres)
  names(moyenne_groupe2_content2)[match(c('flux'), names(moyenne_groupe2_content2))]<-paste0('flux_',nom_avant)
}

moyenne_groupe2_surface<-moyenne_groupe %>% 
  select(OBJECTID,Shape_Area) %>% 
  group_by(OBJECTID) %>% 
  summarise(Shape_Area=mean(Shape_Area))

  combine<- moyenne_groupe2_content2 %>%  
    merge(.,moyenne_groupe2_cate2,by.x='OBJECTID',by.y='OBJECTID') %>% 
  select(OBJECTID,starts_with('flux')) %>% 
    merge(.,moyenne_groupe2_surface,by.x='OBJECTID',by.y='OBJECTID')
  
  #initialisation du bilan
 bilan<- moyenne_groupe2_cate2 %>% 
   ungroup() %>% 
    select(starts_with('flux')) %>% 
   pivot_longer(.,seq(1,8)) 
 bilan<-unique(bilan$value) %>% 
   sort(.) %>% 
   as.data.frame(.) 
 names(bilan)<-'transition'
  
  for (annee in seq(2,9)) {
    tmp<-combine %>% 
      select(c(1,annee,annee+8,Shape_Area))
    
    tmp<-tmp %>% 
      rename(transition=names(tmp)[3]) %>% 
      rename(carbon=names(tmp)[2])
    
    tmp2<-tmp%>% 
      group_by(transition) %>% 
      summarise(total_carbon=sum(carbon)*100,
                total_area=sum(Shape_Area)) %>% 
      mutate(carbon_per_area=total_carbon/total_area)
    
    names(tmp2)[4]<-colnames(moyenne_groupe2_content2)[annee]

    bilan<-tmp2 %>% 
      select(1,4) %>% 
      left_join(bilan,.,by.x='transition',by.y='transition')
    }
  
 
 
bilan2<-bilan %>% 
  pivot_longer(-1)
  
carbon_per_surface<- ggplot(data=bilan2) +
  geom_hline(yintercept=0,color='grey')+
  geom_line(aes(x=as.numeric(name),y= value))+
    geom_point(aes(x=as.numeric(name),y= value))+
    theme_grey(base_size=12)+theme(axis.ticks = element_blank(),legend.position="bottom")+
    facet_wrap(vars(transition))+
    ggtitle('variation du stockage de carbone par m2')+
    labs(x = "Year") +
    labs(y = "kg de C par m2") 
ggsave("carbon_per_surface.png",plot=carbon_per_surface,width=297,height=210,units ='mm')
    
### variation totale de carbone
  
  bilan<- moyenne_groupe2_cate2 %>% 
    ungroup() %>% 
    select(starts_with('flux')) %>% 
    pivot_longer(.,seq(1,8)) 
  
  
  bilan<-unique(bilan$value) %>% 
    sort(.) %>% 
    as.data.frame(.) 
  names(bilan)<-'transition'
  
  for (annee in seq(2,9)) {
    tmp<-combine %>% 
      select(c(1,annee,annee+8,Shape_Area))
    
    tmp<-tmp %>% 
      rename(transition=names(tmp)[3]) %>% 
      rename(carbon=names(tmp)[2])
    
    tmp2<-tmp%>% 
      group_by(transition) %>% 
      summarise(total_carbon=sum(carbon)*100,
                total_area=sum(Shape_Area)) %>% 
      mutate(carbon_per_area=total_carbon/1000000)
    
    names(tmp2)[4]<-colnames(moyenne_groupe2_content2)[annee]
    
    bilan<-tmp2 %>% 
      select(1,4) %>% 
      left_join(bilan,.,by.x='transition',by.y='transition')
  }
  
  bilan2[is.nan(bilan2)]=0
  bilan2<-bilan %>% 
    pivot_longer(-1)   %>% 
    separate(transition,into=c('avant','apres'),sep=" --> ") 
    
  
  carbon_total<-  ggplot(data=bilan2) +
    geom_hline(yintercept=0,color='grey')+
    geom_line(aes(x=as.numeric(name),y= value))+
    geom_point(aes(x=as.numeric(name),y= value))+
    theme_grey(base_size=12)+theme(axis.ticks = element_blank(),legend.position="bottom")+
    facet_grid(apres~avant, drop=TRUE,labeller = label_both)+
    ggtitle('variations totales du stockage de carbone en île de France')+
    labs(x = "Year") +
    labs(y = "tonnes de C") 
  ggsave("carbon_total.png",plot=carbon_total,width=297,height=210,units ='mm') 
  
### variation totale de C en net
  bilan[is.na(bilan)]=0
  
  bilan2<-bilan %>% 
    pivot_longer(-1)   %>% 
    separate(transition,into=c('avant','apres'),sep=" --> ") 
    
    bilan2b<-bilan2 %>% 
      mutate(value_reciproque=value) %>% 
      mutate(value_reciproque=ifelse(avant==apres,0,value_reciproque)) %>% 
      select(-value)
    
    bilan2<-bilan2 %>% 
      left_join(bilan2b,by=c('name'='name','avant'='apres','apres'='avant')) %>% 
      mutate(value_net=value+value_reciproque)
    
    bilan3<-bilan2     %>%
      mutate(dx = pmin(avant, apres), dy = pmax(avant, apres))%>%
      distinct(dx, dy,name,.keep_all = TRUE) %>%
      select(-dx, -dy)
  
  carbon_total_net<-  ggplot(data=bilan3) +
    geom_hline(yintercept=0,color='grey')+
    geom_line(aes(x=as.numeric(name),y= value_net))+
    geom_point(aes(x=as.numeric(name),y= value_net))+
    theme_grey(base_size=12)+theme(axis.ticks = element_blank(),legend.position="bottom")+
    facet_grid(apres~avant, drop=TRUE,labeller = label_both)+
    ggtitle('variations totales nettes du stockage de carbone en île de France')+
    labs(x = "Year") +
    labs(y = "tonnes de C") 
  ggsave("carbon_total_net.png",plot=carbon_total_net,width=297,height=210,units ='mm') 


# evolution des ytransition en fonction du temps --------------------------
  #initialisation du bilan
  bilan<- moyenne_groupe2_cate2 %>% 
    ungroup() %>% 
    select(starts_with('flux')) %>% 
    pivot_longer(.,seq(1,8)) 
  
  bilan<-unique(bilan$value) %>% 
    sort(.) %>% 
    as.data.frame(.) 
  names(bilan)<-'transition'
  for (annee in seq(2,9)) {
    tmp<-combine %>% 
      select(c(1,annee,annee+8,Shape_Area))
    
    tmp<-tmp %>% 
      rename(transition=names(tmp)[3]) %>% 
      rename(carbon=names(tmp)[2])
    
    tmp2<-tmp%>% 
      group_by(transition) %>% 
      summarise(total_area=sum(Shape_Area)/1000000) %>% 
      mutate(flux=total_area/(as.numeric(colnames(moyenne_groupe2_content2)[annee+1])-as.numeric(colnames(moyenne_groupe2_content2)[annee])))
    names(tmp2)[3]<-colnames(moyenne_groupe2_content2)[annee]
    
    bilan<-tmp2 %>% 
      select(1,3) %>% 
      left_join(bilan,.,by.x='transition',by.y='transition')
  }
  
  bilan[is.na(bilan)]<-0
  bilan2<-bilan %>% 
    pivot_longer(-1)
  
  flux_surface<- bilan2 %>% 
    # filter((transition!="Urban --> Urban")&
    #          (transition!="Agric --> Agric")&
    #          (transition!="Agric2 --> Agric2")&
    #          (transition!="Semi-Natural --> Semi-Natural")&
    #          (transition!="Natural2 --> Natural2")&
    #          (transition!="Water --> Water")&
    #          (transition!="Banks --> Banks")&
    #          (transition!="Forest --> Forest")&
    #          (transition!="Coupes --> Coupes")&
    #          (transition!="Meadows --> Meadows")&
    #          (transition!="Natural --> Natural")) %>% 
    mutate(
      value=ifelse(transition=="Urban --> Urban",NA,value),
      value=ifelse(transition=="Agric --> Agric",NA,value),
      value=ifelse(transition=="Agric2 --> Agric2",NA,value),
      value=ifelse(transition=="Semi-Natural --> Semi-Natural",NA,value),
      value=ifelse(transition=="Natural2 --> Natural2",NA,value),
      value=ifelse(transition=="Water --> Water",NA,value),
      value=ifelse(transition=="Banks --> Banks",NA,value),
      value=ifelse(transition=="Forest --> Forest",NA,value),
      value=ifelse(transition=="Coupes --> Coupes",NA,value),
      value=ifelse(transition=="Meadows --> Meadows",NA,value),
      value=ifelse(transition=="Natural --> Natural",NA,value)) %>% 
  ggplot(data=.) +
    geom_line(aes(x=as.numeric(name),y= value))+
    geom_point(aes(x=as.numeric(name),y= value))+
    theme_grey(base_size=12)+theme(axis.ticks = element_blank(),legend.position="bottom")+
    facet_wrap(ncol=7,vars(transition))+
    ggtitle('flux (km2/year)')+
    labs(x = "Year") +
    labs(y = "km2/year") 
    #geom_hline(yintercept=0)
  ggsave("flux_surface.png",plot=flux_surface,width=297,height=210,units ='mm') 
  
## mos
  tmp<-mos.allege2 %>% group_by(annee,code_MOS) %>% summarise(Area=sum(Area)/1000000) %>% 
    #extract(name,"annee","MOS*_81")
    separate (annee, into = c("annee","rien"),sep="_") %>% 
    separate (annee, into = c("rien","annee"),sep="MOS") %>% 
    select((-1)) %>% 
    mutate(annee=as.numeric(annee)) %>% 
    mutate(categorie=case_when(
      code_MOS %in% c('1') ~ "1-Forêts",
      code_MOS %in% c('2') ~ "2-Milieux semi-naturels",
      code_MOS %in% c('3') ~ "3-Espaces agricoles",
      code_MOS %in% c('4') ~ "4-Eau",
      code_MOS %in% c('5') ~ "5-Espaces ouverts artificialisés",
      code_MOS %in% c('6') ~ "6-Habitat individuel",
      code_MOS %in% c('7') ~ "7-Habitat collectif",
      code_MOS %in% c('8') ~ "8-Activités",
      code_MOS %in% c('9') ~ "9-Equipements",
      code_MOS %in% c('10') ~ "10-Transports",
      code_MOS %in% c('11') ~ "11-Carrières, décharges etchantiers",
      TRUE ~ "Urban"
    )) 
  
  ggplot(data=tmp) +
    geom_line(aes(x=annee,y= Area))+
    #geom_point(aes(x=annee,y= Area))+
    theme_light(base_size=14)+theme(axis.ticks = element_blank(),legend.position="bottom")+
    facet_wrap(ncol=4,vars(categorie))+
    ggtitle('variation des surfaces')+
    labs(x = "Année") +
    labs(y = "Surface (km2)") 
  
  #verif que la surface totale est constante (NB: elle est en m2 dans le shp, et en klm2 ici)
  tmp %>% group_by(annee) %>% summarise(Area=sum(Area))
  
  

# avec SDRIF raster -------------------------------------------------------

mos_1982 <- raster(x = "../1- MOS 81 postes/mos_1982_81p.tif")
mos_1982 <- as.data.frame(mos_1982,xy=TRUE) %>% 
  rename('code_MOS'='mos_1982_81p') %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    code_MOS %in% c(seq(28,81)) ~ "Urban"
  ))
  
mos_1994 <- raster(x = "../1- MOS 81 postes/mos_1994_81p.tif")
mos_1994 <- as.data.frame(mos_1994,xy=TRUE)%>% 
  rename('code_MOS'='mos_1994_81p') %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    code_MOS %in% c(seq(28,81)) ~ "Urban"
  ))

mos_2003 <- raster(x = "../1- MOS 81 postes/mos_2003_81p.tif")
mos_2003 <- as.data.frame(mos_2003,xy=TRUE)%>% 
  rename('code_MOS'='mos_2003_81p') %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    code_MOS %in% c(seq(28,81)) ~ "Urban"
  ))

mos_2012 <- raster(x = "../1- MOS 81 postes/mos_2012_81p.tif")
mos_2012 <- as.data.frame(mos_2012,xy=TRUE)%>% 
  rename('code_MOS'='mos_2012_81p') %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    code_MOS %in% c(seq(28,81)) ~ "Urban"
  ))

mos_2017 <- raster(x = "../1- MOS 81 postes/mos_2017_81p.tif")
mos_2017 <- as.data.frame(mos_2017,xy=TRUE)%>% 
  rename('code_MOS'='mos_2017_81p') %>% 
  mutate(categorie=case_when(
    code_MOS %in% c(1,3) ~ "Forest",
    code_MOS %in% c(2) ~ "Coupes",
    code_MOS %in% c(seq(4,5),seq(11,12)) ~ "Banks",
    code_MOS %in% c(6,seq(8,10)) ~ "Agric",
    code_MOS %in% c(7) ~ "Meadows",
    code_MOS %in% c(seq(13,27)) ~ "Semi-Natural",
    code_MOS %in% c(seq(28,81)) ~ "Urban"
  ))

MOS_81_postes_raster<-mos_1982 %>% 
  rename(code_MOS_1982=code_MOS) %>% 
  rename(categorie_1982=categorie)
MOS_81_postes_raster$'code_MOS_1994'<-unlist(mos_1994$code_MOS)
MOS_81_postes_raster$'code_MOS_2003'<-mos_2003$code_MOS
MOS_81_postes_raster$'code_MOS_2012'<-mos_2012$code_MOS
MOS_81_postes_raster$'code_MOS_2017'<-mos_2017$code_MOS
MOS_81_postes_raster$'categorie_1994'<-mos_1994$categorie
MOS_81_postes_raster$'categorie_2003'<-mos_2003$categorie
MOS_81_postes_raster$'categorie_2012'<-mos_2012$categorie
MOS_81_postes_raster$'categorie_2017'<-mos_2017$categorie

n_export_1982 <- raster(x = "../4- Nutrient Delivery Ratio/2- Résultats/1- Sorties InVEST/1982_P_correction/n_export_1982.tif")
n_export_2017 <- raster(x = "../4- Nutrient Delivery Ratio/2- Résultats/1- Sorties InVEST/2017_P_correction/n_export_2017.tif")  

n_export_1982 <-  as.data.frame(n_export_1982,xy=TRUE) %>% 
  filter(!is.na(n_export_1982))



MOS_81_postes_raster$n_export_1982<-n_export_1982$n_export_1982
MOS_81_postes_raster2<-MOS_81_postes_raster %>% 
  filter(!is.na(categorie_1982)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_1982_30mm.tif")  
flood_1982 <-  as.data.frame(flood,xy=TRUE) %>% 
  filter(!is.na(Runoff_retention_m3_1982_30mm)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y)) 

flood <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_1994_30mm.tif")  
flood_1994 <-  as.data.frame(flood,xy=TRUE) %>% 
  filter(!is.na(Runoff_retention_m3_1994_30mm)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_2003_30mm.tif")  
flood_2003 <-  as.data.frame(flood,xy=TRUE) %>% 
  filter(!is.na(Runoff_retention_m3_2003_30mm)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_2012_30mm.tif")  
flood_2012 <-  as.data.frame(flood,xy=TRUE) %>% 
  filter(!is.na(Runoff_retention_m3_2012_30mm)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_2017_30mm.tif")  
flood_2017 <-  as.data.frame(flood,xy=TRUE) %>% 
  filter(!is.na(Runoff_retention_m3_2017_30mm)) %>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood<-flood_1982 
flood$'Runoff_retention_m3_1994_30mm'<-flood_1994$Runoff_retention_m3_1994_30mm
flood$'Runoff_retention_m3_2003_30mm'<-flood_2003$Runoff_retention_m3_2003_30mm
flood$'Runoff_retention_m3_2012_30mm'<-flood_2012$Runoff_retention_m3_2012_30mm
flood$'Runoff_retention_m3_2017_30mm'<-flood_2017$Runoff_retention_m3_2017_30mm

flood<-flood%>% 
  left_join(.,MOS_81_postes_raster2,by=c('x','y'))

flood_tmp <- raster(x = "/Users/vincentviguie/IDEFESE Dropbox/SE_historiques/7-Urban flood risk/Runoff_retention_m3_2017_30mm.tif")  

bassins_versant<-readOGR(dsn="../7-Urban flood risk/flood_risk_service_2017_30mm.shp")
bassins_versant2<-bassins_versant[, -(1:4)] 

test<-rasterize( bassins_versant2,flood_tmp) 
test2<-as.data.frame(test,xy=TRUE)
test3<-test2 %>% 
  filter(!is.na(layer))%>% 
  mutate(x=as.integer(x*10),
         y=as.integer(y))

flood_et_bassin<-flood%>% 
  left_join(.,test3,by=c('x','y'))


# regressions par bassin versant -------------------------------------------

#calcul des flux
tmp<-flood_et_bassin %>% 
  ungroup() %>% 
  group_by(layer) %>% 
  summarise(Runoff_mean_m3_1982_30mm=mean(Runoff_retention_m3_1982_30mm),
            Runoff_mean_m3_1994_30mm=mean(Runoff_retention_m3_1994_30mm),
            Runoff_mean_m3_2003_30mm=mean(Runoff_retention_m3_2003_30mm),
            Runoff_mean_m3_2012_30mm=mean(Runoff_retention_m3_2012_30mm),
            Runoff_mean_m3_2017_30mm=mean(Runoff_retention_m3_2017_30mm)) %>% 
  mutate(evolution=Runoff_mean_m3_2017_30mm-Runoff_mean_m3_1982_30mm)

flood_et_bassin2 <- flood_et_bassin %>% 
  left_join(.,tmp,by='layer')

r1 <- rasterize(cbind(flood_et_bassin2$x/10, flood_et_bassin2$y), flood_tmp, flood_et_bassin2$evolution)
plot(r1) 

##
flood_et_bassin3<-flood_et_bassin2 %>% 
  dplyr::select(evolution,categorie_1982,categorie_2017,layer) %>% 
  mutate(flux=paste(categorie_1982,categorie_2017,sep=" --> ")) %>% 
  mutate(flux=ifelse(
    categorie_1982==categorie_2017,categorie_1982,flux 
  )) %>% 
  mutate(flux=as.factor(flux)) %>% 
  filter(!is.na(flux)) %>% 
  group_by(flux,categorie_1982,categorie_2017) %>% 
  summarise(evolution=mean(evolution))

p <- ggplot(flood_et_bassin3, aes(x=categorie_2017, y=evolution)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_point()+
  facet_wrap('categorie_1982',labeller = label_both)
plot(p)

## evol par bassin versant et transition
flood_et_bassin4<-flood_et_bassin %>% 
  dplyr::select(-c(8,(10:13))) %>% 
  dplyr::select(-(8:12)) %>% 
  pivot_longer(-c(1,2,8)) %>% 
  separate (name, into = c("rien1","rien2","rien3","annee","rien5"),sep="_") %>% 
  dplyr::select(-c(4,5,6,8))
                
flood_et_bassin4b<-flood_et_bassin %>% 
  dplyr::select(-c(8,(10:13))) %>% 
  dplyr::select(-(3:7)) %>% 
  pivot_longer(-c(1,2,8))%>% 
  separate (name, into = c("rien1","annee"),sep="_") %>% 
  dplyr::select(-"rien1")

flood_et_bassin4<-flood_et_bassin4 %>% 
  left_join(.,flood_et_bassin4b,by=c('x','y','annee','layer'))

flood_et_bassin4<-flood_et_bassin4 %>% 
  rename(categorie=value.y,
         retention=value.x)

flood_et_bassin5<-flood_et_bassin4 %>% 
  group_by(layer,categorie,annee) %>% 
  summarise(retention=sum(retention))
  
  # mutate(diff_runoff_1982=Runoff_retention_m3_1994_30mm-Runoff_retention_m3_1982_30mm,
  #        diff_runoff_1994=Runoff_retention_m3_2003_30mm-Runoff_retention_m3_1994_30mm,
  #        diff_runoff_2003=Runoff_retention_m3_2012_30mm-Runoff_retention_m3_2003_30mm,
  #        diff_runoff_2012=Runoff_retention_m3_2017_30mm-Runoff_retention_m3_2012_30mm)

p <- ggplot(flood_et_bassin5, aes(x=annee, y=retention,group = layer)) + 
  #geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_line()+
  facet_wrap('categorie')
plot(p)

p <- ggplot(flood_et_bassin5, aes(x=categorie, y=retention,group = annee)) + 
  #geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_line()+
  facet_wrap('layer')
plot(p)

#### en fonction de la part de la categorie dans le bassin
#pour chaque bassin versant histogramme de l'évolution du runoff retention entre 1982 et 2017 dans chaque categorie
#pb : on voit que le runoff augmente dans l'urbain, et ne peut pas dire si c'est parce que l'urbain ets mieux, ou si c'est p[arce que plus d'urbain]
flood_et_bassin6<-flood_et_bassin5 %>%
  filter(annee==1982 | annee==2017) %>% 
  ungroup() %>% 
  pivot_wider(names_from=annee,values_from = retention) %>% 
  mutate(diff_retention=`2017`-`1982`) %>% 
  group_by(layer) %>% 
  mutate(retention_tot=sum(`1982`)) %>% 
  mutate(diff_retention_relat=diff_retention/retention_tot) %>% 
  ungroup() %>% 
  mutate(categorie=as.factor(categorie)) %>% 
  filter(!is.na(categorie)) %>% 
  mutate(layer=as.factor(layer))

p <- ggplot(flood_et_bassin6, aes(x=categorie, y=diff_retention,group=layer)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_bar(aes(fill=categorie),stat="identity")+
  facet_wrap('layer')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
plot(p)
ggsave("water_retention.png",plot=p,width=297,height=210,units ='mm') 

q <- ggplot(flood_et_bassin6, aes(x=layer, y=diff_retention,group=categorie)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_bar(aes(fill=layer),stat="identity")+
  facet_wrap('categorie')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  xlab('bassin versant')
plot(q)
ggsave("water_retention.png",plot=p,width=297,height=210,units ='mm') 

#### en fonction de la categorie initiale
#pour chaque bassin versant histogramme de l'évolution du runoff retention entre 1982 et 2017 dans chaque categorie initiale en 1982
flood_et_bassin7<-flood_et_bassin4 %>%
  ungroup() %>% 
  filter(annee==1982) %>% 
  rename(categorie_initiale=categorie) %>% 
  dplyr::select(x,y,categorie_initiale)

flood_et_bassin7<-flood_et_bassin4 %>%
  ungroup() %>% 
  left_join(.,flood_et_bassin7,by=c('x','y'))

flood_et_bassin7b<-flood_et_bassin7 %>% 
  filter((annee==1982 )| (annee==2017))%>% 
  ungroup() %>% 
    dplyr::select(-categorie) %>% 
  pivot_wider(names_from=annee,values_from = retention) %>% 
  mutate(diff_retention=`2017`-`1982`) %>% 
  group_by(layer,categorie_initiale) %>% 
  summarise(diff_retention=sum(diff_retention)) %>% 
  ungroup() %>% 
  mutate(categorie_initiale=as.factor(categorie_initiale)) %>% 
  filter(!is.na(categorie_initiale)) %>% 
  mutate(layer=as.factor(layer))

p <- ggplot(flood_et_bassin7b, aes(x=categorie_initiale, y=diff_retention,group=layer)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_bar(aes(fill=categorie_initiale),stat="identity")+
  facet_wrap('layer')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
plot(p)
ggsave("water_retention.png",plot=p,width=297,height=210,units ='mm') 

q <- ggplot(flood_et_bassin6, aes(x=layer, y=diff_retention,group=categorie)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_bar(aes(fill=layer),stat="identity")+
  facet_wrap('categorie')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  xlab('bassin versant')
plot(q)
#ggsave("water_retention.png",plot=p,width=297,height=210,units ='mm') 

#### le final: en fonction de chaque transition
flood_et_bassin8<-flood_et_bassin2 %>% 
  dplyr::select(layer,x,y,Runoff_retention_m3_1982_30mm,Runoff_retention_m3_2017_30mm,categorie_1982,categorie_2017) %>% 
  mutate(flux=paste(categorie_1982,categorie_2017,sep=" --> ")) %>% 
  mutate(flux=ifelse(
    categorie_1982==categorie_2017,categorie_1982,flux 
  )) %>% 
  mutate(flux=as.factor(flux)) %>% 
  filter(!is.na(flux)) %>% 
  group_by(layer,flux,categorie_1982,categorie_2017) %>% 
  summarise(evolution=sum(Runoff_retention_m3_2017_30mm-Runoff_retention_m3_1982_30mm)) %>%
  ungroup() %>% 
  mutate(layer=as.factor(layer))

p <- ggplot(flood_et_bassin8, aes(x=layer, y=evolution)) + 
  geom_hline(yintercept=0,color='grey')+
  #geom_violin()+
  geom_bar(aes(fill=layer),stat="identity")+
  facet_grid(categorie_2017~categorie_1982,labeller = label_both)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  xlab('Bassins versants')+
  ylab('Evolution du runoff retention (m3)')+
  ggtitle('Impact des transitions sur le runoff retention de chaque bassin versant')
plot(p)
ggsave("water_retention.png",plot=p,width=297,height=210,units ='mm') 

# p <- ggplot(flood_et_bassin3, aes(x='', y=evolution)) + 
#   geom_hline(yintercept=0,color='grey')+
#   #geom_violin()+
#   geom_boxplot(outlier.shape = NA)+
#   facet_grid(categorie_2017~categorie_1982,labeller = label_both)
# 
# plot(p)
# ggsave("flood.png",plot=p,width=297,height=210,units ='mm') 


regress<-combine %>% 
  lm(var_Carbone_per.comm~.,.) 
summary(regress)


### patrimoine_naturel
combine<- result_invest %>%  
  select(INSEE,patrimoine_naturel)%>% 
  merge(.,classification4,by.x='INSEE',by.y='INSEE') %>% 
  ungroup() %>% 
  select(-INSEE,-Urban)

regress<-combine %>% 
  lm(patrimoine_naturel~.,.) 
summary(regress)
  

