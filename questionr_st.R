
library(tidyverse)
library(questionr)
library(forcats) # pour recodage,de données construction de classe, fusion de variables etc..
library(nycflights13) # pour diplyr

#HIDE & SEEK

# data(rp2012)
# data(hdv2003)
# hdv2003$qualif
# levels(hdv2003$qualif)
# hdv2003$qualif[hdv2003$qualif == "Ouvrier specialise"] <- "Ouvrier qualifie"


data(flights)
# data(airports)
# data(airlines)


#tableau retard mensuel par aéroport de destination 
tab_retard_IAH<-flights %>% 
  filter(dest=="IAH") %>% 
  group_by(origin,month) %>% 
  summarise (moy_retard = mean(arr_delay)) %>% 
  mutate(moy_corr=ifelse(is.na(moy_retard),0,moy_retard)) %>% 
  select(-moy_retard) %>% 
  pivot_wider(names_from = origin, values_from=moy_corr) # %>% 
  # pivot_longer(c("EWR","JFK","LGA"),names_to = "origines", values_to="moyenne_retard") %>% 
  # arrange(origines,month)


test<-freq(flights$dest)

tab_retard_ABQ<-flights %>% 
  filter(dest=="ABQ") %>% 
  group_by(origin,month) %>% 
  summarise (moy_retard = mean(arr_delay)) %>% 
  mutate(moy_corr=ifelse(is.na(moy_retard),0,moy_retard)) %>% 
  select(-moy_retard) %>% 
  pivot_wider(names_from = origin, values_from=moy_corr) # %>% 


# fonctions

construire_un_tab_mois_ori_des_retards <- function (tab_vol,destination_vol) {
  tab_retard<-tab_vol %>% 
    filter(dest==destination_vol) %>% 
    group_by(origin,month) %>% 
    summarise (moy_retard = mean(arr_delay)) %>% 
    mutate(moy_corr=ifelse(is.na(moy_retard),0,moy_retard)) %>% 
    select(-moy_retard) %>% 
    pivot_wider(names_from = origin, values_from=moy_corr) # %>% 
  
  return(tab_retard)
}

tab_MIA<-construire_un_tab_mois_ori_des_retards(flights,"MIA")

construire_tab_mois_ori_retards <- function (destination_vol) {
  tab_retard<-flights %>% 
    filter(dest==destination_vol) %>% 
    group_by(origin,month) %>% 
    summarise (moy_retard = mean(arr_delay)) %>% 
    mutate(moy_corr=ifelse(is.na(moy_retard),0,moy_retard)) %>% 
    select(-moy_retard) %>% 
    pivot_wider(names_from = origin, values_from=moy_corr) # %>% 
  
  return(tab_retard)
}

tab_MIA_2<-construire_tab_mois_ori_retards("MIA")

## Boucles

# creer un sous-repertoire "sorties" dans le repertoire projet

liste_aeroport_dest <- unique(flights$dest) 
# fabrication d'un vecteur sans doublons contenant les valeurs de flights$dest

# calcul avec uns boucle de la somme des retards depuis "EWR"

# fabrication d'un vecteur de NA pour enregistrer les sorties"

sum_retard_EWR<-rep(NA,length(liste_aeroport_dest))
list_sauve_tab<-list(NA)

 i<-5  
for (i in 1:15) {
    aeroport_dest<-liste_aeroport_dest[i]
    temp<-construire_un_tab_mois_ori_des_retards(flights,aeroport_dest)
    # list_sauve_tab[i]<-temp
    save(temp,file=paste("tableau_retard_",aeroport_dest,".rdata",sep=""))
    write.csv2(temp,file=paste("tableau_retard_",aeroport_dest,".csv",sep=""))
    sum_retard_EWR[i]<-sum(temp$EWR)
}

aeroport_dest<-liste_aeroport_dest[105]
temp2<-construire_un_tab_mois_ori_des_retards(flights,aeroport_dest)



names(sum_retard_EWR)<-liste_aeroport_dest
