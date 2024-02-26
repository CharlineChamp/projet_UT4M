library(readr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
data=read_excel("Reponse_questionnaire_V2.xlsx", sheet="Avec NA")

#trail_participation_last_two_seasons
data$trail_participation_last_two_seasons=ifelse(data$trail_participation_last_two_seasons=="Oui",0,1)
data$trail_participation_last_two_seasons=as.factor(data$trail_participation_last_two_seasons)
ggplot(data, aes(x=trail_participation_last_two_seasons, fill=trail_participation_last_two_seasons))+
  theme_bw()+geom_bar() +
  labs(title = "Avez-vous participé à une ou plusieurs courses de trail \nlors des deux dernières saisons ? ", x = "Participation",                   
  y = "Nombre d'observations")+
  scale_fill_manual(values=c("seagreen1","indianred1"))+scale_x_discrete(breaks=c(0,1,NA),labels=c("Oui","Non","Non renseigné"))+ theme(legend.position = "none")

#La majorité a participé à une ou plusieurs courses de trail lors des deux dernières saisons. 

#distance_trail_participation_last_two_seasons
data$distance_trail_participation_last_two_seasons
data$distance_trail_participation_last_two_seasons=as.factor(data$distance_trail_participation_last_two_seasons)
levels(data$distance_trail_participation_last_two_seasons)
data$distance_trail_participation_last_two_seasons=fct_relevel(data$distance_trail_participation_last_two_seasons, c("< 40km", "De 40km à 80km", "> 80km"))
ggplot(data, aes(x=distance_trail_participation_last_two_seasons, fill=distance_trail_participation_last_two_seasons))+
  theme_bw()+geom_bar() +
  labs(title = "Sur quelle.s distance.s? ", x = "Distance",                   
       y = "Nombre d'observations")+
  scale_fill_manual(values=c("gold","chocolate1","brown2"))+
  theme(legend.position = "none",axis.text.x=element_text(angle=90,hjust=1))+scale_x_discrete(labels=c("< 40km", "De 40km à 80km", "> 80km","Non renseigné"))+ theme(legend.position = "none")


#plutôt homogène, tout type de distance
#pas mal de NA

#frequence_trail_participation_last_two_seasons_by_season
data$frequence_trail_participation_last_two_seasons_by_season
data$frequence_trail_participation_last_two_seasons_by_season=as.factor(data$frequence_trail_participation_last_two_seasons_by_season)
levels(data$frequence_trail_participation_last_two_seasons_by_season)
data$frequence_trail_participation_last_two_seasons_by_season=fct_relevel(data$frequence_trail_participation_last_two_seasons_by_season, 
                                                               c("1 à 3 courses / saison", "4 à 8 courses / saison", "> 8 courses / saison"))
ggplot(data, aes(x=frequence_trail_participation_last_two_seasons_by_season, fill=frequence_trail_participation_last_two_seasons_by_season))+
  theme_bw()+geom_bar() +
  labs(title = "A quelle fréquence par saison? ", x = "Fréquence",                   
       y = "Nombre d'observations")+
  scale_fill_manual(values=c("gold","chocolate1", "brown2"))+
  theme(legend.position = "none",axis.text.x=element_text(angle=90,hjust=1))

#majorité de 1 à 3 courses par saison 
#peu supérieur à 8 par saison


#which_races_UT4M_2023                   
vec_choix = c("Un des 20km","Un des 40km","80km Challenge","100km Master","160km Xtrem / 160km Challenge")
vec_nom_colonne = c("race_20","race_40", "race_80", "race_100", "race_160")
colonne = data$which_races_UT4M_2023

choix_multiples = function(data, choix, nom_colonne, colonne){
  matrice = matrix(nrow = dim(data)[1],ncol = length(choix))
  df = as.data.frame(matrice)
  for (j in 1 :length(choix)){
    vecteur = c()
    for (i in 1 : length(colonne)){
      if(is.na(colonne[i])==FALSE){
        if(str_detect(colonne[i],choix[j])==TRUE){
          vecteur = c(vecteur,0)
        }
        else{
          vecteur = c(vecteur,1)
        }
      }
      else{
        vecteur = c(vecteur,1)
      }
      
    }
    df[j]=vecteur
    
  }
  colnames(df) = nom_colonne
  return(df)
}
df = choix_multiples(data, vec_choix,vec_nom_colonne,colonne)
data = cbind(data,df)

df_moitie=data%>%select(race_20,race_40,race_80,race_100,race_160)

df_moitie=c(length(df_moitie$race_20)-sum(df_moitie$race_20),length(df_moitie$race_40)-sum(df_moitie$race_40),
            length(df_moitie$race_80)-sum(df_moitie$race_80),length(df_moitie$race_100)-sum(df_moitie$race_100),
            length(df_moitie$race_160)-sum(df_moitie$race_160))
vec_choix = c("Un des 20km","Un des 40km","80km Challenge","100km Master","160km Xtrem / 160km Challenge")
data_frame=as.data.frame(cbind(vec_choix,df_moitie))

data_frame$df_moitie=as.numeric(data_frame$df_moitie)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)
data_frame$vec_choix=fct_relevel(data_frame$vec_choix, c("Un des 20km","Un des 40km","80km Challenge","100km Master","160km Xtrem / 160km Challenge"))

ggplot(data_frame, aes(x=vec_choix,y=df_moitie, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Sur quelle.s course.s de l'UT4M êtes vous inscrits en 2023 ? ", x = "Courses",
       y = "Nombre d'observations")+
    scale_fill_manual(values=c("gold","deepskyblue","darkorchid",
                               "orange","brown2"))+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))

#majoritairement sur un des 40 et un des 160
#avec pas mal de 100km


#diet 
data$diet     
levels(data$diet)
data$diet=ifelse(data$diet=="Oui",0,1)
data$diet=as.factor(data$diet)
ggplot(data, aes(x=diet, fill=diet))+
  theme_bw()+geom_bar() +
  labs(title = "Suivez-vous un régime alimentaire particulier? ", x = "Régime alimentaire",                   
       y = "Nombre d'observations")+
  scale_fill_manual(values=c("seagreen1","indianred1"))+scale_x_discrete(breaks=c(0,1,NA),labels=c("Oui","Non","Non renseigné"))+ theme(legend.position = "none")

#pas trop de régime alimentaire spécifique


#which_diet 
vec_choix = c("Régime cétogène" ,"Régime fléxitarien","Régime sans gluten","Régime sans lactose","Régime végétalien","Régime végétarien")
vec_nom_colonne = c("regime_cetogene","regime_flexi", "regime_sans_glu", "regime_sans_lac", "regime_vegetalien","regime_vegetarien")
colonne = data$which_diet

choix_multiples = function(data, choix, nom_colonne, colonne){
  matrice = matrix(nrow = dim(data)[1],ncol = length(choix))
  df = as.data.frame(matrice)
  for (j in 1 :length(choix)){
    vecteur = c()
    for (i in 1 : length(colonne)){
      if(is.na(colonne[i])==FALSE){
        if(str_detect(colonne[i],choix[j])==TRUE){
          vecteur = c(vecteur,0)
        }
        else{
          vecteur = c(vecteur,1)
        }
      }
      else{
        vecteur = c(vecteur,1)
      }
      
    }
    df[j]=vecteur
    
  }
  colnames(df) = nom_colonne
  return(df)
}
df = choix_multiples(data, vec_choix,vec_nom_colonne,colonne)
data = cbind(data,df)

df_moitie=data%>%select(regime_cetogene,regime_flexi, regime_sans_glu, regime_sans_lac, regime_vegetalien,regime_vegetarien)

df_moitie=c(length(df_moitie$regime_cetogene)-sum(df_moitie$regime_cetogene),length(df_moitie$regime_flexi)-sum(df_moitie$regime_flexi),
            length(df_moitie$regime_sans_glu)-sum(df_moitie$regime_sans_glu),length(df_moitie$regime_sans_lac)-sum(df_moitie$regime_sans_lac),
            length(df_moitie$regime_vegetalien)-sum(df_moitie$regime_vegetalien),length(df_moitie$regime_vegetarien)-sum(df_moitie$regime_vegetarien))
data_frame=as.data.frame(cbind(vec_choix,df_moitie))

data_frame$df_moitie=as.numeric(data_frame$df_moitie)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)

ggplot(data_frame, aes(x=vec_choix,y=df_moitie, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Quel régime alimentaire suivez vous ? ", x = "Régime alimentaire",
       y = "Nombre d'observations")+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))


#beaucoup de végétarien 
#puis flexitarien en 2ème

#which_drink_during_training 
vec_choix = c("Boisson énergétique"  ,"Eau","Boisson sucrée","Je ne bois pas")
vec_nom_colonne = c("boiss_energie_training","eau_training", "boisson_sucre_training", "je_ne_bois_pas_training")
colonne = data$which_drink_during_training 

choix_multiples = function(data, choix, nom_colonne, colonne){
  matrice = matrix(nrow = dim(data)[1],ncol = length(choix))
  df = as.data.frame(matrice)
  for (j in 1 :length(choix)){
    vecteur = c()
    for (i in 1 : length(colonne)){
      if(is.na(colonne[i])==FALSE){
        if(str_detect(colonne[i],choix[j])==TRUE){
          vecteur = c(vecteur,0)
        }
        else{
          vecteur = c(vecteur,1)
        }
      }
      else{
        vecteur = c(vecteur,1)
      }
      
    }
    df[j]=vecteur
    
  }
  colnames(df) = nom_colonne
  return(df)
}
df = choix_multiples(data, vec_choix,vec_nom_colonne,colonne)
data = cbind(data,df)

df_moitie=data%>%select(boiss_energie_training,eau_training, boisson_sucre_training, je_ne_bois_pas_training)

df_moitie=c(length(df_moitie$boiss_energie_training)-sum(df_moitie$boiss_energie_training),length(df_moitie$eau_training)-sum(df_moitie$eau_training),
            length(df_moitie$boisson_sucre_training)-sum(df_moitie$boisson_sucre_training),length(df_moitie$je_ne_bois_pas_training)-sum(df_moitie$je_ne_bois_pas_training))
data_frame=as.data.frame(cbind(vec_choix,df_moitie))

data_frame$df_moitie=as.numeric(data_frame$df_moitie)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)

ggplot(data_frame, aes(x=vec_choix,y=df_moitie, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Pendant l'entraînement, vous buvez ? ", x = "Type de boisson",  
       y = "Nombre d'observations")+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))

#pendant l'entrainement, bois principalement eau et boisson énergétique
#peu qui boivent pas


#volume_drink_during_training      
data$volume_drink_during_training
data$volume_drink_during_training=as.factor(data$volume_drink_during_training)
levels(data$volume_drink_during_training)
data$volume_drink_during_training=fct_relevel(data$volume_drink_during_training, c("< 250mL / heure","Entre 250mL et 500mL / heure","> 500mL / heure" ))

ggplot(data, aes(x=volume_drink_during_training, fill=volume_drink_during_training))+
  theme_bw()+geom_bar() +
  labs(title = "Pendant l'entrainement, vous buvez? ", x = "Volume par heure",                   
       y = "Nombre d'observations")+
  scale_fill_manual(values=c("azure2","cadetblue2","cornflowerblue"))+
  theme(legend.position = "none")+scale_x_discrete(labels=c("< 250mL","Entre 250mL et 500mL","> 500mL" , "Non renseigné"))


#majorité entre 250 et 500ml


#which_drink_during_competition    
vec_choix = c("Boisson énergétique"  ,"Eau","Boisson sucrée","Je ne bois pas")
vec_nom_colonne = c("boiss_energie_compet","eau_compet", "boisson_sucre_compet", "je_ne_bois_pas_compet")
colonne = data$which_drink_during_competition

choix_multiples = function(data, choix, nom_colonne, colonne){
  matrice = matrix(nrow = dim(data)[1],ncol = length(choix))
  df = as.data.frame(matrice)
  for (j in 1 :length(choix)){
    vecteur = c()
    for (i in 1 : length(colonne)){
      if(is.na(colonne[i])==FALSE){
        if(str_detect(colonne[i],choix[j])==TRUE){
          vecteur = c(vecteur,0)
        }
        else{
          vecteur = c(vecteur,1)
        }
      }
      else{
        vecteur = c(vecteur,1)
      }
      
    }
    df[j]=vecteur
    
  }
  colnames(df) = nom_colonne
  return(df)
}
df = choix_multiples(data, vec_choix,vec_nom_colonne,colonne)
data = cbind(data,df)

df_moitie=data%>%select(boiss_energie_compet,eau_compet, boisson_sucre_compet, je_ne_bois_pas_compet)

df_moitie=c(length(df_moitie$boiss_energie_compet)-sum(df_moitie$boiss_energie_compet),length(df_moitie$eau_compet)-sum(df_moitie$eau_compet),
            length(df_moitie$boisson_sucre_compet)-sum(df_moitie$boisson_sucre_compet),length(df_moitie$je_ne_bois_pas_compet)-sum(df_moitie$je_ne_bois_pas_compet))
data_frame=as.data.frame(cbind(vec_choix,df_moitie))

data_frame$df_moitie=as.numeric(data_frame$df_moitie)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)

ggplot(data_frame, aes(x=vec_choix,y=df_moitie, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Pendant la compétition, vous buvez ? ", x = "Type de boisson",  
       y = "Nombre d'observations")+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))


#principalement de l'eau et de la boisson énergétique 
#personne ne bois pas 


#volume_drink_during_competition
data$volume_drink_during_competition
data$volume_drink_during_competition=as.factor(data$volume_drink_during_competition)
levels(data$volume_drink_during_competition)
data$volume_drink_during_competition=fct_relevel(data$volume_drink_during_competition, c("< 250mL / heure","Entre 250mL et 500mL / heure","> 500mL / heure" ))

ggplot(data, aes(x=volume_drink_during_competition, fill=volume_drink_during_competition))+
  theme_bw()+geom_bar() +
  labs(title = "Pendant la compétition, vous buvez? ", x = "Volume de boisson bu par heure",                   
       y = "Nombre d'observations")+
  scale_fill_manual(values=c("azure2","cadetblue2","cornflowerblue"))+
  theme(legend.position = "none")

#majorité entre 250 et 500ml

#global_mental_force 
data$global_mental_force
ggplot(data, aes(x=global_mental_force, fill=global_mental_force))+
  theme_bw()+geom_histogram() +
  labs(title = "Force mentale générale", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#sur 4 
#force mentale moyennement haute 

#confidence 
data$confidence
ggplot(data, aes(x=confidence, fill=confidence))+
  theme_bw()+geom_histogram() +
  labs(title = "Confiance ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#sur 4 
#confiance en eux moyennement haute 

#consistency
data$consistency
ggplot(data, aes(x=consistency, fill=consistency))+
  theme_bw()+geom_histogram() +
  labs(title = "Consistance ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#sur 4
#difficultés d'interprétation


#control   
data$control
ggplot(data, aes(x=control, fill=control))+
  theme_bw()+geom_histogram() +
  labs(title = "Contrôle ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#contôlent moyennement leur émotions, se laissent submerge un peu par certaines



#general_internal_speech  
data$general_internal_speech
ggplot(data, aes(x=general_internal_speech, fill=general_internal_speech))+
  theme_bw()+geom_histogram() +
  labs(title = "Utilisation du discours interne en général   ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")

#sur 5 : se parlent plutôt positivement



#training_internal_speech 
data$training_internal_speech
ggplot(data, aes(x=training_internal_speech, fill=training_internal_speech))+
  theme_bw()+geom_histogram() +
  labs(title = "Utilisation du discours interne à l'entrainement  ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")

#sur 5 : ont recours au discours interne pour parvenir aux compétitions  ( achanger en compétition, dico des données faux )


#competition_internal_speech    
data$competition_internal_speech
ggplot(data, aes(x=competition_internal_speech, fill=competition_internal_speech))+
  theme_bw()+geom_histogram() +
  labs(title = "Utilisation du discours interne en compétition  ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#sur 5 : un peu moins recours au discours interne aux entrainements( achanger en compétition, dico des données faux )



#general_precompetitive_anxiety 
data$general_precompetitive_anxiety
ggplot(data, aes(x=general_precompetitive_anxiety, fill=general_precompetitive_anxiety))+
  theme_bw()+geom_histogram() +
  labs(title = "Anxiété générale ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
# anxiété précompétitive très faibel (sur 4)




#somatic_anxiety 
data$somatic_anxiety
ggplot(data, aes(x=somatic_anxiety, fill=somatic_anxiety))+
  theme_bw()+geom_histogram() +
  labs(title = "Anxiété somatique  ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#anxiété somatique assez faible 
#ressenti physique de l'anxiété

#cognitive_anxiety 
data$cognitive_anxiety
ggplot(data, aes(x=cognitive_anxiety, fill=cognitive_anxiety))+
  theme_bw()+geom_histogram() +
  labs(title = "Anxiété cognitive  ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")
#anxiété mentale faible 

#addiction
data$addiction
ggplot(data, aes(x=addiction, fill=addiction))+
  theme_bw()+geom_histogram() +
  labs(title = "Addiction  ", x = "Score",                   
       y = "Nombre d'observations")+
  theme(legend.position = "none")+geom_vline(xintercept=24,linetype="dashed",color="red",size=1)

#beaucoup de sujets à risque de dépendance à l'exercice physique 
