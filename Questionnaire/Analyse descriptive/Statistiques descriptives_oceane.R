library(readxl)
data <- read_excel("Data/Reponse_questionnaire_V2.xlsx")


library(ggplot2)

# Variable coach_for_trail

data$coach_for_trail = ifelse(data$coach_for_trail == "Oui", 0, 1)
data$coach_for_trail = as.factor(data$coach_for_trail)

ggplot(data = data, aes(coach_for_trail, fill = coach_for_trail))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Êtes-vous suivi par un coach dans votre préparation à vos objectifs de trail ?",x = "Présence d'un coach")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Présence d'un coach", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

#Très peu de NA, grande majorité non suivi par un coach

# Variable heart_rate_monitor_train

data$heart_rate_monitor_train = ifelse(data$heart_rate_monitor_train == "Oui", 0, 1)
data$heart_rate_monitor_train = as.factor(data$heart_rate_monitor_train)

ggplot(data = data, aes(heart_rate_monitor_train, fill = heart_rate_monitor_train))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Utilisez-vous un cardiofréquencemètre à l'entraînement ?",x = "Utilisation d'un cardiofréquencemètre")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Utilisation d'un cardiofréquencemètre", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

#Très peu de NA, beaucoup utilisent un cardiofréquencemètre à l'entraînement mais quand même une partie importante n'en utilise pas : environ 1/3

# Variable heart_rate_monitor_competition

data$heart_rate_monitor_competition = ifelse(data$heart_rate_monitor_competition == "Oui", 0, 1)
data$heart_rate_monitor_competition = as.factor(data$heart_rate_monitor_competition)

ggplot(data = data, aes(heart_rate_monitor_competition, fill = heart_rate_monitor_competition))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Utilisez-vous un cardiofréquencemètre en compétition ?",x = "Utilisation d'un cardiofréquencemètre")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Utilisation d'un cardiofréquencemètre", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

#Très peu de NA, preque autant en utilise pas que en utilise 1 : 3/8e

# Variable connected_platform
data$connected_platform = ifelse(data$connected_platform == "Oui", 0, 1)
data$connected_platform = as.factor(data$connected_platform)

ggplot(data = data, aes(connected_platform, fill = connected_platform))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Téléchargez-vous vos données d'entraînement sur une plateforme connectée ?",x = "Téléchargement des données d'entraînement")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Téléchargement des données d'entraînement", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

# Très peu de NA, Bcp utilisent une plateforme pour télécharger leur données d'entraînement : 5/6e

# Variable ITRA_rating_know

data$ITRA_rating_know = ifelse(data$ITRA_rating_know == "Oui", 0, 1)
data$ITRA_rating_know = as.factor(data$ITRA_rating_know)

ggplot(data = data, aes(ITRA_rating_know, fill = ITRA_rating_know))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Connaissez-vous votre côte ITRA ?",x = "Connaissance de la côte ITRA")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Connaissance de la côte ITRA", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

# Très peu de NA, 1/3 ne connaissent pas leur côte ITRA

# Variable ITRA_rating
data$ITRA_rating = as.numeric(data$ITRA_rating)

ggplot(data = data, aes(ITRA_rating, fill = ITRA_rating))+
  geom_histogram() + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Veuillez-indiquer cotre côte ITRA ?",x = "Côte ITRA")
  
ggplot(data = data, aes(ITRA_rating, fill = ITRA_rating))+
  geom_histogram() + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Veuillez-indiquer cotre côte ITRA ?",x = "Côte ITRA")+
  geom_vline(xintercept = 200, linetype='dashed', color='red', size = 1)

ggplot(data = data, aes(ITRA_rating, fill = ITRA_rating))+
  geom_histogram() + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Veuillez-indiquer cotre côte ITRA ?",x = "Côte ITRA")+ xlim(200, 800)

#Côte ITRA sont majoritairement entre 400 et 600

# Variable number_hours_practice_trail_phase_preparation

data$number_hours_practice_trail_phase_preparation = as.factor(data$number_hours_practice_trail_phase_preparation)
data$number_hours_practice_trail_phase_preparation = factor(data$number_hours_practice_trail_phase_preparation,levels=c(
  '⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine'))

ggplot(data = data, aes(number_hours_practice_trail_phase_preparation, fill = number_hours_practice_trail_phase_preparation))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Lors de votre phase de préparation à une course similaire à celle choisie pour l'UT4M 2023, \n en moyenne combien d'heures par semaine pratiquez-vous du trail ?",x = "Nombre d'heures par semaine")+
  scale_fill_manual (values = c("seagreen1", "gold", "sandybrown", "indianred1", "grey"), name = "Nombre d'heures par semaine", labels = c('⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine', "Non renseigné")) + 
  scale_x_discrete(labels = c('< ou = à 3 heures','Entre 3h et 7h','Entre 7h et 12h','> 12 heures', "Non renseigné")) + theme(legend.position = "none")

# Peu de NA la majorité s'entraine entre 3 et 7 heures et pas mal entre 7 et 12h et très peu plus de 12h

# Variable drop_practice_trail_phase_preparation

data$drop_practice_trail_phase_preparation = as.factor(data$drop_practice_trail_phase_preparation)
data$drop_practice_trail_phase_preparation = factor(data$drop_practice_trail_phase_preparation,levels=c('< 1000m / semaine','Entre 1000m et 2000m / semaine', '> 2000m / semaine'))

ggplot(data = data, aes(drop_practice_trail_phase_preparation, fill = drop_practice_trail_phase_preparation))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Lors de votre phase de préparation à une course similaire à celle choisie pour l'UT4M 2023, \n en moyenne combien de dénivelé effectuez-vous par semaine en trail ?",x = "Dénivelé par semaine")+
  scale_fill_manual (values = c("seagreen1", "sandybrown", "indianred1", "grey"), name = "Dénivelé par semaine", labels = c('< 1000m / semaine','Entre 1000m et 2000m / semaine', '> 2000m / semaine', "Non renseigné")) + 
  scale_x_discrete(labels = c('< 1000m','Entre 1000m et 2000m', '> 2000m', "Non renseigné")) + theme(legend.position = "none")

# Peu de NA, mieux réparti, globalement entre 1000 et 2000m un peu moins plus de 2000m

# Variable kilometres_practice_trail_phase_preparation

data$kilometres_practice_trail_phase_preparation = as.factor(data$kilometres_practice_trail_phase_preparation)
data$kilometres_practice_trail_phase_preparation = factor(data$kilometres_practice_trail_phase_preparation,levels=c(
  '< 20km / semaine','Entre 20km et 50km / semaine','Entre 50km et 80km / semaine','> 80km / semaine'))

ggplot(data = data, aes(number_hours_practice_trail_phase_preparation, fill = number_hours_practice_trail_phase_preparation))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Lors de votre phase de préparation à une course similaire à celle choisie pour l'UT4M 2023, \n en moyenne combien de kilomètres effectuez-vous en trail par semaine ?",x = "Nombre de kilomètres par semaine")+
  scale_fill_manual (values = c("seagreen1", "gold", "sandybrown", "indianred1", "grey"), name = "Nombre de kilomètres par semaine", labels = c('< 20km / semaine','Entre 20km et 50km / semaine','Entre 50km et 80km / semaine','> 80km / semaine', "Non renseigné")) + 
  scale_x_discrete(labels = c('< 20km','Entre 20km et 50km','Entre 50km et 80km','> 80km', "Non renseigné")) + theme(legend.position = "none")

# Idem que pour les heures, logique vu le contexte

# Variable number_hours_training_trail_phase_preparation

data$number_hours_training_trail_phase_preparation = as.factor(data$number_hours_training_trail_phase_preparation)
data$number_hours_training_trail_phase_preparation = factor(data$number_hours_training_trail_phase_preparation,levels=c(
  '⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine'))

ggplot(data = data, aes(number_hours_training_trail_phase_preparation, fill = number_hours_training_trail_phase_preparation))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Lors de votre phase de préparation à une course similaire à celle choisie pour l'UT4M 2023, \n en moyenne combien d'heures d'entraînement avez-vous par semaine ?",x = "Nombre d'heures par semaine")+
  scale_fill_manual (values = c("seagreen1", "gold", "sandybrown", "indianred1", "grey"), name = "Nombre d'heures par semaine", labels = c('⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine', "Non renseigné")) + 
  scale_x_discrete(labels = c('< ou = à 3 heures','Entre 3h et 7h','Entre 7h et 12h','> 12 heures', "Non renseigné")) + theme(legend.position = "none")

# Principalement entre 3 et 12h par semaine, très peu moins de 3h et plus de 12h

#-------------------------------------------------------------------------------

# Variable reason_medical_apointment_trail

library(stringr)
library(dplyr)

vec_choix = c("Gestion d'une blessure","Conseils nutritionnel","Conseils sur la prise de médicaments","Conseils sur la récupération","Conseils sur la préparation physique","Visite de contrôle et/ou obtention de certificat.s", "Prescription d'un bilan sanguin")
vec_nom_colonne = c("medical_apointment_injury","medical_apointment_nutrition", "medical_apointment_treatment", "medical_apointment_recuperation", "medical_apointment_physical_preparation", "medical_apointment_control_certificat", "medical_apointment_blood_test")
colonne = data$reason_medical_apointment_trail

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

df_medical_apointment=data%>%select(medical_apointment_injury,medical_apointment_nutrition, medical_apointment_treatment, medical_apointment_recuperation, medical_apointment_physical_preparation, medical_apointment_control_certificat, medical_apointment_blood_test)

df_medical_apointment=c(length(df_medical_apointment$medical_apointment_injury)-sum(df_medical_apointment$medical_apointment_injury),length(df_medical_apointment$medical_apointment_nutrition)-sum(df_medical_apointment$medical_apointment_nutrition),
            length(df_medical_apointment$medical_apointment_treatment)-sum(df_medical_apointment$medical_apointment_treatment),length(df_medical_apointment$medical_apointment_recuperation)-sum(df_medical_apointment$medical_apointment_recuperation),
            length(df_medical_apointment$medical_apointment_physical_preparation)-sum(df_medical_apointment$medical_apointment_physical_preparation), length(df_medical_apointment$medical_apointment_control_certificat)-sum(df_medical_apointment$medical_apointment_control_certificat),
            length(df_medical_apointment$medical_apointment_blood_test)-sum(df_medical_apointment$medical_apointment_blood_test))

vec_choix = c("Gestion d'une blessure","Conseils nutritionnel","Conseils sur la prise de médicaments","Conseils sur la récupération","Conseils sur la préparation physique","Visite de contrôle et/ou obtention de certificat.s", "Prescription d'un bilan sanguin")

data_frame=as.data.frame(cbind(vec_choix,df_medical_apointment))

data_frame$df_medical_apointment=as.numeric(data_frame$df_medical_apointment)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)

library(RColorBrewer)

ggplot(data_frame, aes(x=vec_choix,y=df_medical_apointment, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Quel.s était.ent le.s motif.s des consultations médicales \n pour la pratique du trail ? ", x = "Raison de la visite médicale",
       y = "Nombre d'observations")+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))

# Très nombreuses demandes de certificats, nombreuses blessures et très peu de prise de médoc, homogène sur le reste

# Variable have_this_illness

vec_choix = c("Hypertension artérielle","Diabète","Maladie des artères du cœur","Autre maladie cardiaque","Asthme","Autre maladie pulmonaire")
vec_nom_colonne = c("illness_hypertension","illness_diabetes", "illness_heart_artery_disease", "illness_other_heart_disease", "illness_asthma", "illness_other_lung_disease")
colonne = data$have_this_illness

df = choix_multiples(data, vec_choix,vec_nom_colonne,colonne)
data = cbind(data,df)

df_illness=data%>%select(illness_hypertension,illness_diabetes, illness_heart_artery_disease, illness_other_heart_disease, illness_asthma, illness_other_lung_disease)

df_illness=c(length(df_illness$illness_hypertension)-sum(df_illness$illness_hypertension),length(df_illness$illness_diabetes)-sum(df_illness$illness_diabetes),
                        length(df_illness$illness_heart_artery_disease)-sum(df_illness$illness_heart_artery_disease),length(df_illness$illness_other_heart_disease)-sum(df_illness$illness_other_heart_disease),
                        length(df_illness$illness_asthma)-sum(df_illness$illness_asthma), length(df_illness$illness_other_lung_disease)-sum(df_illness$illness_other_lung_disease))

vec_choix = c("Hypertension artérielle","Diabète","Maladie des artères du cœur","Autre maladie cardiaque","Asthme","Autre maladie pulmonaire")

data_frame=as.data.frame(cbind(vec_choix,df_illness))

data_frame$df_illness=as.numeric(data_frame$df_illness)
data_frame$vec_choix=as.factor(data_frame$vec_choix)
levels(data_frame$vec_choix)

library(RColorBrewer)

ggplot(data_frame, aes(x=vec_choix,y=df_illness, fill=vec_choix))+
  theme_bw()+geom_bar(stat="identity") +
  labs(title = "Présentez-vous actuellement une ou plusieurs des maladies suivantes ?", x = "Pathologie",
       y = "Nombre d'observations")+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1))

# 0 diabète, majorité d'asthme 

# Variable smoke

data$smoke = ifelse(data$smoke == "Oui", 0, 1)
data$smoke = as.factor(data$smoke)

ggplot(data = data, aes(smoke, fill = smoke))+
  geom_bar() + theme_bw() + 
  labs(y = "Nombre d'observations", title = "Fumez-vous ou avez-vous fumé régulièrement au cours de l'année écoulée ?",x = "Fumeur régulier")+
  scale_fill_manual (values = c("seagreen1", "indianred1"), name = "Fumeur régulier", labels = c("Oui", "Non", "Non renseigné")) + 
  scale_x_discrete(breaks = c(0,1,NA), labels = c("Oui", "Non", "Non renseigné")) + theme(legend.position = "none")

# Très très peu de fumeurs
# Biais de réponse

# Variable fulfillment

ggplot(data = data, aes(fulfillment, fill = fulfillment))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de l'épanouissement",x = "Score d'épanouissement")

#Globalement épanouie

# Variable priority

ggplot(data = data, aes(priority, fill = priority))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure des priorités",x = "Score de priorités")

# Le trail leur pourri pas la vie, c'est pas leur priorité, il arrive a bien gérer leur vie personnelle

# Variable intrinsic_motivation

ggplot(data = data, aes(intrinsic_motivation, fill = intrinsic_motivation))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de la motivation intrinsèque",x = "Score de motivation intrinsèque")

# beaucoup pratique avec plaisir et envie personnelle

# Variable identified_motivation

ggplot(data = data, aes(identified_motivation, fill = identified_motivation))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de la motivation identifiée",x = "Score de motivation identifiée")

# Globalement bien, apporte du positif dans sa vie, donne un sens

# Variable integrated_motivation

ggplot(data = data, aes(integrated_motivation, fill = integrated_motivation))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de la motivation intégrée",x = "Score de motivation intégrée")

# Globalement bon, sport qui apporte vraiment, fait parti de leur identité

# Variable introjected_motivation

ggplot(data = data, aes(introjected_motivation, fill = introjected_motivation))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de la motivation introjectée",x = "Score de motivation introjectée")

#  Ils ne sont pas pousser à continuer par le fait que d'arreter ce soit mal


# Variable external_motivation

ggplot(data = data, aes(external_motivation, fill = external_motivation))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure de la motivation externe",x = "Score de motivation externe")

# Vraiment aucune motivation externe motivation pas d'origine extérieur, motivation interne


#-------------------------------------------------------------------------------

# Variable obsessive_hobbies

ggplot(data = data, aes(obsessive_hobbies, fill = obsessive_hobbies))+
  geom_histogram(alpha= 0.6) + theme_bw()+ 
  labs(y = "Nombre d'observations", title = "Mesure des passions obsessives",x = "Score des passions obsessives")

# Passion obsessive moyenne pour le trail, globalement faible, pratique mi-saine et pas par obsession totale mais un peu quand même, peu de cas extremement obsessifs
