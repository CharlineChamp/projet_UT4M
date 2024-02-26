# ----------------------------------------------------------------------------- #
# Chargement des packages
library(dplyr)       # Manipulation de données
library(magrittr)    # Opérateurs de pipe
library(readxl)      # Lecture de fichiers Excel
library(tidyverse)
library(openxlsx)

# ----------------------------------------------------------------------------- #
# Importation des données

# Charger les données de la glycémie
data_glycemie <- read.csv2("Données_Glycémie/data_glucoseV2.csv")

# Charger les données de score
data_score <- read.csv2("Données_course -psycho-alim/data_scoreV1.csv")

# Charger les données de dénivelé par km
data_denivele <- read_excel("Données_course -psycho-alim/denivele160_par_km.xlsx")

# ----------------------------------------------------------------------------- #
# Récupération des données uniquement pour les coureurs du 160km

particpant_160 <- c(1,3,5,7,8,9,12,15,16,17,20,22,24,25,28,30,33,34,35,36)

# Récupération des données de la glycémie
data_glycemie %<>% filter(course == "160 km")

# Charger les données de score
data_score %<>% filter(X1..Quel_est_votre_numero_de_participante_de %in% particpant_160)

# ----------------------------------------------------------------------------- #
# Nettoyage des données de score

data_score %<>% 
  # Sélectionner et renommer les colonnes utiles
  select(numero_participant = "X1..Quel_est_votre_numero_de_participante_de",
         section = "X2..lieu",
         perception = "X3..Perception_effort",
         inconfort = "X4..inconfort.m",
         plaisir = "X5..plaisir.r",
         di.freq = "X6..di.freq",
         di.effic = "X7..di.effic",
         desir = "X8..desir",
         conflit = "X9..conflit",
         resist = "X10..resist",
         desir1 = "X11..desir1", 
         date = "X23..DATE_SAISIE") %>%
  # Renommer les colonnes
  rename_with(~ tolower(gsub("X[0-9]+\\.{2}", "", .)), -numero_participant) %>%
  # Convertir la colonne date au format POSIXct
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"))


# Normalisation pour les valeurs entre 0 et 100
data_score %<>%
  mutate(perception = (perception - 0) / (100 - 0)) %>%
  mutate(inconfort = (inconfort - 0) / (100 - 0)) %>%
  mutate(plaisir = (inconfort - -50) / (50 - -50)) %>%
  mutate(di.freq = (di.freq - 0) / (100 - 0)) %>%
  mutate(di.effic = (di.effic - 0) / (100 - 0)) %>%
  mutate(conflit = (conflit - 0) / (100 - 0)) %>%
  mutate(resist = (resist - 0) / (100 - 0))

# ----------------------------------------------------------------------------- #
# Ajout de la df chrono

data_chrono <- read_excel("Chrono-course-Labo-Ut4M2023.xlsx", sheet = "Labo tps passage 160x")


# Suppresion des colonnes dont nous n'allons pas nous servir 
data_chrono <- data_chrono[,-c(2,3,4,7,8,9,10,11,13,14,15,17,19,20,21,22,24:29)]
colnames(data_chrono) <- c("numero_participant",
                           "Seyssins",
                           "Saint Nizier du Moucherotte",
                           "La Morte",
                           "Riouperoux",
                           "Croix de Chamrousse",
                           "St- Nazaire-les Eymes",
                           "Grenoble")

# Erreur d'importantio => récupération uniquement des heures
data_chrono$`Saint Nizier du Moucherotte` <- format(data_chrono$`Saint Nizier du Moucherotte`, format = "%H:%M:%S")
data_chrono$`La Morte` <- format(data_chrono$`La Morte`, format = "%H:%M:%S")

data_chrono %<>%
  pivot_longer(cols = -numero_participant,
               names_to = "section",
               values_to = "heure_ecoule") %>%
  arrange(numero_participant)

# Appliquer la transformation
data_chrono$heure_ecoule <- sapply(strsplit(data_chrono$heure_ecoule, ":"), function(x) {
  heures <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  secondes <- as.numeric(x[3])
  temps_total_en_heures <- heures + minutes/60 + secondes/3600
  return(temps_total_en_heures)
})

data_chrono %<>% mutate(heure_ecoule = ifelse(section == "Seyssins", 0, heure_ecoule))

# Date de départ
date_depart <- as.POSIXct("2023-07-21 09:00:00")

# Ajouter le temps de course à la date de départ pour obtenir les nouvelles dates
data_chrono$date_chrono <- date_depart + data_chrono$heure_ecoule * 3600  # Convertir les heures en secondes (1 heure = 3600 secondes)



data_score %<>% full_join(data_chrono, by = join_by("section","numero_participant"))
data_score %<>%
  mutate(nouvelle_date = as.POSIXct(ifelse(is.na(date_chrono), date, date_chrono))) %>%
  select(-date, -heure_ecoule, -date_chrono) %>%
  rename(date = nouvelle_date) %>%
  arrange(numero_participant, date)

# Ajouter la nouvelle ligne à la dataframe existante et calculer le temps écoulé en heures
data_score %<>%
  group_by(numero_participant) %>%
  mutate(temps_ecoule_en_heure = ifelse(section == "Seyssins", 0, difftime(date, lag(date), units = "hours")))

# Ajout de la colonne finisher 
abandon <- c(3,4,12,15,16,23,34,35,36)
data_score %<>%
  mutate(finisher = ifelse(!(numero_participant %in% abandon),"finisher","abandon"))

# Ajout de la date de référence pour plus tard
# Pas sur que cette ligne soit utile on verra plus tard
data_score %<>%
  group_by(numero_participant) %>%
  mutate(date_reference = case_when(
    section == "Seyssins" ~ as.POSIXct("2023-07-21 09:00:00", format = "%Y-%m-%d %H:%M:%S"),
    TRUE ~ lag(date, default = NA)
  ))

# ----------------------------------------------------------------------------- #
# Création de la dataframe des km/sections

data_km_section <- data.frame("km"= c(0:175))

data_km_section %<>% 
  mutate(section = case_when(
    km == 0 ~ "Seyssins",
    km > 0 & km <= 12 ~ "Saint Nizier du Moucherotte",
    km >= 13 & km <= 31 ~ "Saint Paul de Varces",
    km >= 32 & km <= 65 ~ "La Morte",
    km >= 66 & km <= 89 ~ "Riouperoux",
    km >= 90 & km <= 99 ~ "Croix de Chamrousse",
    km >= 100 & km <= 133 ~ "St- Nazaire-les Eymes",
    km >= 134 & km <= 148 ~ "Cabane du Bachasson",
    km >= 148 & km <= 175 ~ "Grenoble",
    TRUE ~ NA_character_
  )) %>% 
  mutate(section_psy = case_when(
    km >= 0 & km <= 12 ~ "Montee 1",
    km >= 24 & km <= 31 ~ "Descente 1",
    km >= 40 & km <= 65 ~ "Montee 2",
    km >= 81 & km <= 89 ~ "Descente 2",
    km >= 90 & km <= 99 ~ "Montee 3",
    km >= 109 & km <= 133 ~ "Descente 3",
    km >= 134 & km <= 148 ~ "Montee 4",
    km >= 167 & km <= 175 ~ "Descente 4",
    TRUE ~ NA_character_
  ))

# ----------------------------------------------------------------------------- #
# Jointure de data_score et data_km_section

data_km_section_score <- data_km_section %>% left_join(data_score, by = "section")
data_km_section_score$numero_participant <- as.numeric(data_km_section_score$numero_participant)
data_km_section_score <- data_km_section_score[order(data_km_section_score$numero_participant), ]
data_km_section_score %<>% 
  mutate(perception = ifelse(is.na(section_psy), NA, perception)) %>%
  mutate(inconfort = ifelse(is.na(section_psy), NA, inconfort)) %>%
  mutate(plaisir = ifelse(is.na(section_psy), NA, plaisir)) %>%
  mutate(di.freq = ifelse(is.na(section_psy), NA, di.freq)) %>%
  mutate(di.effic = ifelse(is.na(section_psy), NA, di.effic)) %>%
  mutate(desir = ifelse(is.na(section_psy), NA, desir)) %>%
  mutate(conflit = ifelse(is.na(section_psy), NA, conflit)) %>%
  mutate(resist = ifelse(is.na(section_psy), NA, resist)) %>%
  mutate(desir1 = ifelse(is.na(section_psy), NA, desir1))

 # ----------------------------------------------------------------------------- #
# Traitement des données de dénivelé 

data_denivele$denivele_positif <- c(data_denivele$denivele_pos[1], diff(data_denivele$denivele_pos))
data_denivele$denivele_negatif <- c(data_denivele$denivele_neg[1], diff(data_denivele$denivele_neg))

colnames(data_denivele) <- c("km",
                             "altitutide",
                             "denivele_pos_cum",
                             "denivele_neg_cum",
                             "denivele_positif",
                             "denivele_negatif")
# ----------------------------------------------------------------------------- #
# Jointure de data_km_section_score et denivele 

data_km_section_score_denivele <- data_km_section_score %>% 
  left_join(data_denivele, by = "km")

# ----------------------------------------------------------------------------- #
# Calculer le temps par km en fonction des dénivelés
# CalculAllure <- function(tSec,d=1,Dplus,Dmoins,dSec,DplusSec,DmoinsSec){
#   # Cette fonction calcule le temps mis par km
#   # tSec : temps mis pour la section
#   # d : distance, parcouru par défaut égale à 1
#   # Dplus : Dénivelé positif sur cette distance d
#   # Dmoins : Dénivelé négatif sur cette distance d
#   # dSec : distance de la section en km
#   # DplusSec : Dénivelé positif de la section 
#   # DmoinsSec : Dénivelé négatif de la section 
#   
#   # Calcul de la fonction nominale (VAP)
#   
#   # % de D+ sur la section
#   p.Dplus.Sec = DplusSec/(DplusSec+DmoinsSec)
#   
#   # distance D+ de la section
#   d.Dplus.Sec =p.Dplus.Sec*dSec 
#   
#   # Calcul de la pente moyenne de la section en %
#   pente.Dplus.Sec = DplusSec/(10*d.Dplus.Sec)
#   
#   d.Dmoins.Sec = dSec - d.Dplus.Sec
#   pente.Dmoins.Sec = DmoinsSec/(10*d.Dmoins.Sec)
#   
#   # Calcul de la vitesse nominale sur la section
#   v.n = (d.Dplus.Sec*CoefVAP(pente.Dplus.Sec)+d.Dmoins.Sec*CoefVAP(pente.Dmoins.Sec))/tSec
#   
#   # % de D+ sur la distance d
#   p.Dplus = Dplus/(Dplus+Dmoins)
#   
#   # Distance de montée
#   d.Dplus = p.Dplus * d
#   
#   if(d.Dplus != 0){
#     # Calcul pente sur la distance d
#     pente.Dplus = Dplus/(10*d.Dplus)
#   }else{
#     pente.Dplus = 0
#   }
#   
#   d.Dmoins = d - d.Dplus
#   if(d.Dmoins != 0){
#     pente.Dmoins = Dmoins/(10*d.Dmoins)
#   }else{
#     pente.Dmoins = 0
#   }
#   
#   # Calcul sur le kilomètre 
#   temps = d/v.n*(p.Dplus*CoefVAP(pente.Dplus)+(1-p.Dplus)*CoefVAP(pente.Dmoins))
#   
#   # On retourne en minute
#   return(temps*60)
#   
# }

CalculAllure <- function(tSec, d = 1, Dplus, Dmoins, dSec, DplusSec, DmoinsSec) {
  
  # Calcul des proportions de dénivelé sur la section
  p.Dplus.Sec <- DplusSec / (DplusSec + DmoinsSec)
  
  # Calcul des distances de montée et descente de la section
  d.Dplus.Sec <- p.Dplus.Sec * dSec
  d.Dmoins.Sec <- dSec - d.Dplus.Sec
  
  # Calcul des pentes moyennes de la section
  pente.Dplus.Sec <- DplusSec / (10 * d.Dplus.Sec)
  pente.Dmoins.Sec <- DmoinsSec / (10 * d.Dmoins.Sec)
  
  # Calcul de la vitesse nominale sur la section
  v.n <- (d.Dplus.Sec * CoefVAP(pente.Dplus.Sec) + d.Dmoins.Sec * CoefVAP(pente.Dmoins.Sec)) / tSec
  
  # Calcul des proportions de dénivelé sur la distance d
  p.Dplus <- Dplus / (Dplus + Dmoins)
  
  # Calcul des distances de montée et descente sur la distance d
  d.Dplus <- p.Dplus * d
  d.Dmoins <- d - d.Dplus
  
  # Calcul des pentes moyennes sur la distance d
  pente.Dplus <- ifelse(d.Dplus != 0, Dplus / (10 * d.Dplus), 0)
  pente.Dmoins <- ifelse(d.Dmoins != 0, Dmoins / (10 * d.Dmoins), 0)
  
  # Calcul du temps sur le kilomètre
  temps <- d / v.n * (p.Dplus * CoefVAP(pente.Dplus) + (1 - p.Dplus) * CoefVAP(pente.Dmoins))
  
  # Retourner en minutes
  return(temps * 60)
}


CoefVAP <- function(pente){
  # retourne le coefficient d'allure ajusté en fonction de la pente
  # cf. VAP Strava
  allure <- case_when(
    pente <= -17 ~ ((-pente + 9) / 26),
    pente <= -10 ~ ((-pente + 18) / 35),
    pente <= -5 ~ 0.9,
    pente <= 0 ~ (pente / 50 + 1),
    pente <= 5 ~ (pente / 25 + 1),
    pente <= 10 ~ ((3 * pente + 45) / 50),
    TRUE ~ ((1.9 * pente + 11) / 20)
  )
  return(allure)
}

data_km_section_score_denivele <- data_km_section_score_denivele %>%
  group_by(numero_participant, section) %>%
  mutate(
    km_section = n(),
    temps_par_km_minute = case_when(
      km == 0 ~ 0,
      TRUE ~ CalculAllure(tSec = temps_ecoule_en_heure, Dplus = denivele_positif, Dmoins = denivele_negatif, d = 1, dSec = km_section, DplusSec = sum(denivele_positif), DmoinsSec = sum(denivele_negatif))
    )
  ) %>%
  mutate(
    temps_par_km_minute = case_when(
      km == 175 ~ NA,
      TRUE ~ temps_par_km_minute
  ))
# ) %>%
#   mutate(
#     temps_par_km_minute = case_when(
#       km == 175 ~ temps_ecoule_en_heure * 60 - sum(temps_par_km_minute),
#       TRUE ~ temps_par_km_minute
#     )
#   )




# Créer une colonne "temps_en_minute_cumule" basée sur la distance parcourue
data_km_section_score_denivele <- data_km_section_score_denivele %>%
  group_by(numero_participant) %>%
  mutate(temps_en_minute_cumule = cumsum(temps_par_km_minute))

# Convertir temps_en_minute_cumule en secondes
data_km_section_score_denivele %<>%
  mutate(
    temps_en_seconde_cumule = temps_en_minute_cumule * 60,
    nouvelle_date = as.POSIXct("2023-07-21 09:00:00", format = "%Y-%m-%d %H:%M:%S") + lubridate::seconds(temps_en_seconde_cumule)
  ) %>%
  mutate(nouvelle_date = format(nouvelle_date, format="%Y-%m-%d %H:%M:%S")) %>%
  select(-date) %>%
  select(-temps_en_seconde_cumule)


# ----------------------------------------------------------------------------- #
# Calculer la moyenne de la glycémie par participant et par km


moy_glycemie <- data.frame(numero_participant = numeric(0), km = numeric(0), glucose = numeric(0))

for(i in 1:length(unique(data_km_section_score_denivele$numero_participant))){
  k <- unique(data_km_section_score_denivele$numero_participant)[i]
  km <- as.vector(data_km_section_score_denivele %>% filter(numero_participant == k) %>% select(km))$km
  cpt <- 0
  for(j in km){
    if(j == 0 | (j-1) != (cpt-1)){
      date <- data_km_section_score_denivele %>%
        filter(numero_participant == k) %>%
        filter(km == j)
      
      data_temp <- data_glycemie %>%
        filter(numero_participant == k) %>%
        filter(datetime == date$nouvelle_date)
      
      if(nrow(data_temp) != 0){
        nouvelle_val <- c(k, j, data_temp$glucose)
      }else{
        nouvelle_val <- c(k, j, NA)
      }
      
      moy_glycemie <- rbind(moy_glycemie, nouvelle_val)
      
    } else {
      date_debut <- data_km_section_score_denivele %>%
        filter(numero_participant == k) %>%
        filter(km == j - 1)
      
      date_fin <- data_km_section_score_denivele %>%
        filter(numero_participant == k) %>%
        filter(km == j)
      
      data_temp <- data_glycemie %>%
        filter(numero_participant == k) %>%
        filter(datetime >= date_debut$nouvelle_date & datetime <= date_fin$nouvelle_date)
      
      if(nrow(data_temp) != 0){
        nouvelle_val <- c(k, j, mean(data_temp$glucose))
      }else{
        nouvelle_val <- c(k, j, NA)
      }
      moy_glycemie <- rbind(moy_glycemie, nouvelle_val)
    }
    cpt <- cpt + 1
  }
}

colnames(moy_glycemie) <- c("numero_participant", "km", "moy_glucose")

# ----------------------------------------------------------------------------- #
# Jointure entre ka moyenne glycémique et notre data score 

data_km_section_score_glycemie <- data_km_section_score_denivele %>% left_join(moy_glycemie, by = c("numero_participant", "km"))

# ----------------------------------------------------------------------------- #
# Écrire le dataframe dans le fichier CSV
write.csv2(data_km_section_score_glycemie, file = "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/data_160V1.csv", row.names = FALSE, fileEncoding = "MacRoman")
write.xlsx(data_km_section_score_glycemie, file = "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/data_160V1.xlsx", rowNames = FALSE)
write.xlsx(data_km_section, file = "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/data_section160V1.xlsx", rowNames = FALSE)
