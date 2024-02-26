# ----------------------------------------------------------------------------- #
# Importation des données récoltées par le questionnaire

library(readxl)
library(dplyr)
library(magrittr)
library(openxlsx)
library(stringr)

vecteur_guess <- rep("guess", times = 143)
types_colonnes <- c("date", "date",vecteur_guess)
df <- read_excel("dataset_V0.xlsx", col_types = types_colonnes)

# Cette commande est réalisée pour que nous puissons garder en mémoire les
# données de base
data <- df

# ----------------------------------------------------------------------------- #
# Retrait de la première ligne

data <-  data[-1,]

# ----------------------------------------------------------------------------- #
# Traitement nom de dossier Vide

library(dplyr)
  
# Remplace les noms de dossier qui contiennent NA et qui ont des données par un 
# nom de dossier de type NA_numerodeligne
# Suppression des dossiers qui ne contiennent rien (11cas) ou qui ne contiennent que le nom de dossier(6cas)`

data %<>%
  filter(!is.na(Q1) & !is.na(Q2)) %>%
  distinct() %>%
  mutate(Q64 = ifelse(is.na(Q64) & !is.na(Q1) & !is.na(Q2), paste0("NA_",row_number()), Q64))


# ----------------------------------------------------------------------------- #
# Doublons 
# Cas : Finished == True et Finished == False 

# Identifiez les doublons par rapport aux variables noms de dossiers
doublons <- data %>%
  group_by(Q64) %>%
  filter(n() > 1)  # Conservez uniquement les groupes avec plus d'une observation


# Sélection parmi les doublons des données où Finished == False et Finished == True
false_true <- doublons %>%
  group_by(Q64) %>%
  filter(all(Finished == "False") | Finished == "True") 

# Sélection des cas à supprimer contenant des doublons identifiés
supprimer_false_true <- doublons %>%
  anti_join(false_true)

# Suppression des doublons identifiés dans la dataframe général
data %<>% anti_join(supprimer_false_true)

# ----------------------------------------------------------------------------- #
# Doublons 
# Cas : Finished == False 

# Identifiez les doublons par rapport aux variables noms de dossiers
# Ici nous regardons par rapport à nos nouveaux doublons
doublons_2 <- data %>%
  group_by(Q64) %>%
  filter(n() > 1)  # Conservez uniquement les groupes avec plus d'une observation

# Calcul des cas 
doublons_2_false <- doublons_2  %>% filter(Finished == "False")

# On gère la ou on a Finished == False pour les deux 
# Sélection du meilleur Progress parmi les Finished == False
# Lorsqu'on a la même valeur pour les deux Finished False alors on garde les deux 
# 1 cas que nous devons dcp gérer
false_false <- doublons_2_false %>%
  group_by(Q64) %>%
  filter(Progress == max(Progress))

# Sélection des cas à supprimer contenant des doublons identifiés
supprimmer_false_false <- anti_join(doublons_2_false, false_false)

# Suppression des doublons identifiés dans la dataframe général
data %<>% anti_join(supprimmer_false_false)

# ----------------------------------------------------------------------------- #
# Doublons 
# Cas : Finished == True

doublons_2_true <- doublons_2  %>% filter(Finished == "True")

# On gère la ou on a Finished == True pour les deux et Progress = 100 (20 cas)
# Ses cas sont à traiter plus tard 
true_true <- doublons_2_true %>%
  group_by(Q64) %>%
  filter(StartDate == min(StartDate))

# Sélection des cas à supprimer contenant des doublons identifiés
supprimmer_true_true <- anti_join(doublons_2_true, true_true)

# Suppression des doublons identifiés dans la dataframe général
data %<>% anti_join(supprimmer_true_true)

# ----------------------------------------------------------------------------- #
# Retrait des 10 premières colonnes 

data <- data[, -c(1:10)]



# ----------------------------------------------------------------------------- #
# Renommage des colonnes avant regroupement 

noms_colonnes <- c("file_number",
                   "sex",
                   "age",
                   "weight", 
                   "max_weight",
                   "min_weight",
                   "size",
                   "weekly_volume_profession",
                   "start_trail",
                   "other_practice", 
                   "other_practice_detail",
                   "time_proportion_other_practice",
                   "coach_for_trail",
                   "heart_rate_monitor_train",
                   "heart_rate_monitor_competition",
                   "connected_platform",
                   "ITRA_rating_know",
                   "ITRA_rating",
                   "number_hours_practice_trail_phase_preparation",
                   "drop_practice_trail_phase_preparation",
                   "kilometres_practice_trail_phase_preparation",
                   "number_hours_training_trail_phase_preparation",
                   "trail_participation_last_two_seasons",
                   "distance_trail_participation_last_two_seasons",
                   "frequence_trail_participation_last_two_seasons_by_season",
                   "which_races_UT4M_2023",
                   "diet", 
                   "which_diet",
                   "which_drink_during_training",
                   "volume_drink_during_training",
                   "which_drink_during_competition",
                   "volume_drink_during_competition",
                   "which_eat_during_training",
                   "frequence_hours_eat_during_training",
                   "which_eat_during_competition",
                   "frequence_hours_eat_during_competition",
                   "trail_injury_illness",
                   "trail_health_problem",
                   "other_trail_health_problem",
                   "trail_health_problem_involve_no_training",
                   "time_trail_health_problem_involve_no_training",
                   "frequence_medical_apointment_trail",
                   "reason_medical_apointment_trail",
                   "have_this_illness",
                   "smoke",
                   "fulfillment_1",
                   "fulfillment_2",
                   "fulfillment_3",
                   "fulfillment_4",
                   "fulfillment_5",
                   "fulfillment_6",
                   "fulfillment_7",
                   "fulfillment_8",
                   "priority_1",
                   "priority_2",
                   "priority_3",
                   "priority_4",
                   "priority_5",
                   "priority_6",
                   "priority_7",
                   "priority_8",
                   "priority_9",
                   "priority_10",
                   "priority_11",
                   "priority_12",
                   "motivation_1",
                   "motivation_2",
                   "motivation_3",
                   "motivation_4",
                   "motivation_5",
                   "motivation_6",
                   "motivation_7",
                   "motivation_8",
                   "motivation_9",
                   "motivation_10",
                   "motivation_11",
                   "motivation_12",
                   "motivation_13",
                   "motivation_14",
                   "motivation_15",
                   "motivation_16",
                   "motivation_17",
                   "motivation_18",
                   "motivation_19",
                   "motivation_20",
                   "mental_force_1",
                   "mental_force_2",
                   "mental_force_3",
                   "mental_force_4",
                   "mental_force_5",
                   "mental_force_6",
                   "mental_force_7",
                   "mental_force_8",
                   "mental_force_9",
                   "mental_force_10",
                   "mental_force_11",
                   "mental_force_12",
                   "mental_force_13",
                   "mental_force_14",
                   "internal_speech_1",
                   "internal_speech_2",
                   "internal_speech_3",
                   "internal_speech_4",
                   "internal_speech_5",
                   "internal_speech_6",
                   "internal_speech_7",
                   "internal_speech_8",
                   "precompetitive_anxiety_1",
                   "precompetitive_anxiety_2",
                   "precompetitive_anxiety_3",
                   "precompetitive_anxiety_4",
                   "precompetitive_anxiety_5",
                   "precompetitive_anxiety_6",
                   "precompetitive_anxiety_7",
                   "precompetitive_anxiety_8",
                   "precompetitive_anxiety_9",
                   "precompetitive_anxiety_10",
                   "hobbies_1",
                   "hobbies_2",
                   "hobbies_3",
                   "hobbies_4",
                   "hobbies_5",
                   "hobbies_6",
                   "hobbies_7",
                   "hobbies_8",
                   "hobbies_9",
                   "hobbies_10",
                   "hobbies_11",
                   "hobbies_12",
                   "addiction_1",
                   "addiction_2",
                   "addiction_3",
                   "addiction_4",
                   "addiction_5",
                   "addiction_6"
                   )

colnames(data) <- noms_colonnes

# ----------------------------------------------------------------------------- #
# Création d'une fonction regroupement

library(doBy)

regroupement <- function(data, liste_niveaux, changement = FALSE) {
  nb_col <- length(colnames(data))
  data <- unlist(data)
  data <- factor(data)
  if(changement == FALSE){
    liste_numero <- as.numeric(seq(0, 1 , by = 1/(length(levels(data))-1)))
  }else{
    liste_numero <- as.numeric(seq(1, 0, by = - 1/(length(levels(data))-1)))
  }
  data <- recodeVar(data, liste_niveaux, liste_numero)
  data <- matrix(data, nrow = 359, ncol = nb_col)
  data <- as.data.frame(data)
  data <- sapply(data, as.numeric)
  return(data)
}


# ----------------------------------------------------------------------------- #
# Regroupement variables fulfillment

data_fulfillment <- data[, c(which(colnames(data)=="fulfillment_1"):which(colnames(data)=="fulfillment_8"))]

liste_niveaux <-  c("Pas d'accord du tout", 
                    "Pas d'accord", 
                    "Plutôt pas d'accord", 
                    "Neutre", 
                    "Plutôt d'accord", 
                    "D'accord", 
                    "Tout à fait d'accord")

data_fulfillment <- regroupement(data_fulfillment,liste_niveaux)
fulfillment <- rowSums(data_fulfillment) / 8


# ----------------------------------------------------------------------------- #
# Regroupement variables priority

data_priority <- data[, c(which(colnames(data)=="priority_1"):which(colnames(data)=="priority_12"))]

liste_niveaux <-  c("Pas d'accord du tout", 
                    "Pas d'accord", 
                    "Plutôt pas d'accord", 
                    "Neutre", 
                    "Plutôt d'accord", 
                    "D'accord", 
                    "Tout à fait d'accord")

data_priority <- regroupement(data_priority,liste_niveaux)
priority <- rowSums(data_priority) / 12


# ----------------------------------------------------------------------------- #
# Regroupement variables motivation

data_motivation <- data[, c(which(colnames(data)=="motivation_1"):which(colnames(data)=="motivation_20"))]

liste_niveaux <-  c("Pas vrai du tout", 
                    "Très peu vrai", 
                    "Un peu vrai", 
                    "Moyennement vrai", 
                    "Assez vrai", 
                    "Fortement vrai", 
                    "Complétement vrai")

data_motivation <- regroupement(data_motivation ,liste_niveaux)

intrinsic_motivation <- rowSums(data_motivation[, c(1,6,11,16)])/4
identified_motivation <- rowSums(data_motivation[, c(3,8,13,18)])/4
integrated_motivation <- rowSums(data_motivation[, c(2,7,12,17)])/4
introjected_motivation <- rowSums(data_motivation[, c(4,9,14,19)])/4
external_motivation <- rowSums(data_motivation[, c(5,10,15,20)])/4

motivation <- data.frame(intrinsic_motivation, identified_motivation,  integrated_motivation, introjected_motivation, external_motivation)

# ----------------------------------------------------------------------------- #
# Regroupement variables mental_force

data_mental_force <- data[, c(which(colnames(data)=="mental_force_1"):which(colnames(data)=="mental_force_14"))]

data_mental_force_class <- data_mental_force[, c(1,3,5,6,11,12,13,14)]
data_mental_force_chg <- data_mental_force[, c(2,4,7,8,9,10)]

liste_niveaux <-  c("Pas du tout vrai", 
                    "Pas vrai", 
                    "Vrai", 
                    "Tout à fait vrai")

# changement pour 2, 4, 7, 8, 9 et 10 
data_mental_force_class <- regroupement(data_mental_force_class ,liste_niveaux)
data_mental_force_chg <- regroupement(data_mental_force_chg ,liste_niveaux, T)

# Regroupement 
data_mental_force <- data.frame(data_mental_force_class[,1],
                                 data_mental_force_chg[,1],
                                 data_mental_force_class[,2],
                                 data_mental_force_chg[,2],
                                 data_mental_force_class[,3],
                                 data_mental_force_class[,4],
                                 data_mental_force_chg[,c(3:6)],
                                 data_mental_force_class[, c(5:8)])

global_mental_force <- rowSums(data_mental_force)/14
confidence <- rowSums(data_mental_force[, c(13,5,11,6,14,1)])/6
consistency <- rowSums(data_mental_force[, c(3,12,8,10)])/4
control <- rowSums(data_mental_force[, c(2,4,9,7)])/4

mental_force <- data.frame(global_mental_force, confidence, consistency, control)


# ----------------------------------------------------------------------------- #
# Regroupement variables internal_speech

data_internal_speech <- data[, c(which(colnames(data)=="internal_speech_1"):which(colnames(data)=="internal_speech_8"))]

liste_niveaux <-  c("Jamais", 
                    "Rarement", 
                    "Parfois", 
                    "Souvent", 
                    "Toujours")

data_internal_speech <- regroupement(data_internal_speech ,liste_niveaux)

general_internal_speech <- rowSums(data_internal_speech)/8
competition_internal_speech <- rowSums(data_internal_speech[, c(1:4)])/4
training_internal_speech <- rowSums(data_internal_speech[, c(5:8)])/4

internal_speech <- data.frame(general_internal_speech , training_internal_speech , competition_internal_speech)


# ----------------------------------------------------------------------------- #
# Regroupement variables precompetitive_anxiety

data_precompetitive_anxiety <- data[, c(which(colnames(data)=="precompetitive_anxiety_1"):which(colnames(data)=="precompetitive_anxiety_10"))]

liste_niveaux <-  c("Pas du tout", 
                    "Un peu", 
                    "Moyennement", 
                    "Beaucoup")

data_precompetitive_anxiety <- regroupement(data_precompetitive_anxiety ,liste_niveaux)

general_precompetitive_anxiety <- rowSums(data_precompetitive_anxiety)/10
somatic_anxiety <- rowSums(data_precompetitive_anxiety[, c(1,4,7,9,10)])/5
cognitive_anxiety <- rowSums(data_precompetitive_anxiety[, c(2,3,5,6,8)])/5

precompetitive_anxiety <- data.frame(general_precompetitive_anxiety, somatic_anxiety, cognitive_anxiety)

# ----------------------------------------------------------------------------- #
# Regroupement variables hobbies


data_hobbies <- data[, c(which(colnames(data)=="hobbies_1"):which(colnames(data)=="hobbies_12"))]

liste_niveaux <-  c("Fortement en désaccord", 
                    "Pas d'accord", 
                    "Plutôt pas d'accord", 
                    "Neutre", 
                    "Plutôt d'accord", 
                    "D'accord", 
                    "Fortement d'accord")

data_hobbies <- regroupement(data_hobbies ,liste_niveaux)
 
harmonious_hobbies <- rowSums(data_hobbies[, c(1,3,5,7,9,11)])/6
obsessive_hobbies <- rowSums(data_hobbies[, c(2,4,6,8,10,12)])/6

hobbies <- data.frame(harmonious_hobbies, obsessive_hobbies)

# ----------------------------------------------------------------------------- #
# Regroupement variables addiction

data_addiction <- data[, c(which(colnames(data)=="addiction_1"):which(colnames(data)=="addiction_6"))]

liste_niveaux <-  c("Pas du tout d'accord", 
                    "En désaccord", 
                    "Légèrement en désaccord", 
                    "Légèrement en accord", 
                    "D'accord", 
                    "Tout à fait d'accord")

data_addiction <- regroupement(data_addiction ,liste_niveaux)

addiction <- rowSums(data_addiction)/6

# ----------------------------------------------------------------------------- #
# Retrait des variables que nous avons regroupé

data <- data[, -c(which(colnames(data)=="fulfillment_1"):which(colnames(data)=="addiction_6"))]

# ----------------------------------------------------------------------------- #
# Ajout des variables que nous avons regroupé

data <- cbind(data, fulfillment, priority, motivation, mental_force, internal_speech, precompetitive_anxiety, hobbies, addiction)

# ----------------------------------------------------------------------------- #
# Ajout de valeurs dans les données manquantes

data %<>% 
  mutate(distance_trail_participation_last_two_seasons = ifelse(!is.na(trail_participation_last_two_seasons) & is.na(distance_trail_participation_last_two_seasons), "0 km",distance_trail_participation_last_two_seasons)) %>%
  mutate(frequence_trail_participation_last_two_seasons_by_season = ifelse(!is.na(trail_participation_last_two_seasons) & is.na(frequence_trail_participation_last_two_seasons_by_season), "0 course",frequence_trail_participation_last_two_seasons_by_season)) %>%
  mutate(trail_health_problem = ifelse(trail_injury_illness == "Non","Aucune",trail_health_problem)) %>%
  mutate(trail_health_problem_involve_no_training = ifelse(trail_injury_illness == "Non","Non",trail_health_problem_involve_no_training)) %>%
  mutate(time_trail_health_problem_involve_no_training = ifelse(trail_injury_illness == "Non","0 semaine",time_trail_health_problem_involve_no_training)) %>%
  mutate(time_trail_health_problem_involve_no_training = ifelse(trail_health_problem_involve_no_training == "Non","0 semaine",time_trail_health_problem_involve_no_training)) %>%
  mutate(have_this_illness = ifelse(is.na(have_this_illness),"Non",have_this_illness))

# ----------------------------------------------------------------------------- #
# Modification des différents types concernant nos variables
data$sex <- as.factor(data$sex)
data$age <- as.numeric(data$age)
data$weight <- as.numeric(data$weight)
data$max_weight <- as.numeric(data$max_weight)
data$min_weight <- as.numeric(data$min_weight)
data$size <- as.numeric(data$size)
data$weekly_volume_profession <- as.factor(data$weekly_volume_profession)
data$start_trail <- as.factor(data$start_trail)
data$time_proportion_other_practice <- as.factor(data$time_proportion_other_practice)
data$coach_for_trail <- as.factor(data$coach_for_trail)
data$heart_rate_monitor_train <- as.factor(data$heart_rate_monitor_train)
data$heart_rate_monitor_competition <- as.factor(data$heart_rate_monitor_competition)
data$connected_platform <- as.factor(data$connected_platform)
data$ITRA_rating_know <- as.factor(data$ITRA_rating_know)
data$ITRA_rating <- as.numeric(data$ITRA_rating)
data$number_hours_practice_trail_phase_preparation <- as.factor(data$number_hours_practice_trail_phase_preparation)
data$drop_practice_trail_phase_preparation <- as.factor(data$drop_practice_trail_phase_preparation)
data$kilometres_practice_trail_phase_preparation <- as.factor(data$kilometres_practice_trail_phase_preparation)
data$number_hours_training_trail_phase_preparation <- as.factor(data$number_hours_training_trail_phase_preparation)
data$trail_participation_last_two_seasons <- as.factor(data$trail_participation_last_two_seasons)
data$distance_trail_participation_last_two_seasons <- as.factor(data$distance_trail_participation_last_two_seasons)
data$frequence_trail_participation_last_two_seasons_by_season <- as.factor(data$frequence_trail_participation_last_two_seasons_by_season)
data$diet <- as.factor(data$diet)
data$volume_drink_during_training <- as.factor(data$volume_drink_during_training)
data$volume_drink_during_competition <- as.factor(data$volume_drink_during_competition)
data$frequence_hours_eat_during_training <- as.factor(data$frequence_hours_eat_during_training)
data$frequence_hours_eat_during_competition <- as.factor(data$frequence_hours_eat_during_competition)
data$trail_injury_illness <- as.factor(data$trail_injury_illness)
data$trail_health_problem_involve_no_training <- as.factor(data$trail_health_problem_involve_no_training)
data$time_trail_health_problem_involve_no_training <- as.factor(data$time_trail_health_problem_involve_no_training)
data$frequence_medical_apointment_trail <- as.factor(data$frequence_medical_apointment_trail)
data$smoke <- as.factor(data$smoke)

# ----------------------------------------------------------------------------- #
# Gestion des valeurs aberrantes

data %<>% 
  filter(weight < 100) %>%
  mutate(min_weight = ifelse(min_weight == 2019, 72,min_weight)) %>%
  mutate(max_weight = ifelse(max_weight == 2023, 72,max_weight)) %>%
  mutate(ITRA_rating = ifelse(ITRA_rating < 200, NA, ITRA_rating)) %>% 
  mutate(ITRA_rating_know = ifelse(is.na(ITRA_rating), "Non","Oui"))


# ----------------------------------------------------------------------------- #
# Ajout des résultats 

data_resultats <- read_excel("Bibliographie/Résultats_courses_UT4M_participants_questionnaires.xlsx", sheet = "Resultats")
data_abandons <- read_excel("Bibliographie/Résultats_courses_UT4M_participants_questionnaires.xlsx", sheet = "Abandons")
data_non_inscrits <- read_excel("Bibliographie/Résultats_courses_UT4M_participants_questionnaires.xlsx", sheet = "non inscrits")
data_nb_partants <- read_excel("Bibliographie/nombre_de_partants.xlsx")
# Mise en forme du jeu de données d'abandons
data_abandons$do_not_finish = "oui"
data_abandons$do_not_finish = ifelse(data_abandons$`Liste des abandons.Point` == 'DNS', "DNS","oui")
data_abandons = data_abandons[, c("file_number", "do_not_finish", "Liste des abandons.Point")]
colnames(data_abandons) <- c("file_number","do_not_finish", "abandonment_point")
# Fusion du jeu de données d'abandons et des réponses au questionnaire
data2 = left_join(data,data_abandons, "file_number")
# Mise en forme du jeu de données des résultats
data_resultats = left_join(data_resultats,data_nb_partants[,c("Competition","Scratch_total")], "Competition")
data_resultats = data_resultats[, c("file_number", "Classt scratch","Scratch_total", "Catégorie", "Class cat")]
colnames(data_resultats) <- c("file_number","scratch","Scratch_total", "category", "category_result")
# Fusion du jeu de données des résultats et des réponses au questionnaire
data3 = left_join(data2,data_resultats, "file_number")
# Ajout des données dans la colonne abandon
data3$do_not_finish <- ifelse(!is.na(data3$scratch), "non", data3$do_not_finish)
# Conservation d'une seule course
extraire_apres_virgule <- function(data) {
  for (i in 1:dim(data3)[1]){
    chaine = as.character(data[i,"which_races_UT4M_2023"])
    parties <- strsplit(chaine, ",")
    if (length(parties[[1]]) > 1) {
      data$which_races_UT4M_2023[i] = (trimws(parties[[1]][2]))
    } else {
      data$which_races_UT4M_2023[i] = chaine
    }
  }
  return(data)
}
data <- data3 %>% extraire_apres_virgule()

# ----------------------------------------------------------------------------- #
# Création de notre dataframe sans les noms de dossiers commençant par NA

data_sans_NA  <- data %>% filter(!grepl("NA_", file_number, fixed = TRUE))

# ----------------------------------------------------------------------------- #
# Importation de notre data mis en forme
library(openxlsx)
write.xlsx(list("Avec NA" = data, "Sans NA" = data_sans_NA), file = "dataset_V6.xlsx", rowNames = FALSE)

