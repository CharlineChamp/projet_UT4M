library(readxl)
library(dplyr)
library(gtsummary)
library(forcats)

data <- read_excel("Data/Reponse_questionnaire_V2.xlsx")

# Mise en forme des données

data2 = data[-c(1,7,10,11,26,28,29,31,33,35,38,39,43,44)]

data2$sex <- as.factor(data2$sex)
data2$weekly_volume_profession <- as.factor(data2$weekly_volume_profession)
data2$weekly_volume_profession <- ordered(data2$weekly_volume_profession, 
                                         levels = c("⩽ 35 heures / semaine" , 
                                                "entre 36 et 45 heures / semaine",
                                                    "⩾ 45h / semaine" ))
data2$start_trail <- as.factor(data2$start_trail)
data2$start_trail <- ordered(data2$start_trail, 
                            levels = c("< 2 ans"  ,
                                       "Entre 2 et 5 ans",
                                       "Entre 5 et 10 ans",
                                       "> 10 ans"))
data2$time_proportion_other_practice <- as.factor(data2$time_proportion_other_practice)
data2$time_proportion_other_practice <- ordered(data2$time_proportion_other_practice, 
                                               levels = c("< 20%",
                                                          "Entre 20% et 50%",
                                                          "> 50%"))
data2$coach_for_trail = as.factor(data2$coach_for_trail)
data2$heart_rate_monitor_train = as.factor(data2$heart_rate_monitor_train)
data2$heart_rate_monitor_competition = as.factor(data2$heart_rate_monitor_competition)
data2$connected_platform = as.factor(data2$connected_platform)
data2$ITRA_rating_know = as.factor(data2$ITRA_rating_know)
data2$number_hours_practice_trail_phase_preparation = as.factor(data2$number_hours_practice_trail_phase_preparation)
data2$number_hours_practice_trail_phase_preparation = factor(data2$number_hours_practice_trail_phase_preparation,levels=c(
  '⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine'))
data2$drop_practice_trail_phase_preparation = as.factor(data2$drop_practice_trail_phase_preparation)
data2$drop_practice_trail_phase_preparation = factor(data2$drop_practice_trail_phase_preparation,levels=c('< 1000m / semaine','Entre 1000m et 2000m / semaine', '> 2000m / semaine'))
data2$kilometres_practice_trail_phase_preparation = as.factor(data2$kilometres_practice_trail_phase_preparation)
data2$kilometres_practice_trail_phase_preparation = factor(data2$kilometres_practice_trail_phase_preparation,levels=c(
  '< 20km / semaine','Entre 20km et 50km / semaine','Entre 50km et 80km / semaine','> 80km / semaine'))
data2$number_hours_training_trail_phase_preparation = as.factor(data2$number_hours_training_trail_phase_preparation)
data2$number_hours_training_trail_phase_preparation = factor(data2$number_hours_training_trail_phase_preparation,levels=c(
  '⩽ 3 heures / semaine','Entre 3h et 7h / semaine','Entre 7h et 12h / semaine','> 12 heures / semaine'))
data2$trail_participation_last_two_seasons=as.factor(data2$trail_participation_last_two_seasons)
data2$distance_trail_participation_last_two_seasons=as.factor(data2$distance_trail_participation_last_two_seasons)
data2$distance_trail_participation_last_two_seasons=fct_relevel(data2$distance_trail_participation_last_two_seasons, c("< 40km", "De 40km à 80km", "> 80km"))
data2$frequence_trail_participation_last_two_seasons_by_season=as.factor(data2$frequence_trail_participation_last_two_seasons_by_season)
data2$frequence_trail_participation_last_two_seasons_by_season=fct_relevel(data2$frequence_trail_participation_last_two_seasons_by_season, 
                                                                         c("1 à 3 courses / saison", "4 à 8 courses / saison", "> 8 courses / saison"))
data2$diet=as.factor(data2$diet)
data2$volume_drink_during_training=as.factor(data2$volume_drink_during_training)
data2$volume_drink_during_competition=as.factor(data2$volume_drink_during_competition)
data2$frequence_hours_eat_during_training <- as.factor(data2$frequence_hours_eat_during_training)
data2$frequence_hours_eat_during_training <- ordered(data2$frequence_hours_eat_during_training, 
                                                    levels = c("< 1x / heure",
                                                               "1x ou 2x / heure",
                                                               "> 2x / heure"))
data2$frequence_hours_eat_during_competition <- as.factor(data2$frequence_hours_eat_during_competition)
data2$frequence_hours_eat_during_competition <- ordered(data2$frequence_hours_eat_during_competition, 
                                                       levels = c("< 1x / heure",
                                                                  "1x ou 2x / heure",
                                                                  "> 2x / heure"))
data2$trail_injury_illness <- as.factor(data2$trail_injury_illness)
data2$trail_injury_illness <- ordered(data2$trail_injury_illness, 
                                     levels = c("Oui",
                                                "Non"))
data2$trail_health_problem_involve_no_training <- as.factor(data2$trail_health_problem_involve_no_training)
data2$trail_health_problem_involve_no_training <- ordered(data2$trail_health_problem_involve_no_training, 
                                                         levels = c("Oui",
                                                                    "Non"))
data2$time_trail_health_problem_involve_no_training <- as.factor(data2$time_trail_health_problem_involve_no_training)
data2$time_trail_health_problem_involve_no_training <- ordered(data2$time_trail_health_problem_involve_no_training, 
                                                              levels = c("< 2 semaines",
                                                                         "2 à 4 semaines",
                                                                         "5 à 8 semaines",
                                                                         "> 8 semaines"))
data2$frequence_medical_apointment_trail <- as.factor(data2$frequence_medical_apointment_trail)
data2$frequence_medical_apointment_trail <- ordered(data2$frequence_medical_apointment_trail, 
                                                   levels = c("< 2x / an",
                                                              "2x à 6x / an",
                                                              "> 6x / an"))
data2$smoke = as.factor(data2$smoke)



# Tableau selon la côte ITRA

data2$ITRA_rating_gp = data2$ITRA_rating
data2$ITRA_rating_gp = ifelse(data2$ITRA_rating_gp <= 500, "ITRA < ou = à 500", "ITRA > à 500")

ITRA = data2%>% tbl_summary(by = ITRA_rating_gp, missing_text = "(Données manquantes)",
                                    label = list(sex ~ "Sexe",
                                                 age ~ "Age",
                                                 weight ~ "Poids",
                                                 max_weight ~ "Poids maximal",
                                                 min_weight ~ "Poids minimal",
                                                 weekly_volume_profession ~ "Volume horaire consacré à la profession",
                                                 start_trail ~ "Nombre d'années de pratique du trail",
                                                 #other_practice ~ "",
                                                 time_proportion_other_practice ~ "Proportion de temps accordée à la pratique d'autres sports",
                                                 coach_for_trail ~ "Coach pour le trail",
                                                 heart_rate_monitor_train ~ "Port d'un cardiofréquencemètre en entrainement",
                                                 heart_rate_monitor_competition ~ "Port d'un cardiofréquencemètre en compétition",
                                                 connected_platform ~ "Téléchargement des données sur une plateforme connectée",
                                                 ITRA_rating_know ~ "Connaissance de la côte ITRA",
                                                 number_hours_practice_trail_phase_preparation ~ "Nombre d'heures d'entrainement en trail pour la préparation d'une course",
                                                 drop_practice_trail_phase_preparation ~ "Dénivelé lors des entrainements en trail pour la préparation d'une course ",
                                                 kilometres_practice_trail_phase_preparation ~ "Nombre de kilomètres parcourus pour l'entraînement en trail pour la préparation d'une course",
                                                 number_hours_training_trail_phase_preparation ~ "Nombre d'heures d'entrainement pour la préparation d'une course",
                                                 trail_participation_last_two_seasons ~ "Participation à une ou plusieurs courses de trail lors des 2 dernières saisons",
                                                 distance_trail_participation_last_two_seasons ~ "Distance parcourue lors des dernières courses de trail", 
                                                 frequence_trail_participation_last_two_seasons_by_season ~ "Nombre de courses de trail par saison",
                                                 #which_races_UT4M_2023 ~ "",
                                                 diet ~ "Régime alimentaire",
                                                 #which_diet ~ "",
                                                 #which_drink_during_training ~ "",
                                                 volume_drink_during_training ~ "Volume de boisson pendant l'entraînement",
                                                 #which_drink_during_competition ~ "",
                                                 volume_drink_during_competition ~ "Volume de boisson pendant une compétition",
                                                 #which_eat_during_training ~ "",
                                                 frequence_hours_eat_during_training ~ "Fréquence d'alimentation à l'entraînement",
                                                 #which_eat_during_competition ~ "",
                                                 frequence_hours_eat_during_competition ~ "Fréquence d'alimentation en compétition",
                                                 trail_injury_illness ~ "Blessure ou maladie en lien avec la pratique du trail",
                                                 #trail_health_problem ~ "",
                                                 trail_health_problem_involve_no_training ~ "Problèmes de santé ayant empêché l'entraînement",
                                                 time_trail_health_problem_involve_no_training ~ "Durée d'impossibilité de s'entraîner",
                                                 frequence_medical_apointment_trail ~ "Fréquence des rendez-vous médicaux",
                                                 #reason_medical_apointment_trail ~ "",
                                                 #have_this_illness ~ "",
                                                 smoke ~ "Fumeur régulier",
                                                 fulfillment ~ "Epanouissement",
                                                 priority ~ "Priorités",
                                                 intrinsic_motivation ~ "Motivation intrinsèque",
                                                 identified_motivation ~ "Motivation identifiée",
                                                 integrated_motivation ~ "Motivation intégrée",
                                                 introjected_motivation ~ "Motivation introjectée",
                                                 external_motivation ~ "Motivation externe",
                                                 global_mental_force ~ "Force mentale globale",
                                                 confidence ~ "Confiance",
                                                 consistency ~ "Consistance",
                                                 control ~ "Contrôle",
                                                 general_internal_speech ~ "Discours interne",
                                                 training_internal_speech ~ "Discours interne lors de l'entraînement",
                                                 competition_internal_speech ~ "Discours interne lors de compétition",
                                                 general_precompetitive_anxiety ~ "Anxiété pré-compétitive",
                                                 somatic_anxiety ~ "Anxiété somatique",
                                                 cognitive_anxiety ~ "Anxiété cognitive",
                                                 harmonious_hobbies ~ "Passion harmonieuse",
                                                 obsessive_hobbies ~ "Passion obsessive",
                                                 addiction ~ "Addiction"),
                                    statistic = all_continuous() ~ "{mean} [{p25}, {p75}]",
                                    missing = "no")%>%
  add_p()  %>%
  add_overall(col_label= "**Tous**, N = {N}")  %>%
  modify_header(label ~ "**Variable**", p.value = "**P-valeur**")

ITRA


# Tableau selon la l'occurence d'une blessure

data2$trail_injury_illness_gp = data2$trail_injury_illness
data2$trail_injury_illness_gp = ifelse(data2$trail_injury_illness_gp == "Oui", "Blessure", "Aucune blessure")

Blessure = data2%>% tbl_summary(by = trail_injury_illness_gp, missing_text = "(Données manquantes)",
                            label = list(sex ~ "Sexe",
                                         age ~ "Age",
                                         weight ~ "Poids",
                                         max_weight ~ "Poids maximal",
                                         min_weight ~ "Poids minimal",
                                         weekly_volume_profession ~ "Volume horaire consacré à la profession",
                                         start_trail ~ "Nombre d'années de pratique du trail",
                                         #other_practice ~ "",
                                         time_proportion_other_practice ~ "Proportion de temps accordée à la pratique d'autres sports",
                                         coach_for_trail ~ "Coach pour le trail",
                                         heart_rate_monitor_train ~ "Port d'un cardiofréquencemètre en entrainement",
                                         heart_rate_monitor_competition ~ "Port d'un cardiofréquencemètre en compétition",
                                         connected_platform ~ "Téléchargement des données sur une plateforme connectée",
                                         ITRA_rating ~ "Côte ITRA",
                                         ITRA_rating_know ~ "Connaissance de la côte ITRA",
                                         number_hours_practice_trail_phase_preparation ~ "Nombre d'heures d'entrainement en trail pour la préparation d'une course",
                                         drop_practice_trail_phase_preparation ~ "Dénivelé lors des entrainements en trail pour la préparation d'une course ",
                                         kilometres_practice_trail_phase_preparation ~ "Nombre de kilomètres parcourus pour l'entraînement en trail pour la préparation d'une course",
                                         number_hours_training_trail_phase_preparation ~ "Nombre d'heures d'entrainement pour la préparation d'une course",
                                         trail_participation_last_two_seasons ~ "Participation à une ou plusieurs courses de trail lors des 2 dernières saisons",
                                         distance_trail_participation_last_two_seasons ~ "Distance parcourue lors des dernières courses de trail", 
                                         frequence_trail_participation_last_two_seasons_by_season ~ "Nombre de courses de trail par saison",
                                         #which_races_UT4M_2023 ~ "",
                                         diet ~ "Régime alimentaire",
                                         #which_diet ~ "",
                                         #which_drink_during_training ~ "",
                                         volume_drink_during_training ~ "Volume de boisson pendant l'entraînement",
                                         #which_drink_during_competition ~ "",
                                         volume_drink_during_competition ~ "Volume de boisson pendant une compétition",
                                         #which_eat_during_training ~ "",
                                         frequence_hours_eat_during_training ~ "Fréquence d'alimentation à l'entraînement",
                                         #which_eat_during_competition ~ "",
                                         frequence_hours_eat_during_competition ~ "Fréquence d'alimentation en compétition",
                                         #trail_health_problem ~ "",
                                         trail_health_problem_involve_no_training ~ "Problèmes de santé ayant empêché l'entraînement",
                                         time_trail_health_problem_involve_no_training ~ "Durée d'impossibilité de s'entraîner",
                                         frequence_medical_apointment_trail ~ "Fréquence des rendez-vous médicaux",
                                         #reason_medical_apointment_trail ~ "",
                                         #have_this_illness ~ "",
                                         smoke ~ "Fumeur régulier",
                                         fulfillment ~ "Epanouissement",
                                         priority ~ "Priorités",
                                         intrinsic_motivation ~ "Motivation intrinsèque",
                                         identified_motivation ~ "Motivation identifiée",
                                         integrated_motivation ~ "Motivation intégrée",
                                         introjected_motivation ~ "Motivation introjectée",
                                         external_motivation ~ "Motivation externe",
                                         global_mental_force ~ "Force mentale globale",
                                         confidence ~ "Confiance",
                                         consistency ~ "Consistance",
                                         control ~ "Contrôle",
                                         general_internal_speech ~ "Discours interne",
                                         training_internal_speech ~ "Discours interne lors de l'entraînement",
                                         competition_internal_speech ~ "Discours interne lors de compétition",
                                         general_precompetitive_anxiety ~ "Anxiété pré-compétitive",
                                         somatic_anxiety ~ "Anxiété somatique",
                                         cognitive_anxiety ~ "Anxiété cognitive",
                                         harmonious_hobbies ~ "Passion harmonieuse",
                                         obsessive_hobbies ~ "Passion obsessive",
                                         addiction ~ "Addiction"),
                            statistic = all_continuous() ~ "{mean} [{p25}, {p75}]",
                            missing = "no")%>%
  add_p()  %>%
  add_overall(col_label= "**Tous**, N = {N}")  %>%
  modify_header(label ~ "**Variable**", p.value = "**P-valeur**")

Blessure
