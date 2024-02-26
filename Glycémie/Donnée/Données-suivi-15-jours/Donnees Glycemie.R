library(readxl)
library(dplyr)
library(openxlsx)

# ----------------------------------------------------------------------------- #
# Import des données de glycémie

df <- read_excel("Questionnaire - suivi glycémique-15jours.xlsx")

# ----------------------------------------------------------------------------- #
# Récupération des numéros de participants

nb_rows = dim(df)[1]
data <- df[2:nb_rows,]

colnames(data) <- c(colnames(df[1:17]),
                    "participant_number",
                    "days_date",
                    "time_fell_asleep_night_before",
                    "wake_up_time",
                    "sleep_quality",
                    "training_today",
                    "sport_practised",
                    "other_sport_practised",
                    "training_start_time",
                    "training_end_time",
                    "training_theme",
                    "other_training_theme",
                    "perception_effort_training",
                    "distance_training",
                    "drop_training",
                    "eating_training",
                    "number_and_brand_gel_eaten",
                    "number_and_brand_bar_eaten",
                    "number_and_brand_compote_eaten",
                    "number_and_brand_fruit_paste_eaten",
                    "number_and_brand_other_products_eaten",
                    "drinking_training",
                    "type_drink",
                    "other_type_drink",
                    "drink_quantity",
                    "second_workout_day",
                    "sport_practised_2",
                    "other_sport_practised_2",
                    "training_start_time_2",
                    "training_end_time_2",
                    "training_theme_2",
                    "perception_effort_training_2",
                    "distance_training_2",
                    "drop_training_2",
                    "eating_training_2",
                    "number_and_brand_bar_eaten_2",
                    "number_and_brand_compote_eaten_2",
                    "number_and_brand_gel_eaten_2",
                    "number_and_brand_fruit_paste_eaten_2",
                    "number_and_brand_other_products_eaten_2",
                    "drinking_training_2",
                    "type_drink_2",
                    "other_type_drink_2",
                    "drink_quantity_2",
                    "breakfast",
                    "breakfast_start_time",
                    "breakfast_notes",
                    "morning_snack",
                    "morning_snack_start_time",
                    "morning_snack_notes",
                    "lunch",
                    "lunch_start_time",
                    "lunch_notes",
                    "afternoon_snack",
                    "afternoon_snack_notes",
                    "dinner",
                    "dinner_start_time",
                    "dinner_notes")

nom_colonne = "participant_number"

nums_participants = sort(as.numeric(unique(data[[nom_colonne]])))

# ----------------------------------------------------------------------------- #
# Création d'un fichier par participant

for(numero_participant in nums_participants){
  suivi <- data %>% filter(data[[nom_colonne]] == numero_participant)
  write.xlsx(suivi, file = paste0("glycemic_data_participant",numero_participant,".xlsx"), rowNames = FALSE)
}

# ----------------------------------------------------------------------------- #
# Vérification

sum = 0
for(numero_participant in nums_participants){
  sum = sum + dim(read_excel(paste0("glycemic_data_participant",numero_participant,".xlsx")))[1]
}
sum == dim(data)[1]
