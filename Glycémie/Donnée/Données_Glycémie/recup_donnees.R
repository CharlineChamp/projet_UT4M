library(stringr)
library(openxlsx)
library(dplyr)
library(magrittr)

# Fonction pour traiter un dossier principal
traiter_dossier_principal <- function(chemin_dossier_principal) {
  # Obtenez la liste des sous-dossiers
  sous_dossiers <- list.dirs(path = chemin_dossier_principal, full.names = TRUE, recursive = FALSE)
  
  # Récupération de la course
  course <- tail(unlist(strsplit(chemin_dossier_principal, '/')), 1)
  
  # Initialisez un data frame vide pour stocker les données
  df_final <- data.frame()
  
  # Parcourez chaque sous-dossier
  for (sous_dossier in sous_dossiers) {
    
    # Récupération de numéro du participant
    numero_participant <- tail(unlist(strsplit(sous_dossier, '/')), 1)
    
    # Obtenez la liste des fichiers CSV dans le sous-dossier
    fichiers_csv <- list.files(path = sous_dossier, pattern = "\\.csv$", full.names = TRUE)
    
    # Parcourez chaque fichier CSV
    for (fichier_csv in fichiers_csv) {
      
      # Lisez le fichier CSV
      df_temp <- read.csv2(fichier_csv, header = TRUE)
      
      # Ajoutez le numéro du participant 
      df_temp$participant <- numero_participant
      
      # Ajoutez la course 
      df_temp$course <- course
      
      # Ajoutez le DataFrame résultant au DataFrame final
      df_final <- rbind(df_final, df_temp)
    }
  }
  
  return(df_final)
}

# Spécifiez les chemins des dossiers principaux
chemin1 <- "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/Données_Glycémie/100 km"
chemin2 <- "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/Données_Glycémie/160 km"

# Traitement des dossiers principaux
df_final1 <- traiter_dossier_principal(chemin1)
df_final2 <- traiter_dossier_principal(chemin2)

# Combinez les résultats si nécessaire
df_final <- rbind(df_final1, df_final2)

# Convertir la colonne recorded_timestamp en format de date et heure
df_final %<>% 
  mutate(datetime = as.POSIXct(recorded_timestamp, format = "%d/%m/%Y %H:%M")) %>% 
  mutate(numero_participant = as.numeric(gsub("\\D", "", participant))) %>%
  select(glucose,datetime,numero_participant, course)


# Écrivez le dataframe dans le fichier Excel
write.csv2(df_final, file = "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/Données_Glycémie/data_glucoseV2.csv", row.names = FALSE)
write.xlsx(df_final, "~/Documents/GitHub/UT4M_psycho/Glycémie/Donnée/Données_Glycémie/data_glucoseV2.xlsx", rowNames = FALSE)


