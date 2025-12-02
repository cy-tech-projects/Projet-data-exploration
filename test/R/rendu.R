##========================================
##                TP1                   ##
##========================================



# Importer le jeu de données
Dataset <- read.csv("./R/data_final.csv", header=T, sep=';', row.names=1, encoding="UTF-8", stringsAsFactors=FALSE)

# Vérifier l'importation
cat("Nombre d'observations :", nrow(Dataset), "\n")
cat("Nombre de variables :", ncol(Dataset), "\n")

# Afficher les noms des variables
names(Dataset)

# Structure du dataset
str(Dataset)

# Premier aperçu des données
head(Dataset)

# Résumé numérique initial
summary(Dataset)

# ========================================
# 2. PRÉPARATION DES DONNÉES
# ========================================

# Identifier les noms des colonnes (à adapter selon votre fichier)
# Col 1: Tendance (Hausse/Baisse)
# Col 2: Volatilité (Élevée/Moyenne)
# Col 3-4: Prix et variation
# etc.

# Renommer les colonnes pour plus de clarté
colnames(Dataset) = c("Tendance", "Volatilite", "Prix_BTC", "Var_Prix_BTC",
                      "Volume_BTC", "Var_Volume_BTC", "Prix_ETH", "Var_Prix_ETH",
                      "Volume_ETH", "Var_Volume_ETH", "Prix_snp500", "Var_Prix_snp500",
                      "Prix_Gold", "Var_Prix_Gold", "Fear_Index", 
                      "Fear_Level1", "Fear_Level2", "Var_Fear", "Var_Fear_Pct",
                      "Autre_Variable")
                      
# Vérifier les noms
names(Dataset)

# Transformer les variables qualitatives en factor
Dataset$Tendance = as.factor(Dataset$Tendance)
Dataset$Volatilite = as.factor(Dataset$Volatilite)
Dataset$Fear_Level1 = as.factor(Dataset$Fear_Level1)
Dataset$Fear_Level2 = as.factor(Dataset$Fear_Level2)

# Vérifier les transformations
is.factor(Dataset$Tendance)
levels(Dataset$Tendance)
levels(Dataset$Volatilite)

# ========================================
# 3. ANALYSE DES VARIABLES QUANTITATIVES
# ========================================

cat("\n========== ANALYSE DU PRIX BTC ==========\n")

# Résumé numérique
cat("Moyenne du Prix BTC:", mean(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Écart-type:", sd(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Médiane:", median(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Quartiles:\n")
print(quantile(Dataset$Prix_BTC, na.rm=TRUE))

Q1_BTC = quantile(Dataset$Prix_BTC, na.rm=TRUE)[2]
Q3_BTC = quantile(Dataset$Prix_BTC, na.rm=TRUE)[4]
cat("Q1:", Q1_BTC, " - Q3:", Q3_BTC, "\n")
cat("IQR:", Q3_BTC - Q1_BTC, "\n")

# Graphiques
par(mfrow=c(2,1))  # Créer une grille 1x2 pour les graphiques

# Histogramme
hist(Dataset$Prix_BTC, 
     main='Histogramme du Prix BTC',
     xlab='Prix BTC',
     col='blue',
     breaks=30)

hist(Dataset$Prix_ETH, 
     main='Histogramme du Prix ETH',
     xlab='Prix ETH',
     col='lightgreen',
     breaks=30)
    
hist(Dataset$Prix_Gold, 
     main='Histogramme du Prix Gold',
     xlab='Prix Gold',
     col='gold',
     breaks=30)

hist(Dataset$Prix_snp500, 
     main='Histogramme du Prix S&P 500',
     xlab='Prix S&P 500',
     col='orange',
     breaks=30)
    
hist(Dataset$Fear_Index, 
     main='Histogramme du Fear Index',
     xlab='Fear Index',
     col='purple',
     breaks=10)


# Boîte de Tukey (Boxplot)
boxplot(Dataset$Prix_BTC,
        main='Boxplot du Prix BTC',
        ylab='Prix BTC',
        col='grey')

boxplot(Dataset$Prix_ETH,
        main='Boxplot du Prix ETH',
        ylab='Prix ETH',
        col='lightgreen')

boxplot(Dataset$Prix_Gold,
        main='Boxplot du Prix Gold',
        ylab='Prix Gold',
        col='gold')

boxplot(Dataset$Prix_snp500,
        main='Boxplot du Prix S&P 500',
        ylab='Prix S&P 500',
        col='orange')

boxplot(Dataset$Fear_Index,
        main='Boxplot du Fear Index',
        ylab='Fear Index',
        col='lightblue')

# tableau d'effectifs par intervalles              BTC

# 1. Déterminer les bornes (breaks)
breaks <- hist(Dataset$Prix_BTC, breaks = 30, plot = FALSE)$breaks

# 2. Créer les classes de prix et compter les fréquences par classe
classes_prix <- cut(Dataset$Prix_BTC, breaks = breaks, right = TRUE, include.lowest = TRUE)
tableau_frequence <- as.data.frame(table(classes_prix))
colnames(tableau_frequence) <- c("Intervalle_Prix", "Nombre_Jours")

# 3. Calculer le nombre cumulé (à partir du haut)
tableau_frequence$Cumule_Inverse <- rev(cumsum(rev(tableau_frequence$Nombre_Jours)))

# 4. Construire le tableau final "Seuil de Prix" vs "Jours au-dessus"
# On prend toutes les bornes sauf la dernière, qui est le prix maximum
Bornes_Inferieures <- breaks[-length(breaks)] 

# Les Jours_Au_Dessus correspondent au Cumule_Inverse de la classe suivante.
# On ajoute un '0' à la fin, car il n'y a 0 jour au-dessus de la borne la plus haute.
Tableau_Final <- data.frame(
  Seuil_Prix = Bornes_Inferieures,
  Jours_Au_Dessus = c(tableau_frequence$Cumule_Inverse[-1], 0)
)

# eth

# 1. Déterminer les bornes (breaks)
breaks <- hist(Dataset$Prix_ETH, breaks = 30, plot = FALSE)$breaks

# 2. Créer les classes de prix et compter les fréquences par classe
classes_prix <- cut(Dataset$Prix_ETH, breaks = breaks, right = TRUE, include.lowest = TRUE)
tableau_frequence <- as.data.frame(table(classes_prix))
colnames(tableau_frequence) <- c("Intervalle_Prix", "Nombre_Jours")

# 3. Calculer le nombre cumulé (à partir du haut)
tableau_frequence$Cumule_Inverse <- rev(cumsum(rev(tableau_frequence$Nombre_Jours)))

# 4. Construire le tableau final "Seuil de Prix" vs "Jours au-dessus"
# On prend toutes les bornes sauf la dernière, qui est le prix maximum
Bornes_Inferieures <- breaks[-length(breaks)] 

# Les Jours_Au_Dessus correspondent au Cumule_Inverse de la classe suivante.
# On ajoute un '0' à la fin, car il n'y a 0 jour au-dessus de la borne la plus haute.
Tableau_Final <- data.frame(
  Seuil_Prix = Bornes_Inferieures,
  Jours_Au_Dessus = c(tableau_frequence$Cumule_Inverse[-1], 0)
)

# Afficher le tableau ETH et sa transposition
print(Tableau_Final)

Tableau_Final_ETH.T <- as.data.frame(t(Tableau_Final$Jours_Au_Dessus))
colnames(Tableau_Final_ETH.T) <- as.character(Tableau_Final$Seuil_Prix)
rownames(Tableau_Final_ETH.T) <- "Jours_Au_Dessus"
print(Tableau_Final_ETH.T)


# Gold

# 1. Déterminer les bornes (breaks)
breaks <- hist(Dataset$Prix_Gold, breaks = 30, plot = FALSE)$breaks

# 2. Créer les classes de prix et compter les fréquences par classe
classes_prix <- cut(Dataset$Prix_Gold, breaks = breaks, right = TRUE, include.lowest = TRUE)
tableau_frequence <- as.data.frame(table(classes_prix))
colnames(tableau_frequence) <- c("Intervalle_Prix", "Nombre_Jours")

# 3. Calculer le nombre cumulé (à partir du haut)
tableau_frequence$Cumule_Inverse <- rev(cumsum(rev(tableau_frequence$Nombre_Jours)))

# 4. Construire le tableau final "Seuil de Prix" vs "Jours au-dessus"
# On prend toutes les bornes sauf la dernière, qui est le prix maximum
Bornes_Inferieures <- breaks[-length(breaks)] 

# Les Jours_Au_Dessus correspondent au Cumule_Inverse de la classe suivante.
# On ajoute un '0' à la fin, car il n'y a 0 jour au-dessus de la borne la plus haute.
Tableau_Final <- data.frame(
  Seuil_Prix = Bornes_Inferieures,
  Jours_Au_Dessus = c(tableau_frequence$Cumule_Inverse[-1], 0)
)




# snp500

# 1. Déterminer les bornes (breaks)
breaks <- hist(Dataset$Prix_snp500, breaks = 30, plot = FALSE)$breaks

# 2. Créer les classes de prix et compter les fréquences par classe
classes_prix <- cut(Dataset$Prix_snp500, breaks = breaks, right = TRUE, include.lowest = TRUE)
tableau_frequence <- as.data.frame(table(classes_prix))
colnames(tableau_frequence) <- c("Intervalle_Prix", "Nombre_Jours")

# 3. Calculer le nombre cumulé (à partir du haut)
tableau_frequence$Cumule_Inverse <- rev(cumsum(rev(tableau_frequence$Nombre_Jours)))

# 4. Construire le tableau final "Seuil de Prix" vs "Jours au-dessus"
# On prend toutes les bornes sauf la dernière, qui est le prix maximum
Bornes_Inferieures <- breaks[-length(breaks)] 

# Les Jours_Au_Dessus correspondent au Cumule_Inverse de la classe suivante.
# On ajoute un '0' à la fin, car il n'y a 0 jour au-dessus de la borne la plus haute.
Tableau_Final <- data.frame(
  Seuil_Prix = Bornes_Inferieures,
  Jours_Au_Dessus = c(tableau_frequence$Cumule_Inverse[-1], 0)
)



# Afficher le tableau de résultat
print(Tableau_Final)
# Transposer : colonnes = Seuil_Prix, ligne = Jours_Au_Dessus
Tableau_Final.T <- as.data.frame(t(Tableau_Final$Jours_Au_Dessus))
colnames(Tableau_Final.T) <- as.character(Tableau_Final$Seuil_Prix)
rownames(Tableau_Final.T) <- "Jours_Au_Dessus"
print(Tableau_Final.T)


# fear index
# 1. Déterminer les bornes (breaks)
breaks <- hist(Dataset$Fear_Index, breaks = 30, plot = FALSE)$breaks

# 2. Créer les classes de prix et compter les fréquences par classe
classes_prix <- cut(Dataset$Fear_Index, breaks = breaks, right = TRUE, include.lowest = TRUE)
tableau_frequence <- as.data.frame(table(classes_prix))
colnames(tableau_frequence) <- c("Intervalle_Prix", "Nombre_Jours")

# 3. Calculer le nombre cumulé (à partir du haut)
tableau_frequence$Cumule_Inverse <- rev(cumsum(rev(tableau_frequence$Nombre_Jours)))

# 4. Construire le tableau final "Seuil de Prix" vs "Jours au-dessus"
# On prend toutes les bornes sauf la dernière, qui est le prix maximum
Bornes_Inferieures <- breaks[-length(breaks)] 

# Les Jours_Au_Dessus correspondent au Cumule_Inverse de la classe suivante.
# On ajoute un '0' à la fin, car il n'y a 0 jour au-dessus de la borne la plus haute.
Tableau_Final <- data.frame(
  Seuil_Prix = Bornes_Inferieures,
  Jours_Au_Dessus = c(tableau_frequence$Cumule_Inverse[-1], 0)
)




# Réinitialiser la grille graphique
par(mfrow=c(1,1))

# ========================================
# 2. ANALYSE DES VARIABLES QUALITATIVES
# ========================================

cat("\n========== ANALYSE DE LA TENDANCE ==========\n")

# Table de contingence
tab_tendance = table(Dataset$Tendance)
print(tab_tendance)

# Proportions
cat("\nProportions:\n")
print(prop.table(tab_tendance))

# Table avec marges
cat("\nTable avec totaux:\n")
print(addmargins(tab_tendance))

# Graphiques
par(mfrow=c(1,2))

# Diagramme circulaire
pie(tab_tendance, 
    main="Répartition des Tendances",
    col=c("red", "green"))

# Diagramme en barres
barplot(as.matrix(tab_tendance), 
        legend=TRUE,
        main="Fréquences des Tendances",
        col=c("red", "green"),
        ylab="Effectif")

par(mfrow=c(1,1))

# Analyse de la Volatilité
cat("\n========== ANALYSE DE LA VOLATILITÉ ==========\n")
tab_volatilite = table(Dataset$Volatilite)
print(tab_volatilite)
print(prop.table(tab_volatilite))

barplot(as.matrix(tab_volatilite),
        legend=TRUE,
        main="Répartition de la Volatilité",
        col=c("orange", "blue"),
        ylab="Effectif")

# ========================================
# 5. ANALYSE DU FEAR INDEX
# ========================================

cat("\n========== ANALYSE DU FEAR INDEX ==========\n")

# Statistiques descriptives
cat("Moyenne Fear Index:", mean(Dataset$Fear_Index, na.rm=TRUE), "\n")
cat("Écart-type:", sd(Dataset$Fear_Index, na.rm=TRUE), "\n")
cat("Médiane:", median(Dataset$Fear_Index, na.rm=TRUE), "\n")
print(quantile(Dataset$Fear_Index, na.rm=TRUE))

# Graphiques
par(mfrow=c(1,2))
hist(Dataset$Fear_Index,
     main='Histogramme du Fear Index',
     xlab='Fear Index',
     col='purple',
     breaks=10)

boxplot(Dataset$Fear_Index,
        main='Boxplot du Fear Index',
        ylab='Fear Index',
        col='lightblue')
par(mfrow=c(1,1))

# Distribution des niveaux de peur
tab_fear = table(Dataset$Fear_Level1)
print(tab_fear)
barplot(tab_fear,
        main="Distribution des niveaux de peur",
        col=c("darkred", "orange"),
        las=2)

# ========================================
# 6. TRANSFORMATION : Variable discrétisée
# ========================================

cat("\n========== CRÉATION VARIABLE CATÉGORIELLE ==========\n")

# Créer une variable catégorielle basée sur le Prix BTC
# Calculer les tertiles
tertiles_BTC = quantile(Dataset$Prix_BTC, probs=c(0, 0.33, 0.66, 1), na.rm=TRUE)
cat("Tertiles du Prix BTC:\n")
print(tertiles_BTC)

# Créer la variable catégorielle
Dataset$Prix_BTC_Categorie = cut(Dataset$Prix_BTC,
                                  breaks=tertiles_BTC,
                                  labels=c("BAS", "MOYEN", "HAUT"),
                                  include.lowest=TRUE)

# Tableau de contingence
tab_prix_cat = table(Dataset$Prix_BTC_Categorie)
print(tab_prix_cat)

# Graphique
barplot(tab_prix_cat,
        names.arg=c("BAS", "MOYEN", "HAUT"),
        main="Distribution du Prix BTC par catégorie",
        col=c("red", "yellow", "green"),
        ylab="Effectif")

# ========================================
# 7. ANALYSE BIVARIÉE : Tableaux croisés
# ========================================

cat("\n========== ANALYSE CROISÉE ==========\n")

# Tableau croisé : Tendance vs Volatilité
tab_croise = table(Dataset$Tendance, Dataset$Volatilite)
cat("Table de contingence : Tendance x Volatilité\n")
print(tab_croise)
print(addmargins(tab_croise))

# Proportions
cat("\nProportions en ligne:\n")
print(prop.table(tab_croise, 1))

cat("\nProportions en colonne:\n")
print(prop.table(tab_croise, 2))

# ========================================
# 8. DÉTECTION DES VALEURS ABERRANTES
# ========================================

cat("\n========== DÉTECTION DES OUTLIERS ==========\n")

# Fonction pour détecter les outliers
detect_outliers = function(x) {
  Q1 = quantile(x, 0.25, na.rm=TRUE)
  Q3 = quantile(x, 0.75, na.rm=TRUE)
  IQR = Q3 - Q1
  lower = Q1 - 1.5 * IQR
  upper = Q3 + 1.5 * IQR
  outliers = which(x < lower | x > upper)
  return(outliers)
}

# Détecter outliers sur Prix BTC
outliers_BTC = detect_outliers(Dataset$Prix_BTC)
cat("Nombre d'outliers Prix BTC:", length(outliers_BTC), "\n")
if(length(outliers_BTC) > 0) {
  cat("Indices des outliers:", outliers_BTC, "\n")
  cat("Valeurs aberrantes:", Dataset$Prix_BTC[outliers_BTC], "\n")
}

# Détecter outliers sur Fear Index
outliers_Fear = detect_outliers(Dataset$Fear_Index)
cat("\nNombre d'outliers Fear Index:", length(outliers_Fear), "\n")
if(length(outliers_Fear) > 0) {
  cat("Indices des outliers:", outliers_Fear, "\n")
}

# ========================================
# 9. EXPORT DES RÉSULTATS
# ========================================

# Sauvegarder le dataset modifié
# write.table(Dataset, file='crypto_data_analyse.csv', 
#             sep=';', row.names=TRUE, col.names=TRUE)


