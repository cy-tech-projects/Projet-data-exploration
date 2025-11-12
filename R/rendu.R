##========================================
##                TP1                   ##
##========================================



# Importer le jeu de données
Dataset <- read.csv("R/data_final.csv", header=T, sep=';', row.names=1, encoding="UTF-8")

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
                      "Volume_ETH", "Var_Volume_ETH", "Prix_BNB", "Var_Prix_BNB",
                      "Prix_USDT", "Var_Prix_USDT", "Fear_Index", 
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

#Nombre de jour avec le btc eth au dessus de certains paliers et le gold
# Fonction utilitaire : calcule le nombre de jours au-dessus de plusieurs paliers
make_threshold_table <- function(x, thresholds, name = "") {
    total_days <- sum(!is.na(x))
    counts <- sapply(thresholds, function(t) sum(x > t, na.rm = TRUE))
    df <- data.frame(Seuil = thresholds,
                                     Jours_Au_Dessus = as.integer(counts),
                                     Pourcentage = round(100 * counts / total_days, 2))
    cat("\nTableau pour", name, "\n")
    print(df, row.names = FALSE)
    return(df)
}

# Exemples de paliers (adapter selon vos besoins)
btc_thresholds <- c(20000, 30000, 40000)
eth_thresholds <- c(1000, 2000, 3000)
gold_thresholds <- c(1500, 1800, 2000)  # remplacer si "Autre_Variable" n'est pas l'or

# Tables pour BTC, ETH et "Autre_Variable" (ex: or)
table_btc <- make_threshold_table(Dataset$Prix_BTC, btc_thresholds, "BTC")
table_eth <- make_threshold_table(Dataset$Prix_ETH, eth_thresholds, "ETH")
table_gold <- make_threshold_table(Dataset$Autre_Variable, gold_thresholds, "Autre_Variable")



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
par(mfrow=c(1,2))  # Créer une grille 1x2 pour les graphiques

# Histogramme
hist(Dataset$Prix_BTC, 
     main='Histogramme du Prix BTC',
     xlab='Prix BTC',
     col='blue',
     breaks=10)

# Boîte de Tukey (Boxplot)
boxplot(Dataset$Prix_BTC,
        main='Boxplot du Prix BTC',
        ylab='Prix BTC',
        col='grey')

# Réinitialiser la grille graphique
par(mfrow=c(1,1))

# ========================================
# EXERCICE : Transformation des données
# ========================================

cat("\n========== EXERCICE : TRANSFORMATIONS ==========\n")

# a) Ajouter 10 à la série Prix_BTC
Prix_BTC_plus10 = Dataset$Prix_BTC + 10
cat("\nAjout de 10:\n")
cat("Ancienne moyenne:", mean(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Nouvelle moyenne:", mean(Prix_BTC_plus10, na.rm=TRUE), "\n")
cat("Différence:", mean(Prix_BTC_plus10, na.rm=TRUE) - mean(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Ancien écart-type:", sd(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Nouvel écart-type:", sd(Prix_BTC_plus10, na.rm=TRUE), "\n")
cat("CONCLUSION: L'ajout d'une constante déplace la moyenne mais ne change pas l'écart-type\n")

# b) Multiplier la série Prix_BTC par 2
Prix_BTC_fois2 = Dataset$Prix_BTC * 2
cat("\nMultiplication par 2:\n")
cat("Ancienne moyenne:", mean(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Nouvelle moyenne:", mean(Prix_BTC_fois2, na.rm=TRUE), "\n")
cat("Ancien écart-type:", sd(Dataset$Prix_BTC, na.rm=TRUE), "\n")
cat("Nouvel écart-type:", sd(Prix_BTC_fois2, na.rm=TRUE), "\n")
cat("CONCLUSION: La multiplication par une constante multiplie la moyenne ET l'écart-type\n")

# ========================================
# 4. ANALYSE DES VARIABLES QUALITATIVES
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

cat("\n========== ANALYSE TERMINÉE ==========\n")
cat("N'oubliez pas de sauvegarder votre script!\n")



















