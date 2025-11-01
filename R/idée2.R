# ============================================================================
# ANALYSE STATISTIQUE BITCOIN - CODE R COMPLET (VERSION FINALE)
# ============================================================================

# Installation des packages nécessaires (décommenter si besoin)
# install.packages(c("FactoMineR", "factoextra"))

# ============================================================================
# CONFIGURATION EXPORT PDF
# ============================================================================

# Créer un dossier pour les résultats si nécessaire
if (!dir.exists("./resultats")) {
  dir.create("./resultats")
}

# Ouvrir le fichier PDF pour tous les graphiques
pdf("./resultats/analyse_bitcoin_complete.pdf", width=11, height=8.5)

# Configuration des marges et layout
par(mar=c(5,4,4,2)+0.1)

# ============================================================================
# 1) LECTURE ET PRÉPARATION DES DONNÉES - SOLUTION POUR VIRGULES
# ============================================================================

# Votre fichier utilise des VIRGULES comme séparateur !
tab <- read.csv("./data/multi_asset_dataset.csv", 
                header=TRUE, 
                stringsAsFactors=FALSE)

# Vérification de la lecture
cat("Dimensions du fichier:", dim(tab), "\n")
cat("Nombre de colonnes:", ncol(tab), "\n")
cat("\nNoms de colonnes:\n")
print(colnames(tab))

# Renommer les colonnes pour simplifier
# Adapter selon vos colonnes réelles
colnames(tab) <- c("date", "close", "ret", "vol_30d", "trend", "vol_level", 
                   "ret_bucket", "mois", "jour_semaine", "close_eth", 
                   "vol_btc", "vol_eth", "close_sp500", "close_gold",
                   "ret_eth", "ret_sp500", "ret_gold", "vol30_gold",
                   "vol30_sp500", "vol30_eth", "corr30_btc_gold",
                   "corr30_btc_sp500", "corr30_btc_eth", "jour_mois", "mois_num")

# Conversion de la date
tab$date <- as.Date(tab$date)

# Conversion des colonnes numériques (car lues comme character)
numeric_cols <- c("close", "ret", "vol_30d", "close_eth", "vol_btc", "vol_eth",
                  "close_sp500", "close_gold", "ret_eth", "ret_sp500", "ret_gold",
                  "vol30_gold", "vol30_sp500", "vol30_eth", "corr30_btc_gold",
                  "corr30_btc_sp500", "corr30_btc_eth", "jour_mois", "mois_num")

for (col in numeric_cols) {
  if (col %in% colnames(tab)) {
    tab[[col]] <- as.numeric(tab[[col]])
  }
}

# Afficher la structure finale
cat("\n=== STRUCTURE FINALE DES DONNÉES ===\n")
str(tab)
cat("\nAperçu des données:\n")
print(head(tab))
cat("\nRésumé rapide:\n")
print(summary(tab[, c("close", "ret", "vol_30d", "trend", "vol_level")]))

# ============================================================================
# 2) STATISTIQUES DESCRIPTIVES - VARIABLES QUANTITATIVES
# ============================================================================

cat("\n=== RÉSUMÉS NUMÉRIQUES ===\n")

# Résumés détaillés
cat("\n--- Close (Prix BTC) ---\n")
print(summary(tab$close))

cat("\n--- Rendements BTC ---\n")
print(summary(tab$ret))

cat("\n--- Volatilité 30j ---\n")
print(summary(tab$vol_30d))

cat("\n--- Volume BTC ---\n")
print(summary(tab$vol_btc))

# Histogrammes
par(mfrow=c(2,2))

hist(tab$close, main="Distribution du Close BTC", xlab="Close (USD)", 
     col="lightblue", breaks=50)

hist(tab$vol_btc, main="Distribution du Volume BTC", xlab="Volume", 
     col="lightgreen", breaks=50)

hist(tab$ret, main="Distribution des Rendements BTC", xlab="Rendement", 
     col="lightyellow", breaks=50)

hist(tab$vol_30d, main="Distribution Volatilité 30j", xlab="Vol 30d", 
     col="lightcoral", breaks=50)

par(mfrow=c(1,1))

# Boxplots pour détecter les outliers
par(mfrow=c(2,2))

boxplot(tab$close, main="Boxplot Close BTC", ylab="Close (USD)", col="lightblue")
boxplot(tab$vol_btc, main="Boxplot Volume", ylab="Volume", col="lightgreen")
boxplot(tab$ret, main="Boxplot Rendements", ylab="Rendement", col="lightyellow")
boxplot(tab$vol_30d, main="Boxplot Volatilité", ylab="Vol 30d", col="lightcoral")

par(mfrow=c(1,1))

# ============================================================================
# 3) VARIABLES QUALITATIVES
# ============================================================================

# Conversion en facteurs
tab$trend <- as.factor(tab$trend)
tab$vol_level <- factor(tab$vol_level, 
                        levels=c("Faible", "Moyenne", "Élevée"), 
                        ordered=TRUE)
tab$ret_bucket <- as.factor(tab$ret_bucket)
tab$mois <- factor(tab$mois, 
                   levels=c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin",
                            "Juil", "Août", "Sept", "Oct", "Nov", "Déc"))

cat("\n=== TABLES DE CONTINGENCE ===\n")

# Distribution Trend
t_trend <- table(tab$trend, useNA="ifany")
cat("\nDistribution Trend:\n")
print(t_trend)
cat("Proportions (%):\n")
print(round(prop.table(t_trend)*100, 2))

# Distribution Volatilité
t_vol <- table(tab$vol_level, useNA="ifany")
cat("\nDistribution Niveau de Volatilité:\n")
print(t_vol)
cat("Proportions (%):\n")
print(round(prop.table(t_vol)*100, 2))

# Distribution Ret_bucket
t_ret <- table(tab$ret_bucket, useNA="ifany")
cat("\nDistribution Buckets de Rendements:\n")
print(t_ret)
cat("Proportions (%):\n")
print(round(prop.table(t_ret)*100, 2))

# Distribution Mois
t_mois <- table(tab$mois, useNA="ifany")
cat("\nDistribution par Mois:\n")
print(t_mois)

# Visualisations
par(mfrow=c(2,2))

barplot(t_trend, main="Distribution Trend", 
        col=c("red", "green"), ylab="Fréquence")

barplot(t_vol, main="Distribution Volatilité", 
        col=c("lightgreen", "orange", "red"), ylab="Fréquence")

barplot(t_ret, main="Distribution Ret Buckets", 
        col=rainbow(length(t_ret)), ylab="Fréquence")

barplot(t_mois, main="Distribution par Mois", 
        col=rainbow(12), ylab="Fréquence", las=2, cex.names=0.7)

par(mfrow=c(1,1))

# ============================================================================
# 4) TRANSFORMATION QUANTI -> QUALI (CLASSES)
# ============================================================================

# Création de classes pour le close
brks_close <- quantile(tab$close, probs=c(0, .25, .5, .75, 1), na.rm=TRUE)
tab$close_bucket <- cut(tab$close, breaks=unique(brks_close), 
                        include.lowest=TRUE, 
                        labels=c("Bas", "Moyen-Bas", "Moyen-Haut", "Élevé")[1:(length(unique(brks_close))-1)])

cat("\nClasses de Close:\n")
print(table(tab$close_bucket))

barplot(table(tab$close_bucket), main="Distribution par Classes de Prix", 
        col="steelblue", ylab="Fréquence")

# ============================================================================
# 5) ANALYSE BIVARIÉE - QUANTI x QUANTI
# ============================================================================

cat("\n=== ANALYSE BIVARIÉE QUANTITATIVE ===\n")

# 5.1) BTC Close vs ETH Close
plot(tab$close_eth, tab$close, 
     main="BTC vs ETH (Prix)",
     xlab="Close ETH (USD)", ylab="Close BTC (USD)", 
     pch=20, col=rgb(0,0,1,0.3))

cor_btc_eth <- cor(tab$close_eth, tab$close, use="complete.obs")
cat("\nCorrélation BTC-ETH (prix):", round(cor_btc_eth, 4), "\n")

# Régression
modele_eth <- lm(close ~ close_eth, data=tab)
abline(modele_eth, col="red", lwd=2)
cat("R² BTC~ETH:", round(summary(modele_eth)$r.squared, 4), "\n")

# 5.2) BTC vs SP500
plot(tab$close_sp500, tab$close, 
     main="BTC vs SP500",
     xlab="Close SP500", ylab="Close BTC (USD)", 
     pch=20, col=rgb(1,0,0,0.3))

cor_btc_sp500 <- cor(tab$close_sp500, tab$close, use="complete.obs")
cat("Corrélation BTC-SP500:", round(cor_btc_sp500, 4), "\n")

# 5.3) Rendements BTC vs rendements autres actifs
par(mfrow=c(2,2))

# BTC vs ETH rendements
plot(tab$ret_eth, tab$ret, 
     main="Rendements: BTC vs ETH",
     xlab="Rendement ETH", ylab="Rendement BTC", 
     pch=20, col=rgb(0,0,1,0.3))
abline(lm(ret ~ ret_eth, data=tab), col="red", lwd=2)
cor_ret_eth <- cor(tab$ret_eth, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_eth, 3)), bty="n")

# BTC vs SP500 rendements
plot(tab$ret_sp500, tab$ret, 
     main="Rendements: BTC vs SP500",
     xlab="Rendement SP500", ylab="Rendement BTC", 
     pch=20, col=rgb(1,0,0,0.3))
abline(lm(ret ~ ret_sp500, data=tab), col="blue", lwd=2)
cor_ret_sp500 <- cor(tab$ret_sp500, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_sp500, 3)), bty="n")

# BTC vs Gold rendements
plot(tab$ret_gold, tab$ret, 
     main="Rendements: BTC vs Gold",
     xlab="Rendement Gold", ylab="Rendement BTC", 
     pch=20, col=rgb(1,0.5,0,0.3))
abline(lm(ret ~ ret_gold, data=tab), col="darkgreen", lwd=2)
cor_ret_gold <- cor(tab$ret_gold, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_gold, 3)), bty="n")

# Volatilité vs Rendement absolu
plot(tab$vol_30d, abs(tab$ret), 
     main="Volatilité vs |Rendement|",
     xlab="Volatilité 30j", ylab="|Rendement BTC|", 
     pch=20, col=rgb(0.5,0,0.5,0.3))

par(mfrow=c(1,1))

# Matrice de corrélation complète
cat("\n--- MATRICE DE CORRÉLATION ---\n")
vars_corr <- c("close", "ret", "vol_30d", "close_eth", "close_sp500", 
               "close_gold", "ret_eth", "ret_sp500", "ret_gold")
mat_cor <- cor(tab[, vars_corr], use="complete.obs")
print(round(mat_cor, 3))

# ============================================================================
# 6) ANALYSE BIVARIÉE - QUALI x QUANTI
# ============================================================================

cat("\n=== ANALYSE QUALI x QUANTI ===\n")

# Boxplots conditionnels
par(mfrow=c(2,2))

boxplot(close ~ trend, data=tab, 
        main="Prix BTC par Tendance", 
        ylab="Close (USD)", 
        col=c("red", "green"))

boxplot(close ~ vol_level, data=tab, 
        main="Prix BTC par Niveau de Volatilité", 
        ylab="Close (USD)", 
        col=c("lightgreen", "orange", "red"))

boxplot(ret ~ trend, data=tab, 
        main="Rendements par Tendance", 
        ylab="Rendement", 
        col=c("red", "green"))

boxplot(vol_30d ~ trend, data=tab, 
        main="Volatilité par Tendance", 
        ylab="Vol 30d", 
        col=c("red", "green"))

par(mfrow=c(1,1))

# Moyennes par modalités
cat("\nMoyennes de Close par Trend:\n")
print(tapply(tab$close, tab$trend, mean, na.rm=TRUE))

cat("\nMoyennes de Close par Niveau de Volatilité:\n")
print(tapply(tab$close, tab$vol_level, mean, na.rm=TRUE))

cat("\nMoyennes de Rendement par Trend:\n")
print(tapply(tab$ret, tab$trend, mean, na.rm=TRUE))

cat("\nMoyennes de Volatilité par Trend:\n")
print(tapply(tab$vol_30d, tab$trend, mean, na.rm=TRUE))

# ANOVA
cat("\n--- ANOVA: Close ~ Trend ---\n")
anova_close_trend <- aov(close ~ trend, data=tab)
print(summary(anova_close_trend))

cat("\n--- ANOVA: Rendement ~ Trend ---\n")
anova_ret_trend <- aov(ret ~ trend, data=tab)
print(summary(anova_ret_trend))

# ============================================================================
# 7) ANALYSE BIVARIÉE - QUALI x QUALI
# ============================================================================

cat("\n=== ANALYSE QUALI x QUALI ===\n")

# Tableau de contingence Trend x Vol_level
T <- table(tab$trend, tab$vol_level)
cat("\nTableau de contingence Trend x Vol_level:\n")
print(T)

prof_lignes <- prop.table(T, margin=1)
prof_colonnes <- prop.table(T, margin=2)

cat("\nProfils lignes (%):\n")
print(round(prof_lignes * 100, 2))

cat("\nProfils colonnes (%):\n")
print(round(prof_colonnes * 100, 2))

# Visualisations
par(mfrow=c(1,2))

barplot(T, beside=TRUE, legend=TRUE, 
        main="Trend x Vol_level (effectifs)",
        col=c("red", "green"),
        ylab="Fréquence",
        args.legend=list(x="topleft", cex=0.8))

barplot(t(prof_lignes), beside=TRUE, legend=TRUE,
        main="Profils lignes (%)",
        col=c("lightgreen", "orange", "red"),
        ylab="Proportion",
        args.legend=list(x="topleft", cex=0.8))

par(mfrow=c(1,1))

# Test du Chi-2
if (all(T > 0)) {
  chi2_test <- chisq.test(T)
  cat("\nTest d'indépendance Chi-2:\n")
  print(chi2_test)
}

# ============================================================================
# 8) ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
# ============================================================================

cat("\n=== ANALYSE EN COMPOSANTES PRINCIPALES ===\n")

# Charger les packages
if (requireNamespace("FactoMineR", quietly=TRUE) && 
    requireNamespace("factoextra", quietly=TRUE)) {
  
  library(FactoMineR)
  library(factoextra)
  
  # Variables pour l'ACP
  vars_acp <- c("close", "ret", "vol_30d", "vol_btc", "close_eth", 
                "close_sp500", "close_gold", "ret_eth", "ret_sp500", "ret_gold")
  
  # Données complètes
  num <- tab[, vars_acp]
  num <- num[complete.cases(num), ]
  
  cat("Variables utilisées:", vars_acp, "\n")
  cat("Nombre d'observations complètes:", nrow(num), "\n\n")
  
  # ACP
  res_pca <- PCA(num, scale.unit=TRUE, graph=FALSE)
  
  # Résumé
  print(summary(res_pca))
  
  cat("\nValeurs propres et variance expliquée:\n")
  print(res_pca$eig)
  
  # Scree plot
  print(fviz_eig(res_pca, addlabels=TRUE, 
                 main="Scree plot - Variance expliquée par composante"))
  
  # Cercle des corrélations
  print(fviz_pca_var(res_pca, col.var="contrib", 
                     gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel=TRUE,
                     title="Cercle des corrélations - Variables"))
  
  # Contributions
  cat("\nContributions des variables à PC1:\n")
  print(round(sort(res_pca$var$contrib[,1], decreasing=TRUE), 2))
  
  cat("\nContributions des variables à PC2:\n")
  print(round(sort(res_pca$var$contrib[,2], decreasing=TRUE), 2))
  
  # Projection des individus
  print(fviz_pca_ind(res_pca, 
                     col.ind="cos2",
                     gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel=FALSE,
                     title="Projection des individus",
                     alpha.ind=0.3))
  
  # Biplot
  print(fviz_pca_biplot(res_pca, 
                        col.var="red", 
                        col.ind=rgb(0,0,1,0.2),
                        repel=TRUE,
                        title="Biplot - Variables et Individus"))
  
} else {
  cat("Packages FactoMineR et/ou factoextra non installés.\n")
  cat("Installez-les avec: install.packages(c('FactoMineR', 'factoextra'))\n")
}

cat("\n=== ANALYSE TERMINÉE AVEC SUCCÈS ===\n")
cat("Tous les graphiques et statistiques ont été générés.\n")