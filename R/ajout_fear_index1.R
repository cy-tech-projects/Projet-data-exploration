# ============================================================================
# ANALYSE FEAR & GREED INDEX - BITCOIN
# ============================================================================
# Ce script analyse la relation entre l'indice Fear & Greed et le Bitcoin
# Auteur: Votre Nom
# Date: 2025
# ============================================================================

# Packages nécessaires
# install.packages(c("FactoMineR", "factoextra", "corrplot"))
library(FactoMineR)
library(factoextra)
library(corrplot)

# ============================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("CHARGEMENT DES DONNÉES AVEC FEAR & GREED INDEX\n")
cat(rep("=", 70), "\n\n", sep="")

# Charger le dataset enrichi
tab <- read.csv("./data/bitcoin_with_fear_greed.csv", 
                header=TRUE, 
                stringsAsFactors=FALSE)

# Conversion de la date
tab$date <- as.Date(tab$date)

# Conversion des variables numériques
numeric_cols <- c("close", "ret", "vol_30d", "vol_btc", "vol_eth",
                  "close_eth", "close_sp500", "close_gold",
                  "ret_eth", "ret_sp500", "ret_gold",
                  "fgi_score", "fgi_change", "fgi_pct_change",
                  "fgi_ma7", "fgi_ma30", "fgi_signal", "fgi_volatility",
                  "fgi_regime_change")

for (col in numeric_cols) {
  if (col %in% colnames(tab)) {
    tab[[col]] <- as.numeric(tab[[col]])
  }
}

# Conversion des variables qualitatives
tab$trend <- as.factor(tab$trend)
tab$vol_level <- factor(tab$vol_level, 
                        levels=c("Faible", "Moyenne", "Élevée"), 
                        ordered=TRUE)
tab$fgi_category <- factor(tab$fgi_category,
                           levels=c("Extreme Fear", "Fear", "Neutral", 
                                   "Greed", "Extreme Greed"),
                           ordered=TRUE)
tab$fgi_simple <- factor(tab$fgi_simple,
                        levels=c("Fear", "Neutral", "Greed"),
                        ordered=TRUE)

# Aperçu
cat("Dimensions:", dim(tab), "\n")
cat("Période:", min(tab$date, na.rm=TRUE), "à", max(tab$date, na.rm=TRUE), "\n\n")

cat("Colonnes Fear & Greed disponibles:\n")
fgi_cols <- colnames(tab)[grepl("^fgi_", colnames(tab))]
cat(paste("  -", fgi_cols, collapse="\n"), "\n\n")

# ============================================================================
# 2. STATISTIQUES DESCRIPTIVES FEAR & GREED
# ============================================================================

cat(rep("=", 70), "\n", sep="")
cat("STATISTIQUES DESCRIPTIVES - FEAR & GREED INDEX\n")
cat(rep("=", 70), "\n\n", sep="")

# Résumé du score
cat("--- Score Fear & Greed ---\n")
print(summary(tab$fgi_score))

cat("\n--- Distribution par catégorie ---\n")
print(table(tab$fgi_category))
cat("\nProportions (%):\n")
print(round(prop.table(table(tab$fgi_category)) * 100, 2))

cat("\n--- Distribution simplifiée ---\n")
print(table(tab$fgi_simple))
cat("\nProportions (%):\n")
print(round(prop.table(table(tab$fgi_simple)) * 100, 2))

# ============================================================================
# 3. VISUALISATIONS UNIVARIÉES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("VISUALISATIONS FEAR & GREED INDEX\n")
cat(rep("=", 70), "\n\n", sep="")

# Créer le dossier de sortie
if (!dir.exists("./resultats")) {
  dir.create("./resultats")
}

# Ouvrir le PDF
pdf("./resultats/analyse_fear_greed.pdf", width=11, height=8.5)

# --- PAGE DE TITRE ---
plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.7, "ANALYSE FEAR & GREED INDEX", cex=3, font=2, col="#2C3E50")
text(0.5, 0.55, "Bitcoin Market Sentiment", cex=2, col="#34495E")
text(0.5, 0.35, paste("Période:", min(tab$date), "-", max(tab$date)), 
     cex=1.3, col="#7F8C8D")
text(0.5, 0.25, paste(nrow(tab), "observations"), cex=1.3, col="#7F8C8D")
text(0.5, 0.1, format(Sys.time(), "%d %B %Y"), cex=1.2, col="#95A5A6")

# --- DISTRIBUTIONS ---
par(mfrow=c(2,2), mar=c(4,4,3,2))

# Histogramme du score
hist(tab$fgi_score, 
     main="Distribution Score Fear & Greed",
     xlab="Score FGI (0-100)", ylab="Fréquence",
     col="#3498DB", border="white", breaks=30, las=1)
abline(v=c(25, 45, 55, 75), col="red", lty=2, lwd=2)
abline(v=median(tab$fgi_score, na.rm=TRUE), col="darkblue", lwd=3)
legend("topright", c("Médiane", "Seuils"), 
       col=c("darkblue", "red"), lwd=c(3,2), lty=c(1,2), bty="n")

# Boxplot du score
boxplot(tab$fgi_score, 
        main="Boxplot Score FGI",
        ylab="Score (0-100)", col="#3498DB", las=1, outcol="red")
abline(h=c(25, 45, 55, 75), col="red", lty=2)
text(1.3, 25, "Extreme Fear/Fear", cex=0.8, col="red")
text(1.3, 50, "Neutral", cex=0.8, col="red")
text(1.3, 75, "Greed/Extreme Greed", cex=0.8, col="red")

# Distribution par catégorie
t_cat <- table(tab$fgi_category)
barplot(t_cat, main="Distribution par Catégorie",
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        ylab="Fréquence", las=2, cex.names=0.8)

# Distribution simplifiée
t_simple <- table(tab$fgi_simple)
barplot(t_simple, main="Distribution Simplifiée",
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        ylab="Fréquence", las=1)

# --- ÉVOLUTION TEMPORELLE ---
par(mfrow=c(2,1), mar=c(4,4,3,2))

# Score dans le temps
plot(tab$date, tab$fgi_score, type="l", 
     main="Évolution du Score Fear & Greed",
     xlab="Date", ylab="Score FGI", col="#2980B9", lwd=1.5, las=1)
abline(h=c(25, 45, 55, 75), col="gray70", lty=2)
abline(h=50, col="black", lwd=2)
grid()

# Avec moyenne mobile
plot(tab$date, tab$fgi_score, type="l", 
     main="Score FGI avec Moyennes Mobiles",
     xlab="Date", ylab="Score", col="gray70", lwd=1, las=1)
lines(tab$date, tab$fgi_ma7, col="#E74C3C", lwd=2)
lines(tab$date, tab$fgi_ma30, col="#2ECC71", lwd=2)
legend("topleft", c("Score", "MA 7j", "MA 30j"),
       col=c("gray70", "#E74C3C", "#2ECC71"), lwd=c(1,2,2), bty="n")
grid()

# ============================================================================
# 4. RELATION FEAR & GREED x PRIX BITCOIN
# ============================================================================

cat(rep("=", 70), "\n", sep="")
cat("ANALYSE BIVARIÉE: FEAR & GREED x BITCOIN\n")
cat(rep("=", 70), "\n\n", sep="")

par(mfrow=c(2,2), mar=c(4,4,3,2))

# Nuage de points FGI vs Prix BTC
plot(tab$fgi_score, tab$close,
     main="Prix BTC vs Fear & Greed",
     xlab="Score FGI", ylab="Prix BTC (USD)",
     pch=20, col=rgb(0.2,0.4,0.8,0.3), las=1)
abline(lm(close ~ fgi_score, data=tab), col="red", lwd=2)
cor_fgi_close <- cor(tab$fgi_score, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_fgi_close, 3)),
       bty="n", text.col="red", cex=1.2)
grid()

# FGI vs Rendements BTC
plot(tab$fgi_score, tab$ret,
     main="Rendements BTC vs Fear & Greed",
     xlab="Score FGI", ylab="Rendement BTC",
     pch=20, col=rgb(0.8,0.2,0.2,0.3), las=1)
abline(h=0, col="gray50", lty=2)
abline(lm(ret ~ fgi_score, data=tab), col="darkred", lwd=2)
cor_fgi_ret <- cor(tab$fgi_score, tab$ret, use="complete.obs")
legend("topright", legend=paste("r =", round(cor_fgi_ret, 3)),
       bty="n", text.col="darkred", cex=1.2)
grid()

# FGI Change vs Rendements
plot(tab$fgi_change, tab$ret,
     main="Rendements vs Variation FGI",
     xlab="Variation FGI", ylab="Rendement BTC",
     pch=20, col=rgb(0.5,0,0.5,0.3), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ fgi_change, data=tab), col="purple", lwd=2)
cor_fgi_change_ret <- cor(tab$fgi_change, tab$ret, use="complete.obs")
legend("topright", legend=paste("r =", round(cor_fgi_change_ret, 3)),
       bty="n", text.col="purple", cex=1.2)
grid()

# Signal FGI vs Rendements
plot(tab$fgi_signal, tab$ret,
     main="Rendements vs Signal FGI",
     xlab="Signal FGI (écart MA30)", ylab="Rendement BTC",
     pch=20, col=rgb(0,0.5,0.5,0.3), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ fgi_signal, data=tab), col="darkgreen", lwd=2)
grid()

# --- Prix BTC par catégorie FGI ---
par(mfrow=c(2,2), mar=c(6,4,3,2))

boxplot(close ~ fgi_category, data=tab,
        main="Prix BTC par Catégorie FGI",
        xlab="", ylab="Prix BTC (USD)",
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        las=2, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ fgi_simple, data=tab,
        main="Prix BTC par Sentiment (simplifié)",
        xlab="", ylab="Prix BTC (USD)",
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(ret ~ fgi_category, data=tab,
        main="Rendements par Catégorie FGI",
        xlab="", ylab="Rendement BTC",
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        las=2, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(ret ~ fgi_simple, data=tab,
        main="Rendements par Sentiment (simplifié)",
        xlab="", ylab="Rendement BTC",
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        las=1, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

# Statistiques par groupe
cat("\n--- Moyennes de Prix par Catégorie FGI ---\n")
print(tapply(tab$close, tab$fgi_category, mean, na.rm=TRUE))

cat("\n--- Moyennes de Rendements par Catégorie FGI ---\n")
print(tapply(tab$ret, tab$fgi_category, mean, na.rm=TRUE))

cat("\n--- ANOVA: Prix ~ FGI Category ---\n")
anova_close_fgi <- aov(close ~ fgi_category, data=tab)
print(summary(anova_close_fgi))

cat("\n--- ANOVA: Rendements ~ FGI Category ---\n")
anova_ret_fgi <- aov(ret ~ fgi_category, data=tab)
print(summary(anova_ret_fgi))

# ============================================================================
# 5. TABLEAUX CROISÉS: TREND x FGI
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("ANALYSE CROISÉE: TREND x FEAR & GREED\n")
cat(rep("=", 70), "\n\n", sep="")

# Tableau de contingence
T_trend_fgi <- table(tab$trend, tab$fgi_simple)
cat("Tableau de contingence Trend x FGI:\n")
print(T_trend_fgi)

# Profils
prof_lignes <- prop.table(T_trend_fgi, margin=1)
cat("\nProfils lignes (%):\n")
print(round(prof_lignes * 100, 2))

# Visualisation
par(mfrow=c(1,2), mar=c(6,4,3,2))

barplot(T_trend_fgi, beside=TRUE, legend=TRUE,
        main="Trend x Sentiment FGI",
        col=c("#E74C3C", "#2ECC71"),
        ylab="Fréquence", las=1,
        args.legend=list(x="topleft", title="Trend"))

barplot(t(prof_lignes), beside=TRUE, legend=TRUE,
        main="Profils Lignes (%)",
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        ylab="Proportion", las=1,
        args.legend=list(x="topleft", title="FGI"))

# Test Chi-2
cat("\n--- Test Chi-2 d'indépendance ---\n")
chi2_test <- chisq.test(T_trend_fgi)
print(chi2_test)

# ============================================================================
# 6. CORRÉLATIONS MULTIVARIÉES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("MATRICE DE CORRÉLATION AVEC FGI\n")
cat(rep("=", 70), "\n\n", sep="")

# Variables pour corrélations
vars_cor <- c("close", "ret", "vol_30d", "fgi_score", "fgi_change",
              "fgi_ma7", "fgi_ma30", "fgi_signal", "fgi_volatility",
              "close_eth", "close_sp500", "close_gold")

# Matrice de corrélation
mat_cor <- cor(tab[, vars_cor], use="complete.obs")
print(round(mat_cor, 3))

# Visualisation avec corrplot
par(mfrow=c(1,1), mar=c(2,2,3,2))
corrplot(mat_cor, method="color", type="upper",
         tl.col="black", tl.srt=45, tl.cex=0.8,
         addCoef.col="black", number.cex=0.6,
         title="Matrice de Corrélation avec FGI",
         mar=c(0,0,2,0))

# ============================================================================
# 7. ACP AVEC FGI
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("ANALYSE EN COMPOSANTES PRINCIPALES (avec FGI)\n")
cat(rep("=", 70), "\n\n", sep="")

vars_acp <- c("close", "ret", "vol_30d", "vol_btc",
              "fgi_score", "fgi_ma30", "fgi_volatility",
              "close_eth", "close_sp500", "close_gold")

num_acp <- tab[, vars_acp]
num_acp <- num_acp[complete.cases(num_acp), ]

cat("Variables utilisées:", vars_acp, "\n")
cat("Observations complètes:", nrow(num_acp), "\n\n")

res_pca <- PCA(num_acp, scale.unit=TRUE, graph=FALSE)

print(summary(res_pca))

cat("\nValeurs propres:\n")
print(res_pca$eig)

# Scree plot
print(fviz_eig(res_pca, addlabels=TRUE,
               main="Variance Expliquée (avec FGI)"))

# Cercle des corrélations
print(fviz_pca_var(res_pca, col.var="contrib",
                   gradient.cols=c("#2ECC71", "#F39C12", "#E74C3C"),
                   repel=TRUE,
                   title="Cercle des Corrélations (avec FGI)"))

# Contributions
cat("\nContributions à PC1:\n")
print(round(sort(res_pca$var$contrib[,1], decreasing=TRUE), 2))

cat("\nContributions à PC2:\n")
print(round(sort(res_pca$var$contrib[,2], decreasing=TRUE), 2))

# ============================================================================
# 8. ANALYSE DES CHANGEMENTS DE RÉGIME
# ============================================================================

cat("\n", rep("=", 70), "\n", sep="")
cat("ANALYSE DES CHANGEMENTS DE RÉGIME FGI\n")
cat(rep("=", 70), "\n\n", sep="")

# Compter les changements
n_to_greed <- sum(tab$fgi_regime_change == 1, na.rm=TRUE)
n_to_fear <- sum(tab$fgi_regime_change == -1, na.rm=TRUE)

cat("Passages vers Greed:", n_to_greed, "\n")
cat("Passages vers Fear:", n_to_fear, "\n\n")

# Rendement moyen lors des changements
cat("Rendement moyen lors des passages:\n")
cat("  Vers Greed:", 
    mean(tab$ret[tab$fgi_regime_change == 1], na.rm=TRUE), "\n")
cat("  Vers Fear:", 
    mean(tab$ret[tab$fgi_regime_change == -1], na.rm=TRUE), "\n")
cat("  Pas de changement:", 
    mean(tab$ret[tab$fgi_regime_change == 0], na.rm=TRUE), "\n\n")

# Visualisation
par(mfrow=c(1,1), mar=c(4,4,3,2))
plot(tab$date, tab$fgi_score, type="l",
     main="Changements de Régime FGI",
     xlab="Date", ylab="Score FGI", col="gray70", lwd=1, las=1)
points(tab$date[tab$fgi_regime_change == 1], 
       tab$fgi_score[tab$fgi_regime_change == 1],
       col="#2ECC71", pch=19, cex=1.5)
points(tab$date[tab$fgi_regime_change == -1], 
       tab$fgi_score[tab$fgi_regime_change == -1],
       col="#E74C3C", pch=19, cex=1.5)
legend("topleft", c("Score FGI", "Passage Greed", "Passage Fear"),
       col=c("gray70", "#2ECC71", "#E74C3C"),
       lty=c(1,NA,NA), pch=c(NA,19,19), lwd=c(1,NA,NA), bty="n")
grid()

# ============================================================================
# FERMETURE PDF ET RÉSUMÉ
# ============================================================================

dev.off()

cat("\n", rep("=", 70), "\n", sep="")
cat("✓ ANALYSE TERMINÉE AVEC SUCCÈS\n")
cat(rep("=", 70), "\n\n", sep="")
cat("Fichier PDF créé: ./resultats/analyse_fear_greed.pdf\n")
cat("\nCorrélations clés:\n")
cat(sprintf("  FGI x Prix BTC:     %.4f\n", cor_fgi_close))
cat(sprintf("  FGI x Rendements:   %.4f\n", cor_fgi_ret))
cat(sprintf("  Δ FGI x Rendements: %.4f\n", cor_fgi_change_ret))
cat("\n")