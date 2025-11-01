# ============================================================================
# ANALYSE STATISTIQUE BITCOIN AVEC FEAR & GREED INDEX
# ============================================================================
# Script complet pour l'exploration de données avec corrélations
# Projet: Data Exploration Bitcoin
# ============================================================================

# Installation des packages nécessaires (décommenter si besoin)
# install.packages(c("FactoMineR", "factoextra", "corrplot", "RColorBrewer"))

# ============================================================================
# CONFIGURATION EXPORT PDF
# ============================================================================

# Créer un dossier pour les résultats si nécessaire
if (!dir.exists("./resultats")) {
  dir.create("./resultats")
}

# Ouvrir le fichier PDF pour tous les graphiques
pdf("./resultats/analyse_bitcoin_fear_greed_complete.pdf", width=11.69, height=8.27)

# Configuration des marges et layout
par(mar=c(5,4,4,2)+0.1)

# ============================================================================
# PAGE DE TITRE
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.75, "ANALYSE STATISTIQUE BITCOIN", cex=3.2, font=2, col="#2C3E50")
text(0.5, 0.65, "avec Fear & Greed Index", cex=2.5, font=2, col="#3498DB")
text(0.5, 0.5, "Exploration de Données & Corrélations Multi-Actifs", 
     cex=1.8, col="#34495E")
text(0.5, 0.35, "Analyse Complète des Relations et Dépendances", 
     cex=1.5, col="#7F8C8D")
text(0.5, 0.15, format(Sys.time(), "%d %B %Y"), cex=1.3, col="#95A5A6")

# ============================================================================
# 1) LECTURE ET PRÉPARATION DES DONNÉES
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("CHARGEMENT DES DONNÉES ENRICHIES\n")
cat(strrep("=", 70), "\n\n", sep="")

# Chargement du fichier avec Fear & Greed
tab <- read.csv("./data/bitcoin_with_fear_greed.csv", 
                header=TRUE, 
                stringsAsFactors=FALSE)

# Vérification de la lecture
cat("Dimensions du fichier:", dim(tab), "\n")
cat("Nombre de colonnes:", ncol(tab), "\n\n")

# Conversion de la date
tab$date <- as.Date(tab$date)

# Conversion des colonnes numériques
numeric_cols <- c("close", "ret", "vol_30d", "close_eth", "vol_btc", "vol_eth",
                  "close_sp500", "close_gold", "ret_eth", "ret_sp500", "ret_gold",
                  "vol30_gold", "vol30_sp500", "vol30_eth", "corr30_btc_gold",
                  "corr30_btc_sp500", "corr30_btc_eth", "jour_mois", "mois_num",
                  "fgi_score", "fgi_change", "fgi_pct_change", "fgi_ma7", 
                  "fgi_ma30", "fgi_signal", "fgi_volatility", "fgi_regime_change")

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
tab$ret_bucket <- as.factor(tab$ret_bucket)
# tab$mois <- factor(tab$mois, 
#                    levels=c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin",
#                            "Juil", "Août", "Sept", "Oct", "Nov", "Déc"))
tab$fgi_category <- factor(tab$fgi_category,
                           levels=c("Extreme Fear", "Fear", "Neutral", 
                                   "Greed", "Extreme Greed"),
                           ordered=TRUE)
tab$fgi_simple <- factor(tab$fgi_simple,
                        levels=c("Fear", "Neutral", "Greed"),
                        ordered=TRUE)

# Afficher la structure finale
cat("Structure finale des données:\n")
cat("Période:", min(tab$date, na.rm=TRUE), "à", max(tab$date, na.rm=TRUE), "\n")
cat("Nombre d'observations:", nrow(tab), "\n")
cat("Variables totales:", ncol(tab), "\n\n")

# Aperçu
cat("Aperçu des données:\n")
print(head(tab[, c("date", "close", "ret", "fgi_score", "fgi_category", "trend")]))

# ============================================================================
# PAGE SECTION: STATISTIQUES DESCRIPTIVES
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "1. STATISTIQUES DESCRIPTIVES", cex=3, font=2, col="#2980B9")
text(0.5, 0.35, "Analyse univariée des variables", cex=1.8, col="#34495E")

# ============================================================================
# 2) STATISTIQUES DESCRIPTIVES - VARIABLES QUANTITATIVES
# ============================================================================

cat(strrep("=", 70), "\n", sep="")
cat("STATISTIQUES DESCRIPTIVES\n")
cat(strrep("=", 70), "\n\n", sep="")

# Résumés détaillés
cat("--- Prix BTC ---\n")
print(summary(tab$close))

cat("\n--- Rendements BTC ---\n")
print(summary(tab$ret))

cat("\n--- Volatilité 30j BTC ---\n")
print(summary(tab$vol_30d))

cat("\n--- Score Fear & Greed ---\n")
print(summary(tab$fgi_score))

cat("\n--- Variation FGI ---\n")
print(summary(tab$fgi_change))

# Histogrammes - Variables BTC
par(mfrow=c(2,3), mar=c(4,4,3,2))

hist(tab$close, main="Distribution Prix BTC", xlab="Close (USD)", 
     col="#3498DB", border="white", breaks=50, las=1)
abline(v=median(tab$close, na.rm=TRUE), col="red", lwd=2, lty=2)

hist(tab$vol_btc, main="Distribution Volume BTC", xlab="Volume", 
     col="#2ECC71", border="white", breaks=50, las=1)
abline(v=median(tab$vol_btc, na.rm=TRUE), col="red", lwd=2, lty=2)

hist(tab$ret, main="Distribution Rendements BTC", xlab="Rendement", 
     col="#F39C12", border="white", breaks=50, las=1)
abline(v=0, col="red", lwd=2, lty=2)
abline(v=median(tab$ret, na.rm=TRUE), col="blue", lwd=2, lty=2)

hist(tab$vol_30d, main="Distribution Volatilité 30j", xlab="Vol 30d", 
     col="#E74C3C", border="white", breaks=50, las=1)
abline(v=median(tab$vol_30d, na.rm=TRUE), col="red", lwd=2, lty=2)

hist(tab$fgi_score, main="Distribution Score FGI", xlab="Score (0-100)", 
     col="#9B59B6", border="white", breaks=30, las=1)
abline(v=c(25, 45, 55, 75), col="red", lty=2, lwd=1.5)
abline(v=median(tab$fgi_score, na.rm=TRUE), col="darkblue", lwd=2)

hist(tab$fgi_change, main="Distribution Variation FGI", xlab="Variation", 
     col="#16A085", border="white", breaks=50, las=1)
abline(v=0, col="red", lwd=2, lty=2)

par(mfrow=c(1,1))

# Boxplots
par(mfrow=c(2,3), mar=c(4,4,3,2))

boxplot(tab$close, main="Boxplot Prix BTC", ylab="Close (USD)",
        col="#3498DB", border="#2980B9", las=1, outcol="red", outpch=19)
grid(ny=NULL, nx=NA)

boxplot(tab$vol_btc, main="Boxplot Volume", ylab="Volume",
        col="#2ECC71", border="#27AE60", las=1, outcol="red", outpch=19)
grid(ny=NULL, nx=NA)

boxplot(tab$ret, main="Boxplot Rendements", ylab="Rendement",
        col="#F39C12", border="#E67E22", las=1, outcol="red", outpch=19)
abline(h=0, col="red", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(tab$vol_30d, main="Boxplot Volatilité", ylab="Vol 30d",
        col="#E74C3C", border="#C0392B", las=1, outcol="red", outpch=19)
grid(ny=NULL, nx=NA)

boxplot(tab$fgi_score, main="Boxplot Score FGI", ylab="Score",
        col="#9B59B6", border="#8E44AD", las=1, outcol="red", outpch=19)
abline(h=c(25, 50, 75), col="red", lty=2)
grid(ny=NULL, nx=NA)

boxplot(tab$fgi_change, main="Boxplot Variation FGI", ylab="Variation",
        col="#16A085", border="#138D75", las=1, outcol="red", outpch=19)
abline(h=0, col="red", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

par(mfrow=c(1,1))

# ============================================================================
# PAGE SECTION: VARIABLES QUALITATIVES
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "2. VARIABLES QUALITATIVES", cex=3, font=2, col="#8E44AD")
text(0.5, 0.35, "Distributions et fréquences", cex=1.8, col="#34495E")

# ============================================================================
# 3) VARIABLES QUALITATIVES
# ============================================================================

cat(strrep("=", 70), "\n", sep="")
cat("VARIABLES QUALITATIVES\n")
cat(strrep("=", 70), "\n\n", sep="")

# Distribution Trend
t_trend <- table(tab$trend, useNA="ifany")
cat("Distribution Trend:\n")
print(t_trend)
cat("Proportions (%):\n")
print(round(prop.table(t_trend)*100, 2))

# Distribution Volatilité
t_vol <- table(tab$vol_level, useNA="ifany")
cat("\nDistribution Niveau de Volatilité:\n")
print(t_vol)
cat("Proportions (%):\n")
print(round(prop.table(t_vol)*100, 2))

# Distribution Fear & Greed Catégories
t_fgi_cat <- table(tab$fgi_category, useNA="ifany")
cat("\nDistribution Catégories Fear & Greed:\n")
print(t_fgi_cat)
cat("Proportions (%):\n")
print(round(prop.table(t_fgi_cat)*100, 2))

# Distribution Fear & Greed Simple
t_fgi_simple <- table(tab$fgi_simple, useNA="ifany")
cat("\nDistribution Fear & Greed Simplifié:\n")
print(t_fgi_simple)
cat("Proportions (%):\n")
print(round(prop.table(t_fgi_simple)*100, 2))

# Visualisations
par(mfrow=c(2,3), mar=c(6,4,3,2))

# Trend
barplot(t_trend, main="Distribution Tendance", 
        col=c("#E74C3C", "#2ECC71"), ylab="Fréquence", las=1)
text(x=barplot(t_trend, plot=FALSE), y=t_trend, 
     labels=t_trend, pos=3, cex=1.2, font=2)

# Volatilité
barplot(t_vol, main="Distribution Volatilité", 
        col=c("#2ECC71", "#F39C12", "#E74C3C"), ylab="Fréquence", las=1)
text(x=barplot(t_vol, plot=FALSE), y=t_vol, 
     labels=t_vol, pos=3, cex=1.2, font=2)

# Ret bucket
t_ret <- table(tab$ret_bucket, useNA="ifany")
barplot(t_ret, main="Buckets Rendements", 
        col=rainbow(length(t_ret)), ylab="Fréquence", las=2, cex.names=0.7)

# FGI Catégories
barplot(t_fgi_cat, main="Catégories Fear & Greed", 
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        ylab="Fréquence", las=2, cex.names=0.7)

# FGI Simple
barplot(t_fgi_simple, main="FGI Simplifié", 
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        ylab="Fréquence", las=1)
text(x=barplot(t_fgi_simple, plot=FALSE), y=t_fgi_simple, 
     labels=t_fgi_simple, pos=3, cex=1.2, font=2)

# Mois
t_mois <- table(tab$mois, useNA="ifany")
barplot(t_mois, main="Distribution par Mois", 
        col=rainbow(12), ylab="Fréquence", las=2, cex.names=0.7)

par(mfrow=c(1,1))

# Pie charts
par(mfrow=c(2,2), mar=c(2,2,3,2))

pie(t_trend, main="Répartition Tendances", 
    col=c("#E74C3C", "#2ECC71"), 
    labels=paste(names(t_trend), "\n", round(prop.table(t_trend)*100,1), "%"),
    cex=1.1)

pie(t_vol, main="Répartition Volatilité", 
    col=c("#2ECC71", "#F39C12", "#E74C3C"), 
    labels=paste(names(t_vol), "\n", round(prop.table(t_vol)*100,1), "%"),
    cex=0.9)

pie(t_fgi_simple, main="Répartition Sentiment FGI", 
    col=c("#E74C3C", "#F39C12", "#2ECC71"), 
    labels=paste(names(t_fgi_simple), "\n", 
                 round(prop.table(t_fgi_simple)*100,1), "%"),
    cex=1.1)

pie(t_fgi_cat, main="Répartition Catégories FGI", 
    col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"), 
    labels=paste(gsub(" ", "\n", names(t_fgi_cat)), "\n", 
                 round(prop.table(t_fgi_cat)*100,1), "%"),
    cex=0.7)

par(mfrow=c(1,1))

# ============================================================================
# PAGE SECTION: SÉRIES TEMPORELLES
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "3. ÉVOLUTIONS TEMPORELLES", cex=3, font=2, col="#16A085")
text(0.5, 0.35, "Séries temporelles et tendances", cex=1.8, col="#34495E")

# ============================================================================
# 4) ÉVOLUTIONS TEMPORELLES
# ============================================================================

par(mfrow=c(3,1), mar=c(4,4,3,2))

# Prix BTC dans le temps
plot(tab$date, tab$close, type="l", 
     main="Évolution Prix BTC",
     xlab="Date", ylab="Close (USD)", col="#2980B9", lwd=1.5, las=1)
grid()

# Score FGI dans le temps
plot(tab$date, tab$fgi_score, type="l", 
     main="Évolution Score Fear & Greed",
     xlab="Date", ylab="Score FGI (0-100)", col="#9B59B6", lwd=1.5, las=1)
abline(h=c(25, 45, 55, 75), col="gray70", lty=2)
abline(h=50, col="black", lwd=2)
grid()

# Rendements dans le temps
plot(tab$date, tab$ret, type="l", 
     main="Évolution Rendements BTC",
     xlab="Date", ylab="Rendement", col="#E74C3C", lwd=1, las=1)
abline(h=0, col="black", lwd=2)
grid()

par(mfrow=c(1,1))

# Prix BTC avec FGI coloré par niveau
par(mfrow=c(1,1), mar=c(4,4,3,2))
plot(tab$date, tab$close, type="n",
     main="Prix BTC coloré selon Fear & Greed",
     xlab="Date", ylab="Prix BTC (USD)", las=1)

# Colorer selon FGI
colors_fgi <- ifelse(tab$fgi_score <= 25, "#E74C3C",
              ifelse(tab$fgi_score <= 45, "#E67E22",
              ifelse(tab$fgi_score <= 55, "#F39C12",
              ifelse(tab$fgi_score <= 75, "#2ECC71", "#27AE60"))))

points(tab$date, tab$close, col=colors_fgi, pch=20, cex=0.7)
lines(tab$date, tab$close, col="gray70", lwd=0.5)

legend("topleft", 
       legend=c("Extreme Fear", "Fear", "Neutral", "Greed", "Extreme Greed"),
       col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
       pch=20, pt.cex=1.5, bty="n", ncol=2)
grid()

# Score FGI avec moyennes mobiles
par(mfrow=c(1,1), mar=c(4,4,3,2))
plot(tab$date, tab$fgi_score, type="l", col="gray70", lwd=1,
     main="Score FGI avec Moyennes Mobiles",
     xlab="Date", ylab="Score FGI", las=1)
lines(tab$date, tab$fgi_ma7, col="#E74C3C", lwd=2)
lines(tab$date, tab$fgi_ma30, col="#2ECC71", lwd=2)
abline(h=50, col="black", lty=2)
legend("topleft", c("Score", "MA 7j", "MA 30j"),
       col=c("gray70", "#E74C3C", "#2ECC71"), 
       lwd=c(1,2,2), bty="n")
grid()

# ============================================================================
# PAGE SECTION: CORRÉLATIONS
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "4. ANALYSE DES CORRÉLATIONS", cex=3, font=2, col="#C0392B")
text(0.5, 0.35, "Relations entre variables", cex=1.8, col="#34495E")

# ============================================================================
# 5) ANALYSE BIVARIÉE - CORRÉLATIONS PRINCIPALES
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSE DES CORRÉLATIONS\n")
cat(strrep("=", 70), "\n\n", sep="")

# Nuages de points principaux
par(mfrow=c(2,3), mar=c(4,4,3,2))

# 1. FGI vs Prix BTC
plot(tab$fgi_score, tab$close,
     main="Prix BTC vs Fear & Greed",
     xlab="Score FGI", ylab="Prix BTC (USD)",
     pch=20, col=rgb(0.2,0.4,0.8,0.4), las=1)
abline(lm(close ~ fgi_score, data=tab), col="red", lwd=2)
cor_fgi_close <- cor(tab$fgi_score, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_fgi_close, 3)),
       bty="n", text.col="red", cex=1.3, font=2)
grid()

# 2. FGI vs Rendements BTC
plot(tab$fgi_score, tab$ret,
     main="Rendements BTC vs Fear & Greed",
     xlab="Score FGI", ylab="Rendement BTC",
     pch=20, col=rgb(0.8,0.2,0.2,0.4), las=1)
abline(h=0, col="gray50", lty=2)
abline(lm(ret ~ fgi_score, data=tab), col="darkred", lwd=2)
cor_fgi_ret <- cor(tab$fgi_score, tab$ret, use="complete.obs")
legend("topright", legend=paste("r =", round(cor_fgi_ret, 3)),
       bty="n", text.col="darkred", cex=1.3, font=2)
grid()

# 3. Variation FGI vs Rendements
plot(tab$fgi_change, tab$ret,
     main="Rendements vs Variation FGI",
     xlab="Variation FGI", ylab="Rendement BTC",
     pch=20, col=rgb(0.5,0,0.5,0.4), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ fgi_change, data=tab), col="purple", lwd=2)
cor_fgi_change_ret <- cor(tab$fgi_change, tab$ret, use="complete.obs")
legend("topright", legend=paste("r =", round(cor_fgi_change_ret, 3)),
       bty="n", text.col="purple", cex=1.3, font=2)
grid()

# 4. BTC vs ETH
plot(tab$close_eth, tab$close,
     main="BTC vs ETH (Prix)",
     xlab="Close ETH (USD)", ylab="Close BTC (USD)",
     pch=20, col=rgb(0.2,0.6,0.8,0.4), las=1)
abline(lm(close ~ close_eth, data=tab), col="blue", lwd=2)
cor_btc_eth <- cor(tab$close_eth, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_btc_eth, 3)),
       bty="n", text.col="blue", cex=1.3, font=2)
grid()

# 5. BTC vs SP500
plot(tab$close_sp500, tab$close,
     main="BTC vs SP500",
     xlab="Close SP500", ylab="Close BTC (USD)",
     pch=20, col=rgb(0.8,0.2,0.2,0.4), las=1)
abline(lm(close ~ close_sp500, data=tab), col="darkred", lwd=2)
cor_btc_sp500 <- cor(tab$close_sp500, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_btc_sp500, 3)),
       bty="n", text.col="darkred", cex=1.3, font=2)
grid()

# 6. BTC vs Gold
plot(tab$close_gold, tab$close,
     main="BTC vs Gold",
     xlab="Close Gold (USD)", ylab="Close BTC (USD)",
     pch=20, col=rgb(0.8,0.6,0,0.4), las=1)
abline(lm(close ~ close_gold, data=tab), col="darkgoldenrod", lwd=2)
cor_btc_gold <- cor(tab$close_gold, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_btc_gold, 3)),
       bty="n", text.col="darkgoldenrod", cex=1.3, font=2)
grid()

par(mfrow=c(1,1))

# Rendements croisés
par(mfrow=c(2,2), mar=c(4,4,3,2))

# BTC vs ETH rendements
plot(tab$ret_eth, tab$ret,
     main="Rendements: BTC vs ETH",
     xlab="Rendement ETH", ylab="Rendement BTC",
     pch=20, col=rgb(0.2,0.4,0.8,0.3), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ ret_eth, data=tab), col="blue", lwd=2)
cor_ret_eth <- cor(tab$ret_eth, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_eth, 3)),
       bty="n", text.col="blue", cex=1.2, font=2)
grid()

# BTC vs SP500 rendements
plot(tab$ret_sp500, tab$ret,
     main="Rendements: BTC vs SP500",
     xlab="Rendement SP500", ylab="Rendement BTC",
     pch=20, col=rgb(0.8,0.2,0.2,0.3), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ ret_sp500, data=tab), col="red", lwd=2)
cor_ret_sp500 <- cor(tab$ret_sp500, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_sp500, 3)),
       bty="n", text.col="red", cex=1.2, font=2)
grid()

# BTC vs Gold rendements
plot(tab$ret_gold, tab$ret,
     main="Rendements: BTC vs Gold",
     xlab="Rendement Gold", ylab="Rendement BTC",
     pch=20, col=rgb(0.8,0.6,0,0.3), las=1)
abline(h=0, v=0, col="gray50", lty=2)
abline(lm(ret ~ ret_gold, data=tab), col="darkgoldenrod", lwd=2)
cor_ret_gold <- cor(tab$ret_gold, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_ret_gold, 3)),
       bty="n", text.col="darkgoldenrod", cex=1.2, font=2)
grid()

# Volatilité vs |Rendement|
plot(tab$vol_30d, abs(tab$ret),
     main="Volatilité vs |Rendement|",
     xlab="Volatilité 30j", ylab="|Rendement BTC|",
     pch=20, col=rgb(0.5,0,0.5,0.3), las=1)
abline(lm(abs(ret) ~ vol_30d, data=tab), col="purple", lwd=2)
grid()

par(mfrow=c(1,1))

# ============================================================================
# MATRICES DE CORRÉLATION
# ============================================================================

# Charger corrplot
if (requireNamespace("corrplot", quietly=TRUE)) {
  library(corrplot)
  
  # Matrice 1: Variables principales
  cat("\n--- MATRICE DE CORRÉLATION PRINCIPALE ---\n")
  vars_cor1 <- c("close", "ret", "vol_30d", "vol_btc", 
                 "fgi_score", "fgi_change", "fgi_ma30",
                 "close_eth", "close_sp500", "close_gold")
  
  mat_cor1 <- cor(tab[, vars_cor1], use="complete.obs")
  print(round(mat_cor1, 3))
  
  # Visualisation 1
  par(mfrow=c(1,1), mar=c(2,2,3,2))
  corrplot(mat_cor1, method="color", type="full",
           tl.col="black", tl.srt=45, tl.cex=0.9,
           addCoef.col="black", number.cex=0.7,
           title="Matrice de Corrélation - Variables Principales",
           mar=c(0,0,2,0), col=colorRampPalette(c("#E74C3C","white","#2ECC71"))(100))
  
  # Matrice 2: Rendements
  cat("\n--- MATRICE DE CORRÉLATION RENDEMENTS ---\n")
  vars_cor2 <- c("ret", "ret_eth", "ret_sp500", "ret_gold", 
                 "fgi_change", "fgi_pct_change")
  
  mat_cor2 <- cor(tab[, vars_cor2], use="complete.obs")
  print(round(mat_cor2, 3))
  
  # Visualisation 2
  par(mfrow=c(1,1), mar=c(2,2,3,2))
  corrplot(mat_cor2, method="circle", type="full",
           tl.col="black", tl.srt=45, tl.cex=1,
           addCoef.col="black", number.cex=0.9,
           title="Matrice de Corrélation - Rendements & FGI",
           mar=c(0,0,2,0), col=colorRampPalette(c("#E74C3C","white","#2ECC71"))(100))
  
  # Matrice 3: Volatilités
  cat("\n--- MATRICE DE CORRÉLATION VOLATILITÉS ---\n")
  vars_cor3 <- c("vol_30d", "vol_btc", "vol_eth", "vol30_sp500", 
                 "vol30_gold", "vol30_eth", "fgi_volatility")
  
  mat_cor3 <- cor(tab[, vars_cor3], use="complete.obs")
  print(round(mat_cor3, 3))
  
  # Visualisation 3
  par(mfrow=c(1,1), mar=c(2,2,3,2))
  corrplot(mat_cor3, method="square", type="full",
           tl.col="black", tl.srt=45, tl.cex=0.9,
           addCoef.col="black", number.cex=0.8,
           title="Matrice de Corrélation - Volatilités",
           mar=c(0,0,2,0), col=colorRampPalette(c("#3498DB","white","#E74C3C"))(100))
  
  # Matrice 4: Corrélations roulantes
  cat("\n--- MATRICE CORRÉLATIONS ROULANTES 30j ---\n")
  vars_cor4 <- c("close", "ret", "fgi_score", 
                 "corr30_btc_gold", "corr30_btc_sp500", "corr30_btc_eth")
  
  mat_cor4 <- cor(tab[, vars_cor4], use="complete.obs")
  print(round(mat_cor4, 3))
  
  # Visualisation 4
  par(mfrow=c(1,1), mar=c(2,2,3,2))
  corrplot(mat_cor4, method="number", type="full",
           tl.col="black", tl.srt=45, tl.cex=1,
           number.cex=1,
           title="Corrélations Roulantes 30j BTC",
           mar=c(0,0,2,0), col=colorRampPalette(c("#9B59B6","white","#F39C12"))(100))
  
  # Heatmap complète
  par(mfrow=c(1,1), mar=c(5,5,3,2))
  vars_complete <- c("close", "ret", "vol_30d", "fgi_score", "fgi_change",
                     "close_eth", "close_sp500", "close_gold",
                     "ret_eth", "ret_sp500", "ret_gold",
                     "fgi_ma7", "fgi_ma30", "fgi_signal")
  
  mat_complete <- cor(tab[, vars_complete], use="complete.obs")
  
  # Heatmap manuelle avec valeurs
  image(1:ncol(mat_complete), 1:nrow(mat_complete), t(mat_complete),
        col=colorRampPalette(c("#E74C3C", "white", "#2ECC71"))(100),
        xlab="", ylab="", axes=FALSE, 
        main="Heatmap Complète - Toutes Corrélations")
  axis(1, at=1:ncol(mat_complete), labels=colnames(mat_complete), 
       las=2, cex.axis=0.7)
  axis(2, at=1:nrow(mat_complete), labels=rownames(mat_complete), 
       las=2, cex.axis=0.7)
  
  # Ajouter les valeurs
  for(i in 1:nrow(mat_complete)) {
    for(j in 1:ncol(mat_complete)) {
      text(j, i, round(mat_complete[i,j], 2), cex=0.6)
    }
  }
  
} else {
  cat("Package corrplot non installé. Installation recommandée.\n")
  
  # Matrice simple sans corrplot
  vars_corr <- c("close", "ret", "vol_30d", "fgi_score", "fgi_change",
                 "close_eth", "close_sp500", "close_gold")
  mat_cor <- cor(tab[, vars_corr], use="complete.obs")
  
  cat("\n--- MATRICE DE CORRÉLATION ---\n")
  print(round(mat_cor, 3))
}

# ============================================================================
# PAGE SECTION: ANALYSE QUALI x QUANTI
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "5. ANALYSE QUALI x QUANTI", cex=3, font=2, col="#D35400")
text(0.5, 0.35, "Comparaisons par groupes", cex=1.8, col="#34495E")

# ============================================================================
# 6) ANALYSE BIVARIÉE - QUALI x QUANTI
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSE QUALI x QUANTI\n")
cat(strrep("=", 70), "\n\n", sep="")

# Boxplots conditionnels - Prix
par(mfrow=c(2,3), mar=c(6,4,3,2))

boxplot(close ~ trend, data=tab,
        main="Prix BTC par Tendance",
        xlab="", ylab="Close (USD)",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ vol_level, data=tab,
        main="Prix BTC par Volatilité",
        xlab="", ylab="Close (USD)",
        col=c("#2ECC71", "#F39C12", "#E74C3C"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ fgi_simple, data=tab,
        main="Prix BTC par Sentiment FGI",
        xlab="", ylab="Close (USD)",
        col=c("#E74C3C", "#F39C12", "#2ECC71"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ fgi_category, data=tab,
        main="Prix BTC par Catégorie FGI",
        xlab="", ylab="Close (USD)",
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        las=2, cex.names=0.7, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ ret_bucket, data=tab,
        main="Prix BTC par Bucket Rendement",
        xlab="", ylab="Close (USD)",
        col=rainbow(length(unique(tab$ret_bucket))), 
        las=2, cex.names=0.6, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ mois, data=tab,
        main="Prix BTC par Mois",
        xlab="", ylab="Close (USD)",
        col=rainbow(12), las=2, cex.names=0.7, outcol="red")
grid(ny=NULL, nx=NA)

par(mfrow=c(1,1))

# Boxplots conditionnels - Rendements
par(mfrow=c(2,3), mar=c(6,4,3,2))

boxplot(ret ~ trend, data=tab,
        main="Rendements par Tendance",
        xlab="", ylab="Rendement",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(ret ~ vol_level, data=tab,
        main="Rendements par Volatilité",
        xlab="", ylab="Rendement",
        col=c("#2ECC71", "#F39C12", "#E74C3C"), las=1, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(ret ~ fgi_simple, data=tab,
        main="Rendements par Sentiment FGI",
        xlab="", ylab="Rendement",
        col=c("#E74C3C", "#F39C12", "#2ECC71"), las=1, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(ret ~ fgi_category, data=tab,
        main="Rendements par Catégorie FGI",
        xlab="", ylab="Rendement",
        col=c("#E74C3C", "#E67E22", "#F39C12", "#2ECC71", "#27AE60"),
        las=2, cex.names=0.7, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(vol_30d ~ trend, data=tab,
        main="Volatilité par Tendance",
        xlab="", ylab="Vol 30d",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(fgi_score ~ trend, data=tab,
        main="Score FGI par Tendance",
        xlab="", ylab="Score FGI",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
abline(h=50, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

par(mfrow=c(1,1))

# Moyennes par modalités
cat("\n--- MOYENNES PAR GROUPES ---\n\n")

cat("Prix BTC par Tendance:\n")
print(tapply(tab$close, tab$trend, mean, na.rm=TRUE))

cat("\nPrix BTC par Niveau de Volatilité:\n")
print(tapply(tab$close, tab$vol_level, mean, na.rm=TRUE))

cat("\nPrix BTC par Sentiment FGI:\n")
print(tapply(tab$close, tab$fgi_simple, mean, na.rm=TRUE))

cat("\nPrix BTC par Catégorie FGI:\n")
print(tapply(tab$close, tab$fgi_category, mean, na.rm=TRUE))

cat("\n\nRendements BTC par Tendance:\n")
print(tapply(tab$ret, tab$trend, mean, na.rm=TRUE))

cat("\nRendements BTC par Sentiment FGI:\n")
print(tapply(tab$ret, tab$fgi_simple, mean, na.rm=TRUE))

cat("\nRendements BTC par Catégorie FGI:\n")
print(tapply(tab$ret, tab$fgi_category, mean, na.rm=TRUE))

cat("\n\nScore FGI par Tendance:\n")
print(tapply(tab$fgi_score, tab$trend, mean, na.rm=TRUE))

# Graphiques des moyennes
par(mfrow=c(2,2), mar=c(8,4,3,2))

moy_trend <- tapply(tab$close, tab$trend, mean, na.rm=TRUE)
barplot(moy_trend, main="Prix Moyen par Tendance",
        ylab="Close Moyen (USD)", col=c("#E74C3C", "#2ECC71"), las=1)
text(x=barplot(moy_trend, plot=FALSE), y=moy_trend,
     labels=format(round(moy_trend, 0), big.mark=" "), pos=3, cex=1.2, font=2)

moy_fgi_simple <- tapply(tab$close, tab$fgi_simple, mean, na.rm=TRUE)
barplot(moy_fgi_simple, main="Prix Moyen par Sentiment FGI",
        ylab="Close Moyen (USD)", col=c("#E74C3C", "#F39C12", "#2ECC71"), las=1)
text(x=barplot(moy_fgi_simple, plot=FALSE), y=moy_fgi_simple,
     labels=format(round(moy_fgi_simple, 0), big.mark=" "), pos=3, cex=1.2, font=2)

moy_ret_trend <- tapply(tab$ret, tab$trend, mean, na.rm=TRUE)
barplot(moy_ret_trend, main="Rendement Moyen par Tendance",
        ylab="Rendement Moyen", col=c("#E74C3C", "#2ECC71"), las=1)
abline(h=0, col="blue", lwd=2, lty=2)
text(x=barplot(moy_ret_trend, plot=FALSE), y=moy_ret_trend,
     labels=paste(round(moy_ret_trend*100, 2), "%"), pos=3, cex=1.2, font=2)

moy_ret_fgi <- tapply(tab$ret, tab$fgi_simple, mean, na.rm=TRUE)
barplot(moy_ret_fgi, main="Rendement Moyen par Sentiment FGI",
        ylab="Rendement Moyen", col=c("#E74C3C", "#F39C12", "#2ECC71"), las=1)
abline(h=0, col="blue", lwd=2, lty=2)
text(x=barplot(moy_ret_fgi, plot=FALSE), y=moy_ret_fgi,
     labels=paste(round(moy_ret_fgi*100, 2), "%"), pos=3, cex=1.2, font=2)

par(mfrow=c(1,1))

# ANOVA
cat("\n\n--- TESTS ANOVA ---\n\n")

cat("ANOVA: Prix ~ Tendance\n")
anova_close_trend <- aov(close ~ trend, data=tab)
print(summary(anova_close_trend))

cat("\nANOVA: Prix ~ Sentiment FGI\n")
anova_close_fgi <- aov(close ~ fgi_simple, data=tab)
print(summary(anova_close_fgi))

cat("\nANOVA: Prix ~ Catégorie FGI\n")
anova_close_fgi_cat <- aov(close ~ fgi_category, data=tab)
print(summary(anova_close_fgi_cat))

cat("\nANOVA: Rendement ~ Tendance\n")
anova_ret_trend <- aov(ret ~ trend, data=tab)
print(summary(anova_ret_trend))

cat("\nANOVA: Rendement ~ Sentiment FGI\n")
anova_ret_fgi <- aov(ret ~ fgi_simple, data=tab)
print(summary(anova_ret_fgi))

cat("\nANOVA: Rendement ~ Catégorie FGI\n")
anova_ret_fgi_cat <- aov(ret ~ fgi_category, data=tab)
print(summary(anova_ret_fgi_cat))

# ============================================================================
# PAGE SECTION: ANALYSE QUALI x QUALI
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "6. ANALYSE QUALI x QUALI", cex=3, font=2, col="#8E44AD")
text(0.5, 0.35, "Tableaux de contingence", cex=1.8, col="#34495E")

# ============================================================================
# 7) ANALYSE BIVARIÉE - QUALI x QUALI
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSE QUALI x QUALI\n")
cat(strrep("=", 70), "\n\n", sep="")

# Tableau 1: Trend x Vol_level
T1 <- table(tab$trend, tab$vol_level)
cat("Tableau de contingence: Trend x Volatilité\n")
print(T1)

prof_lignes1 <- prop.table(T1, margin=1)
prof_colonnes1 <- prop.table(T1, margin=2)

cat("\nProfils lignes (%):\n")
print(round(prof_lignes1 * 100, 2))

cat("\nProfils colonnes (%):\n")
print(round(prof_colonnes1 * 100, 2))

# Tableau 2: Trend x FGI Simple
T2 <- table(tab$trend, tab$fgi_simple)
cat("\n\nTableau de contingence: Trend x Sentiment FGI\n")
print(T2)

prof_lignes2 <- prop.table(T2, margin=1)
prof_colonnes2 <- prop.table(T2, margin=2)

cat("\nProfils lignes (%):\n")
print(round(prof_lignes2 * 100, 2))

cat("\nProfils colonnes (%):\n")
print(round(prof_colonnes2 * 100, 2))

# Tableau 3: Vol_level x FGI Simple
T3 <- table(tab$vol_level, tab$fgi_simple)
cat("\n\nTableau de contingence: Volatilité x Sentiment FGI\n")
print(T3)

prof_lignes3 <- prop.table(T3, margin=1)
cat("\nProfils lignes (%):\n")
print(round(prof_lignes3 * 100, 2))

# Visualisations tableaux croisés
par(mfrow=c(2,2), mar=c(6,5,3,2))

# Graph 1
barplot(T1, beside=TRUE, legend=TRUE,
        main="Trend x Volatilité",
        col=c("#E74C3C", "#2ECC71"),
        ylab="Fréquence", las=1,
        args.legend=list(x="topleft", title="Trend", cex=0.8))

# Graph 2
barplot(t(prof_lignes1), beside=TRUE, legend=TRUE,
        main="Profils Lignes Trend x Vol",
        col=c("#2ECC71", "#F39C12", "#E74C3C"),
        ylab="Proportion", las=1,
        args.legend=list(x="topleft", title="Volatilité", cex=0.8))

# Graph 3
barplot(T2, beside=TRUE, legend=TRUE,
        main="Trend x Sentiment FGI",
        col=c("#E74C3C", "#2ECC71"),
        ylab="Fréquence", las=1,
        args.legend=list(x="topleft", title="Trend", cex=0.8))

# Graph 4
barplot(t(prof_lignes2), beside=TRUE, legend=TRUE,
        main="Profils Lignes Trend x FGI",
        col=c("#E74C3C", "#F39C12", "#2ECC71"),
        ylab="Proportion", las=1,
        args.legend=list(x="topleft", title="FGI", cex=0.8))

par(mfrow=c(1,1))

# Mosaicplots
par(mfrow=c(1,2), mar=c(5,5,3,2))

mosaicplot(T1, main="Mosaïque: Trend x Volatilité",
           color=c("#2ECC71", "#F39C12", "#E74C3C"),
           las=1, cex.axis=0.9)

mosaicplot(T2, main="Mosaïque: Trend x Sentiment FGI",
           color=c("#E74C3C", "#F39C12", "#2ECC71"),
           las=1, cex.axis=0.9)

par(mfrow=c(1,1))

# Tests Chi-2
cat("\n\n--- TESTS CHI-2 D'INDÉPENDANCE ---\n\n")

cat("Chi-2: Trend x Volatilité\n")
chi2_1 <- chisq.test(T1)
print(chi2_1)

cat("\n\nChi-2: Trend x Sentiment FGI\n")
chi2_2 <- chisq.test(T2)
print(chi2_2)

cat("\n\nChi-2: Volatilité x Sentiment FGI\n")
chi2_3 <- chisq.test(T3)
print(chi2_3)

# ============================================================================
# PAGE SECTION: ACP
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "7. ANALYSE EN COMPOSANTES PRINCIPALES", 
     cex=3, font=2, col="#16A085")
text(0.5, 0.35, "Réduction dimensionnelle avec FGI", cex=1.8, col="#34495E")

# ============================================================================
# 8) ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSE EN COMPOSANTES PRINCIPALES (ACP)\n")
cat(strrep("=", 70), "\n\n", sep="")

# Charger les packages
if (requireNamespace("FactoMineR", quietly=TRUE) &&
    requireNamespace("factoextra", quietly=TRUE)) {
  
  library(FactoMineR)
  library(factoextra)
  
  # Variables pour l'ACP
  vars_acp <- c("close", "ret", "vol_30d", "vol_btc",
                "fgi_score", "fgi_ma30", "fgi_volatility", "fgi_signal",
                "close_eth", "close_sp500", "close_gold",
                "ret_eth", "ret_sp500", "ret_gold")
  
  # Données complètes
  num <- tab[, vars_acp]
  num <- num[complete.cases(num), ]
  
  cat("Variables utilisées:\n")
  print(vars_acp)
  cat("\nNombre d'observations complètes:", nrow(num), "\n\n")
  
  # ACP
  res_pca <- PCA(num, scale.unit=TRUE, graph=FALSE)
  
  # Résumé
  cat("Résumé de l'ACP:\n")
  print(summary(res_pca))
  
  cat("\n\nValeurs propres et variance expliquée:\n")
  print(res_pca$eig)
  
  cat("\n\nContributions des variables à PC1:\n")
  print(round(sort(res_pca$var$contrib[,1], decreasing=TRUE), 2))
  
  cat("\n\nContributions des variables à PC2:\n")
  print(round(sort(res_pca$var$contrib[,2], decreasing=TRUE), 2))
  
  cat("\n\nContributions des variables à PC3:\n")
  print(round(sort(res_pca$var$contrib[,3], decreasing=TRUE), 2))
  
  # Scree plot
  print(fviz_eig(res_pca, addlabels=TRUE,
                 main="Scree Plot - Variance Expliquée (avec FGI)",
                 barfill="#3498DB", barcolor="#2980B9", linecolor="#E74C3C"))
  
  # Cercle des corrélations PC1-PC2
  print(fviz_pca_var(res_pca, axes=c(1,2), col.var="contrib",
                     gradient.cols=c("#2ECC71", "#F39C12", "#E74C3C"),
                     repel=TRUE,
                     title="Cercle des Corrélations - PC1 vs PC2"))
  
  # Cercle des corrélations PC1-PC3
  print(fviz_pca_var(res_pca, axes=c(1,3), col.var="contrib",
                     gradient.cols=c("#2ECC71", "#F39C12", "#E74C3C"),
                     repel=TRUE,
                     title="Cercle des Corrélations - PC1 vs PC3"))
  
  # Cercle des corrélations PC2-PC3
  print(fviz_pca_var(res_pca, axes=c(2,3), col.var="contrib",
                     gradient.cols=c("#2ECC71", "#F39C12", "#E74C3C"),
                     repel=TRUE,
                     title="Cercle des Corrélations - PC2 vs PC3"))
  
  # Projection des individus
  print(fviz_pca_ind(res_pca,
                     col.ind="cos2",
                     gradient.cols=c("#3498DB", "#9B59B6", "#E74C3C"),
                     repel=FALSE,
                     title="Projection des Individus - PC1 vs PC2",
                     alpha.ind=0.3))
  
  # Biplot
  print(fviz_pca_biplot(res_pca,
                        col.var="red",
                        col.ind=rgb(0.2,0.4,0.8,0.2),
                        repel=TRUE,
                        title="Biplot - Variables et Individus"))
  
  # Contributions détaillées
  par(mfrow=c(1,2), mar=c(8,4,3,2))
  
  barplot(sort(res_pca$var$contrib[,1], decreasing=TRUE),
          main="Contributions à PC1",
          ylab="Contribution (%)", las=2, cex.names=0.7,
          col=rainbow(length(vars_acp)))
  
  barplot(sort(res_pca$var$contrib[,2], decreasing=TRUE),
          main="Contributions à PC2",
          ylab="Contribution (%)", las=2, cex.names=0.7,
          col=rainbow(length(vars_acp)))
  
  par(mfrow=c(1,1))
  
} else {
  cat("Packages FactoMineR et/ou factoextra non installés.\n")
  cat("Installez-les avec: install.packages(c('FactoMineR', 'factoextra'))\n")
}

# ============================================================================
# PAGE SECTION: CHANGEMENTS DE RÉGIME FGI
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "8. CHANGEMENTS DE RÉGIME", cex=3, font=2, col="#C0392B")
text(0.5, 0.35, "Analyse des transitions Fear ↔ Greed", cex=1.8, col="#34495E")

# ============================================================================
# 9) ANALYSE DES CHANGEMENTS DE RÉGIME FGI
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep="")
cat("ANALYSE DES CHANGEMENTS DE RÉGIME FGI\n")
cat(strrep("=", 70), "\n\n", sep="")

# Statistiques sur les changements
n_to_greed <- sum(tab$fgi_regime_change == 1, na.rm=TRUE)
n_to_fear <- sum(tab$fgi_regime_change == -1, na.rm=TRUE)
n_stable <- sum(tab$fgi_regime_change == 0, na.rm=TRUE)

cat("Nombre de passages vers Greed:", n_to_greed, "\n")
cat("Nombre de passages vers Fear:", n_to_fear, "\n")
cat("Nombre de jours stables:", n_stable, "\n\n")

# Rendement moyen lors des changements
cat("Rendement moyen lors des passages:\n")
ret_to_greed <- mean(tab$ret[tab$fgi_regime_change == 1], na.rm=TRUE)
ret_to_fear <- mean(tab$ret[tab$fgi_regime_change == -1], na.rm=TRUE)
ret_stable <- mean(tab$ret[tab$fgi_regime_change == 0], na.rm=TRUE)

cat("  Vers Greed:", round(ret_to_greed*100, 3), "%\n")
cat("  Vers Fear:", round(ret_to_fear*100, 3), "%\n")
cat("  Stable:", round(ret_stable*100, 3), "%\n\n")

# Prix moyen lors des changements
cat("Prix moyen lors des passages:\n")
cat("  Vers Greed:", round(mean(tab$close[tab$fgi_regime_change == 1], na.rm=TRUE), 0), "USD\n")
cat("  Vers Fear:", round(mean(tab$close[tab$fgi_regime_change == -1], na.rm=TRUE), 0), "USD\n")
cat("  Stable:", round(mean(tab$close[tab$fgi_regime_change == 0], na.rm=TRUE), 0), "USD\n\n")

# Visualisations
par(mfrow=c(2,1), mar=c(4,4,3,2))

# Score FGI avec changements marqués
plot(tab$date, tab$fgi_score, type="l",
     main="Changements de Régime FGI",
     xlab="Date", ylab="Score FGI (0-100)", col="gray70", lwd=1, las=1)