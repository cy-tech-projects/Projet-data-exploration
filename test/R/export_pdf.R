# ============================================================================
# EXPORT PDF PROFESSIONNEL - ANALYSE BITCOIN
# ============================================================================
# Ce script génère un PDF avec mise en page professionnelle
# ============================================================================

# Charger les données (assumer que tab existe déjà depuis l'analyse principale)
# Sinon, décommenter et exécuter d'abord le script d'analyse complet

# ============================================================================
# CONFIGURATION
# ============================================================================

# Créer le dossier de sortie
if (!dir.exists("./resultats")) {
  dir.create("./resultats")
}

# Ouvrir le PDF avec format A4 landscape
pdf("./resultats/analyse_bitcoin_rapport_complet.pdf", 
    width=11.69, height=8.27, # Format A4 paysage
    paper="a4r")

# ============================================================================
# PAGE DE TITRE
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.7, "ANALYSE STATISTIQUE", cex=3, font=2, col="#2C3E50")
text(0.5, 0.6, "DU BITCOIN", cex=3, font=2, col="#2C3E50")
text(0.5, 0.45, "Analyse Multi-Actifs & Corrélations", cex=1.8, col="#34495E")
text(0.5, 0.35, paste("Période:", min(tab$date, na.rm=TRUE), "-", 
                      max(tab$date, na.rm=TRUE)), cex=1.3, col="#7F8C8D")
text(0.5, 0.25, paste(nrow(tab), "observations journalières"), cex=1.3, col="#7F8C8D")
text(0.5, 0.1, format(Sys.time(), "%d %B %Y"), cex=1.2, col="#95A5A6")

# ============================================================================
# SECTION 1: STATISTIQUES DESCRIPTIVES
# ============================================================================

# Page titre de section
plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "1. STATISTIQUES DESCRIPTIVES", cex=2.5, font=2, col="#2980B9")
text(0.5, 0.35, "Analyse univariée des variables quantitatives", cex=1.5, col="#34495E")

# Histogrammes
par(mfrow=c(2,2), mar=c(4,4,3,2))

hist(tab$close, main="Distribution du Prix BTC", 
     xlab="Close (USD)", ylab="Fréquence",
     col="#3498DB", border="white", breaks=50, las=1)
abline(v=median(tab$close, na.rm=TRUE), col="red", lwd=2, lty=2)
legend("topright", "Médiane", col="red", lty=2, lwd=2, bty="n")

hist(tab$vol_btc, main="Distribution du Volume BTC", 
     xlab="Volume", ylab="Fréquence",
     col="#2ECC71", border="white", breaks=50, las=1)
abline(v=median(tab$vol_btc, na.rm=TRUE), col="red", lwd=2, lty=2)

hist(tab$ret, main="Distribution des Rendements BTC", 
     xlab="Rendement", ylab="Fréquence",
     col="#F39C12", border="white", breaks=50, las=1)
abline(v=0, col="red", lwd=2, lty=2)
abline(v=median(tab$ret, na.rm=TRUE), col="blue", lwd=2, lty=2)
legend("topright", c("Zéro", "Médiane"), col=c("red","blue"), 
       lty=2, lwd=2, bty="n", cex=0.8)

hist(tab$vol_30d, main="Distribution Volatilité 30j", 
     xlab="Volatilité", ylab="Fréquence",
     col="#E74C3C", border="white", breaks=50, las=1)
abline(v=median(tab$vol_30d, na.rm=TRUE), col="red", lwd=2, lty=2)

# Boxplots
par(mfrow=c(2,2), mar=c(4,4,3,2))

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

boxplot(tab$vol_30d, main="Boxplot Volatilité", ylab="Volatilité 30j",
        col="#E74C3C", border="#C0392B", las=1, outcol="red", outpch=19)
grid(ny=NULL, nx=NA)

# ============================================================================
# SECTION 2: VARIABLES QUALITATIVES
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "2. VARIABLES QUALITATIVES", cex=2.5, font=2, col="#8E44AD")
text(0.5, 0.35, "Distributions et fréquences", cex=1.5, col="#34495E")

# Graphiques des variables qualitatives
par(mfrow=c(2,2), mar=c(5,4,3,2))

# Trend
t_trend <- table(tab$trend)
barplot(t_trend, main="Distribution Tendance", 
        col=c("#E74C3C", "#2ECC71"), ylab="Fréquence", las=1,
        names.arg=names(t_trend))
text(x=barplot(t_trend, plot=FALSE), y=t_trend, labels=t_trend, pos=3, cex=1.2)

# Volatilité
t_vol <- table(tab$vol_level)
barplot(t_vol, main="Distribution Niveau de Volatilité", 
        col=c("#2ECC71", "#F39C12", "#E74C3C"), ylab="Fréquence", las=1)
text(x=barplot(t_vol, plot=FALSE), y=t_vol, labels=t_vol, pos=3, cex=1.2)

# Ret bucket
t_ret <- table(tab$ret_bucket)
barplot(t_ret, main="Distribution Buckets de Rendements", 
        col=rainbow(length(t_ret)), ylab="Fréquence", las=2, cex.names=0.7)

# Mois
t_mois <- table(tab$mois)
barplot(t_mois, main="Distribution par Mois", 
        col=rainbow(12), ylab="Fréquence", las=2, cex.names=0.8)

# Pie chart pour Trend
par(mfrow=c(1,2), mar=c(2,2,3,2))
pie(t_trend, main="Répartition Tendances", 
    col=c("#E74C3C", "#2ECC71"), labels=paste(names(t_trend), "\n", 
                                              round(prop.table(t_trend)*100,1), "%"))

pie(t_vol, main="Répartition Niveaux de Volatilité", 
    col=c("#2ECC71", "#F39C12", "#E74C3C"), 
    labels=paste(names(t_vol), "\n", round(prop.table(t_vol)*100,1), "%"))

# ============================================================================
# SECTION 3: CORRÉLATIONS BTC vs AUTRES ACTIFS
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "3. CORRÉLATIONS MULTI-ACTIFS", cex=2.5, font=2, col="#16A085")
text(0.5, 0.35, "Analyse bivariée BTC vs ETH, SP500, Gold", cex=1.5, col="#34495E")

# Prix BTC vs autres actifs
par(mfrow=c(2,2), mar=c(4,4,3,2))

# BTC vs ETH
plot(tab$close_eth, tab$close, main="BTC vs ETH (Prix)",
     xlab="Close ETH (USD)", ylab="Close BTC (USD)", 
     pch=20, col=rgb(0.2,0.4,0.8,0.4), las=1)
abline(lm(close ~ close_eth, data=tab), col="red", lwd=2)
cor_val <- cor(tab$close_eth, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="red")
grid()

# BTC vs SP500
plot(tab$close_sp500, tab$close, main="BTC vs SP500",
     xlab="Close SP500", ylab="Close BTC (USD)", 
     pch=20, col=rgb(0.8,0.2,0.2,0.4), las=1)
abline(lm(close ~ close_sp500, data=tab), col="darkred", lwd=2)
cor_val <- cor(tab$close_sp500, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="darkred")
grid()

# BTC vs Gold
plot(tab$close_gold, tab$close, main="BTC vs Gold",
     xlab="Close Gold (USD)", ylab="Close BTC (USD)", 
     pch=20, col=rgb(0.8,0.6,0,0.4), las=1)
abline(lm(close ~ close_gold, data=tab), col="darkgoldenrod", lwd=2)
cor_val <- cor(tab$close_gold, tab$close, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="darkgoldenrod")
grid()

# Évolution temporelle du prix BTC
plot(tab$date, tab$close, type="l", main="Évolution Prix BTC dans le temps",
     xlab="Date", ylab="Close BTC (USD)", col="#2980B9", lwd=1.5, las=1)
grid()

# Rendements BTC vs autres actifs
par(mfrow=c(2,2), mar=c(4,4,3,2))

# BTC vs ETH rendements
plot(tab$ret_eth, tab$ret, main="Rendements: BTC vs ETH",
     xlab="Rendement ETH", ylab="Rendement BTC", 
     pch=20, col=rgb(0.2,0.4,0.8,0.3), las=1)
abline(lm(ret ~ ret_eth, data=tab), col="blue", lwd=2)
abline(h=0, v=0, col="gray50", lty=2)
cor_val <- cor(tab$ret_eth, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="blue")
grid()

# BTC vs SP500 rendements
plot(tab$ret_sp500, tab$ret, main="Rendements: BTC vs SP500",
     xlab="Rendement SP500", ylab="Rendement BTC", 
     pch=20, col=rgb(0.8,0.2,0.2,0.3), las=1)
abline(lm(ret ~ ret_sp500, data=tab), col="red", lwd=2)
abline(h=0, v=0, col="gray50", lty=2)
cor_val <- cor(tab$ret_sp500, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="red")
grid()

# BTC vs Gold rendements
plot(tab$ret_gold, tab$ret, main="Rendements: BTC vs Gold",
     xlab="Rendement Gold", ylab="Rendement BTC", 
     pch=20, col=rgb(0.8,0.6,0,0.3), las=1)
abline(lm(ret ~ ret_gold, data=tab), col="darkgoldenrod", lwd=2)
abline(h=0, v=0, col="gray50", lty=2)
cor_val <- cor(tab$ret_gold, tab$ret, use="complete.obs")
legend("topleft", legend=paste("r =", round(cor_val, 3)), 
       bty="n", cex=1.2, text.col="darkgoldenrod")
grid()

# Volatilité vs Rendement absolu
plot(tab$vol_30d, abs(tab$ret), main="Volatilité vs |Rendement|",
     xlab="Volatilité 30j", ylab="|Rendement BTC|", 
     pch=20, col=rgb(0.5,0,0.5,0.3), las=1)
abline(lm(abs(ret) ~ vol_30d, data=tab), col="purple", lwd=2)
grid()

# Matrice de corrélation visuelle
par(mfrow=c(1,1), mar=c(5,5,3,2))
vars_corr <- c("close", "ret", "vol_30d", "close_eth", "close_sp500", 
               "close_gold", "ret_eth", "ret_sp500", "ret_gold")
mat_cor <- cor(tab[, vars_corr], use="complete.obs")

# Heatmap manuelle
image(1:ncol(mat_cor), 1:nrow(mat_cor), t(mat_cor), 
      col=colorRampPalette(c("red", "white", "blue"))(100),
      xlab="", ylab="", axes=FALSE, main="Matrice de Corrélation")
axis(1, at=1:ncol(mat_cor), labels=colnames(mat_cor), las=2, cex.axis=0.8)
axis(2, at=1:nrow(mat_cor), labels=rownames(mat_cor), las=2, cex.axis=0.8)

# Ajouter les valeurs
for(i in 1:nrow(mat_cor)) {
  for(j in 1:ncol(mat_cor)) {
    text(j, i, round(mat_cor[i,j], 2), cex=0.8)
  }
}

# ============================================================================
# SECTION 4: ANALYSE QUALITATIVE x QUANTITATIVE
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "4. ANALYSE QUALI x QUANTI", cex=2.5, font=2, col="#D35400")
text(0.5, 0.35, "Comparaisons par groupes", cex=1.5, col="#34495E")

# Boxplots conditionnels
par(mfrow=c(2,2), mar=c(5,4,3,2))

boxplot(close ~ trend, data=tab, 
        main="Prix BTC par Tendance", 
        ylab="Close (USD)", xlab="Tendance",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(close ~ vol_level, data=tab, 
        main="Prix BTC par Niveau de Volatilité", 
        ylab="Close (USD)", xlab="Niveau Volatilité",
        col=c("#2ECC71", "#F39C12", "#E74C3C"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

boxplot(ret ~ trend, data=tab, 
        main="Rendements par Tendance", 
        ylab="Rendement", xlab="Tendance",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
abline(h=0, col="blue", lwd=2, lty=2)
grid(ny=NULL, nx=NA)

boxplot(vol_30d ~ trend, data=tab, 
        main="Volatilité par Tendance", 
        ylab="Volatilité 30j", xlab="Tendance",
        col=c("#E74C3C", "#2ECC71"), las=1, outcol="red")
grid(ny=NULL, nx=NA)

# Moyennes par groupes
par(mfrow=c(1,2), mar=c(8,4,3,2))

moy_trend <- tapply(tab$close, tab$trend, mean, na.rm=TRUE)
barplot(moy_trend, main="Prix Moyen par Tendance",
        ylab="Close Moyen (USD)", col=c("#E74C3C", "#2ECC71"), las=1)
text(x=barplot(moy_trend, plot=FALSE), y=moy_trend, 
     labels=round(moy_trend, 0), pos=3, cex=1.2)

moy_vol <- tapply(tab$close, tab$vol_level, mean, na.rm=TRUE)
barplot(moy_vol, main="Prix Moyen par Niveau Volatilité",
        ylab="Close Moyen (USD)", 
        col=c("#2ECC71", "#F39C12", "#E74C3C"), las=1)
text(x=barplot(moy_vol, plot=FALSE), y=moy_vol, 
     labels=round(moy_vol, 0), pos=3, cex=1.2)

# ============================================================================
# SECTION 5: ANALYSE QUALI x QUALI
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.5, "5. ANALYSE QUALI x QUALI", cex=2.5, font=2, col="#C0392B")
text(0.5, 0.35, "Tableaux de contingence et dépendances", cex=1.5, col="#34495E")

# Tableau de contingence
par(mfrow=c(1,2), mar=c(5,5,3,2))

T <- table(tab$trend, tab$vol_level)
barplot(T, beside=TRUE, legend=TRUE, 
        main="Trend x Vol_level (Effectifs)",
        col=c("#E74C3C", "#2ECC71"),
        ylab="Fréquence", las=1,
        args.legend=list(x="topleft", title="Trend"))

prof_lignes <- prop.table(T, margin=1)
barplot(t(prof_lignes), beside=TRUE, legend=TRUE,
        main="Profils Lignes (Proportions)",
        col=c("#2ECC71", "#F39C12", "#E74C3C"),
        ylab="Proportion", las=1,
        args.legend=list(x="topleft", title="Vol Level"))

# Mosaicplot
par(mfrow=c(1,1), mar=c(5,5,3,2))
mosaicplot(T, main="Mosaïque: Trend x Niveau Volatilité",
           color=c("#2ECC71", "#F39C12", "#E74C3C"),
           las=1)

# ============================================================================
# SECTION 6: ACP (si packages disponibles)
# ============================================================================

if (requireNamespace("FactoMineR", quietly=TRUE) && 
    requireNamespace("factoextra", quietly=TRUE)) {
  
  library(FactoMineR)
  library(factoextra)
  
  plot.new()
  par(mar=c(0,0,0,0))
  text(0.5, 0.5, "6. ANALYSE EN COMPOSANTES PRINCIPALES", 
       cex=2.5, font=2, col="#2C3E50")
  text(0.5, 0.35, "Réduction dimensionnelle et structure des données", 
       cex=1.5, col="#34495E")
  
  vars_acp <- c("close", "ret", "vol_30d", "vol_btc", "close_eth", 
                "close_sp500", "close_gold", "ret_eth", "ret_sp500", "ret_gold")
  
  num <- tab[, vars_acp]
  num <- num[complete.cases(num), ]
  
  res_pca <- PCA(num, scale.unit=TRUE, graph=FALSE)
  
  print(fviz_eig(res_pca, addlabels=TRUE, 
                 main="Scree Plot - Variance Expliquée",
                 barfill="#3498DB", barcolor="#2980B9"))
  
  print(fviz_pca_var(res_pca, col.var="contrib", 
                     gradient.cols=c("#2ECC71", "#F39C12", "#E74C3C"),
                     repel=TRUE,
                     title="Cercle des Corrélations"))
  
  print(fviz_pca_ind(res_pca, 
                     col.ind="cos2",
                     gradient.cols=c("#3498DB", "#9B59B6", "#E74C3C"),
                     repel=FALSE,
                     title="Projection des Individus",
                     alpha.ind=0.3))
  
  print(fviz_pca_biplot(res_pca, 
                        col.var="red", 
                        col.ind=rgb(0.2,0.4,0.8,0.2),
                        repel=TRUE,
                        title="Biplot - Variables et Individus"))
}

# ============================================================================
# PAGE DE FIN
# ============================================================================

plot.new()
par(mar=c(0,0,0,0))
text(0.5, 0.6, "FIN DU RAPPORT", cex=3, font=2, col="#2C3E50")
text(0.5, 0.45, "Analyse Statistique Bitcoin", cex=2, col="#34495E")
text(0.5, 0.3, paste("Généré le", format(Sys.time(), "%d %B %Y à %H:%M")), 
     cex=1.3, col="#7F8C8D")
text(0.5, 0.15, paste(nrow(tab), "observations analysées"), 
     cex=1.2, col="#95A5A6")

# ============================================================================
# FERMETURE ET SAUVEGARDE
# ============================================================================

dev.off()

cat("\n========================================\n")
cat("✓ PDF PROFESSIONNEL CRÉÉ AVEC SUCCÈS !\n")
cat("========================================\n")
cat("Fichier: ./resultats/analyse_bitcoin_rapport_complet.pdf\n")
cat("Nombre de pages:", "~20-25 pages avec toutes les analyses\n")
cat("Format: A4 paysage (11.69 x 8.27 pouces)\n")
cat("========================================\n\n")

# Créer également un résumé textuel
sink("./resultats/resume_analyses.txt")
cat("RÉSUMÉ DES ANALYSES - BITCOIN\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")
cat("Date de génération:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Période analysée:", min(tab$date, na.rm=TRUE), "à", max(tab$date, na.rm=TRUE), "\n")
cat("Nombre d'observations:", nrow(tab), "\n\n")

cat("STATISTIQUES CLÉS\n")
cat(strrep("-", 60), "\n")
cat("Prix BTC moyen:", round(mean(tab$close, na.rm=TRUE), 2), "USD\n")
cat("Prix BTC médian:", round(median(tab$close, na.rm=TRUE), 2), "USD\n")
cat("Prix BTC min:", round(min(tab$close, na.rm=TRUE), 2), "USD\n")
cat("Prix BTC max:", round(max(tab$close, na.rm=TRUE), 2), "USD\n")
cat("Rendement moyen:", round(mean(tab$ret, na.rm=TRUE)*100, 3), "%\n")
cat("Volatilité moyenne (30j):", round(mean(tab$vol_30d, na.rm=TRUE), 4), "\n\n")

cat("CORRÉLATIONS PRINCIPALES\n")
cat(strrep("-", 60), "\n")
cat("BTC-ETH (prix):", round(cor(tab$close_eth, tab$close, use="complete.obs"), 4), "\n")
cat("BTC-SP500 (prix):", round(cor(tab$close_sp500, tab$close, use="complete.obs"), 4), "\n")
cat("BTC-Gold (prix):", round(cor(tab$close_gold, tab$close, use="complete.obs"), 4), "\n")
cat("BTC-ETH (rendements):", round(cor(tab$ret_eth, tab$ret, use="complete.obs"), 4), "\n")
cat("BTC-SP500 (rendements):", round(cor(tab$ret_sp500, tab$ret, use="complete.obs"), 4), "\n")
cat("BTC-Gold (rendements):", round(cor(tab$ret_gold, tab$ret, use="complete.obs"), 4), "\n\n")

cat("DISTRIBUTION PAR TENDANCE\n")
cat(strrep("-", 60), "\n")
print(table(tab$trend))
cat("\n")

cat("DISTRIBUTION PAR NIVEAU DE VOLATILITÉ\n")
cat(strrep("-", 60), "\n")
print(table(tab$vol_level))

sink()

cat("✓ Résumé textuel créé: ./resultats/resume_analyses.txt\n")
cat("\nTous les fichiers sont dans le dossier: ./resultats/\n")