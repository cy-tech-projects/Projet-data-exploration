# Packages de base; pour des jolis graphes, on peut ajouter factoextra/FactoMineR si souhaité.
# install.packages("FactoMineR"); install.packages("factoextra")

tab <- read.csv("./data/btc_spot_1d.csv")

# 1) Sous-ensemble numérique + nettoyage
num <- subset(tab, select=c(open, high, low, close, volume, range, ret, vol_10d))
num <- num[complete.cases(num), ]

# 2) Centrage-réduction (cf. ACP normée)
num_sc <- scale(num)

# 3) ACP
pca <- prcomp(num_sc, center=FALSE, scale.=FALSE)
summary(pca)               # variances expliquées (inertie)
pca$rotation               # "charges" (corrélations ~ interprétation axes)
pca$x[1:5,]                # coordonnées des individus

# 4) Scree plot (valeurs propres)
plot(pca, type="l", main="Scree plot (prcomp)")

# 5) Corrélations variables-axes (approx via charges, car données standardisées)
# Pour une sortie "cercle des corrélations" plus directe, préférer FactoMineR/factoextra (optionnel).
# library(FactoMineR); PCA(num, scale.unit=TRUE, graph=TRUE)

