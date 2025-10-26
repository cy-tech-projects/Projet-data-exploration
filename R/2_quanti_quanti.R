tab <- read.csv("../data/btc_spot_1d.csv")

# 1) Nuage & corrélation
plot(tab$open, tab$close, main="Close en fonction de Open",
     xlab="Open", ylab="Close")
cor(tab$open, tab$close, use="complete.obs")

# 2) Régression linéaire (cf. TD : lm, summary, abline)
modele <- lm(close ~ open, data=tab)
summary(modele)
abline(modele$coef[1], modele$coef[2], col="red", lwd=2)

# 3) Résidus standardisés & points atypiques (cf. restd / seuil ±2)
restd <- rstandard(modele)
plot(fitted(modele), restd, ylim=range(-2,2,restd),
     main="Résidus standardisés", xlab="Ajustés", ylab="Résidus std")
abline(h=c(-2,2), col="red", lwd=2)

# 4) Coefficient de détermination
# (directement dans summary(modele)$r.squared)
summary(modele)$r.squared

# 5) Prédictions exemples
newx <- data.frame(open=c(10000, 30000, 60000))
predict(modele, newdata=newx)
