# 1) Lecture
tab <- read.csv("../data/btc_spot_1d.csv")

# 2) Résumés numériques (quantitatif)
summary(tab$close)        # moyenne, médiane, quantiles, etc.
summary(tab$volume)

# Histogrammes & boîtes (cf. TD univarié)
hist(tab$close, main="Histogramme Close", xlab="Close")
boxplot(tab$close, main="Boxplot Close", ylab="Close")

# Outliers visuels sur ret
boxplot(tab$ret, main="Boxplot Retours (ret)", ylab="ret")

# 3) Variables qualitatives existantes /créées : trend (nominale), vol_level (ordinale)
tab$trend <- as.factor(tab$trend)
tab$vol_level <- factor(tab$vol_level, levels=c("Faible","Moyenne","Élevée"), ordered=TRUE)

# Table de contingence + parts (cf. TD : table, prop.table)
t_trend <- table(tab$trend)
t_vol   <- table(tab$vol_level)

print(t_trend); print(t_vol)
prop.table(t_trend)           # fréquences
prop.table(t_vol)

# 4) Transformation quanti -> quali par classes (ex. close en 4 classes)
brks <- quantile(tab$close, probs=c(0, .25, .5, .75, 1), na.rm=TRUE)
tab$close_bucket <- cut(tab$close, breaks=unique(brks), include.lowest=TRUE)
table(tab$close_bucket)
