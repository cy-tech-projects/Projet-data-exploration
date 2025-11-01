tab <- read.csv("./data/btc_spot_1d.csv")
tab$trend <- factor(tab$trend)  # nominale
tab$vol_level <- factor(tab$vol_level, levels=c("Faible","Moyenne","Élevée"), ordered=TRUE)

# 1) Tableau des effectifs
T <- table(tab$trend, tab$vol_level)
T

# 2) Profils lignes & colonnes (prop.table)
prof_lignes   <- prop.table(T, margin=1)  # chaque ligne somme à 1
prof_colonnes <- prop.table(T, margin=2)  # chaque colonne somme à 1
round(prof_lignes, 3)
round(prof_colonnes, 3)

# 3) Visualisation simple
barplot(T, beside=TRUE, legend=TRUE, main="Trend x Vol_level (effectifs)")
barplot(t(prof_lignes), beside=TRUE, legend=TRUE, main="Profils lignes")

# 4) (Optionnel) Test d’indépendance (Chi-2)
chisq.test(T)

