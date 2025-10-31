tab <- read.csv("./data/btc_spot_1d.csv")
tab$trend <- as.factor(tab$trend)
tab$vol_level <- factor(tab$vol_level, levels=c("Faible","Moyenne","Élevée"), ordered=TRUE)

# 1) Boxplots : close ~ trend / vol_level
boxplot(close ~ trend, data=tab, main="Close par tendance", ylab="Close")
boxplot(close ~ vol_level, data=tab, main="Close par niveau de volatilité", ylab="Close")

# 2) Moyennes par modalités (cf. tapply)
tapply(tab$close, tab$trend, mean, na.rm=TRUE)
tapply(tab$close, tab$vol_level, mean, na.rm=TRUE)

# 3) Décomposition de la variance (intuitif)
# Var_totale = Var_expliquée (entre-groupes) + Var_résiduelle (intra)
y <- tab$close
g <- tab$trend
mu <- mean(y, na.rm=TRUE)

# entre-groupes
mu_g <- tapply(y, g, mean, na.rm=TRUE)
n_g  <- tapply(y, g, function(z) sum(!is.na(z)))
sse  <- sum( n_g * (mu_g - mu)^2 )                     # somme carrés expliquée
# intra-groupes
ssr <- sum( (y - mu_g[as.character(g)])^2, na.rm=TRUE) # résiduelle
sst <- var(y, na.rm=TRUE) * (sum(!is.na(y)) - 1)

c(sse=sse, ssr=ssr, sst=sst, ratio_exp = sse/sst)  # ratio_exp = "rapport de corrélation"

