#
#
#
#
library(raster)
library(sf)
library(ggplot2)

plot_theme <- theme_classic() +
  theme(plot.title = element_text(size=45, face="bold", hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 40),
        axis.title = element_text(size = 40),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

data <- raster::stack("data-raw/sdm_0_4.gri")

####################################################

bobo <- data[["Bobolink_0"]]
bobo_list <- lapply(0:20, function(x) {
  bobo + x*((data[["Bobolink_4"]] - bobo)/20)
})
names(bobo_list) <- c(1990:2010)


jay <- data[["Gray_Jay_0"]]
jay_list <- lapply(0:20, function(x) {
  jay + x*((data[["Gray_Jay_4"]] - jay)/20)
})
names(jay_list) <- c(1990:2010)

####################################################

#svg(filename = "images/bdi/bobo.svg",width = 14, height = 8)
#par(mfrow = c(1,2))
#plot(bobo_list[[1]], zlim=c(0,0.4), main = "Dolichonyx oryzivorus (1990)", legend.width = 2, legend.lab="Probabilité d'occurrence")
#plot(bobo_list[[20]], zlim=c(0,0.4), main = "Dolichonyx oryzivorus (2010)", legend.width = 2, legend.lab="Probabilité d'occurrence")
#dev.off()
#
#
#svg(filename = "images/bdi/jay.svg",width = 14, height = 8)
#par(mfrow = c(1,2))
#plot(jay_list[[1]], zlim=c(0,1), main = "Perisoreus canadensis (1990)", legend.width = 2, legend.lab="Probabilité d'occurrence")
#plot(jay_list[[20]], zlim=c(0,1), main = "Perisoreus canadensis (2010)", legend.width = 2, legend.lab="Probabilité d'occurrence")
#dev.off()

####################################################
#Somme des rasters
####################################################

sum_bobo <- lapply(bobo_list, function(x) {
  return(sum(x[,,], na.rm = TRUE)*9)
})

bobo_df <- data.frame(year = c(1990:2010), somme = do.call(rbind, sum_bobo))

p <- ggplot(data = bobo_df, aes(year)) +
  geom_line(aes(y = somme), col = "black", lwd = 1.2) +
  labs(title = "Dolichonyx oryzivorus", y = "Aires de distribution (km2)", x = "Années") +
  coord_cartesian(ylim = c(110000, 280000)) +
  plot_theme

ggsave(file="images/bdi/bobo_sum.svg", plot=p, width=15, height=10)


sum_jay <- lapply(jay_list, function(x) {
  return(sum(x[,,], na.rm = TRUE)*9)
})

jay_df <- data.frame(year = c(1990:2010), somme = do.call(rbind, sum_jay))

p <- ggplot(data = jay_df, aes(year)) +
  geom_line(aes(y = somme), col = "black", lwd = 1.2) +
  labs(title = "Perisoreus canadensis", y = "Aires de distribution (km2)", x = "Années") +
  coord_cartesian(ylim = c(190000, 710000)) +
  plot_theme

ggsave(file="images/bdi/jay_sum.svg", plot=p, width=15, height=10)

####################################################
#Taux de croissance
####################################################

bobo_df[1,3] <- NA 
colnames(bobo_df)[3] <- "gr"
bobo_df[2:nrow(bobo_df),"gr"] <- log10(bobo_df[2:nrow(bobo_df),"somme"]/bobo_df[1:nrow(bobo_df)-1,"somme"])

p <- ggplot(data = bobo_df, aes(year)) +
  geom_line(aes(y = gr), col = "black", lwd = 1.2) +
  labs(title = "Dolichonyx oryzivorus", y = "log10(taux de croissance)", x = "Années") +
  coord_cartesian(ylim = c(-0.06, 0.03)) +
  plot_theme

ggsave(file="images/bdi/bobo_gr.svg", plot=p, width=15, height=10)


jay_df[1,3] <- NA 
colnames(jay_df)[3] <- "gr"
jay_df[2:nrow(jay_df),"gr"] <- log10(jay_df[2:nrow(jay_df),"somme"]/jay_df[1:nrow(jay_df)-1,"somme"])

p <- ggplot(data = jay_df, aes(year)) +
  geom_line(aes(y = gr), col = "black", lwd = 1.2) +
  labs(title = "Perisoreus canadensis", y = "log10(taux de croissance)", x = "Années") +
  coord_cartesian(ylim = c(-0.06, 0.03)) +
  plot_theme

ggsave(file="images/bdi/jay_gr.svg", plot=p, width=15, height=10)

####################################################
#Taux de croissance moyen
####################################################

bdi <- data.frame(year=c(1990:2010), mean_gr = NA, bdi = NA, co = NA, ex = NA)
bdi[2:nrow(bdi),"mean_gr"] <- (bobo_df[2:nrow(bobo_df),"gr"] + jay_df[2:nrow(jay_df),"gr"])/2

p <- ggplot(data = bdi, aes(year)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Toutes espèces", y = "log10(taux de croissance) moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.06, 0.03)) +
  plot_theme

ggsave(file="images/bdi/mean_gr.svg", plot=p, width=14, height=10)

####################################################
#Calcul de l'indice
####################################################

bdi[1,"bdi"] <- 1
for(i in 2:nrow(bdi)) {
  bdi[i,"bdi"] <- bdi[i-1,"bdi"]*10^bdi[i,"mean_gr"]
}

p <- ggplot(data = bdi, aes(year)) +
  geom_line(aes(y = bdi), col = "black", lwd = 1.2) +
  labs(title = "Biodiversity Distribution Index", y = "Valeur de l'indice", x = "Années") +
  coord_cartesian(ylim = c(0.75, 1.10)) +
  plot_theme

ggsave(file="images/bdi/bdi_final.svg", plot=p, width=14, height=10)

####################################################
#Colonisation / Extinction
####################################################

coex_bobo <- lapply(bobo_list, function(x) {
  co = sum(x[x>=0], na.rm = TRUE)
  ex = sum(x[x<0], na.rm = TRUE)
  return(cbind(co, ex))
})
