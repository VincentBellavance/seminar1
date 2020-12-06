library(ggplot2)
library(sf)
library(mgcv)

lpi_data <- as.data.frame(readRDS("data-raw/lpd_qc_fake.rds"))
lpi_data <- lpi_data[lpi_data$year_obs>=1990,]

# create colorblind-friendly palette
#pal <- c("#56B4E9", "#D55E00", "#E69F00", "#0072B2", "#009E73", "#999999")
#palvalues <- c(taxa[2:length(taxa)], "inconnu")

plot_theme <- theme_classic() +
  theme(plot.title = element_text(size=45, face="bold", hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 40),
        axis.title = element_text(size = 40),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

p <- ggplot(data = lpi_data[lpi_data$org_event == 1449,], aes(x = year_obs)) +
  geom_point(aes(y = obs_value), col = "black", lwd = 4) +
  labs(title = "Population 1", y = "Abondance", x = "Années") +
  coord_cartesian(ylim = c(800, 8800)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/plie_data.svg", plot=p, width=10, height=8)

#############################################################################################################

p <- ggplot(data = lpi_data[lpi_data$org_event == 12306,], aes(x = year_obs)) +
  geom_point(aes(y = obs_value), col = "black", lwd = 4) +
  labs(title = "Population 2", y = "Abondance", x = "Années") +
  coord_cartesian(ylim = c(0, 101)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/bal_noire_data.svg", plot=p, width=10, height=8)


#############################################################################################################################################
#GAM
#############################################################################################################################################

x <- lpi_data[lpi_data$org_event == 1449,]

mod1 <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = x, method = "REML")

pred_year <- c(min(x$year_obs):max(x$year_obs))
pred_df <- data.frame(year_obs=pred_year, log_obs=NA)
mod1_df <- data.frame(log_obs = predict(mod1, pred_df, k=nrow(x)/2), year_obs = pred_year)
mod1_df <- dplyr::left_join(mod1_df, x[,c("year_obs", "obs_value")], by = "year_obs")
mod1_df[,"obs_value"] <- log10(mod1_df[,"obs_value"])

p <- ggplot(data = mod1_df, aes(year_obs)) +
  geom_line(aes(y = log_obs), col = "black", lwd = 1.2) +
  geom_point(aes(y=obs_value), col = "red", lwd = 4, alpha = 0.5) +
  labs(title = "Population 1", y = "Abondance", x = "Années") +
  coord_cartesian(ylim = c(0,5)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/plie_gam.svg", plot=p, width=10, height=8)

#############################################################################################################################################

x <- lpi_data[lpi_data$org_event == 12306,]

mod2 <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = x, method = "REML")

pred_year <- c(min(x$year_obs):max(x$year_obs))
pred_df <- data.frame(year_obs=pred_year, log_obs=NA)
mod2_df <- data.frame(log_obs = predict(mod2, pred_df, k=nrow(x)/2), year_obs = pred_year)
mod2_df <- dplyr::left_join(mod2_df, x[,c("year_obs", "obs_value")], by = "year_obs")
mod2_df[,"obs_value"] <- log10(mod2_df[,"obs_value"])

p <- ggplot(data = mod2_df, aes(year_obs)) +
  geom_line(aes(y = log_obs), col = "black", lwd = 1.2) +
  geom_point(aes(y= obs_value), col = "red", lwd = 4, alpha = 0.5) +
  labs(title = "Population 2", y = "Abondance", x = "Années") +
  coord_cartesian(ylim = c(0,5)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/bal_noire_gam.svg", plot=p, width=10, height=8)


#############################################################################################################################################
#Growthrates
#############################################################################################################################################

mod1_df[2:nrow(mod1_df),4] <- mod1_df[2:nrow(mod1_df),1]-mod1_df[1:nrow(mod1_df)-1,1]
colnames(mod1_df)[4] <- "growthrates"

p <- ggplot(data = mod1_df, aes(year_obs)) +
  geom_line(aes(y = growthrates), col = "black", lwd = 1.2) +
  labs(title = "Population 1", y = "Taux de croissance", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/plie_gr.svg", plot=p, width=10, height=8)

#############################################################################################################################################

mod2_df[2:nrow(mod2_df),4] <- mod2_df[2:nrow(mod2_df),1]-mod2_df[1:nrow(mod2_df)-1,1]
colnames(mod2_df)[4] <- "growthrates"

p <- ggplot(data = mod2_df, aes(year_obs)) +
  geom_line(aes(y = growthrates), col = "black", lwd = 1.2) +
  labs(title = "Population 2", y = "Taux de croissance", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/bal_noire_gr.svg", plot=p, width=10, height=8)

#############################################################################################################################################
# Mean growthrates by taxa
#############################################################################################################################################

test <- lapply(unique(lpi_data[lpi_data$system == "Marine" & lpi_data$taxa == "poissons","org_event"]), function(x) {
  y <- lpi_data[lpi_data$org_event == x,]

  if(nrow(y) < 6) {
    return(NULL)
  } else {
    mod <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = y, method = "REML")
    pred_year <- c(min(y$year_obs):max(y$year_obs))
    pred_df <- data.frame(year_obs=pred_year, obs_value=NA)
    mod_df <- data.frame(obs_value = predict(mod, pred_df, k=nrow(y)/2), year_obs = pred_year)
    mod_df[2:nrow(mod_df),3] <- mod_df[2:nrow(mod_df),1]-mod_df[1:nrow(mod_df)-1,1]
    colnames(mod_df)[3] <- "growthrates"
    return(mod_df)
  }
})

min_year <- min(do.call(rbind, test)$year_obs)
max_year <- max(do.call(rbind, test)$year_obs)

marine_fish <- data.frame(year_obs=c(min_year:max_year), mean_gr=rep(NA, length(c(min_year:max_year))))

all <- do.call(rbind, test)

marine_fish[,"mean_gr"] <- do.call(rbind,
  lapply(c(min_year:max_year), function(x) {
    mean(all[all$year_obs == x, "growthrates"], na.rm = TRUE)
  })
)

p <- ggplot(data = marine_fish, aes(year_obs)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Poissons marins", y = "log10(tx croissance) moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/marine_fish_gr.svg", plot=p, width=10, height=8)

################################################################################################################################################

test <- lapply(unique(lpi_data[lpi_data$system == "Marine" & lpi_data$taxa == "mammifères","org_event"]), function(x) {
  y <- lpi_data[lpi_data$org_event == x,]

  if(nrow(y) < 7) {
    return(NULL)
  } else {
    if(nrow(y[y$obs_value == 0,]) > 0) {
      y[y$obs_value == 0, "obs_value"] <- sum(y$obs_value)*0.001
    }
    mod <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = y, method = "REML")
    pred_year <- c(min(y$year_obs):max(y$year_obs))
    pred_df <- data.frame(year_obs=pred_year, obs_value=NA)
    mod_df <- data.frame(obs_value = predict(mod, pred_df, k=nrow(y)/2), year_obs = pred_year)
    mod_df[2:nrow(mod_df),3] <- mod_df[2:nrow(mod_df),1]-mod_df[1:nrow(mod_df)-1,1]
    colnames(mod_df)[3] <- "growthrates"
    return(mod_df)
  }
})

min_year <- min(do.call(rbind, test)$year_obs)
max_year <- max(do.call(rbind, test)$year_obs)

marine_mamm <- data.frame(year_obs=c(min_year:max_year), mean_gr=rep(NA, length(c(min_year:max_year))))

all <- do.call(rbind, test)

marine_mamm[,"mean_gr"] <- do.call(rbind,
  lapply(c(min_year:max_year), function(x) {
    mean(all[all$year_obs == x, "growthrates"], na.rm = TRUE)
  })
)

p <- ggplot(data = marine_mamm, aes(year_obs)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Mammifères marins", y = "log10(tx croissance) moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/marine_mamm_gr.svg", plot=p, width=10, height=8)

#############################################################################################################################################
# Mean growthrates by system
#############################################################################################################################################

test <- lapply(unique(lpi_data[lpi_data$system == "Marine","org_event"]), function(x) {
  y <- lpi_data[lpi_data$org_event == x,]

  if(nrow(y) < 10) {
    return(NULL)
  } else {
    if(nrow(y[y$obs_value == 0,]) > 0) {
      y[y$obs_value == 0, "obs_value"] <- sum(y$obs_value)*0.001
    }
    mod <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = y, method = "REML")
    pred_year <- c(min(y$year_obs):max(y$year_obs))
    pred_df <- data.frame(year_obs=pred_year, obs_value=NA)
    mod_df <- data.frame(obs_value = predict(mod, pred_df, k=nrow(y)/2), year_obs = pred_year)
    mod_df[2:nrow(mod_df),3] <- mod_df[2:nrow(mod_df),1]-mod_df[1:nrow(mod_df)-1,1]
    colnames(mod_df)[3] <- "growthrates"
    return(mod_df)
  }
})

min_year <- min(do.call(rbind, test)$year_obs)
max_year <- max(do.call(rbind, test)$year_obs)

marine <- data.frame(year_obs=c(min_year:max_year), mean_gr=rep(NA, length(c(min_year:max_year))))

all <- do.call(rbind, test)

marine[,"mean_gr"] <- do.call(rbind,
  lapply(c(min_year:max_year), function(x) {
    mean(all[all$year_obs == x, "growthrates"], na.rm = TRUE)
  })
)

p <- ggplot(data = marine, aes(year_obs)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Toutes pop marines", y = "log10(tx croissance) moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/marine_gr.svg", plot=p, width=10, height=8)

######################################################################################################################################

test <- lapply(unique(lpi_data[lpi_data$system == "Terrestrial","org_event"]), function(x) {
  y <- lpi_data[lpi_data$org_event == x,]

  if(nrow(y) < 10) {
    return(NULL)
  } else {
    if(nrow(y[y$obs_value == 0,]) > 0) {
      y[y$obs_value == 0, "obs_value"] <- sum(y$obs_value)*0.001
    }
    mod <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = y, method = "REML")
    pred_year <- c(min(y$year_obs):max(y$year_obs))
    pred_df <- data.frame(year_obs=pred_year, obs_value=NA)
    mod_df <- data.frame(obs_value = predict(mod, pred_df, k=nrow(y)/2), year_obs = pred_year)
    mod_df[2:nrow(mod_df),3] <- mod_df[2:nrow(mod_df),1]-mod_df[1:nrow(mod_df)-1,1]
    colnames(mod_df)[3] <- "growthrates"
    return(mod_df)
  }
})

min_year <- min(do.call(rbind, test)$year_obs)
max_year <- max(do.call(rbind, test)$year_obs)

terrestre <- data.frame(year_obs=c(min_year:max_year), mean_gr=rep(NA, length(c(min_year:max_year))))

all <- do.call(rbind, test)

terrestre[,"mean_gr"] <- do.call(rbind,
  lapply(c(min_year:max_year), function(x) {
    mean(all[all$year_obs == x, "growthrates"], na.rm = TRUE)
  })
)

p <- ggplot(data = terrestre, aes(year_obs)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Toutes pop terrestres", y = "log10(tx croissance) moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/terrestre_gr.svg", plot=p, width=10, height=8)

#############################################################################################################################################
# Global mean growthrates
#############################################################################################################################################

test <- lapply(unique(lpi_data$org_event), function(x) {
  y <- lpi_data[lpi_data$org_event == x,]

  if(nrow(y) < 10) {
    return(NULL)
  } else {
    if(nrow(y[y$obs_value == 0,]) > 0) {
      y[y$obs_value == 0, "obs_value"] <- sum(y$obs_value)*0.001
    }
    mod <- mgcv::gam(log10(obs_value)~s(year_obs), family = gaussian(),data = y, method = "REML")
    pred_year <- c(min(y$year_obs):max(y$year_obs))
    pred_df <- data.frame(year_obs=pred_year, obs_value=NA)
    mod_df <- data.frame(obs_value = predict(mod, pred_df, k=nrow(y)/2, se.fit = TRUE), year_obs = pred_year)
    colnames(mod_df)[1:2] <- c("obs_value", "se")
    mod_df[2:nrow(mod_df),4] <- mod_df[2:nrow(mod_df),1]-mod_df[1:nrow(mod_df)-1,1]
    colnames(mod_df)[4] <- "growthrates"
    return(mod_df)
  }
})

min_year <- min(do.call(rbind, test)$year_obs)
max_year <- max(do.call(rbind, test)$year_obs)

global <- data.frame(year_obs=c(min_year:max_year), mean_gr=rep(NA, length(c(min_year:max_year))), se=rep(NA, length(c(min_year:max_year))))

all <- do.call(rbind, test)

global[,c("mean_gr", "se")] <- do.call(rbind,
  lapply(c(min_year:max_year), function(x) {
    cbind(
      mean(all[all$year_obs == x, "growthrates"], na.rm = TRUE),
      mean(all[all$year_obs == x, "se"], na.rm = TRUE)/50
    )
  })
)

p <- ggplot(data = global, aes(year_obs)) +
  geom_line(aes(y = mean_gr), col = "black", lwd = 1.2) +
  labs(title = "Toutes les populations", y = "Taux de croissance moyen", x = "Années") +
  coord_cartesian(ylim = c(-0.15, 0.1)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/global_gr.svg", plot=p, width=16, height=8)

#############################################################################################################################################
# Calculate LPI
#############################################################################################################################################

lpi <- data.frame(year_obs = global$year_obs, lpi = NA, cilo = NA, cihi = NA)
lpi[1,c(2:4)] <- 1 # initial value is 1 
for(i in 2:nrow(global)){
  lpi[i,2] <- lpi[i-1,2]*10^global[i,"mean_gr"]
  lpi[i,3] <- lpi[i-1,3]*10^(global[i,"mean_gr"]-1.96*global[i,"se"])
  lpi[i,4] <- lpi[i-1,4]*10^(global[i,"mean_gr"]+1.96*global[i,"se"]) 
}

p <- ggplot(data = lpi, aes(year_obs)) +
  geom_ribbon(aes(ymin = cilo, ymax = cihi),fill = "#56B4E9", alpha = .7) +
  geom_line(aes(y = lpi), col = "white", lwd = 1.2) +
  geom_hline(yintercept = 1, lty = 2, col = "black") +
  labs(y = "Valeur de l'indice", x = "Années") +
  coord_cartesian(ylim = c(0,2)) +
  plot_theme

# save plot as svg  
ggsave(file="images/lpi/lpi.svg", plot=p, width=16, height=8)
