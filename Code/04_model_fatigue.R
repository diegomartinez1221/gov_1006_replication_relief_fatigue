
# Load Packages
list.of.packages <- c("dplyr","R2jags","R2WinBUGS")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(R2jags)
library(R2WinBUGS)

load("Data/npitch.Rdata")
pitch <- read.csv("Data/all_pitches_pred.csv")

# Change the type of Factor Variables
relief.lookback$year <- as.numeric(as.character(relief.lookback$year))
relief.lookback$pitcher <- as.numeric(as.character(relief.lookback$pitcher))
relief.lookback$date <- as.Date(relief.lookback$date)
relief.lookback$gameday_link <- as.character(relief.lookback$gameday_link)
relief.lookback$pitcher_name <- as.character(relief.lookback$pitcher_name)
relief.lookback[,6:13] <- sapply(relief.lookback[,6:13], function(x){
  as.numeric(as.character(x))
})
pitch$pitcher_name <- as.character(pitch$pitcher_name)
pitch$gameday_link <- as.character(pitch$gameday_link)

# Only Include Regular Season pitches from 2013-2015
reg_gameday <- pitch %>%
  filter(reg_season == "True") %>%
  select(gameday_link)

reg_gameday <- as.character(reg_gameday[,1])

relief.lookback <- relief.lookback %>%
  filter(gameday_link %in% reg_gameday)

pitch <- pitch %>%
  filter(gameday_link %in% reg_gameday)

# Aggregate Stuff (for each pitch)
# Overall Pitches
avg_stuff_all <- pitch %>%
  group_by(gameday_link, pitcher, pitcher_name) %>%
  summarise(mean_stuff = mean(z_stuff), num.pitches = n()) %>%
  filter(num.pitches >= 3) %>%
  select(-num.pitches) %>%
  arrange(desc(mean_stuff))

npitch_all <- inner_join(avg_stuff_all, relief.lookback, by = c("gameday_link","pitcher",
                                                                "pitcher_name"))
#Individual Pitches
avg_stuff_indpitch <- pitch %>%
  group_by(gameday_link, pitcher, pitcher_name, pitch_type) %>%
  summarise(mean_stuff = mean(z_stuff), mean_spin = mean(spin_rate),
            mean_velo = mean(start_speed), mean_vz0 = mean(vz0), 
            num.pitches = n()) %>%
  filter(num.pitches >= 3) %>%
  select(-num.pitches) %>%
  arrange(desc(mean_stuff))
 
# Four-Seam Fastballs
npitch_ff <- avg_stuff_indpitch %>%
  filter(pitch_type == "FF") %>%
  inner_join(relief.lookback)

# Two-Seam Fastballs
npitch_ft <- avg_stuff_indpitch %>%
  filter(pitch_type == "FT") %>%
  inner_join(relief.lookback)

# Curveballs
npitch_cu <- avg_stuff_indpitch %>%
  filter(pitch_type == "CU") %>%
  inner_join(relief.lookback)

# Changeups
npitch_ch <- avg_stuff_indpitch %>%
  filter(pitch_type == "CH") %>%
  inner_join(relief.lookback)

# Cutters
npitch_fc <- avg_stuff_indpitch %>%
  filter(pitch_type == "FC") %>%
  inner_join(relief.lookback)

# Splitters
npitch_fs <- avg_stuff_indpitch %>%
  filter(pitch_type == "FS") %>%
  inner_join(relief.lookback)

# Sinkers
npitch_si <- avg_stuff_indpitch %>%
  filter(pitch_type == "SI") %>%
  inner_join(relief.lookback)

# Sliders
npitch_sl <- avg_stuff_indpitch %>%
  filter(pitch_type == "SL") %>%
  inner_join(relief.lookback)

# All Fastballs
npitch_fast <- avg_stuff_indpitch %>%
  filter(pitch_type %in% c("FF","FT","FS")) %>%
  inner_join(relief.lookback)

# Frequentist Model
library(lme4)
lmer1 <- lmer(mean_stuff ~ (1|pitcher) + npitch1 + npitch2 + npitch3, 
              data = npitch_sl)
summary(lmer1)


# JAGS Model

runJAGSmodel <- function(pitch, y, n.iter = 2000, n.thin = 1){
  mod.data <- list(Y = as.numeric(scale(unlist(pitch[,y]))),
                   X1 = pitch$npitch1,
                   X2 = pitch$npitch2,
                   X3 = pitch$npitch3,
                   X4 = pitch$npitch4,
                   X5 = pitch$npitch5,
                   X6 = pitch$npitch6,
                   X7 = pitch$npitch7,
                   n = nrow(pitch),
                   pitcher = as.factor(pitch$pitcher),
                   n.pitcher = length(unique(pitch$pitcher))
  )
  
  
  pitch_model <- function(){
    for(i in 1:n){
      mu[i] <- alpha[pitcher[i]] + beta[pitcher[i]]*(gamma[pitcher[i]]*X1[i] + gamma[pitcher[i]]^2*X2[i] + 
                                                       gamma[pitcher[i]]^3*X3[i])# + gamma[pitcher[i]]^4*X4[i] + gamma[pitcher[i]]^5*X5[i])# + 
      #gamma^6*X6[i] + gamma^7*X7[i]
      Y[i] ~ dnorm(mu[i], tau)
    }
    
    tau ~ dgamma(.1,.1)
    
    sd.a ~ dt(0,.1,1)%_%T(0,)
    phi.a <- sd.a^(-2)
    
    sd.b ~dt(0,0.1,1)%_%T(0,)
    phi.b <- sd.b^(-2)
    
    
    sd.g ~ dt(0,.1,1)%_%T(0,)
    phi.g <- sd.g^(-2)
    
    mu.g ~ dnorm(0,4)
    mu.b ~ dnorm(0,10)
    
    
    for(p in 1:n.pitcher){
      alpha[p] ~ dnorm(0,phi.a)
      beta[p] ~ dnorm(mu.b,phi.b)
      gamma[p] ~ dnorm(mu.g,phi.g)
      
    }
  }
  
  # Create a function that provides intial values for WinBUGS
  pitch.inits = function() {
    alpha <- rep(0, mod.data$n.pitcher)
    gamma <- rep(0, mod.data$n.pitcher)
    beta <- rep(0, mod.data$n.pitcher)
    sd.b <- .1
    sd.a <- 1
    sd.g <- 1
    mu.b <- -0.005
    mu.g <- 0.5
    return(list(alpha=alpha, gamma=gamma,
                sd.a = sd.a, sd.g = sd.g, mu.g=mu.g,mu.b=mu.b))
  }
  
  
  parameters = c("alpha","gamma","sd.a", "sd.g","beta", "sd.b", "mu.g","mu.b")
  
  
  pitch.model.file = paste(getwd(),"Output","pitch-model.txt", sep="/")
  write.model(pitch_model, pitch.model.file)
  
  pitch.sim = jags(mod.data, inits=pitch.inits, parameters, model.file=pitch.model.file,
                   n.iter = n.iter, n.thin = n.thin)
  
  #Runs chain until convergence, returns samples post-burn-in
  pitch.bugs = as.mcmc(pitch.sim$BUGSoutput$sims.matrix)
  pitch.bugs
}

velo_model <- runJAGSmodel(npitch_ff, y = "mean_stuff")

(n.pitcher.ff <- length(unique(npitch_ff$pitcher)))
#Get output into matrices
alpha.ff.mat <- gamma.ff.mat <- beta.ff.mat <- matrix(numeric(3000*n.pitcher.ff),ncol = n.pitcher.ff)
for(p in 1:n.pitcher.ff){
  gamma.ff.mat[,p] <- as.numeric(velo_model[,paste0("gamma[",p,"]")])
  alpha.ff.mat[,p] <- as.numeric(velo_model[,paste0("alpha[",p,"]")])
  beta.ff.mat[,p] <- as.numeric(velo_model[,paste0("beta[",p,"]")])
}

#Examine posterior means
library(ggplot2)
alpha.ff.mean <- apply(alpha.ff.mat,2,mean)
alpha.ff.df <- data.frame(cbind(1:n.pitcher.ff), alpha.ff.mean)
colnames(alpha.ff.df) <- c("pitcher", "mean")
alpha.quantiles <- quantile(alpha.ff.mean, c(0.025, 0.975))
ggplot(alpha.ff.df, mapping = aes(x = pitcher, y = mean)) + 
  geom_point() + 
  geom_hline(yintercept = mean(alpha.ff.mean), color = "red", lwd = 2) + 
  geom_hline(yintercept = alpha.quantiles, color = "blue", lwd = 2, lty = 2) +
  theme(text = element_text(size=15, face = "bold")) +
  xlab("Reliever") +
  ylab(expression(alpha[i])) + 
  ggtitle("Pitch Quality Intercepts")

plot(alpha.ff.mean, xlab = "Reliever", 
     ylab = "Alpha Value", main = "Overall Stuff Intercept",
     pch = 19, cex = .7)
abline(h = mean(alpha.ff.mean), col = "red", lwd = 2)
abline(h = alpha.quantiles, col = "blue", lty = 2, lwd = 2)


plot(apply(gamma.ff.mat,2,mean), xlab = "Reliever", 
     ylab = "Phi Value", main = "Effectiveness Decay",
     pch = 19, cex = .7)

plot(apply(beta.ff.mat,2,mean), xlab = "Reliever", 
     ylab = "Beta Value", main = "Baseline Effectiveness Decay",
     pch = 19, cex = .7)

alpha.ff.mean <- apply(alpha.ff.mat,2,mean)
beta.ff.mean <- apply(beta.ff.mat,2,mean)
gamma.ff.mean <- apply(gamma.ff.mat,2,mean)

ff.dset <- as.data.frame(cbind(as.numeric(levels(as.factor(npitch_ff$pitcher))), 
                               alpha.ff.mean, beta.ff.mean, gamma.ff.mean)) %>%
  arrange(desc(alpha.ff.mean))
colnames(ff.dset)[1] <- "pitcher"
