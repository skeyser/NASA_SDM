###################################################################
###############Script for running Bayesian Harvest Model###########
###################################################################

pacman::p_load(nimble,
               coda,
               stringr,
               sf,
               parallel)

load(here::here("Generated_DFs/Fisher_2018_HarvestModel_10km.Rdata"))
cov <- st_read("Generated_DFs/final_covariates_5_poly_10km.shp")
# cov$ssl <- scale(cov$ssl)
# cov$scv <- scale(cov$scv)
# cov$fwos <- scale(cov$fwos)
# cov$tree <- scale(cov$tree)
# cov$road <- scale(cov$road)

# inspect data and constants used in modeling
# refer to README file for description of what they are
str(data_harvest_only)
str(constants_harvest_only)

# inspect environmental predictors
plot(cov[1], border = NA)
plot(cov[2], border = NA)
plot(cov[3], border = NA)
plot(cov[4], border = NA)
plot(cov[5], border = NA)


harvest <- nimbleCode({
  #........................................
  # PRIORS
  #........................................
  
  #Priors for SDM #
  
  #CAR prior for spatial random effect
  s[1:ncell] ~ dcar_normal(adj[1:neigh], weights[1:neigh], num[1:ncell], tau)
  
  #precision of CAR prior
  tau ~ dgamma(1, 1)
  
  #regression coefficient for ssl
  b_ssl ~ dnorm(0, 2)
  
  #regression coefficient for scv
  b_scv ~ dnorm(0, 2)
  
  #regression coefficient  for fwos
  b_fwos ~ dnorm(0, 2)
  
  #regession coefficient for forest cover
  b_tree ~ dnorm(0, 2)
  
  #regression coefficient for road density
  b_road ~ dnorm(0, 2)
  
  #intercept for linear function scaling grid- and county-level expected abundance
  gamma0 ~ dnorm(0, 2)
  
  #slope for linear function scaling grid- and county-level expected abundance
  gamma1 ~ dnorm(0, 2)
  #.................................................
  # likelihood
  #.................................................
  
  # SDM - model for latent state
  for(i in 1:ncell){
    log(lambda[i]) <- s[i] + b_ssl*ssl[i] + b_scv*scv[i] + b_fwos*fwos[i] + b_tree*tree[i] + b_road*road[i]
    n[i] ~ dpois(lambda[i])
  }
  
  for(r in 1:ntr){
    log(lambda_tr[r]) <- gamma0 + gamma1*log(sum(lambda[low[r]:high[r]]))
    harvest[r] ~ dpois(lambda_tr[r])
  }
  
})

#............................................
# Prepare model to run
#............................................

# function to provide random initial values for parameters 
inits <- function(){
  base::list(n = rep(1, constants_harvest_only$ncell),
             b_ssl = runif(1, -1, 1),
             b_scv = runif(1, -1, 1),
             b_fwos = runif(1, -1, 1),
             b_tree = runif(1, -1, 1),
             b_road = runif(1, -1, 1),
             gamma0 = runif(1, -1, 1), 
             gamma1 = runif(1, -1, 1),
             tau = rgamma(1, 1, 1),
             s = rep(0, base::length(data_harvest_only$num)))}

# parameters to monitor
#keepers <- c("lambda", "b_ssl", "b_fwos", "b_tree", "b_road", "gamma0", "gamma1")
keepers <- c("lambda", "b_ssl", "b_scv", "b_fwos", "b_tree", "b_road", "gamma0", "gamma1")

data_harvest_only <- base::list(
  harvest = as.double(data_harvest_only$harvest), 
  num = data_harvest_only$num, 
  adj = data_harvest_only$adj, 
  weights = data_harvest_only$weights, 
  ssl = as.vector(data_harvest_only$ssl),
  scv = as.vector(data_harvest_only$scv),
  fwos = as.vector(data_harvest_only$fwos),
  tree = as.vector(data_harvest_only$tree), 
  road = as.vector(data_harvest_only$roads)
)

constants_harvest_only <- base::list(
  ncell = constants_harvest_only$ncell, 
  ntr = constants_harvest_only$ntr,
  low = constants_harvest_only$low, 
  high = constants_harvest_only$high, 
  neigh = constants_harvest_only$neigh
)

nc <- 3 # number of chains
nb <- 100 # number of initial MCMC iterations to discard
ni <- nb + 1000 # total number  of iterations

#New 
start <- Sys.time()

#New 
#Make clusters
cl <- makeCluster(nc)

parallel::clusterExport(cl, c("harvest",
                              "inits",
                              "data_harvest_only",
                              "constants_harvest_only",
                              "keepers",
                              "nb",
                              "ni"))

for(j in seq_along(cl)){
  set.seed(j)
  init <- inits()
  clusterExport(cl[j], "init")
}

# .......................................................................
# RUN MODEL
# .......................................................................

# create model
out <- clusterEvalQ(cl, {
  library(nimble)
  library(coda)
  
  model <- nimble::nimbleModel(code = harvest,
                               #name = "harvest",
                               data = data_harvest_only, 
                               constants = constants_harvest_only, 
                               inits = init)
  
  # check to see if everything is initialized
  model$initializeInfo()
  
  # compile the model
  c_model <- nimble::compileNimble(model)
  
  model_conf <- nimble::configureMCMC(model)
  
  model_conf$addMonitors(keepers)
  
  model_mcmc <- nimble::buildMCMC(model_conf)
  
  c_model_mcmc <- nimble::compileNimble(model_mcmc, project = model)
  
  out1 <- nimble::runMCMC(c_model_mcmc, 
                          nburnin = nb, 
                          niter = ni) 
  #nchains = nc)
  return(as.mcmc(out1))
}) #end parallel run

end <- Sys.time()
start - end
#stopCluster(cl)

# .......................................................................
# INSPECT RESULTS
# .......................................................................

outmcmc <- coda::as.mcmc.list(out)

pacman::p_load(tidyverse)
rhat <- as_tibble(coda::gelman.diag(outmcmc[,1:4])[[1]],
                  rownames = "parameter") %>% 
  setNames(., c("parameter", "rhat", "upper")) %>% 
  dplyr::select(-upper)

# extract mean and SD occurrence probability of each grid cell
samplesdf <- do.call(rbind, samples_mcmc)
lambda <- samplesdf[, which(stringr::str_detect(string = colnames(samplesdf), pattern = 'lambda\\['))]
lambda_mean <- apply(lambda, 2, mean)
lambda_sd <- apply(lambda, 2, sd)

# map mean and standard deviation of occurrence probability
par(mfrow=c(1,1))
cov$lambda_mean <- lambda_mean
#cov.test <- cov[cov$lambda_mean >= quantile(cov$lambda_mean, prob = 0.97), ]
plot(cov.test["lambda_mean"], border = NA)
cov$lambda_sd <- lambda_sd
plot(cov["lambda_sd"], border = NA)
