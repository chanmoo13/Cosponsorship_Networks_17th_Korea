#################################################################################
########################    Cosponsorship Networks in    ########################
########################  the 17th National Assembly of  ########################
########################        Republic of Korea        ########################
#################################################################################


#### Chanmoo Park,  Woncheol Jang
#### Department of Statistics,
#### Seoul National University, KOREA
#### Any issues for codes : "chanmoo13@snu.ac.kr" or "chanmoopark13@gmail.com" (Chanmoo Park)



########## Fitting ERGM for valued networks ##########



load("Data/constructedNetwork.RData")
library("network")
library("ergm.count")
set.seed(2017)




###### First Period ######


# Model B
baseline.1 <- ergm(net.1 ~ sum + sum(pow=1/2) + nonzero,
                   response = "count", reference = ~Poisson, 
                   control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 20000, seed = 2017),
                   eval.loglik = TRUE)
summary(baseline.1)

# Model BM
mutual.1 <- ergm(net.1 ~ sum + sum(pow=1/2) + nonzero + mutual(form="min"),
                 response = "count", reference = ~Poisson, 
                 control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 20000, seed = 2017),
                 eval.loglik = TRUE)
summary(mutual.1)

# Model BP
party.1 <- ergm(net.1 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum"),
                response = "count", reference = ~Poisson, 
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                eval.loglik = TRUE)
summary(party.1)

# Model BN
nelec.1 <- ergm(net.1 ~ sum + sum(pow=1/2) + nonzero + nodeocov("nelec", form="sum") + nodeicov("nelec", form="sum"), 
                 response = "count", reference = ~Poisson, 
                 control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 20000, seed = 2017),
                 eval.loglik = TRUE)
summary(nelec.1)

# Model BMPN
all.1 <- ergm(net.1 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum")
              + nodeicov("nelec", form="sum") + nodeocov("nelec", form="sum") + mutual("min"),
              response = "count", reference = ~Poisson, 
              control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 100000),
              eval.loglik = FALSE)
summary(all.1)

test.1 <- ergm(net.1 ~ sum + nonzero + nodematch("party", diff=TRUE, form="sum")
               + nodeicov("nelec", form="sum") + nodeocov("nelec", form="sum") + mutual("min"),
               response = "count", reference = ~Poisson, 
               control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 100000),
               eval.loglik = FALSE)



###### Second Period ######


# Model B
baseline.2 <- ergm(net.2 ~ sum + sum(pow=1/2) + nonzero,
                   response = "count", reference = ~Poisson,
                   control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                   eval.loglik = TRUE)

# Model BM
mutual.2 <- ergm(net.2 ~ sum + sum(pow=1/2) + nonzero + mutual(form="min"),
                 response = "count", reference = ~Poisson,
                 control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                 eval.loglik = TRUE)

# Model BP
party.2 <- ergm(net.2 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 70000, seed = 2017),
                eval.loglik = TRUE)

# Model BN
nelec.2 <- ergm(net.2 ~ sum + sum(pow=1/2) + nonzero + nodeocov("nelec", form="sum") + nodeicov("nelec", form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                eval.loglik = TRUE)

# Model BMPN
all.2 <- ergm(net.2 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum")
              + nodeicov("nelec", form="sum") + nodeocov("nelec", form="sum") + mutual("min"),
              response = "count", reference = ~Poisson,
              control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 150000, seed = 2017),
              eval.loglik = TRUE)



###### Third Period ######


# Model B
baseline.3 <- ergm(net.3 ~ sum + sum(pow=1/2) + nonzero,
                   response = "count", reference = ~Poisson,
                   control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                   eval.loglik = TRUE)

# Model BM
mutual.3 <- ergm(net.3 ~ sum + sum(pow=1/2) + nonzero + mutual(form="min"),
                 response = "count", reference = ~Poisson,
                 control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                 eval.loglik = TRUE)

# Model BP
party.3 <- ergm(net.3 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 70000, seed = 2017),
                eval.loglik = TRUE)

# Model BN
nelec.3 <- ergm(net.3 ~ sum + sum(pow=1/2) + nonzero + nodeocov("nelec", form="sum") + nodeicov("nelec", form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                eval.loglik = TRUE)

# Model BMPN
all.3 <- ergm(net.3 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum")
              + nodeicov("nelec", form="sum") + nodeocov("nelec", form="sum") + mutual("min"),
              response = "count", reference = ~Poisson,
              control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 150000, seed = 2017),
              eval.loglik = TRUE)
                                     


###### Fourth Period ######


# Model B
baseline.4 <- ergm(net.4 ~ sum + sum(pow=1/2) + nonzero,
                   response = "count", reference = ~Poisson,
                   control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                   eval.loglik = TRUE)

# Model BM
mutual.4 <- ergm(net.4 ~ sum + sum(pow=1/2) + nonzero + mutual(form="min"),
                 response = "count", reference = ~Poisson,
                 control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                 eval.loglik = TRUE)

# Model BP
party.4 <- ergm(net.4 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 70000, seed = 2017),
                eval.loglik = TRUE)

# Model BN
nelec.4 <- ergm(net.4 ~ sum + sum(pow=1/2) + nonzero + nodeocov("nelec", form="sum") + nodeicov("nelec", form="sum"),
                response = "count", reference = ~Poisson,
                control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 50000, seed = 2017),
                eval.loglik = TRUE)

# Model BMPN
all.4 <- ergm(net.4 ~ sum + sum(pow=1/2) + nonzero + nodematch("party", diff=TRUE, form="sum")
              + nodeicov("nelec", form="sum") + nodeocov("nelec", form="sum") + mutual("min"),
              response = "count", reference = ~Poisson,
              control = control.ergm(MCMLE.trustregion = 1000, MCMLE.maxit = 1000, MCMC.samplesize = 150000, seed = 2017),
              eval.loglik = TRUE)

save.image(file = "Data/fittedResults.RData")
