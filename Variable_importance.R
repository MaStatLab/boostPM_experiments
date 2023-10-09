
# This is a program code to reproduce the result given in Section 3.3

# Required packages
library(Rcpp)
library(RcppArmadillo)
library(ggplot2)
library(ggpubr)
library(mvtnorm)
library(boostPM)


set.seed(1)

# Generate the data and run the boosting for the three scenarios
d = 10
n = 10000
importance_store = matrix(NA, nrow = 3, ncol = d)

for(type in 1:3){
  data = matrix(NA, nrow = n, ncol = d)  
  
  if(type == 1){
    params = 2^seq(0,by=1,length.out=d)
    for(j in 1:d){
      data[,j] = rbeta(n, params[j], params[j])
    }
  }else if(type == 2){
    rho_vec = seq(0.1, 0.9, by = 0.2)
    
    Sig = matrix(NA, nrow=2, ncol=2)
    diag(Sig) = 1.0
    
    for(j in 1:(d/2)){
      Sig[1,2] = Sig[2,1] = rho_vec[j]
      
      data[,(2*(j-1)+1):(2*j)] = pnorm(rmvnorm(n, sigma=Sig))
    }
  }else if(type == 3){
    groups = list()
    groups[[1]] = c(1)
    groups[[2]] = c(2,3)
    groups[[3]] = c(4,5,6)
    groups[[4]] = c(7,8,9,10)
    
    for(j in 1:4){
      rho = 0.9
      Sig = matrix(NA, nrow=j, ncol=j)
      diag(Sig) = 1.0
      Sig[col(Sig) != row(Sig)] = rho
      data[,groups[[j]]] = pnorm(rmvnorm(n, sigma=Sig))
    }
  }
  
  out = boosting(# parameters for boosting
    data = data, #data = n X d matrix
    add_noise = FALSE, # add uniform noises if there are tied values
    Omega = cbind(rep(0,d), rep(1, d)), # we can input the information of the sample space, ("Omega" in the paper)
    # if Omega = NULL, the sample space is automatically set according to the range
    ntree_max_marginal = 1000, # # trees per dimension used in the first stage
    ntree_max_dependence = 5000, # # trees used in the second stage
    c0 = 0.1, # c0 = global scale of the learning parameter
    gamma = 0.0, # gamma = stronger regularization for small nodes
    max_resol = 15, # maximum resolution (depth) of trees
    min_obs = 10, # if # obs in a node < min_obs, this node is no longer split
    early_stop = c(0,50), # if it is c(1e-5, 50), this means we move to the next step
    # when the average improvement given by the recent 50 trees is less than 1e-5
    nbins = 100, # # grid points for splitting = 2^J-1 
    max_n_var = d, # this is an experimental one so should be set to d 
    # parameters for the PT-based weak learner
    alpha = 0.9, # prior prob of dividing a node = alpha * (1 + depth)^beta
    beta = 0.0,
    precision = 1.0 # precision of the theta prior
  )
  
  importance_store[type,] = out$variable_importance
}




df1 = data.frame(Variable = factor(1:d), VI = importance_store[1,])

p1 = ggplot(data=df1, aes(x=Variable, y=VI)) + geom_bar(stat="identity") +
  labs(y = "Importance") +
  labs(title = paste("Scenario (", 1, ")", sep = "")) +
  coord_flip()

df2 = data.frame(Variable = factor(1:d), VI = importance_store[2,])

p2 = ggplot(data=df2, aes(x=Variable, y=VI)) + geom_bar(stat="identity") +
  labs(y = "Importance") +
  labs(title = paste("Scenario (", 2, ")", sep = "")) +
  coord_flip()

df3 = data.frame(Variable = factor(1:d), VI = importance_store[3,])

p3 = ggplot(data=df3, aes(x=Variable, y=VI)) + geom_bar(stat="identity") +
  labs(y = "Importance") +
  labs(title = paste("Scenario (", 3, ")", sep = "")) +
  coord_flip()

ggarrange(p1, p2, p3, nrow = 1)