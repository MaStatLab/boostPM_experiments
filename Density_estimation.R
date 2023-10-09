
# This program runs the boosting algorithm under the settings used in Section 3.1 and 3.2
# Since learning the high-dimensional structures takes at least several minutes,
# in this program we run the boosting once under a fixed setting for a simulated data

# Required packages
library(Rcpp)
library(RcppArmadillo)
library(boostPM)

# Radom seed
# Set to 1,...,30 in Section 3.1
# Set to 1 in Section 3.2
set.seed(1)

# Generate the data sets for the simulation study (Section 3.1)
# ( For the bechmark datasets used in Section 3.2, 
#  they can be obtained through the repositories
#  available at https://github.com/gpapamak/maf and https://zenodo.org/record/4559067 )

# function to simulate the data sets
setwd("location_of_Simulation.R")
source("Simulation.R")

d = 4 # Dimension
n = 10000 # Size of training and test data sets

model_name = "1_correlation" # Selected from ("1_correlation", "2_clusters", "3_uniforms")
n_trees_marginal = 1000 # # Trees for estimating each marginal distribution (set to 0 when use the one-stage strategy)
n_trees_joint = 5000 # # Trees for estimating the dependency structure

c0 = 0.1 # Global shrinkage parameter
gamma = 0.5 # Local shrinkage parameter

max_resol_boosting = 15 # Maximum resolution (set to 50 in Section 3.2)

# Parameter for the adoptive stopping
# When this is c(0, 50), this means we terminate the current stage
# if the average improvement given in the most recent 50 trees is below 0
# If this is set to NULL, we don't do the adoptive stopping
# The default is c(0, 50), but set to NULL in the first experiment of Section 3.1 
early_stop_setting = c(0, 50)

# simulate the data sets
if(model_name == "1_correlation"){
  tuple_size = 4
}else{
  tuple_size = 2
}

data_train = c()
data_test = c()

for(j in 1:(d/tuple_size)){
  data_train = cbind(data_train, Generate_data(model_name, tuple_size, n))
}

for(j in 1:(d/tuple_size)){
  data_test = cbind(data_test, Generate_data(model_name, tuple_size, n))
}

# Run the boosting to estimate the density function

out = boosting(# parameters for boosting
  data = data_train, #data = n X d matrix
  add_noise = FALSE, # add uniform noises if there are tied values
  Omega = cbind(rep(0,d), rep(1, d)), # we can input the information of the sample space, ("Omega" in the paper)
  # if Omega = NULL, the sample space is automatically set according to the range
  ntree_max_marginal = n_trees_marginal, # # trees per dimension used in the first stage
  ntree_max_dependence = n_trees_joint, # # trees used in the second stage
  c0 = c0, # c0 = global scale of the learning parameter
  gamma = gamma, # gamma = stronger regularization for small nodes
  max_resol = max_resol_boosting, # maximum resolution (depth) of trees
  min_obs = 10, # if # obs in a node < min_obs, this node is no longer split
  early_stop = early_stop_setting, # if it is c(1e-5, 50), this means we move to the next step
  # when the average improvement given by the recent 50 trees is less than 1e-5
  nbins = 100, # # grid points for splitting 
  max_n_var = d, # this is an experimental one so should be set to d 
  # parameters for the PT-based weak learner
  alpha = 0.9, # prior prob of dividing a node = alpha * (1 + depth)^beta
  beta = 0.0,
  precision = 1.0 # precision of the theta prior
)

# evalute the densities at the points in the test data
out_dens = eval_density_b(list_boosting = out, # simply use the output of the boosting function
                          eval_points = data_test # matrix of evaluation points
)

# compute the predictive score
print(mean(out_dens$log_densities))

# it is also possible to obtain the growth of the predictive score
# this is used in the first experiment of Section 3.1 
# ( Note that the plot looks different from those given in the paper
#   because we use the one-stage strategy in the corresonding experiment)
plot(out_dens$mean_log_dens_path, xlab = "# trees", ylab = "predictive scores", type = "l", lwd = 2)
