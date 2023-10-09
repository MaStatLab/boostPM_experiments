Generate_data = function(model_name, d, n){

  data = matrix(NA, nrow = n, ncol = d)

  if(model_name == "1_correlation"){

    dim_pair = 4
    limit_abs = 4

    cor = 0.9

    cov_mat = matrix(NA,nrow=dim_pair,ncol=dim_pair)

    for(j1 in 1:dim_pair){
      for(j2 in 1:dim_pair){
        if(j1 == j2){
          cov_mat[j1,j2] = 1
        }else{
          cov_mat[j1,j2] = cor^abs(j1-j2)
        }
      }
    }

    chol_mat = chol(cov_mat)

    for(j in 1:(d/dim_pair)){
      for(i in 1:n){

        z = rep(10,dim_pair)

        while((min(z) < -limit_abs) | (max(z) > limit_abs)){
          z = t(chol_mat) %*% rnorm(dim_pair)
        }

        data[i,((j-1)*dim_pair+1):(j*dim_pair)] = z

      }
    }

    #Normalize the data
    data = (data - (-limit_abs)) / (2*limit_abs)

  }else if(model_name == "2_clusters"){ #In the paper, this model is labeled as "clusters"

    w = c(1/10, 3/10, 3/10, 3/10)
    cum_w = cumsum(w)

    parameters_x = matrix(NA, ncol = 4, nrow = 4)
    parameters_x[1,] = c(1, 1)
    parameters_x[2,] = c(15,45)
    parameters_x[3,] = c(45,15)
    parameters_x[4,] = c(37.5,22.5)

    parameters_y = matrix(NA, ncol = 4, nrow = 4)
    parameters_y[1,] = c(1, 1)
    parameters_y[2,] = c(15,45)
    parameters_y[3,] = c(22.5,37.5)
    parameters_y[4,] = c(45,15)

    for(i in 1:n){
      u = runif(1)
      sampled = 0

      for(j in 1:4){
        if((u < cum_w[j]) & (sampled == 0)){
          data[i,1] = rbeta(1, parameters_x[j,1], parameters_x[j,2])
          data[i,2] = rbeta(1, parameters_y[j,1], parameters_y[j,2])

          sampled = 1
        }
      }
    }

  }else if(model_name == "3_uniforms"){

    w = c(1/3, 1/3, 1/3)
    b1_x = c(0.1, 0.45)
    b1_y = c(0.35, 0.9)

    b2_x = c(0.2, 0.8)
    b2_y = c(0.45, 0.5)

    b3_x = c(0.7, 0.9)
    b3_y = c(0.05, 0.6)

    for(i in 1:n){
      u = runif(1)
      if(u < w[1]){
        x_temp = b1_x[1] + runif(1) * (b1_x[2] - b1_x[1])
        y_temp = b1_y[1] + runif(1) * (b1_y[2] - b1_y[1])
      }else if(u < w[1]+w[2]){
        x_temp = b2_x[1] + runif(1) * (b2_x[2] - b2_x[1])
        y_temp = b2_y[1] + runif(1) * (b2_y[2] - b2_y[1])
      }else{
        x_temp = b3_x[1] + runif(1) * (b3_x[2] - b3_x[1])
        y_temp = b3_y[1] + runif(1) * (b3_y[2] - b3_y[1])
      }

      data[i,1] = x_temp
      data[i,2] = y_temp
    }

  }

  return(data)
}
