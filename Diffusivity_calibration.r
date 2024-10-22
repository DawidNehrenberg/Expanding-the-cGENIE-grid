##libraries#####################################################################
 library(tidyverse)
 library(httpgd)
 hgd()
##Functions#####################################################################
 diff_param <- function(k_scalar = "", dt = "", x = ""){
     #k*dt/x^2
     result <- matrix(ncol = 3, nrow = length(k_scalar))
     for(i in seq_along(k_scalar)){
     k <- 1494.4383544921875 * k_scalar[i]
     r <- ((k * dt) / x^2)
     result[i,] <- c(r, k, k_scalar[i])
     }
     colnames(result) <- c("r", "k", "k_scalar")
     result <- as.data.frame(result)
     return(result)
  }
##Horizontal Diffusivity values#################################################
 #36x36
  #timestep in s = 65745
  k <- 1494.4383544921875
  diff_param(k_scalar = 1, dt = 65745, x = (1111 * 1000))
  #r = 7.9599e-5
 #48x48
  #lower bound given by
  #(36 * 36 * 16) / (48 * 48 * 16) = 0.5625
  #upper bound given by
  #(48 * 48 * 16) / (36 * 36 * 16) = 1.7777
  k_range <- seq(from = 0.56, to = 1.77, by = 0.05)
  diff_48 <- diff_param(k_scalar = k_range, dt = 65745, x = (832.5 * 1000))
  ggplot(diff_48, aes(x = k_scalar, y = r)) +
    geom_point() +
    geom_hline(yintercept = 7.9599e-5) +
    theme_bw()
  diff_param_48 <- seq.int(from = 0.5625, to = 1.78, length.out = 20)
  horizontal_diff_48 <- diff_param_48 * k
 #72x72
  #lower bound given by
  #(36 * 36 * 16) / (72 * 72 * 16) = 0.25
  #upper bound given by
  #(72 * 72 * 16) / (36 * 36 * 16) = 4
  k_range <- seq(from = 0.25, to = 4, by = 0.05)
  #change dt and x values
  diff_72 <- diff_param(k_scalar = k_range, dt = 65745, x = (832.5 * 1000))
    ggplot(diff_72, aes(x = k_scalar, y = r)) +
    geom_point() +
    geom_hline(yintercept = 7.9599e-5) +
    theme_bw()
  horizontal_diff_72 <- diff_param_72 * k