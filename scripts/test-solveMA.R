library(tidyverse)
library(glue)
library(Rcpp)
library(RcppArmadillo)

gh <- "~/Documents/Projects/urban-wage-premium"
dropbox <- "~/Dropbox/UrbanWagePremium" 


# Load Data 
Y <- read_csv(glue("{dropbox}/data/matlab/input/Y1940.csv"), col_names = FALSE)
Y <- as.matrix(Y)
Counties <- length(Y)
th <- 8
tau <- read_csv(glue("{dropbox}/data/matlab/input/tau1940cost1.csv"), col_names = FALSE)
tau <- as.matrix(tau)

# Load C++ function
Rcpp::sourceCpp(glue("{gh}/scripts/solveMA.cpp"))

# Load matlab results
ma <- read_csv(glue("{dropbox}/data/matlab/output/MA_cpp_test.csv"), col_names = c("fips", "ma_matlab"))
ma$ma_cpp <- SolveMA(Y, tau, th, Counties)

# Compare results 
plot(ma$ma_cpp, ma$ma_matlab)



