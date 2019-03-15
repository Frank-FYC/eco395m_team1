#####
#knn attempt
rmse = function(y, ypred) {sqrt(mean((y-ypred)^2))}

k_grid = unique(round(exp(seq(log(n_train), log(2), length=100))))
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  
  knn_model = knn.reg(df_train, df_test, y_df_train, k = k)
  rmse(y_df_test, knn_model$pred)
}

rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)


revlog_trans <- function(base = exp(1)) {
  
  ## Define the desired transformation.
  trans <- function(x){
    -log(x, base)
  }
  ## Define the reverse of the desired transformation
  inv <- function(x){
    base^(-x)
  }
  ## Creates the transformation
  scales::trans_new(paste("revlog-", base, sep = ""),
                    trans,
                    inv,  ## The reverse of the transformation
                    log_breaks(base = base), ## default way to define the scale breaks
                    domain = c(1e-100, Inf) 
  )
}

p_out = ggplot(data=rmse_grid_out) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=1.5) + 
  xlim(0,100)

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

p_out + geom_vline(xintercept=k_best, color='lightblue', size=1)

# fit the model at the optimal k
knn_model = knn.reg(df_train, df_test, y_df_train, k = k_best)
rmse_best = rmse(y_df_test, knn_model$pred)

p_out + geom_path(data=knn_model, mapping = aes(x=KHOU, y=ypred), color='red', size=1.5)



