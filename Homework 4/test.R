# fancy plot matrix with stuff, see http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
pairs.panels(Z_std[,1:4], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

summary(pcr1)

# Show the model fit
par(mfrow=c(2,2))
plot(fitted(pcr1), y_wine_quality, main="Wine Quality and PC 1")
plot(seq(1,11,by=1), pc_wine$rotation[,1])
plot(seq(1,11,by=1), pc_wine$rotation[,2])
plot(seq(1,11,by=1), pc_wine$rotation[,3])