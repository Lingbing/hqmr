library(SpatioTemporal); library(reshape2); library(ggplot2)

data <- as.data.frame(Cut(hqmr.cube))
data$date <- date.montha
D <- data[, -79]
rownames(D) <- date.month
# creating obs
# melt the data to meet the needs of using calc.smooth.trends() in the SpatioTemporal pkg

# obs2 <- melt(data = data, id.vars = "date", variable.name = "ID", value.name = "obs")
# trend <- calc.smooth.trends(obs = obs2$obs, date=obs2$date, ID = obs2$ID)$svd
# 
# ggplot(trend, aes(date, V2)) + geom_line()

#compute cross-validation for 1 to 6 basis functions
res.cv <- SVD.smooth.cv(as.matrix(D[, sample(1:78, 78)]), 1:5, niter = 100)

#plot cross-validation statistics
par(mfcol=c(2,2),mar=c(4,4,.5,.5))
plot(res.cv$CV.stat$RMSE,type="l",ylab="RMSE")
plot(res.cv$CV.stat$R2,type="l",ylab="R2")
plot(res.cv$CV.stat$BIC,type="l",ylab="BIC")