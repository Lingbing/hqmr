library(SpatioTemporal); library(reshape2); library(ggplot2)

data <- as.data.frame(Cut(hqmr.cube))
data$date <- date.month
D <- data[, -79]
rownames(D) <- date.month

# compute cross-validation for 1 to 6 basis functions
res.cv <- SVD.smooth.cv(as.matrix(D[348:1200,]), 1:5, niter = 100)

# plot cross-validation statistics
par(mfcol=c(2,2), mar=c(4, 4, .5, .5))
plot(res.cv$CV.stat$RMSE,type="l", ylab="RMSE")
plot(res.cv$CV.stat$R2,type="l", ylab="R2")
plot(res.cv$CV.stat$BIC,type="l", ylab="BIC")
# we have found two basis functions, by cutting the beginning part of the data
timerange <- 349:1200
pdata <- D[timerange,] # part data
pdata$date <- date.month[timerange]
pdata <- melt(data = pdata, id.vars = "date", variable.name = "ID", value.name = "obs")

## computetemporal smooths
F <- calc.smooth.trends(obs = pdata$obs, date = pdata$date, ID = pdata$ID)
trend <- F$svd
ggplot(trend, aes(date, V1)) + geom_line() + geom_line(aes(date, V2), col = 2, lty = 2)

## create data matrix
Cr <- create.data.matrix(obs = pdata$obs, date = pdata$date, ID = pdata$ID)
beta <- matrix(NA, dim(Cr)[2], dim(trend)[2])
beta.std <- beta
## extract the temporal trends
trend$date <- NULL # drop the date column
for(i in seq_len(dim(Cr)[2])){
  tmp <- summary(lm(Cr[, i] ~ as.matrix(trend)))
  beta[i, ] <- tmp$coefficients[, 1]
  beta.std[i, ] <- tmp$coefficients[, 2]
}
colnames(beta) <- c("const", colnames(trend))
rownames(beta) <- colnames(Cr)
dimnames(beta.std) <- dimnames(beta)

head(beta); head(beta.std)

# 
our.data.model <- create.data.model(our.data, LUR = NA)


## create a vector divding data into four seasons
I.season <- matrix(NA, length(date.month), 1)
I.season[months(date.month) %in% c("December", "January", "February"), 1] <- "DJF"
I.season[months(date.month) %in% c("March", "April", "May"), 1] <- "MAM"
I.season[months(date.month) %in% c("June", "July", "August"), 1] <- "JJA"
I.season[months(date.month) %in% c("September", "October", "November"), 1] <- "SON"
I.season <- factor(I.season, levels = c("DJF", "MAM", "JJA", "SON"))