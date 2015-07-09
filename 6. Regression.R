#SOURCE: http://www.statmethods.net/stats/regression.html
#*******************************MAIN PROGRAM*******************************#
#load("SantarosaAllPCA.Rda") #If you made the third or fourth step (3. PCA)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaAllPCA.Rda") #Absolute path from my computer
#load("SantarosaNormalized.Rda") #If you made the second or fourth step (2. NormalizedDataset)
load("D:/Dropbox/Marianela Iturriaga/data/SantarosaNormalized.Rda") #Absolute path from my computer

#Regression
PCA1to3<-as.data.frame(santarosa.pca$x[,1:3])
performance <- santarosaNormalized[,2154]

PC1 <- PCA1to3[,1]
PC2 <- PCA1to3[,2]
PC3 <- PCA1to3[,3]

fit <- lm(performance ~ PC1 + PC2 + PC3, data = PCA1to3)
summary(fit)

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

# Diagnostic plots usign ggplot
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

#Full screen
layout(matrix(c(1),2,2))
par("mar")
par(mar=c(1,1,1,1))

#dev.off()

#Residuals v/s Fitted Values
ggplot(fit, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
  geom_point() +
  geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
  xlab("Fitted Values") +
  ylab("Residuals")

#K-fold cross-validation
#install.packages("latticeExtra")
#install.packages("DAAG")
library(DAAG)
layout(matrix(c(1),2,2))
par(mar = c(5.1,4.1,4.1,2.1))
asd <- cv.lm(df = PCA1to3, fit, m=10) # 10 fold cross-validation


asd$performance - asd$Predicted

for (i in 1:nrow(asd)) {
  asd[i,4] - asd[i,5]
}


res <- y - (fit$coefficients[[3]]*PC3 + fit$coefficients[[3]]*PC2 + fit$coefficients[[2]]*PC1 + fit$coefficients[[1]])
res
layout(matrix(c(1),2,2))
par(mar=c(5.1,4.1,4.1,2.1))
plot(y,res)

plot(y,fit$residuals)

#When --> Error in plot.new() : figure margins too large
layout(matrix(c(1),2,2))
par("mar")
par(mar=c(1,1,1,1))











####Cross validation####
# Assessing R2 shrinkage using 10-Fold Cross-Validation 
install.packages("bootstrap")
library(bootstrap)
# define functions 
theta.fit <- function(x,y){
  lsfit(x,y)
}
theta.predict <- function(fit,x){
  cbind(1,x)%*%fit$coefficients
}

# matrix of predictors
X_x <- as.matrix(X[c("PC1","PC2","PC3")])
# vector of predicted values
y_y <- as.matrix(y)

results <- crossval(X_x,y_y,theta.fit,theta.predict,ngroup=798)
cor(y, fit$fitted.values)**2 # raw R2 
cor(y,results$cv.fit)**2 # cross-validated R2





#######Variable selection#######
# Stepwise Regression
install.packages("MASS")
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova # display results


# All Subsets Regression
install.packages("leaps")
library(leaps)
attach(X)
leaps<-regsubsets(y~PC1+PC2+PC3,data=X,nbest=3)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
install.packages("car")
library(car)
subsets(leaps, statistic="rsq")



#Relative importance
# Calculate Relative Importance for Each Predictor
install.packages("relaimpo")
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
