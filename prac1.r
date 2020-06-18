EIA <- read.csv('EIA.csv')

EIA$density<-EIA$count/EIA$area
attach(EIA)


# first group the data by gridcodes and find the mean density for each cell
require("dplyr")
library("ggplot2")
newdata <- group_by(EIA, GridCode) %>% summarise(x.pos=first(x.pos), y.pos=first(y.pos), area=first(area), density=mean(density))
# pick a nice colour scheme
col<-colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),max=255)))(100)
# plot the data
# png("first.png")
p<-ggplot(newdata)
p <- p + geom_tile(aes(x=x.pos, y=y.pos, fill=density, height=1000, width=1000)) + scale_fill_gradientn(colours=col, space="Lab", na.value="grey50", guide="colourbar")
p + theme_bw() + coord_equal()

# 1. Which of the following statements relating to factor variables is FALSE? [1]
# a) We don’t need to assume linearity between factor variables and the response
# b) A different coefficient is estimated for each (non-baseline) factor level
# c) We can predict between factor levels -> Cannot predict between factor levels i.e. months 1-4 cannot predict month 1.5
# d) More coefficients are estimated when we use factors


# month as continuous
fit.full<- lm(density ~ tidestate + observationhour + DayOfMonth + MonthOfYear + impact + Year + x.pos + y.pos, data=EIA)
summary(fit.full)

# month as a factor
fit.full.fac <- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + impact + Year +
x.pos + y.pos, data=EIA)
summary(fit.full.fac)

# 2. How many coefficients does changing month to a factor add to the model? [1]
#  It adds 10

res <- AIC(fit.full, fit.full.fac)
print(res)

library("car")
vif_res <- vif(fit.full.fac)
print(vif_res)

library("pedometrics")
step_vif_res <- stepVIF(fit.full.fac)
print(step_vif_res)

#
# 3. Which of the following about collinearity is FALSE? [1]
# a) A confidence interval for the Year coefficient is over 48 times wider than it would be for a model with no collinear variables. -> False, conf interval is 4 root(48) times wider
# b) The GVIF is equivalent to the VIF but adjusted for multiple covariates.
# c) It is appropriate to assess models with and without the collinear variables and use AIC score to choose the
# best model. In this instance the preferred model is one with impact removed.
# d) The ‘stepVIF‘ function identifies two collinear variables and uses R-squared to determine which variable to drop. In this case Year is dropped from the model.
# e) Pairwise comparisons between covariates, such as scatter plots and covariance values may be used to assess collinearity prior to modelling.

as.formula(fit.full.fac)
new <- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + Year + x.pos + y.pos, data=EIA)

fit.interac <- update(new, . ~ . + Year:y.pos + Year:x.pos)
summary(fit.interac)


Anova(fit.interac)
#
# 4. True or False? The null hypothesis for the F-test is that a model with a particular covariate included is no better than a model with that covariate removed.
# In this case, all variables except DayOfMonth and Year:y.pos appear to have significant relationships (at the 5% level) with density. [1]
# True I think?

best <- step(fit.interac, direction="both")
summary(best)

require(MuMIn)
options(na.action='na.fail')
dredge <- dredge(fit.interac)
print(head(dredge))

BIC_step <- step(fit.interac, direction="both", criterion = "BIC")
as.formula(BIC_step)

back <- step(fit.interac, direction="backward")
as.formula(back)
as.formula(best)


# 5. Which of the following about model selection is FALSE? [1]
# a) The best stepwise model is the same as that from all possible subsets
# b) The model with ‘y.pos:Year‘ included is within 2 AIC points of the model with it not included and the
# weights of the two models are almost equal (∼25% weight compared with ∼25% weight).
# c) The best all possible subsets model contains only the observation hour, x.pos, y.pos and year covariates.
# d) Owing to the large sample size, it makes no difference to covariate selection whether we do all possible
# subsets selection using AIC or AICc.

# A or C -> Check

# 6. True or False? Using BIC for stepwise selection does not change the covariates selected in the final model. -> No it does not

Anova(fit.interac)
Anova(best)
Anova(fit.full)
Anova(get.models(dredge, subset = 1)[[1]])


# 7. Which of the following statements about model selection is FALSE? [1]
# a) Hypothesis testing (F-test) suggests that the model without the year:y.pos interaction is preferred to the full model.
#   TRUE?
# b) Dredge, using AICc, suggests that there is little to choose between a model a) without either interaction term, b) with both interaction terms and c) with only the x.pos interaction term retained.
#   TRUE
# c) Forwards and backwards stepwise selection using AIC chooses a model with the year:x.pos interaction term retained.
#   TRUE
# d) Forwards and backwards stepwise selection using BIC returns the same model as backwards selection using hypothesis testing.
#   False?

# 8. Which of the following is most suitable for assessing the fit of this model? [1]
# a) Median Residual
# b) p-value -> P value of what? Surely this is the issue
# c) Adjusted R-Squared
# d) F-statistic
# e) Multiple R-Squared
# f ) Residual Standard Error

# Variance of Residuals
summary(best)
(summary(best)$sigma)^2
# Variance of Slope
vcov(best)

# 9. What is the estimate of the error variance for this model? Give your answer to two decimal places. [1]
# 27.17


# 10. Which of the following statements about assumption tests is FALSE? [1]
# a) The null hypothesis for ‘shapiro.test‘ is that the errors are normally distributed
# b) The null hypothesis for ‘ncvTest‘ is that there is non-constant error variance
  # False
# c) The null hypothesis for ‘durbinWatsonTest‘ is that the errors are independent

ncvTest(best)
ks.test(resid(best), y= pnorm)
# 11. Which of the following, about the validity of assumptions for the AIC-based stepwise selection model with interaction terms is TRUE? [1]
# a) Independence is probably not reasonable given the sampling design and the result of the independence assumption test. There is evidence of non-constant error variance (very small p-value) and non-normality
# (right skewed histogram)
  # TRUE
# b) Independence is probably reasonable given the sampling design and the result of the independence
# assumption test. There is evidence of non-constant error variance (very small p-value) and non-normality
# (right skewed histogram)
# c) Independence is probably not reasonable given the sampling design and the result of the independence
# assumption test. There is evidence of constant error variance (very small p-value) and non-normality (right skewed histogram)


require(nlme)
EIA$sqrtdensity<-sqrt(density)
fit.gls.pwr <-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, method='ML', weights=varPower())

fit.gls.exp <-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, method='ML', weights=varExp())

as.formula(fit.gls.exp)


# 12. True or False? The exponential model is a better representation of the error variance than a constant. [1]
# True

plot(fitted(fit.gls.exp), residuals(fit.gls.exp, type='response'))
png("meanvar.png")
cut.fit<-cut(fitted(fit.gls.exp), breaks=quantile(fitted(fit.gls.exp), probs=seq(0,1,length=20)))
means1<- tapply(fitted(fit.gls.exp), cut.fit, mean)
var_1<- tapply(residuals(fit.gls.exp), cut.fit, var)
points <- ((0.523664^2)*exp(2*1.5438*fitted(fit.gls.exp)))
plot(means1, var_1, main="Mean Variance Plot",
        xlab="Fitted Means",
        ylab="Variance of Residuals")
points(fitted(fit.gls.exp), points, pch='.')
dev.off()
summary(fit.gls.exp)
# 13. TODO Save your mean-variance plot and upload to Moodle. Use sensible axis labels and give your plot a title. Your file should be one of jpeg, png or pdf.


# 14. Which of the following about the mean-variance relationship is FALSE?
# a) The exponential model slightly underestimates the variance for the smaller predicted root-density values
# b) The exponential model severely overestimates the variance for the highest predicted root-density values
# c) The residual variance suggests that a variance term that is allowed to both increase and decrease might be preferred.



par(mfrow=c(1,2))
acf(residuals(fit.gls.exp, type='response'))
acf(residuals(fit.gls.exp, type='normalized'))
par(mfrow=c(1,1))


EIA$block<-paste(Year, MonthOfYear, DayOfMonth, GridCode, sep='')
require(dplyr)
EIA2<-arrange(EIA, block, Year, MonthOfYear, DayOfMonth, GridCode)

fit.gls.exp.corr.ac1 <-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
MonthOfYear + impact:x.pos + impact:y.pos, data = EIA2, method='ML', weights=varExp(), correlation=corAR1(form =~1|GridCode/DayOfMonth))

as.formula(fit.gls.exp.corr.ac1)

par(mfrow=c(1,2))
acf(residuals(fit.gls.exp.corr.ac1, type='response'))
acf(residuals(fit.gls.exp.corr.ac1, type='normalized'))

a <- ?residuals


par(mfrow=c(1,1))

fit.gls.exp.corr.ac2 <- update(fit.gls.exp.corr.ac1, corr = corARMA(p = 2, q = 0, form = ~ 1 | GridCode/DayOfMonth))

par(mfrow=c(1,2))
acf(residuals(fit.gls.exp.corr.ac2, type='response'))
acf(residuals(fit.gls.exp.corr.ac2, type='normalized'))
par(mfrow=c(1,1))

AIC(fit.gls.exp.corr.ac2, fit.gls.exp.corr.ac1)

# 15. Which of the following about GLS models is FALSE? [1]
# a) The AR(2) model is the best model for the errors since the AIC score is the lowest
# b) We cannot use the AIC to choose between models with different covariates unless the models are fitted
# using maximum likelihood
# c) The normalized residual acf plot for the AR(2) confirms the AIC result, that the AR(2) model fits better
# the correlation in the residuals than the AR(1) model.
# d) The AR(2) model reduces the correlation at most lags to near zero and so has dealt with the issues
# regarding correlation in model residuals. We can expect that the standard errors for the estimated coefficients have been reduced accordingly and we can trust any model selection results that use hypothesis testing.
# a/d

Anova(fit.gls.exp.corr.ac2)


fit.gls.exp.corr.ac1 <-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
MonthOfYear + impact:x.pos + impact:y.pos, data = EIA2, method='ML', weights=varExp(), correlation=corAR1(form =~1|block))

fit.gls.exp.corr.ac2 <- update(fit.gls.exp.corr.ac1, corr = corARMA(p = 2, q = 0, form = ~ 1 | GridCode/block))

final <- update(fit.gls.exp.corr.ac2, .~. -y.pos -MonthOfYear -impact:x.pos -impact:y.pos)
as.formula(final)

head(EIA2)


# 16. After backwards selection using hypothesis testing, select all the variables that remain in your model. [1]
# a) tidestate
# b) observation hour
# c) month of the year NO
# d) x-position
# e) y-position NO
# f) impact
# g) x-position:impact NO
# h) y-position:impact NO
newdat_1 <- data.frame("tidestate" = as.factor("SLACK"), "observationhour" = 10, "MonthOfYear" = 6, x.pos=1500, y.pos=1000, impact=0)

myprediction_1<-MuMIn:::predict.gls(final, newdata = newdat_1, se.fit=TRUE)
myprediction_1$fit ^ 2

# 17. Make a prediction from your best model for both before and after impact using the
# relevant covariate values given below. Give your answers in density and to 2 decimal places. [1]
# 0.80
interval_1 <- c(myprediction_1$fit^2 - myprediction_1$se.fit^2 * 1.96, myprediction_1$fit^2 + myprediction_1$se.fit^2 * 1.96)
interval_1

# 18. Calculate a 95% confidence interval for the before impact prediction from the previous question.
# Please give each of your answers as a density and to two decimal places. [1]
# 0.72 0.87


newdat_2 <- data.frame("tidestate" = as.factor("SLACK"), "observationhour" = 10, "MonthOfYear" = 6, x.pos=1500, y.pos=1000, impact=1)
myprediction_2<-MuMIn:::predict.gls(final, newdata = newdat_2, se.fit=TRUE)
myprediction_2$fit^2

interval_2 <- c(myprediction_2$fit^2 - myprediction_2$se.fit^2 * 1.96, myprediction_2$fit^2 + myprediction_2$se.fit^2 * 1.96)
interval_2

# 19. Calculate a 95% confidence interval for the after impact prediction.
# Please give each of your answers as a density and to two decimal places. [1]
# 0.55 0.68

fit.gls.exp.corr.ac1_new <-update(fit.gls.exp, correlation=corAR1(form =~1|GridCode/DayOfMonth))
as.formula(fit.gls.exp.corr.ac1_new)

Anova(fit.gls.exp.corr.ac1_new)


# 20.Which of the following about summarising the models you have fitted is FALSE? [1]
# The best linear model underestimated the variance and found significant covariates, owing to unaccounted for residual correlation and a poor model of the mean-variance relationship.
# The addition of a more appropriate mean-variance relationship (exponential model) using a GLS model found that month of the year was no longer significant.
# The inclusion of a model for the correlation in residuals removed several covariates, which would otherwise lead a researcher to come to the wrong conclusions.
# false
# It is unlikely that there are any further improvements to be made to this model now that all the assumptions are met.
