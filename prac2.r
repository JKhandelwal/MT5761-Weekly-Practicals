EIA <- read.csv('EIA.csv')

EIA$impact<-as.factor(EIA$impact)
EIA$MonthOfYear<-as.factor(EIA$MonthOfYear)
EIA$Year <- as.factor(EIA$Year)
attach(EIA)

EIA

fit.poisSqrt<- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos +
Year:x.pos + Year:y.pos, data=EIA, family=poisson(link='sqrt'))

summary(fit.poisSqrt)


# 1. If the linear predictor for the fit.poisSqrt model is:
# ηit = β0 + β1x1it + ... + β26x26it
# and the coefficients are listed in the order of the output produced using the code above, which of the following describes the fit.poisSqrt model when the tide is ’Slack’, in Month 10 and Year 10? [1]
# (a) ηit = β0 +β2x2it +β3x3it +β4x4it +β13x13it +β16x16it +β19x19it +β20x20it +β21x21it +β24x24it
# (b) ηit = β0 + β1x1it + β2x2it + β3x3it + β4x4it + β5x5it + β6x6it + β7x7it + β8x8it + β9x9it +
# β10x10it + β11x11it + β12x12it + β13x13it + β16x16it + β19x19it + β20x20it + β21x21it + β24x24it
# (c) ηit = β0 + β1x1it + β2x2it + β3x3it + β4x4it + β5x5it + β6x6it + β7x7it + β8x8it + β9x9it + β10x10it + β11x11it + β12x12it + β13x13it + β14x14it + β15x15it + β16x16it + β17x17it + β18x18it + β19x19it + β20x20it + β21x21it + β22x22it + β23x23it + β24x24it + β25x25it + β26x26it
# (d) ηit = β0 +β1x1it +β2x2it +β4x4it +β13x13it +β16x16it +β19x19it +β20x20it +β21x21it +β24x24it
# (e) The correct answer is not provided as an option.

#  The answer is a

# 2. Which of the following BEST describes the fit.poisSqrt model? [1] (a) yit ∼ Poisson(λit = η2 )
# it
# (b) yit ∼ Poisson(λit = exp(ηit)areait)
# (c) yit ∼ Poisson(λit = η2 area2 ) it it
# (d) yit ∼ Poisson(λit = exp(η2 )) it
# (e) yit ∼ Poisson(λit = exp(ηit))
# A

fit.pois<- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos +
Year:x.pos + Year:y.pos, data=EIA, family=poisson)

# 3. If the linear predictor for the fit.pois model is:
# ηit = β0 + β1x1it + ... + β26x26it
# and the coefficients are listed in the order of the output produced using the code above, which of the following describes the fit.pois model when the tide is ’Flood’, in Month 1 and Year 12?[1]
# (a) ηit = β0 + β1x1it + β3x3it + β4x4it + β18x18it + β19x19it + β20x20it + β23x23it + β26x26it
# (b) ηit = β0 + β1x1it + β2x2it + β3x3it + β4x4it + β5x5it + β6x6it + β7x7it + β8x8it + β9x9it + β10x10it + β11x11it + β12x12it + β13x13it + β14x14it + β15x15it + β16x16it + β17x17it + β18x18it + β19x19it + β20x20it + β21x21it + β22x22it + β23x23it + β24x24it + β25x25it + β26x26it
# (c) The correct answer is not provided as an option.
# A is the right answer

summary(fit.pois)

#
# 4. Which of the following BEST describes the fit.pois model? [1]
# (a) yit ∼ Poisson(λit = η2 ) it
# (b) yit ∼ Poisson(λit = exp(ηit)areait) (c) yit ∼ Poisson(λit = η2 area2 )
# it it
# (d) yit ∼ Poisson(λit = exp(η2 )) it
# (e) yit ∼ Poisson(λit = exp(ηit))
# E

fit.poisOff <- glm(count ~ tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos +
Year:x.pos + Year:y.pos + offset(log(area)), data=EIA, family=poisson(link="log"))


library(car)

vif(fit.poisOff)

# 5. Which of the following about collinearity is FALSE? [1] (a) While there are no concerns about collinearity in this case, the standard errors for the year
#  coefficients are twice the size of what they would be if x.pos was not included.
# (b) The values in the GVIF(1/(2∗Df)) are used to quantify collinearity when there is
# one coefficient associated with one or more of the model covariates.
# (c) Fitting collinear covariates together in a model can result in unstable estimates standard errors.
# (d) Ignoring intolerable levels of collinearity in a model can result in one or more being excluded from the model due to large p-values.
# (e) One remedy for collinearity is to exclude one of the collinear covariates from the refitting the new model.
# A

AIC(fit.pois, fit.poisOff, fit.poisSqrt)


# 6. Which of the following about these AIC results is FALSE?
# 3
# more than with large covariates model and
# [1]
# (a) Comparing the AIC scores for the fit.pois and fit.poisOff models is not useful to determine which model is preferable.
# (b) In this case, the offset must be specified using the log() function owing to the log link function used.
# (c) We can use details about the survey design (and survey implementation) to help us decide if an offset term is required.
# (d) If the survey effort is uneven and we fail to include this information in the model, then we may draw false conclusions about model covariates.
# (e) An equivalent alternative to including an offset term in a model is to include the effort covariate in the model, to account for uneven survey effort.
# E

step.poisOff <- step(fit.poisOff, direction="both")
step.poisOff_BIC <- step(fit.poisOff, direction="both", k=log(nrow(EIA)))

summary(step.poisOff)
as.formula(step.poisOff_BIC)


# 7. Carry out automated stepwise selection on the fit.poisOff governed by the AIC and BIC criteria: call these new models step.poisOff and step.poisOff BIC respectively (as before set direction = "both"). Based on these stepwise-selection results, which of the following is FALSE? [1]
# (a) The model selection results are the same regardless of whether the AIC and BIC criteria are used.
# (b) The model chosen using the AIC score suggests that the relationship between the response and the x-coordinate changes with year.
# (c) The model chosen using the BIC suggests the cost of including the DayOfMonth covariate outweighs the benefits of doing so.
# (d) There are three coefficients allocated to the tidestate covariate because there are four categories for tidestate.
# (e) The model chosen using the AIC criteria assumes the relationship between the y-coordinate and the response is nonlinear in nature.
# D
Anova(step.poisOff_BIC)


# 8. Perform likelihood ratio test results using the Anova function for the step.poisOff BIC model. Based on these results, which of the following is FALSE? [1]
# (a) The p-value associated with the interaction term, Year:x.pos is calculated by comparing the test statistic of 73.8 with a reference χ2 distribution with df = 3.
# (b) Each p-value is based on comparisons between the likelihood values for a model with and without each covariate separately, while retaining all other covariates in the model.
# (c) While the (default) anova function always returns the same results as the (default) Anova function, we are using the latter here because it automatically returns p-values.
# (d) The Anova results suggests that all model terms should be retained in the model, regardless of whether the 5% or 1% level is used to decide covariate retention.
# 4
#  (e) The Df column represents the number of coefficients associated with each model term.
# C

require(MuMIn)
options(na.action='na.fail')
dredge_v <- dredge(step.poisOff_BIC)
print(head(dredge_v))

print(head(dredge_v))

dredge_v[100]

# 9. Carry out all-possible-subsets selection on the step.poisOff BIC model using the dredge func-
# tion and the default ranking criteria. Based on these results, which of the following is FALSE?[1]
# (a) The model ranked the highest in this case has a very similar AICc score to the second ranking model resulting in a very similar model weight.
# (b) We could use the model weights to ‘model-average’ which would result in model predictions which are a weighted average in line with the model weights.
# (c) While this function suggests no terms should be dropped from the BIC-selected model, this function returns output which tells us how other candidate models compare with the highest ranked model in this case.
# (d) For small sample sizes, the AICc might suggest a different model is preferred compared with results obtained using the AIC.
# (e) This function investigates the fit of all possible models while the stepwise selection function does not necessarily consider all candidate models.
# A

summary(step.poisOff_BIC)


# 10. Based on the step.poisOff BIC model results, which of the following is FALSE? [1]
# (a) There is no significant difference (at the 1% level) between average numbers (per unit area)
# in an EBB or FLOOD tide state.
# (b) There is no significant difference (at the 5% level) between average numbers (per unit area) in month 1 and months 8, 10, 11 or 12. Average numbers (per unit area) in all the other months are significantly different to average values in month 1.
# (c) While average numbers (per unit area) are significantly fewer in years 11 & 12 compared with year 9, there is no evidence for a different in average numbers in years 9 and 10.
# (d) The relationship between the x-coordinate and average numbers (per unit area) is signifi- cantly steeper in years 10 & 12 compared with the x-coordinate relationship in year 9.
# (e) The relationship between the y-coordinate and average numbers (per unit area) is signif- icantly steeper (at 5% level) in year 10 compared with the y-coordinate relationship in year 9, but significantly shallower (at 5% level) in year 12 compared with the y-coordinate relationship in year 9.
# D

as.formula(step.poisOff_BIC)

str(EIA)
newdat_month_1 <- data.frame("tidestate" = as.factor("EBB"), "observationhour"=10,"MonthOfYear" = as.factor(1), "Year"=as.factor(11), x.pos=-2061, y.pos=-1158, area=mean(EIA$area))

myprediction_month_1 <- predict(step.poisOff_BIC, newdat_month_1, type="response")
myprediction_month_1


# 11. Based on the step.poisOff BIC model, what is the predicted value on the scale of the response,
# when tidestate=EBB, observationhour=10, month=1, year=11, x.pos=-2061, y.pos=-1158
# and the area of the cell is the mean of the area of all cells in the EIA data set?
# Report your answer to 3 decimal places. [1]
# 1.11

newdat_month_5 <- data.frame("tidestate" = as.factor("EBB"), "observationhour"=10,"MonthOfYear" = as.factor(5), "Year"=as.factor(11), x.pos=-2061, y.pos=-1158, area=mean(EIA$area))
myprediction_month_5 <- predict(step.poisOff_BIC, newdat_month_5, type="response")
myprediction_month5
myprediction_month_5/ myprediction_month_1

summary(step.poisOff_BIC)

exp(-2.087e-01)
# 12. Based on the step.poisOff BIC model, what is the ratio of the predicted numbers in month 5
# compared to predicted numbers in month 1? Report your answer to 3 decimal places. [1]
# -0.965

# 2.087e-01

step.poisOffOD <- update(step.poisOff_BIC, .~. ,family=quasipoisson)
summary(step.poisOffOD)
# 13. Check for overdispersion in the step.poisOff BIC model using family=quasipoisson.
# Call this new model step.poisOffOD. Based on these results, what is the estimated dispersion parameter?
# Report your result to one decimal place. [1]
# 15.1


# 14. Compare the p-values with the overdispersed model with the p-values obtained under a strictly Poisson model (when the dispersion parameter=1). Which of the following is FALSE? [1]
# (a) The strictly Poisson model and the overdispersed model return identical model coefficients, only the standard errors about these parameter estimates differ.
# (b) Inallcases,thestandarderrorsarelargerundertheoverdispersedmodelsincethedispersion parameter is estimated to be larger than 1.
# (c) A likelihood ratio test can be performed using the overdispersed model in the same way it is performed using the strictly Poisson model.
# (d) Larger standard errors result in larger p-values and thus ignoring overdispersion when it is present can result in model covariates being retained when there is no genuine relationship with the response.
# (e) Based on the p-values in the overdispersed model, both interaction terms would be dropped from the step.poisOff BIC model (at 5 % significance level).
# b



# 15. Based on the overdispersed results, is the following statement TRUE or FALSE? [1]
# While there is compelling evidence that average numbers (per unit area) change with the
# x- coordinate and y-coordinate, there is no evidence that either of these relationships
# change with year.
# TRUE

final <- update(step.poisOffOD, .~. -Year:x.pos -Year:y.pos)
summary(final)

library("car")

residualPlots(final)


# 16. Using the residualPlots function in the car library in R, which of the following statements about linearity is TRUE? [1]
# (a) The relationship of each covariate with the response is linear in a Poisson-based model.
# (b) The residualPlots function output tells us that we should remove tidestate from the model.
# (c) The last plot is most appropriate for assessing whether the mean-variance relationship is modelled appropriately.
# (d) observationhour has evidence of non-linearity on the link scale.
# D

plot(EIA$count, fitted(final))

# 17. Make a plot of observed vs fitted. Which of the following statements is FALSE? [1]
# (a) Observed counts greater than 5 are severely under predicted.
# (b) A good fitting model should show scatter about the 45o line
# (c) There are negative fitted values.
# (d) The range of observed counts is larger than the range of predicted counts.
# C

plot(fitted(final), residuals(final, type='pearson'))

# 18. Plot fitted values vs scaled residuals to assess the mean-variance relationship. Which of the following statements is FALSE? [1]
# (a) There is a known linear mean-variance relationship for a Poisson-based model.
# (b) There should be pattern in a plot of fitted vs scaled residuals for the mean-variance rela- tionship of an overdispersed Poisson-based model to hold.
# (c) It is difficult to tell from this plot whether the mean-variance relationship is appropriately modelled. Binning the data may help here.
# 7
#
#  (d) The variance of the final model increases at a rate approximately fifteen times faster than the mean.




par(mfrow=c(1,2))
acf(residuals(final, type='pearson'))
acf(residuals(final, type='normalized'))

# 19. Use acf plots to determine the nature of any scaled residual correlation present. Which of the following statements is FALSE? [1]
# (a) The first data point is correlated with the 41st point
# (b) There is correlation through time within gridcodes
# (c) The acf plot indicates we should remove every 41st data point to deal with any correlation present.
# (d) If we re-order the data by gridcode, we may see a different pattern in the acf plot.
# C

summary(final)

# 20. Which of the following about summarising our model is FALSE? [1]
# (a) The final model identified that there was a decline in animal density during the study period.
# (b) There was no spatially explicit decline identified
# (c) There was indication that some covariates are inappropriately modelled (e.g. should be tried as non-linear terms.)
# (d) If there is non-independence in the model residuals, the current p-values are likely to be too small, which may lead to one or more covariates being kept in the model that should not be.
# (e) We cannot use a GLS model with Poisson errors so therefore this is the best model we can achieve.
