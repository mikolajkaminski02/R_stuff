#installing packages
install.packages("dplyr")
library(dplyr)

#reading file
wine <- winequality_red
wine

#huj


#checking variables
str(wine)
head(wine)
dim(wine)
glimpse(wine)
summary(wine)

#graphic represtentation of variable "quality"
hist(wine$quality)

#also checking the variabe "quality"
mean(wine$quality)
mode(wine$quality)
summary(wine$quality)


wine_sq <- sqrt(wine$quality)
hist(wine_sq)

#instaling packages for checking data distribution
install.packages("fitdistrplus")
library(fitdistrplus)

#checking data distribution
par(mar = c(1,1,1,1))
descdist(wine$quality, discrete = FALSE)

#normality test
ks.test(wine$quality, "pnorm", mean = mean(wine$quality),
        sd = sd(wine$quality))
# p_value < 0,05

#LINEAR REGRESSION
linear_reg_for_quality = lm(quality ~ fixed_acidity+volatile_acidity+
                              citric_acid
                    +residual_sugar+chlorides+free_sulfur_dioxide+
                      total_sulfur_dioxide+density+pH+sulphates+
                      alcohol, data=wine) 
summary(linear_reg_for_quality)

#Testing for linearity
plot(linear_reg_for_quality, 1)
install.packages("car")
library(car)
durbinWatsonTest(linear_reg_for_quality)
#Value 0.121 shows that we have positive autocorealtion

#Creating correlation matrix

wine.cor = cor(wine)
wine.cor = cor(wine, method = c("spearman"))
install.packages("Hmisc")
library("Hmisc")
wine.rcorr = rcorr(as.matrix(wine))
wine.coeff =wine.rcorr$r
wine.p = wine.rcorr$Pine.rcorr

#checking coefficients between variables
install.packages("corrplot")
library(corrplot)

corrplot(wine.cor)

#creating lm for quality and alcohol 
lin_qua_alc = lm(quality ~ alcohol, data = wine)
summary(lin_qua_alc)
plot(lin_qua_alc)

#creating logistic model for most impactfull variables

linear_reg_for_quality_only_four = lm(quality ~ alcohol + volatile_acidity +
                                        citric_acid + sulphates, 
                                      data = wine)
summary(linear_reg_for_quality_only_four)
plot(linear_reg_for_quality_only_four)

#checking assumptions on anova test

anova(linear_reg_for_quality_only_four)
#so citric acid has the highest P so it is rejected
#conducting analysis with only 3 variables that are the
#most significant for the model

final_linear_model_quality = lm(quality ~ alcohol + volatile_acidity + 
                                  sulphates, data = wine)=
summary(final_linear_model_quality)
plot(final_linear_model_quality)

#conducting anova to check 

anova(final_linear_model_quality)

#all variables are significant for this model
#conducting performance test

glance(final_linear_model_quality) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

#different approach using GLM

#recasting dependent variable Quality into categorical dependent variable

hist(wine$quality)
summary(wine$quality)
sort(wine$quality)
wine <- within(wine, {
  quality.cat <- NA
  quality.cat[quality < 6] <- "Poor"
  quality.cat[quality >= 6]<- "Excelent"
})

str(wine)
summary(wine$quality)
wine$quality.cat <- factor(wine$quality.cat, levels = c("Excelent", 
                                                        "Poor"))
str(wine)
summary(wine$quality.cat)

fit_full <- glm(quality.cat~alcohol,
                data = wine, family = binomial())
summary(fit_full)
reduced_model_alc <- glm(quality.cat~alcohol,
                     data = wine, 
                     family = binomial())
reduced_model_acid <- glm(quality.cat~volatile_acidity,
                     data = wine, 
                     family = binomial())
reduced_model_sulph <- glm(quality.cat~sulphates,
                     data = wine, 
                     family = binomial())
summary(reduced_model) 
anova(reduced_model_acid)
anova(reduced_model_alc)
anova(reduced_model_sulph)

exp(coef(reduced_model_alc))
exp(coef(reduced_model_sulph))
exp(coef(reduced_model_acid))


