# Load packages

library(VGAM)
library(sjPlot)
library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(broom)
library(parameters)
library(webshot)
library(report)
library(ggpubr)
library(olsrr)

# Read data

final_df<- read.csv("final_data/final_df.csv")

##Testing the pre-registered main hypothesis with primary DV
lm_model <- lm(learning_time ~ scale(pep_effect) + scale(self_efficacy), data = final_df)
summary(lm_model)

#Reporting it
report(lm_model)

##Testing the pre-registered main hypothesis with secondary DV
lm_model_num <- lm(learning_num ~ scale(pep_effect) + scale(self_efficacy), data = final_df)
summary(lm_model_num)

#Reporting it
report(lm_model_num)

#Exploratory analysis, primary DV: adding explicit score to the model as well
lm_model_explicit <- lm(learning_time ~ scale(self_efficacy) +  scale(iqms_avg) + scale(pep_effect), data = final_df)
summary(lm_model_explicit)

#Reporting it
report(lm_model_explicit)


#Exploratory analysis, secondary DV: adding explicit score to the model as well
lm_model_explicit_num <- lm(learning_num ~ scale(self_efficacy) +  scale(iqms_avg) + scale(pep_effect), data = final_df)
summary(lm_model_explicit_num)

#Reporting it
report(lm_model_explicit_num)


#get model results in .doc file
tab_model(lm_model_explicit,
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/lm_table_time.doc")


tab_model(lm_model_explicit_num,
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/lm_table_item.doc")



#let's create a plot
plot_model(lm_model, type = "pred", terms = c("pep_effect"), show.data = TRUE)


#Check assumptions of first model with time DV
check_model(lm_model)
#The posterior predictive check of the model shows that the LR model is not adequate
#Testing the normality of residuals with Kolmogorov-Smirnov test
ols_test_normality(lm_model)

#It's not normally distributed

#This is the case because the distribution of the outcome is positively skewed, bounded at zero.

qplot(final_df$learning_time)


# Therefore, we created a (square-root) transformed variable to make it normally distributed
qplot(final_df$learning_sqrt)

# Here, we can see the transformation does not help with the zeros, therefore, we need to use a two-step model
# first, a logistic regression to predict if the implicit score explains any variance in predicting if someone decided to view any solutions, 
#then a linear regression among those who viewed at least one solution. The linear regression is the one that is important for us. 

# Binomial model
binary_model <- glm(binary_learning ~ scale(pep_effect) + scale(self_efficacy),
                    family = "binomial",
                    data = final_df)
summary(binary_model)


# Keeping only cases when people looked at at least 1 solution
reduced<- final_df %>% 
  filter(binary_learning == 1) 


# Linear model with transformed score
lm_model_transformed <- lm(learning_sqrt ~ scale(pep_effect) + scale(self_efficacy), data = reduced)
summary(lm_model_transformed)

tab_model(lm_model_transformed, 
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE)


# Including the explicit score in the model as well
binary_model_explicit <- glm(binary_learning ~ scale(self_efficacy) + scale(iqms_avg) + scale(pep_effect),
                             family = "binomial",
                             data = final_df)
summary(binary_model_explicit)



tab_model(binary_model_explicit, 
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/binary_model_explicit.doc")

# Including the explicit score in the linear model as well

lm_model_transformed_explicit <- lm(learning_sqrt ~ scale(self_efficacy) + scale(iqms_avg) + scale(pep_effect), data = reduced)
summary(lm_model_transformed_explicit)


tab_model(lm_model_explicit, 
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/lm_model_explicit.doc")

#The two-step model shows that the implicit score is predictive of learning time in the linear regression
#and not in the binomial model. The explicit score shows only tendency in the linear regression and 
#it is not predictive in the binomial model


##Testing assumptions

check_model(lm_model_explicit)

#Assumptions are fine now



#let's create a black and white plot 
linear_plot_i<-
  plot_model(lm_model_transformed, type= "pred", terms = "pep_effect[all]", show.data = TRUE,
           title = "", line.size = NULL) +
  labs(y = "Time spent on viewing the solutions",
       x = "Implicit score of growth mindset") +
  scale_y_continuous(labels=c("100 ms", "400 ms"), breaks = c(10, 20)) +
  theme_classic()


#save plot
png(file="plots/linear_plot_i.png",
    width=800, height=611, res = 150)
plot(linear_plot_i)
dev.off()


##Hierarhical regression first time-based variable

lm_model_1 <- lm(scale(learning_sqrt) ~  scale(self_efficacy), data = reduced)
summary(lm_model_1)
lm_model_2 <- lm(scale(learning_sqrt) ~  scale(self_efficacy) + scale(pep_effect), data = reduced)
lm_model_3 <- lm(scale(learning_sqrt) ~  scale(self_efficacy) + scale(pep_effect) + scale(iqms_avg), data = reduced)

anova(lm_model_1, lm_model_2, lm_model_3)
anova(lm_model_1, lm_model_3)

summary(lm_model_2)$r.squared - summary(lm_model_1)$r.squared
summary(lm_model_3)$r.squared - summary(lm_model_2)$r.squared
summary(lm_model_3)$r.squared - summary(lm_model_1)$r.squared



##Testing hypothesis on the pre-registered secondary outcome in a more adequate model
#DV is count, thus start with a poisson regression

#First, checking the regular poisson regression

mod_1 <- glm(learning_num ~ scale(pep_effect) +scale(self_efficacy), 
             family = "poisson", 
             data = final_df)
summary(mod_1)

tab_model(mod_1,
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE)

check_zeroinflation(mod_1)

plot_model(mod_1, type= "pred", terms = "pep_effect[all]", show.data = TRUE,
           title = "", line.size = NULL) +
  labs(y = "Time spent on viewing the solutions",
       x = "Implicit score of growth mindset") +
  theme_classic()
#it shows zero-inflation, and the outcome shows right-censorship as well. We need to run a 
#two-step model: binomial for the binary data, and censored poisson for those that
#looked at least 1 solution

#First, testing the binomial model
binary_model <- glm(binary_learning ~ scale(pep_effect) + scale(self_efficacy),
                    family = "binomial",
                    data = final_df)
summary(binary_model)
#it's not significant, as in the main hypothesis, showing the same results

#now, let's create a dataframe, which includes only those, who looked at at least 1 solution
reduced<- final_df %>% 
  filter(binary_learning == 1) %>% 
         #creating an extra variable for the censored values
  mutate(status = if_else(learning_num == 12, 0, 1))

#running the censored poisson regression with implicit score

cens_pois <- vglm(SurvS4(learning_num, status) ~ scale(pep_effect) + scale(self_efficacy), 
                  cens.poisson, data = reduced, trace = TRUE)
summary(cens_pois)

#running the censored poisson regression with implicit and explicit scores

cens_pois_explicit <- vglm(SurvS4(learning_num, status) ~ scale(pep_effect) + scale(self_efficacy) + scale(iqms_avg), 
                  cens.poisson, data = reduced, trace = TRUE)
summary(cens_pois_explicit)
#it supports the main analysis, so the implicit score is significantly 
#predictive of the rate ratio of extra solutions viewed


#save out the table
tab_model(cens_pois_explicit,
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/cens_pois_table_explicit.doc")

#create predicted values
cens_pois_pred<- predict(cens_pois_explicit, type = "response")
reduced$cens_pois_pred <- cens_pois_pred


#plot the model
cens_poins_plot_i<-
  ggplot(reduced, aes(x = pep_effect, y = cens_pois_pred)) +
  geom_point(aes(y=learning_num, alpha =1), size = 2.2, show.legend = FALSE) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), level = 0.95, 
              color = "black", fill = "#b9b9b9", size = 0.7) +
  labs(y = "Number of solutions viewed",
       x = "Implicit score of growth mindset") +
theme_classic()+
  scale_y_continuous(breaks=seq(1, 12)) 

cens_poins_plot_e<-
  ggplot(reduced, aes(x = iqms_avg, y = cens_pois_pred)) +
  geom_point(aes(y=learning_num, alpha =1), size = 2.2, show.legend = FALSE) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), level = 0.95, 
              color = "black", fill = "#b9b9b9", size = 0.7) +
  labs(y = "",
       x = "Explicit score of growth mindset") +
  theme_classic()+
  scale_y_continuous(breaks=seq(1, 12)) 


#merge the plots
both_plots_cens_pois<-
  ggarrange(cens_poins_plot_i, cens_poins_plot_e, 
            labels = c("", ""),
            ncol = 2, nrow = 1)

#save plot
png(file="plots/both_plots_cens_pois.png",
    width=1200, height=665, res = 150)
plot(both_plots_cens_pois)
dev.off()


