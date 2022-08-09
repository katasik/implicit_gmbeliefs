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
lm_model_explicit <- lm(learning_time ~ scale(pep_effect) + scale(self_efficacy) + scale(iqms_avg), data = final_df)
summary(lm_model_explicit)

#Reporting it
report(lm_model_explicit)

#get model results in .doc file
tab_model(lm_model_explicit,
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/lm_table.doc")


#Exploratory analysis, secondary DV: adding explicit score to the mdoel as well
lm_model_explicit_num <- lm(learning_num ~ scale(pep_effect) + scale(self_efficacy) + scale(iqms_avg), data = final_df)
summary(lm_model_explicit_num)

#Reporting it
report(lm_model_explicit_num)

##Hierarhical regression first dependent variable


lm_model_1 <- lm(scale(learning_time) ~  scale(self_efficacy), data = final_df)
summary(lm_model_1)
lm_model_2 <- lm(scale(learning_time) ~  scale(self_efficacy) + scale(iqms_avg), data = final_df)
lm_model_3 <- lm(scale(learning_time) ~  scale(self_efficacy) + scale(iqms_avg) + scale(pep_effect), data = final_df)

anova(lm_model_1, lm_model_2, lm_model_3)
anova(lm_model_1, lm_model_3)

summary(lm_model_2)$r.squared - summary(lm_model_1)$r.squared
summary(lm_model_3)$r.squared - summary(lm_model_2)$r.squared
summary(lm_model_3)$r.squared - summary(lm_model_1)$r.squared


##Hierarhical regression secondary dependent variable

lm_model_1_num <- lm(scale(learning_num) ~  scale(self_efficacy), data = final_df)
summary(lm_model_1_num)
lm_model_2_num <- lm(scale(learning_num) ~  scale(self_efficacy) + scale(iqms_avg), data = final_df)
lm_model_3_num <- lm(scale(learning_num) ~  scale(self_efficacy) + scale(iqms_avg) + scale(pep_effect), data = final_df)

anova(lm_model_1_num, lm_model_2_num, lm_model_3_num)
anova(lm_model_1_num, lm_model_3_num)

summary(lm_model_2_num)$r.squared - summary(lm_model_1_num)$r.squared
summary(lm_model_3_num)$r.squared - summary(lm_model_2_num)$r.squared
summary(lm_model_3_num)$r.squared - summary(lm_model_1_num)$r.squared


#let's create a plot
plot_model(lm_model, type = "pred", terms = c("pep_effect"), show.data = TRUE)


check_model(lm_model)
#The posterior predictive check of the model shows that the LR model is not adequate
#This is the case because the distribution of the outcome is positively skewed, with zeros included.

qplot(final_df$learning_time)

#This is a gamma distribution, bounded at zero. 
#Thus, we should run a zero-inflated gamma regression

zigamma_model <- glmmTMB(learning_time ~ scale(pep_effect) + scale(self_efficacy),
                         family = ziGamma(link = "log"),
                         ziformula = ~ scale(pep_effect) + scale(self_efficacy),
                         data = final_df)
summary(zigamma_model)
report(zigamma_model)

model_parameters(zigamma_model, exponentiate = TRUE)

zigamma_model_explicit <- glmmTMB(learning_time ~ scale(pep_effect) + scale(self_efficacy) + scale(iqms_avg),
                         family = ziGamma(link = "log"),
                         ziformula = ~ scale(pep_effect) + scale(self_efficacy),
                         data = final_df)
summary(zigamma_model_explicit)
report(zigamma_model_explicit)

model_parameters(zigamma_model_explicit, exponentiate = TRUE)

#the gamma coefficients are correct, however the binomial part of the model actually calculates the probability of zero
#rather than the probability of 1. From a stackoverflow answer:
#"glmmTMB is predicting the probability of a zero rather than of a non-zero value"
#this just means that we have to reverse the minus values to plus values in the binomial part of the model
#but just to confirm, the following code shows the same, but positive values in the binomial part of the model:

binary_model <- glm(binary_learning ~ scale(pep_effect) + scale(self_efficacy),
                         family = "binomial",
                         data = final_df)
summary(binary_model)

binary_model_explicit <- glm(binary_learning ~ scale(pep_effect) + scale(self_efficacy) + scale(iqms_avg),
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

#The zero-inflated gamma model shows that the implicit and explicit scores are predictive of learning time in the gamma regression
#and not in the binomial model.

##Testing assumptions

check_zigamma_model <- simulateResiduals(fittedModel = zigamma_model, n = 500)
plot(check_zigamma_model)

#Assumptions are fine


#Let's create an APA table with the exponentiated results 
#(remember to change the binomial results to the exponentiated positive values)
tab_model(zigamma_model_explicit, 
          show.intercept = TRUE,
          show.est = TRUE,
          show.se = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.aic = TRUE,
          file = "tables/zigamma_table_explicit.doc")

#Predictions from  gamma model
final_df$prob.of.con <- predict(zigamma_model_explicit, what="Conditional model", type="response")


#let's create a black and white plot 
gamma_plot_i<-
  plot_model(zigamma_model_explicit, type= "pred", terms = "pep_effect[all]", show.data = TRUE,
           title = "", line.size = NULL) +
  labs(y = "Time spent on viewing the solutions",
       x = "Implicit score of growth mindset") +
  theme_classic()


gamma_plot_e<-
  plot_model(zigamma_model_explicit, type= "pred", terms = "iqms_avg[all]", show.data = TRUE,
             title = "", line.size = NULL) +
  labs(y = "",
       x = "Explicit score of growth mindset") +
  theme_classic()


both_plots<-
  ggarrange(gamma_plot_i, gamma_plot_e, 
          labels = c("", ""),
          ncol = 2, nrow = 1)

#save plot
png(file="plots/both_plots.png",
    width=1200, height=665, res = 150)
plot(both_plots)
dev.off()


##Testing hypothesis on the pre-registered secondary outcome

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
#it's not significant, as in the main hypothesis

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



both_plots_cens_pois<-
  ggarrange(cens_poins_plot_i, cens_poins_plot_e, 
            labels = c("", ""),
            ncol = 2, nrow = 1)

#save plot
png(file="plots/both_plots_cens_pois.png",
    width=1200, height=665, res = 150)
plot(both_plots_cens_pois)
dev.off()
