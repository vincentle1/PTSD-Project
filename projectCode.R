
library(tidyverse)
library(readspss)
library(psych)

#load dataframe, then convert to tibble

scher_data <- read.sav("Mindfulness Dataset with Calculations LeV WPA.sav")
scher_tibble <- as_tibble(scher_data)

#Create tibble containing only students and variables of interest. ptsd_copy is used for AAQ recoding.

ptsd_copy <- scher_tibble %>%
  filter(`filter_$` == "Selected") %>%
  select(ID_number, PMBS_threatofharm, PMBS_selfworthjudgement, PMBS_reliabilitytrustworthiness, PMBS_total, AAQ_1, AAQ_2,AAQ_3, AAQ_4, AAQ_5, AAQ_6, AAQ_7, AAQ_8, AAQ_9, PCL_Tot_CDS)
view(ptsd_copy)
glimpse(ptsd_copy)

#recode
 ptsd_recode <- ptsd_copy %>%
    mutate_at(vars(AAQ_1, AAQ_4, AAQ_5, AAQ_6), ~recode(., 
                                                       'never true' = 7,
                                                        'often untrue' = 6,
                                                        'sometimes untrue' = 5,
                                                        'undecided' = 4,
                                                        'sometimes true' = 3,
                                                        'often true' = 2,
                                                        'always true' = 1)) %>%
    mutate_at(vars(AAQ_2, AAQ_3, AAQ_7, AAQ_8, AAQ_9), ~recode (.,
                                                               'never true' = 1,
                                                                'often untrue' = 2,
                                                                'sometimes untrue' = 3,
                                                                'undecided' = 4,
                                                                'sometimes true' = 5,
                                                                'often true' = 6,
                                                                'always true' = 7))

#Substitute NAs with means of the columns
ptsd_recode$AAQ_6[is.na(ptsd_recode$AAQ_6)] <- mean(ptsd_recode$AAQ_6, na.rm = TRUE)
ptsd_recode$PMBS_threatofharm[is.na(ptsd_recode$PMBS_threatofharm)] <- mean(ptsd_recode$PMBS_threatofharm, na.rm = TRUE)


ptsd_imputed <- ptsd_recode %>%
  mutate(AAQ_6 = replace(AAQ_6,
                          is.na(AAQ_6),
                          mean(AAQ_6, na.rm = TRUE)),
         
         AAQ_Tot_Score = AAQ_1 + AAQ_2 + AAQ_3 + AAQ_4 + AAQ_5 + AAQ_6 + AAQ_7 + AAQ_8 + AAQ_9,
         AAQ_Tot_Score = round(AAQ_Tot_Score, 2),
        
         PMBS_threatofharm = replace(PMBS_threatofharm,
                          is.na(PMBS_threatofharm),
                          mean(PMBS_threatofharm, na.rm = TRUE)),
         
         PMBS_selfworthjudgement = replace(PMBS_selfworthjudgement,
                                is.na(PMBS_selfworthjudgement),
                                mean(PMBS_selfworthjudgement, na.rm = TRUE)),
         
         PMBS_reliabilitytrustworthiness = replace(PMBS_reliabilitytrustworthiness,
                                        is.na(PMBS_reliabilitytrustworthiness),
                                        mean(PMBS_reliabilitytrustworthiness, na.rm = TRUE)),
         
         PMBS_total = PMBS_threatofharm + PMBS_selfworthjudgement + PMBS_reliabilitytrustworthiness,
         PMBS_total = round(PMBS_total, 2))

view(ptsd_imputed)
        

#Finish recoding and create one final tibble

ptsd_final <- select(ptsd_imputed, ID_number, PMBS_total, AAQ_Tot_Score, PCL_Tot_CDS)

#Graph points

ggplot(ptsd_final, aes(x = PMBS_total, y = PCL_Tot_CDS)) +
  geom_point() +
  geom_smooth(method = "lm")

#Graph histogram

ggplot(ptsd_final, aes(PMBS_total)) +
  geom_histogram()



#Bivariate correlations

#standard correlations

cor(ptsd_final, method = "pearson")

cor.test(ptsd_final$PMBS_total, ptsd_final$AAQ_Tot_Score, method = "pearson")

#HMISC Package, uncorrected for family
rcorr(as.matrix(ptsd_final), type = "pearson")

#Package corrected for family wise comparisons
corr.test(ptsd_final)

#summarystats

summarize(ptsd_final, sd = sd(PCL_Tot_CDS))

#Hierarchical Regressions

mal_model <- lm(PCL_Tot_CDS ~ PMBS_total, data = ptsd_final)
malexp_model <- lm(PCL_Tot_CDS ~ PMBS_total + AAQ_Tot_Score, data = ptsd_final)

reverse_model <- lm(PCL_Tot_CDS ~ AAQ_Tot_Score + PMBS_total, data = ptsd_final)

summary(reverse_model)



summary(mal_model)
summary(malexp_model)

#standardize the coefficients
library(QuantPsyc)

lm.beta(malexp_model)

#calculate confidence intervals
confint(malexp_model)

#compare models
anova(mal_model, malexp_model)

#check residual distribution
library(moderndive)
regression_points <- get_regression_points(malexp_model)
regression_points

ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 4) +
  labs(x = "Residual")

ggplot(regression_points, aes(x = AAQ_Tot_Score, y = residual)) +
  geom_point() +
  labs(x = "AAQ", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

glimpse(ptsd_final$PCL_Tot_CDS)

summarize(ptsd_final, mean = mean(PCL_Tot_CDS))
,
