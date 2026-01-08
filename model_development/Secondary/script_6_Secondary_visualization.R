#####################################################################
# Purpose: Secondary Viz (retrained CanOSSEM) 
# Author: namanpaul
# Last modified by: namanpaul
# R Version: # use sessionInfo()
# Date: # use Sys.Date()
#####################################################################


library(tidyverse)

load('pathmodel_output/Secondary_ranger/MAIN_MODEL/Secondary_ranger_prediction_set_with_predictions.RData')

# read the var desc file
var_desc <- read_csv("path/var_desc.csv")


#update season values and #separate date and rCN
Secondary_ranger_prediction_set_with_predictions <- Secondary_ranger_prediction_set_with_predictions %>%
  mutate(date_val = ymd(str_sub(rCN_date_identifier, 1,10)),
         CanOSSEM_rCN = as.numeric(str_sub(rCN_date_identifier, -6)))



a <- Secondary_ranger_prediction_set_with_predictions %>% 
  ggplot(., aes(x =date_val, y = `Predicted - Observed`))+
  geom_point((aes(colour = factor(season))))+
  ylab(expression(Predicted~Daily~Mean~PM[2.5]~-~Observed~Daily~Mean~PM[2.5]  ( mu~g/m^3)))+
  xlab('Year')+
  scale_color_discrete(name="Season")+
  geom_hline(yintercept = 0, colour = "blue", linetype = 'dashed')+
  theme_bw()
#annotate("rect", xmin = as.Date('2016-03-01'), xmax = as.Date('2016-08-01'), ymin = -330, ymax = 320,
#         alpha = .15)+
#annotate("text", x = as.Date('2016-08-01'), y = -350, label = "Fort McMurray Interface Fire")+
#annotate("rect", xmin = as.Date('2017-06-01'), xmax = as.Date('2017-11-01'), ymin = -160, ymax = 250,
#         alpha = .15)+
#annotate("rect", xmin = as.Date('2018-06-01'), xmax = as.Date('2018-11-01'), ymin = -280, ymax = 110,
#         alpha = .15)+
#annotate("rect", xmin = as.Date('2019-03-01'), xmax = as.Date('2019-08-01'), ymin = -280, ymax = 260,
#         alpha = .15)+
#annotate("text", x = as.Date('2018-05-01'), y = -320, label = "British Columbia Wildfires")+
#annotate("rect", xmin = as.Date('2020-06-01'), xmax = as.Date('2020-11-01'), ymin = -230, ymax = 200,
#         alpha = .15)+
#annotate("text", x = as.Date('2020-05-01'), y = -320, label = "USA Wildfires")+
#annotate("rect", xmin = as.Date('2021-06-01'), xmax = as.Date('2021-11-01'), ymin = -510, ymax = 350,
#         alpha = .15)+
#annotate("text", x = as.Date('2021-05-01'), y = -540, label = "British Columbia Wildfires")+
#annotate("rect", xmin = as.Date('2022-06-01'), xmax = as.Date('2022-12-01'), ymin = -170, ymax = 250,
#         alpha = .15)#+


#a

ggsave("Secondary_ranger_performance_prediction_set.png", a, width = 24, height = 18, units = "cm", dpi = 300)

# importance --------------------------------------------------------------

load('pathmodel_output/Secondary_ranger/MAIN_MODEL/Secondary_ranger_importance.RData')

importance <- importance %>% 
  mutate(var = row.names(importance)) %>% 
  left_join(., var_desc,
            by = 'var') %>% 
  rename(Variables = description,
         Importance = `importance(Secondary_ranger)`) %>% 
  arrange(desc(Importance))



importance$Variables <- factor(importance$Variables, levels = importance$Variables)




b <- importance %>% 
  ggplot(.,aes(y=Importance, x=Variables, fill= Importance))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_x_discrete(limits = rev(levels(importance$Variables)))+
  scale_fill_gradient("Importance", low = "pink", high = "blue")+
  theme_bw()+geom_col()#+
b

ggsave("Secondary_ranger_importance.png", b, width = 24, height = 18, units = "cm", dpi = 300)




# compute the correlation -------------------------------------------------

# compute mean fractional bias/r2 -----------------------------------------
library(caret)
library(ggpubr)

PM25_r  <- cor(Secondary_ranger_prediction_set_with_predictions$predicted_pm25, 
               Secondary_ranger_prediction_set_with_predictions$daily_mean_pm25)


lm_fit <- lm(Secondary_ranger_prediction_set_with_predictions$predicted_pm25 ~ Secondary_ranger_prediction_set_with_predictions$daily_mean_pm25)

PM25_R2  <- summary(lm_fit)$r.squared

print('PM25 R2')
(PM25_R2)


c <- ggscatter(Secondary_ranger_prediction_set_with_predictions, 
               x = 'daily_mean_pm25', y = 'predicted_pm25', 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "pearson")+
  theme_bw()+
  labs(x=expression(Observed~Daily~Mean~PM[2.5]~ ( mu~g/m^3)),
       y=expression(Predicted~Daily~Mean~PM[2.5]~ ( mu~g/m^3)))

c

ggsave("Secondary_ranger_P10_r2.png", c, width = 20, height = 12, units = "cm", dpi=300)






#R2 gives us a measure on how much of the variation in the actual variable y  can be explained by the predicted variable y_hat.
#By switching their roles, we change the RSS/TSS term, thus we obtain a different R2 value.

y <- Secondary_ranger_prediction_set_with_predictions$daily_mean_pm25
y_hat <- Secondary_ranger_prediction_set_with_predictions$predicted_pm25
(y_bar <- mean(y))
(RSS <- sum((y-y_hat)^2))
(TSS <-  sum((y-y_bar)^2))
(R2 <-  1 - RSS/TSS)
cat("Coefficient of determination= ", R2)

