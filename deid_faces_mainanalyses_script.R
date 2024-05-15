
library(dplyr)
library(lmerTest)
library(patchwork)
library(sjPlot)
library(emmeans)
library(ggplot2)

#Script to reproduce main analyses

#change path
df = read.csv("/deid_data_faces.csv", stringsAsFactors = FALSE)

#reward ROI model
model_reward_main <- glmer(drinks_next ~ alc_conv_within*reward + baseline_drink_frequency + baseline_drink_amount + signal.count + centered_conv_bw + (1 |id),
                           family = binomial(link = "logit"),
                           data = df, control = glmerControl(optimizer = "bobyqa"))
tab_model(model_reward_main)
summ(model_reward_main)

######## probing interactions ######

# (A) reward ROI model

round(sd(df$reward), digits =2) #.24

#p-values
emmeans::test(emtrends(model_reward_main, ~ reward,at=list(reward = c(-0.24, 0, .24)), var="alc_conv_within"))
reward_probs = ggeffects::ggpredict(model_reward_main,
                                    terms = c("reward [.24]", "alc_conv_within")) %>% data.frame()

#predicted probabilities (did not talk about alcohol at +1SD reward activity)
round(reward_probs$predicted[1], digits= 2)
round(reward_probs$conf.low[1], digits= 2)
round(reward_probs$conf.high[1], digits= 2)

#predicted probabilities (talked abut alcohol at +1SD reward activity)
round(reward_probs$predicted[2], digits= 2)
round(reward_probs$conf.low[2], digits= 2)
round(reward_probs$conf.high[2], digits= 2)

reward_ci = confint(emtrends(model_reward_main,
                            ~ reward, at=list(reward = c(-.24, 0, .24)),
                            var="alc_conv_within"))

#(+1SD reward) odds ratio + CI
round(exp(reward_ci$alc_conv_within.trend[3]), digits =2)
round(exp(reward_ci$asymp.LCL[3]), digits =2)
round(exp(reward_ci$asymp.UCL[3]), digits =2)

#(average reward) odds ratio + CI
round(exp(reward_ci$alc_conv_within.trend[2]), digits =2)
round(exp(reward_ci$asymp.LCL[2]), digits =2)
round(exp(reward_ci$asymp.UCL[2]), digits =2)

#(-1SD reward) odds ratio + CI
round(exp(reward_ci$alc_conv_within.trend[1]), digits =2)
round(exp(reward_ci$asymp.LCL[1]), digits =2)
round(exp(reward_ci$asymp.UCL[1]), digits =2)

######plot
plot_reward = emmip(model_reward_main,
                    reward ~
                      alc_conv_within, at = list(reward =c(-.24,0,.24),
                                                alc_conv_within =c(0,1)),
                    type="re", plotit=T, CIs = TRUE)
plot_reward
plot_reward <- plot_reward +
  scale_color_manual(values=c("#CC6677", "#DDCC77","#6699CC"),
                     breaks = c(.24,0,-.24),
                     labels=c("+1 SD", "Mean", "-1SD")) +
  labs(title = "",
       x= "Alcohol conversation",
       y=" Predicted probability of \n next-day alcohol use",
       color="Reward system \n activity ",
       fill="Reward system \n activity") +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.08, 0.12, 0.16, 0.20)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme(legend.title.align=0.5) +
  theme(legend.title.align=0.5)
plot_reward


######### mentalizing

#mentalizing model
model_mentalizing_main <- glmer(drinks_next ~ alc_conv_within*mentalizing + baseline_drink_frequency + baseline_drink_amount + signal.count + centered_conv_bw + (1 |id),
                                family = binomial(link = "logit"),
                                data = df, control = glmerControl(optimizer = "bobyqa"))
tab_model(model_mentalizing_main)
summ_model(model_mentalizing_main)

######## probing interactions ######

# (B) mentalizing ROI model

round(sd(df$mentalizing), digits =2) #0.31

#p-values
emmeans::test(emtrends(model_mentalizing_main, ~ mentalizing,at=list(mentalizing = c(-0.31, 0, .31)), var="alc_conv_within"))

mentalizing_probs = ggeffects::ggpredict(model_mentalizing_main,
                                    terms = c("mentalizing[.31]", "alc_conv_within")) %>% data.frame()

#predicted probabilities (did not talk about alcohol at +1SD mentalizing activity)
round(mentalizing_probs$predicted[1], digits= 2)
round(mentalizing_probs$conf.low[1], digits= 2)
round(mentalizing_probs$conf.high[1], digits= 2)

#predicted probabilities (talked abut alcohol at +1SD mentalizing activity)
round(mentalizing_probs$predicted[2], digits= 2)
round(mentalizing_probs$conf.low[2], digits= 2)
round(mentalizing_probs$conf.high[2], digits= 2)

ment_ci = confint(emtrends(model_mentalizing_main,
                             ~ mentalizing, at=list(mentalizing = c(-.31, 0, .31)),
                             var="alc_conv_within"))

#(+1SD mentalizing) odds ratio + CI
round(exp(ment_ci$alc_conv_within.trend[3]), digits =2)
round(exp(ment_ci$asymp.LCL[3]), digits =2)
round(exp(ment_ci$asymp.UCL[3]), digits =2)

#(average mentalizing) odds ratio + CI
round(exp(ment_ci$alc_conv_within.trend[2]), digits =2)
round(exp(ment_ci$asymp.LCL[2]), digits =2)
round(exp(ment_ci$asymp.UCL[2]), digits =2)

#(-1SD mentalizing) odds ratio + CI
round(exp(ment_ci$alc_conv_within.trend[1]), digits =2)
round(exp(ment_ci$asymp.LCL[1]), digits =2)
round(exp(ment_ci$asymp.UCL[1]), digits =2)

######plot
plot_mentalizing = emmip(model_mentalizing_main,
                    mentalizing ~
                      alc_conv_within, at = list(mentalizing =c(-.31,0,.31),
                                                 alc_conv_within =c(0,1)),
                    type="re", plotit=T, CIs = TRUE)
plot_mentalizing

plot_mentalizing <- plot_mentalizing +
  scale_color_manual(values=c("#CC6677", "#DDCC77","#6699CC"),
                     breaks = c(.31,0,-.31),
                     labels=c("+1 SD", "Mean", "-1SD")) +
  labs(title = "",
       x= "Alcohol conversation",
       y=" Predicted probability of \n next-day alcohol use",
       color="Mentalizing system \n activity ",
       fill="Mentalizing system \n activity") +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.08, 0.12, 0.16, 0.20)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme(legend.title.align=0.5) +
  theme(legend.title.align=0.5)
plot_mentalizing

# Arrange plots side by side
plot_side_by_side <-  plot_reward + plot_mentalizing
plot_side_by_side


