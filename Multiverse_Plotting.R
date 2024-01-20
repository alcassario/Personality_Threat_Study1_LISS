

# libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(lattice)


# loading data 
mains <- readRDS("mains_for_multiplot.rds")
interactions <- readRDS("interactions_for_multiplot.rds")

# how many models for each set of analyses?
max(mains$Mod_Number)
max(interactions$Mod_Number)

# interactions 
interactions$Direction <- ifelse(interactions$Significance == 1 & interactions$estimate < 0, "Liberal", 
                                 ifelse(interactions$Significance == 1 & interactions$estimate > 0, 
                                        "Conservative", "Not Significant"))

interactions$Theory <- ifelse(str_detect(interactions$term, "o:") == TRUE & interactions$estimate > 0 & interactions$p.value < .05, "Threat Constraint", 
                              ifelse(str_detect(interactions$term, "c:") == TRUE & interactions$estimate > 0 & interactions$p.value < .05, "Neg. Bias", 
                                     ifelse(str_detect(interactions$term, "o:") == TRUE & interactions$estimate < 0 & interactions$p.value < .05, "Neg. Bias", "Neither")))

interactions <- interactions %>% arrange(estimate)
interactions$n_term <- 1:nrow(interactions)

# reorder facets so aesthetics map main analysis figures 
interactions <- interactions %>% mutate(Theory = factor(Theory)) %>%
  mutate(Theory=fct_relevel(Theory, c("Neg. Bias", "Threat Constraint", "Neither")))

# figure 
plot1 <- ggplot(interactions, aes(x = n_term)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey90") +
  geom_point(aes(y = estimate, color = as.factor(Theory)), size = .5) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#000000")) +
  facet_grid(grouping ~ ., scales = "free", space = "free") +
  labs(title = "Personality Interactions",
       y = "Regression Coefficient") +
 theme_cowplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.y = element_text(size = 16), 
    strip.text = element_text(size = 16)
  )
plot1 # now need way to make sure terms included in mod are coded and mod spec included


# main effects
mains <- subset(mains, mains$term != "o" & mains$term != "c"  & mains$term != "e" & mains$term != "a" & mains$term != "n" & mains$term != "wave")

mains$Direction <- ifelse(mains$Significance == 1 & mains$estimate < 0, "Liberal", 
                                 ifelse(mains$Significance == 1 & mains$estimate > 0, "Conservative", "Not Significant"))
mains <- mains %>% arrange(estimate)
mains$grouping <- "Main Effects"
mains$n_effect <- 1:nrow(mains)

plot2 <- ggplot(mains, aes(x = n_effect)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey90") +
  geom_point(aes(y = estimate, color = as.factor(Direction)), size = .5) +
  geom_hline(yintercept = 0) +
  ylim(-.1, NA)+
  scale_color_manual(values = c("#FF0000", "#56B4E9", "#000000")) +
  facet_grid(grouping ~ ., scales = "free", space = "free") +
  labs(title = "Threat Main Effects",
       y = "Regression Coefficient") +
  theme_cowplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.y = element_text(size = 16), 
    strip.text = element_text(size = 16)
  )
plot2 

##### is predictor included in specification? ####
 mains$o <- ifelse(str_detect(mains$spec, "o_") == TRUE | str_detect(mains$spec, "_o_") == TRUE | 
                     str_detect(mains$spec, "_o") == TRUE, 1, 0)
 
mains$c <- ifelse(str_detect(mains$spec, "c_") == TRUE| str_detect(mains$spec, "_c_") == TRUE| 
                   str_detect(mains$spec, "_c-") == TRUE, 1, 0)
 
mains$e <- ifelse(str_detect(mains$spec, "_e_") == TRUE, 1, 0)
 
mains$a <- ifelse(str_detect(mains$spec, "_a_") == TRUE | str_detect(mains$spec, "a_") == TRUE | 
                     str_detect(mains$spec, "_a-") == TRUE, 1, 0)
 
mains$n <- ifelse(str_detect(mains$spec, "_n") == TRUE | 
                     str_detect(mains$spec, "_n-") == TRUE, 1, 0)
 
mains$covid <- ifelse(str_detect(mains$spec, "covid") == TRUE, 1, 0)
mains$homicide <- ifelse(str_detect(mains$spec, "homicide") == TRUE, 1, 0)
mains$immigration <- ifelse(str_detect(mains$spec, "immigration") == TRUE & str_detect(mains$spec, "immigration_") == FALSE, 1, 0)
mains$unemployment <- ifelse(str_detect(mains$spec, "unemployment") == TRUE, 1, 0)

# ##### coding for interactions #####
interactions$o <- ifelse(str_detect(interactions$spec, "o_") == TRUE | str_detect(interactions$spec, "_o_") == TRUE |
                    str_detect(interactions$spec, "_o") == TRUE, 1, 0)

interactions$c <- ifelse(str_detect(interactions$spec, "c_") == TRUE| str_detect(interactions$spec, "_c_") == TRUE|
                    str_detect(interactions$spec, "_c-") == TRUE, 1, 0)

interactions$e <- ifelse(str_detect(interactions$spec, "_e_") == TRUE, 1, 0)

interactions$a <- ifelse(str_detect(interactions$spec, "_a_") == TRUE | str_detect(interactions$spec, "a_") == TRUE |
                    str_detect(interactions$spec, "_a-") == TRUE, 1, 0)

interactions$n <- ifelse(str_detect(interactions$spec, "_n_") == TRUE |
                    str_detect(interactions$spec, "_n-") == TRUE, 1, 0)

interactions$covid <- ifelse(str_detect(interactions$spec, "covid") == TRUE, 1, 0)
interactions$homicide <- ifelse(str_detect(interactions$spec, "homicide") == TRUE, 1, 0)
interactions$immigration <- ifelse(str_detect(interactions$spec, "immigration") == TRUE & str_detect(interactions$spec, "immigration_") == FALSE, 1, 0)
interactions$unemployment <- ifelse(str_detect(interactions$spec, "unemployment") == TRUE, 1, 0)


# pivot longer gathering o:unemployment
interactions_long <- interactions %>% pivot_longer((o:unemployment), names_to = "predictor", values_to = "present")
interactions_long <- subset(interactions_long, interactions_long$present == 1)
interactions_long <- subset(interactions_long, interactions_long$Threat != "wave")
interactions_long <- interactions_long %>% pivot_longer(c("Threat", "Moderator", "Outcome", "predictor"), names_to = "type", values_to = "variable")
interactions_long$type <- ifelse(interactions_long$type == "predictor", "Predictor", interactions_long$type)

mains_long <- mains %>% pivot_longer((o:unemployment), names_to = "predictor", values_to = "present")
mains_long <- subset(mains_long, mains_long$present == 1)
mains_longer <- mains_long %>% pivot_longer(c("term", "predictor"), names_to = "class", values_to = "predictor")


# bottom panel for main effects 
mains_longer$class <- ifelse(mains_longer$class == "term", "Threat", 
                             ifelse(mains_longer$class == "predictor", "Personality Control", mains_longer$class))

# remove threat indicators # 
# filter data such that if class = Personality predictor does not equal a threat #
mains_longer$omit <- ifelse(mains_longer$class == "Personality Control" & (mains_longer$predictor == "immigration"  | 
                              mains_longer$predictor == "covid" | mains_longer$predictor == "homicide" | 
                              mains_longer$predictor == "unemployment"), 1, 0)

mains_longer <- mains_longer %>% filter(omit != 1)

# renaming personality vars for figure 
mains_longer$predictor <- ifelse(mains_longer$predictor == "o", "open", 
                                 ifelse(mains_longer$predictor == "c", "conscientious",
                                        ifelse(mains_longer$predictor == "e", "extraverted",
                                               ifelse(mains_longer$predictor == "a", "agreeable", 
                                                      ifelse(mains_longer$predictor == "n", "neurotic", mains_longer$predictor)))))
  


mains_longer <- mains_longer %>% pivot_longer(c("predictor", "Outcome"), names_to = "type", values_to =  "predictor")
mains_longer$class <- ifelse(mains_longer$type == "Outcome", "Outcome", mains_longer$class)

# code interaction and main effects varnames to match 
mains_longer$class <- ifelse(mains_longer$class == "Personality Control", "Control", mains_longer$class)
interactions_long$type <- ifelse(interactions_long$type == "Predictor", "Control", interactions_long$type)

# releveling these type/class so figures match  
interactions_long <- interactions_long %>% mutate(type = fct_relevel(type, "Moderator", "Threat", "Control", "Outcome"))
mains_longer <- mains_longer %>% mutate(class = fct_relevel(class, "Threat", "Control", "Outcome"))


# plotting main effects 
plot.multiverse.mains <-
  ggplot(data = mains_longer, aes(x = n_effect, y = fct_rev(predictor), color = Direction)) +
  geom_point(shape = "|", size = 4, alpha = 0.6) +
  scale_color_manual(values = c("#FF0000","#56B4E9", "#000000")) +
  facet_grid(class ~ ., scales = "free", space = "free") +
  labs(y = "Variables", 
       x = "Main Effect",
       caption = "Note: Error bars are 95% confidence intervals.") +
  theme_cowplot() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) 

plot.multiverse.mains    


# need to relevel personality and threat predictors so they're next to each other
interactions_long <- interactions_long %>% mutate(variable = fct_relevel(variable, "immigration", "unemployment", 
                                                                         "covid", "homicide", "o", "c", "e", "a", "n", 
                                                                         "symbolic", "mothers_work", "income", "fathers_work",
                                                                         "moms_working", "immigrant_culture", "unions", "immigration_ability", 
                                                                         "marriage", "gender_kids"), 
                                                                         variable = fct_recode(variable, "agreeable" = "a", "conscientious" = "c", "extraverted" = "e", 
                                                                                               "neurotic" = "n", "open" = "o"))




# relevel variables to match other figure
interactions_long <- interactions_long %>% mutate(variable = fct_relevel(variable, "covid", "homicide", "immigration", "unemployment", 
                                                                         "agreeable", "conscientious", "extraverted", "neurotic", "open",
                                                                         "eu", "fathers_work", "gender_kids", "immigrant_culture", "immigration_ability",
                                                                         "income", "marriage", "moms_working", "mothers_work", "symbolic", "unions"))

# to make figures look nicer, remove personality and threat from control panel IF 
# they are part of the focal interaction (this is displayed in "threat" and "moderator" facets)
interactions_long$omit <- ifelse(interactions_long$term == "c:immigration" & interactions_long$type == "Control" & 
                              (interactions_long$variable == "conscientious" | interactions_long$variable == "immigration"), 1, 
                              ifelse(interactions_long$term == "o:unemployment" & interactions_long$type == "Control" & 
                                       (interactions_long$variable == "open" | interactions_long$variable == "unemployment"), 1, 
                                          ifelse(interactions_long$term == "o:immigration" & interactions_long$type == "Control" & 
                                              (interactions_long$variable == "open" | interactions_long$variable == "immigration"), 1, 
                                              ifelse(interactions_long$term == "c:covid" & interactions_long$type == "Control" &
                                                       (interactions_long$variable == "conscientious" | interactions_long$variable == "covid"), 1, 
                                                     ifelse(interactions_long$term == "o:homicide" & interactions_long$type == "Control" & 
                                                              (interactions_long$variable == "open" | interactions_long$variable == "homicide"), 1, 
                                                            ifelse(interactions_long$term == "c:homicide" & interactions_long$type == "Control" & 
                                                                     (interactions_long$variable == "conscientious" | interactions_long$variable == "homicide"), 1, 
                                                                   ifelse(interactions_long$term == "o:covid" & interactions_long$type == "Control" & 
                                                                            (interactions_long$variable == "open" | interactions_long$variable == "covid"), 1, 
                                                                          ifelse(interactions_long$term == "c:unemployment" & interactions_long$type == "Control" & 
                                                                                   interactions_long$variable == "conscientious" | interactions_long$variable == "unemployment", 1, 0))))))))


interactions_long <- subset(interactions_long, interactions_long$omit == 0)

# figure 
plot.multiverse.ints <-
  ggplot(data = interactions_long, aes(x = n_term, y = fct_rev(variable), color = Theory)) +
  geom_point(shape = "|", size = 4, alpha = 0.6) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#000000")) +
  facet_grid(type ~ ., scales = "free", space = "free") +
  labs(y = "Variables", 
       x = "Interaction Effect",
       caption = "Note: Error bars are 95% confidence intervals.") +
  theme_cowplot() +
  theme(
    legend.position = "none", axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) 
plot.multiverse.ints


# add top and bottom figures figures save mains 
plots <- list(plot2, plot.multiverse.mains)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

g <- do.call("grid.arrange", c(grobs, ncol = 1))
ggsave("mainsmultiplot.pdf", g, height = 17, width = 12, units = "in")


# add top and bottom figures figures save interactions 
plots <- list(plot1, plot.multiverse.ints)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

g <- do.call("grid.arrange", c(grobs, ncol = 1))
ggsave("intsmultiplot.pdf", g, height = 23, width = 15, units = "in")
