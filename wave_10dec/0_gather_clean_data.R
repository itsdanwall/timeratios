#--------------
# Gather and clean data for the titrated values for ITC uncertainty
# Daniel Wall
# 3 November 2015
#--------------

setwd("/git_repositories/timeratios/wave_10dec/")
source("load_packages.R")
rm(list = ls())


#----read in the data----

timerat <- read_csv("raw_data/Time_Ratios_6_December.csv")


# change column names
names(timerat) <- tolower(names(timerat))

# remove test runs
timerat <- timerat %>%
  filter(finished == 1)




# get data so I can work on it for when people are switching
# dissimilarity ids
diss_id <- read.csv("raw_data/diss_ids.csv") 

# filter data 
diss <- timerat[, str_detect(names(timerat), "^(responseid|ul)")] %>%
  melt(id = "responseid") %>%
  filter(!(is.na(value))) %>%
  group_by(responseid)

# get which condition the participant was in
diss_m <- diss %>%
  group_by(responseid) %>%
  summarise(ratcng = ifelse("ul5_6_1" %in% variable, "small", "large"))

diss <- merge(diss, diss_m, by = "responseid", all.x=TRUE) %>%
  mutate(ss_time = str_extract(variable, "\\d+"),
         ll_time = str_extract(variable, "\\d+_1$") %>% str_extract("\\d+"))


# now merge with dissimilarity ids
diss <- merge(diss, diss_id, by = c("ss_time", "ll_time"))

dissc <- diss %>%
  dcast(responseid + id + ratcng ~ delayed, value.var = "value") %>%
  mutate(diss_diff = delayed - not_delayed,
         ratcng = factor(ratcng))

disssum <- dissc %>%
  group_by(ratcng) %>%
  summarise(mean_diffcng = mean(diss_diff, na.rm = TRUE),
            se_diffcng = std.error(diss_diff, na.rm = TRUE)) %>%
  mutate(lower = mean_diffcng - 1.96*se_diffcng,
         upper = mean_diffcng + 1.96*se_diffcng)

ggplot(disssum, aes(x = ratcng, y = mean_diffcng)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  ylab("Difference between not delayed and delayed intervals")
  
# mixed effects model
diss_lmer <- lmer(diss_diff ~ ratcng + (1|responseid),
                  dissc)

Anova(diss_lmer, type = 3)
summary(diss_lmer)

eff_df <- Effect(c("ratcng"), diss_lmer) %>%
  as.data.frame()

ggplot(eff_df, aes(x = ratcng, y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))


#### now let's look at pairs of choices

dissc1 <- dissc %>%
  mutate(id_pair = ifelse(id > 6, id - 6, id))


# plot it
diss1_sum <- dissc1 %>%
  group_by(ratcng, id_pair) %>%
  summarise(mean_dissdiff = mean(diss_diff, na.rm=TRUE),
            se_dissdiff = std.error(diss_diff, na.rm=TRUE)) %>%
  mutate(lower = mean_dissdiff - 1.96*se_dissdiff,
         upper = mean_dissdiff + 1.96*se_dissdiff)

ggplot(diss1_sum, aes(x = id_pair, y = mean_dissdiff, fill = ratcng, color = ratcng)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge")


# do t tests
diss_t_tests <- dissc1 %>%
  group_by(id_pair) %>%
  do(tidy(t.test(diss_diff ~ ratcng, data = .)))



##### now lets look at the intertemporal choices #####
itc <- timerat[, str_detect(names(timerat), "^(responseid|itc\\d+_\\d+)")] %>%
  melt(id = "responseid") %>%
  filter(!(is.na(value))) %>%
  mutate(ss_time = str_extract(variable, "\\d+"),
         ll_time = str_extract(variable, "\\d+$")) %>%
  group_by(responseid) %>%
    merge(diss_id, by = c("ss_time", "ll_time")) %>%
  mutate(value = factor(value),
         diss_set = factor(diss_set),
         delayed = factor(delayed),
         id_pair = ifelse(id > 6, id - 6, id),
         ratcng = ifelse(diss_set == 1, "small",
                         ifelse(diss_set == 2, "large", NA)) %>%
           factor)

  
#summarise and plot the data
itc_sum <- itc %>%
  group_by(diss_set, delayed) %>%
  summarise(n_ll = sum(value == "2"),
            n_tot = n()) %>%
  mutate(per_ll = n_ll/n_tot) %>%
  group_by(diss_set, delayed) %>%
  do( tidy(prop.test(.$n_ll, .$n_tot))) %>%
  ungroup %>%
  mutate(diss_set = factor(diss_set),
         delayed = factor(delayed))


ggplot(itc_sum,
       aes(x = diss_set, y = estimate, group = delayed, fill = delayed, 
           ymax = conf.high,  ymin = conf.low)) +
  geom_bar(stat="identity", position = "dodge") +
  ylab("Percent Larger Later") +
  geom_errorbar(position = "dodge")
  

# now lets run a model on this
# itc <- itc %>%
#   mutate(value = factor(value),
#          diss_set = factor(diss_set),
#          delayed = factor(delayed),
#          id_pair = ifelse(id > 6, id - 6, id),
#          ratcng = ifelse(diss_set == 1, "small",
#                          ifelse(diss_set == 2, "large", NA)) %>%
#            factor)

itc_lmer <- glmer(value ~ diss_set * delayed + (1|responseid),
                  itc, family = "binomial")

summary(itc_lmer)

itc_eff <- Effect(c("diss_set",  "delayed"), itc_lmer) %>%
  as.data.frame()

ggplot(itc_eff, aes(x = diss_set, y = fit, group = delayed,
                    fill = delayed,
                    ymin = lower, ymax = upper)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(position = "dodge") +
  ylab("Logistic Regression Coefficient")

# now look at each unique id pair

# get summaries for each id

itc_sum_id <- itc %>%
  group_by(ratcng, delayed, id_pair) %>%
  summarise(n_ll = sum(value == "2"),
            n_tot = n()) %>%
  mutate(per_ll = n_ll/n_tot) %>%
  group_by(ratcng, delayed, id_pair) %>%
  do( tidy(prop.test(.$n_ll, .$n_tot))) %>%
  ungroup %>%
  mutate(ratcng = factor(ratcng),
         delayed = factor(delayed),
         id_pair = factor(id_pair, levels = 1:6))

ggplot(itc_sum_id, aes(x = id_pair, y = estimate, group = delayed, fill = delayed)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), position = "dodge") +
  facet_grid(ratcng ~ .)

itc_lmers <- itc %>%
  group_by(id_pair) %>%
  do(tidy(glmer(value ~ delayed * ratcng + (1|responseid), data = ., family = "binomial")))

itc_glms <- itc %>%
  group_by(id_pair) %>%
  do(tidy(glm(value ~ delayed * ratcng, data = ., family = "binomial"))) %>%
  ungroup()%>%
  mutate(lower = estimate + 1.96*std.error,
         upper = estimate - 1.96*std.error,
         id_pair = factor(id_pair, levels = 1:6))

itc_glms_sub <- itc_glms %>%
  filter(term != "(Intercept)")

ggplot(itc_glms_sub, aes(x = id_pair, y = estimate, group = term,  fill = term)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = upper, ymin = lower), position = "dodge")
  

# omnibus glm
all_glm <- glm(value ~ delayed * ratcng + factor(id_pair), data = itc, family = "binomial")

summary(all_glm)

all_int_glm <- glm(value ~ delayed * ratcng * factor(id_pair), data = itc, family = "binomial")

summary(all_int_glm)
Anova(all_int_glm, type = 3)


# all_int_glmer <- glmer(value ~ delayed * ratcng * factor(id_pair) + (1|responseid), data = itc, family = "binomial")
# 
# summary(all_int_glmer)

# save ids for a table later
diss_id_c <- diss_id %>%
  mutate(id_par = ifelse(id > 6, id - 6, id),
         ratiochange = ifelse(diss_set == 1, "small", "large")) %>%
  melt(id = c("id_par", "ratiochange", "delayed"), 
       measure = c("ss_time", "ll_time")) %>%
  dcast(id_par ~ ratiochange + delayed + variable)

write.csv(diss_id_c, "diss_id_c.csv")

save.image("analyzed_data/analyzed_data.Rdata")
