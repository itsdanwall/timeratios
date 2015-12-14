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
  geom_errorbar(aes(ymin = lower, ymax = upper))
  
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


# now lets look at the intertemporal choices
itc <- timerat[, str_detect(names(timerat), "^(responseid|itc\\d+_\\d+)")] %>%
  melt(id = "responseid") %>%
  filter(!(is.na(value))) %>%
  mutate(ss_time = str_extract(variable, "\\d+"),
         ll_time = str_extract(variable, "\\d+$")) %>%
  group_by(responseid) %>%
    merge(diss_id, by = c("ss_time", "ll_time"))
  
  
itc_sum <- itc %>%
  group_by(diss_set, delayed) %>%
  summarise(n_ll = sum(value == "2"),
            n_tot = n()) %>%
  mutate(per_ll = n_ll/n_tot) %>%
  group_by(diss_set, delayed) %>%
  do( tidy(prop.test(.$n_ll, .$n_tot)))


ggplot(itc_sum,
       aes(x = diss_set, y = estimate, group = delayed, ymax = conf.high,  ymin = conf.low)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(position = "dodge")
  

save.image("analyzed_data/analyzed_data.Rdata")
