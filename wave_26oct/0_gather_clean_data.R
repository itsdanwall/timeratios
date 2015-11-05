#--------------
# Gather and clean data for the titrated values for ITC uncertainty
# Daniel Wall
# 3 November 2015
#--------------

setwd("/git_repositories/timeratios/wave_26oct/")
source("load_packages.R")
rm(list = ls())


#----read in the data----

pzwhl <- read_csv("raw_data/prizewheel_21Oct.csv")

# change column names
names(pzwhl) <- tolower(names(pzwhl))

# remove test runs
pzwhl <- pzwhl %>%
  filter(finished == 1)

# get data so I can work on it for when people are switching
diss <- pzwhl[, str_detect(names(pzwhl), "(responseid|rs_\\d_\\d|rb_\\d_\\d)")] %>%
  melt(id = "responseid") %>%
  mutate(diss_type = str_extract(variable, "(rs|rb)") %>%
           factor,
         item = str_extract(variable, "\\d") %>% 
           factor)

# mixed effects model
diss_lmer <- lmer(value ~ diss_type * item + (1|responseid),
                  diss)

Anova(diss_lmer, type = 3)
summary(diss_lmer)

eff_df <- Effect(c("diss_type", "item"), diss_lmer) %>%
  as.data.frame()

ggplot(eff_df, aes(x = item, y = fit, group = diss_type, color = diss_type)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper))


save.image("analyzed_data/analyzed_data.Rdata")
