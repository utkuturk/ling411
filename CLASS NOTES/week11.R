setwd("~/Dropbox/Academia/Masters/REMEDIAL 1ST/METHODOLOGY/DATABASE/wals-language.csv")

df <- read.csv("language.csv")

View(df)
library(magrittr)
library(dplyr)

df$iso_code
df$X1A.Consonant.Inventories %>% head()
df$X19A.Presence.of.Uncommon.Consonants %>% head()
unique(as.character(df$X1A.Consonant.Inventories))
unique(as.character(df$X19A.Presence.of.Uncommon.Consonants))

dfEssential <- df %>% dplyr::select(iso_code, Name, 
                                    X1A.Consonant.Inventories, 
                                    X19A.Presence.of.Uncommon.Consonants) %>%
  rename(isoCode = iso_code,
         name = Name,
         consInvSize = X1A.Consonant.Inventories,
         uncommCons = X19A.Presence.of.Uncommon.Consonants)
dfEssential$usableLanguage <- with(dfEssential, consInvSize != "" & uncommCons != "")

head(dfEssential , 10)


dfEssential %>% group_by(usableLanguage) %>% 
  summarize(N = length(usableLanguage)) %>%
  mutate(share = N/sum(N))

dfAnalysis <- dfEssential %>% subset(usableLanguage) %>% dplyr::select(-usableLanguage)

head(dfAnalysis , 10)
View(dfAnalysis)
dfAnalysis %<>% group_by(consInvSize) %>% 
                mutate(hasUncomm = ifelse(uncommCons == "1 None" , 0 , 1))

dfAnalysis %>% group_by(consInvSize) %>% summarize(uncommCons = mean(hasUncomm) , 
                                                   N = length(hasUncomm))

# MORE EXPLANATORY WAY
library(ggplot2)

#Funny one
dfAnalysis %>% group_by(consInvSize , uncommCons) %>% summarize(N = length(consInvSize)) %>%
                                                      ggplot(aes(consInvSize, uncommCons)) + geom_point(aes(size = N))
ggplot(dfAnalysis , aes(uncommCons)) + geom_histogram( stat = "count") + facet_wrap(~consInvSize) 


library(brms)

View(dfAnalysis)

dfAnalysis$consInvSize %<>% as.character() %>% as.factor()

m1 <- brm(hasUncomm ~ consInvSize, data = dfAnalysis , family = bernoulli("logit"))
m1

contrasts(dfAnalysis$consInvSize)
# we have too much difference inside the confidence interval of consInvSize2Moderatelysmall,
# is it because we have too wide intercept?
# how can we be sure about a category's interval if we are not sure about the intercept?

# but we see that average one's confidence interval is above zero for sure.
# so we can say that there is definitely a difference between small and average, yet no between small and moderately small.
# but we compare everything to the small languages in this model. but we are interested in comparing small to moderately small,
# moderately small to average, average to moderately large...
# so we need to change contrasts.

contrasts(dfAnalysis$consInvSize) <- MASS::contr.sdif(5)
# why this is set up this way? verify that.

m2 <- brm(hasUncomm ~ consInvSize, data = dfAnalysis , family = bernoulli("logit"))
m2

# now it is compared to each other row by row. now chech m2. we are only sure about that average one matters, but not the other ones.
# so our sample size is too small.