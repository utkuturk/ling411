setwd("~/Dropbox/Academia/Masters/REMEDIAL 1ST/METHODOLOGY/DATABASE")
dfmatrix <- read.csv2("matrix.csv")
View(dfmatrix)

library(brms)
fitmatrix <- brm(formula = rating ~ 1 , data = dfmatrix , family = cumulative)
summary(fitmatrix)

head(dfmatrix$movie)

contrasts(dfmatrix$movie)

contrasts(dfmatrix$movie) <- contr.sum(2)

fit1matrix <- brm(formula = rating ~ 1 + movie, data = dfmatrix , family = cumulative)
summary(fit1matrix)

samples <- rstan::extract(fit1matrix$fit)

mean(samples$b < 0)

fit2matrix <- brm(formula = rating ~ 1 + movie + (1|participant.id), data = dfmatrix , family = cumulative)
# too less data

library(languageR)
data("danish")

ggplot(danish , aes(PrevError , LogRT)) + stat_boxplot()

danish$cPrevError <- ifelse(danish$PrevError == "CORRECT" , -.5 , .5)

fitDanishog <- brm(formula = LogRT ~ 1 + cPrevError, data = danish , chains = 4, cores = 4)
summary(fitDanishog)
exp(6.79)

fitDanish <- brm(formula = LogRT ~ 1 + cPrevError + (1|Subject), data = danish , chains = 4, cores = 4)
summary(fitDanish)

#########################3

library(ggplot2)
library(languageR)
library(magrittr)
library(dplyr)

data(danish)
ggplot(danish , aes(PrevError , LogRT)) + stat_boxplot()
head(danish)

danish$cPrevError <- ifelse(danish$PrevError == "CORRECT" , -.5 , .5)

# plot showing average logRTs after cdorrect and incorrect responses on the previous trial, broken down by gender

danishAverages <- danish %>% group_by(Sex, PrevError) %>%
  summarize(avLogRT = mean(LogRT) , 
            avg_RT = mean(exp(LogRT))) %>% as.data.frame()

plotBase <- danishAverages %>% ggplot(aes(x = PrevError , y = avg_RT))
plotBaseLog <- danishAverages %>% ggplot(aes(x = PrevError , y = avLogRT))
plotLine <- geom_line(aes(group = Sex , color = Sex))
plotDot <- geom_point(aes(group = Sex , color = Sex))

plotBaseLog + plotLine + plotDot

plotBase + plotLine + plotDot

# bar or column plot is not really meaningful because we care about the difference
plotBase + geom_col(aes(group = Sex , fill = Sex) , position = "dodge")
plotBase + geom_bar(aes(group = Sex , fill = Sex) , stat = "identity" , position = "dodge")
plotBaseLog + geom_col(aes(group = Sex , fill = Sex) , position = "dodge")
plotBaseLog + geom_bar(aes(group = Sex , fill = Sex) , stat = "identity" , position = "dodge")

p <- plotBaseLog + plotLine + plotDot + facet_wrap(~Sex)

# there may be participants who have not data in everycell, plot by participant

danishAverages_bySubject <- danish %>% group_by(Subject, PrevError) %>%
  summarize(avLogRT = mean(LogRT) , 
            avg_RT = mean(exp(LogRT))) %>% as.data.frame()
danishAverages_bySubject %>% ggplot(aes(x = PrevError , y = avLogRT , group = Subject)) + geom_line() + geom_point()
danishAverages_bySubject %>% ggplot(aes(x = PrevError , y = avg_RT , group = Subject)) + geom_line() + geom_point()

p <- plotBaseLog + plotLine + plotDot + facet_wrap(~Sex)
print(p)
p_pretty <- p + ggplot2::xlab("") + ylab("Average log RT") + ggtitle("Post Error")
ggsave(p_pretty , file = "./example.pdf" , width = 5 , height = 5 )

# you see 2s21 do not have post error data. we need to exclude 2s21, by subsetting, do it on raw data

danish_Errorsubset <- danish %>% subset(Subject != "2s21")

# if we wanna exclude more than one
# danish_Errorsubset <- danish %>% subset(!Subject %in% c("2s21" , "2s22"))

# removed unused levels
danish_Errorsubset %<>% droplevels()
unique(danish_Errorsubset$Subject) %>% sort()

# now we can work on data again as we have deleted the empty cells, model

danish_Errorsubset$cPrevError <- ifelse(danish_Errorsubset$PrevError == "CORRECT" , -.5 , .5)

# model 1 : no random effects
### (1|Subjects) : varying intercepts by Subjects, we use this when we do not wanna account for this variation

library(brms)
m1 <- brm(formula = LogRT ~ 1 + cPrevError , data = danish_Errorsubset , family = gaussian() , chains = 1)
summary(m1)

plot(m1)
# we could be looking at differences of participants

# model with varying effects
m2 <- brm(formula = LogRT ~ 1 + cPrevError + (1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )

plot(m2)
summary(m2)
summary(m1)

# our model was logRT ~ a + b*cPrevError
# we have to dısregard participant differences
#     so our model now: logRT~ a + a_predicted + b*cPrevError => model m2
# but now we have to disregard stupidly crazy edges and differences
#     so our model now: logRT~ a + a_predicted + (b+b_predicted)*cPrevError => model m3
# we do this to account for the big deviations and also not to deal with them.

m3 <- brm(formula = LogRT ~ 1 + cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
summary(m3)
plot(m3)

# negative correlation in corr(intercept , cPrevError) means that the higher slopes can affect less 
# than the lower ones. but the credible interval is almost 2 which can be max 2. n
# with more participants we will get more clear picture. we have nothing to say about correlation.

# lets include the sex
# get the contrast matrix for sex
contrasts(danish_Errorsubset$Sex)
# set the contrast to sum contrasts
contrasts(danish_Errorsubset$Sex) <- contr.sum(2)

# dont use it
contrasts(danish_Errorsubset$Sex) %<>% contr.sum(nrow(.))


m4 <- brm(formula = LogRT ~ 1 + Sex + cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
# but this model doesnt inform us. The difference is what we wanna now. So interaction.

m5 <- brm(formula = LogRT ~ 1 + Sex * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 4 , core = 4 )
danish_Errorsubset %>% select(Sex, Subject) %>% unique %>% .$Sex %>% summary()

summary(m5)
# model 6: no sex, but word freq categorical
# we will include a 3-way prior
# we will use log word freq

words <- danish_Errorsubset %>% dplyr::select(Word, LogWordFreq) %>% unique()
head(words)
plot(words$LogWordFreq)


# low high or middle word freq

danish_Errorsubset$LogWordFreqCat <- danish_Errorsubset$LogWordFreq %>% Hmisc::cut2(g = 3)
levels(danish_Errorsubset$LogWordFreqCat) <- c("low" , "mid" , "high")

contrasts(danish_Errorsubset$LogWordFreqCat)
contrasts(danish_Errorsubset$LogWordFreqCat) <- contr.sum(3)

m6 <- brm(formula = LogRT ~ 1 + LogWordFreqCat * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m6)
# look at the contrast to get an idea.
# 7th model
# it is always better to use continous variable.
# remember the reason

m7 <- brm(formula = LogRT ~ 1 + LogWordFreq * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m7)

# center the predictor, contrasts affect each other.
m8 <- brm(formula = LogRT ~ 1 + scale(LogWordFreq) * cPrevError + (cPrevError + 1|Subject), 
          data = danish_Errorsubset , family = gaussian() , 
          chains = 1)
summary(m8)
ss