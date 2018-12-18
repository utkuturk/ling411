# LIBRARIES#####
library(dplyr)
library(ggplot2)
library(magrittr)
library(coda)
library(brms)
library(languageR)

# DATA AND THE MANIPULATION####
data("dative")
dative$corpus_id <- ifelse(dative$Speaker == "NA" 
                           , 1 
                           , 0)

dative %<>% dplyr::select(corpus_id
                          , DefinOfRec
                          , DefinOfTheme
                          , AccessOfRec
                          , AccessOfTheme
                          , RealizationOfRecipient)
dative[is.na(dative)] <- 1
dative$corpus <- ifelse(dative$corpus_id == 1 
                        , "Switchboard" 
                        , "Treebank Wall Street") %>% as.factor()
dative$RR <- ifelse(dative$RealizationOfRecipient == "NP" 
                    , 1 
                    , 0)
dative$DR <- ifelse(dative$DefinOfRec == "definite" 
                    , 1 
                    , 0)
dative$DT <- ifelse(dative$DefinOfTheme == "definite" 
                    , 1 
                    , 0)

# 1 FOR SWITCHBOARD, 0 FOR TREEBANK


# USE HELMERT


# MARGINAL HISTOGRAMS####
ggplot(dative, aes(corpus_id)) + geom_histogram( stat = "count") + facet_grid(~RR)
ggplot(dative, aes(AccessOfTheme)) + geom_histogram( stat = "count") + facet_grid(RealizationOfRecipient~corpus)
ggplot(dative, aes(AccessOfRec)) + geom_histogram( stat = "count") + facet_grid(~RR)
ggplot(dative, aes(DefinOfRec)) + geom_histogram( stat = "count") + facet_grid(~RR)
ggplot(dative, aes(DefinOfTheme)) + geom_histogram( stat = "count") + facet_grid(~RR)


# MARGINAL PLOTS####
sC <- dative %>% group_by(SemanticClass) %>% dplyr::summarize(pRR = mean(RR) , nRR = length(RR)) 
sC %>% ggplot(aes(SemanticClass , pRR)) + theme_light() + geom_bar(stat="identity")

aot <- dative %>% group_by(AccessOfTheme) %>% dplyr::summarize(pRR = mean(RR) , nRR = length(RR)) 
aot %>% ggplot(aes(AccessOfTheme , pRR)) + theme_light() + geom_bar(stat="identity")

aor <- dative %>% group_by(AccessOfRec) %>% dplyr::summarize(pRR = mean(RR) , nRR = length(RR)) 
aor %>% ggplot(aes(AccessOfRec , pRR)) + theme_light() + geom_bar(stat="identity")

dot <- dative %>% group_by(DefinOfTheme) %>% dplyr::summarize(pRR = mean(RR) , nRR = length(RR)) 
dot %>% ggplot(aes(DefinOfTheme , pRR)) + theme_light() + geom_bar(stat="identity")

dor <- dative %>% group_by(DefinOfRec) %>% dplyr::summarize(pRR = mean(RR) , nRR = length(RR)) 
dor %>% ggplot(aes(DefinOfRec , pRR)) + theme_light() + geom_bar(stat="identity")



# 2-VARIABLE INTERACTION PLOTS OVER RR####


##  DEFINITENESS INTERACTION
i_def <- dative %>% group_by(DefinOfRec , DefinOfTheme, corpus) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_def %>% ggplot(aes( x = DefinOfRec , y = nRR , 
                      group = DefinOfTheme , color = DefinOfTheme)) + 
  geom_line() + geom_point()

###   MEAN
i_def %>% ggplot(aes( x = DefinOfRec , y = pRR , 
                      group = DefinOfTheme , fill = DefinOfTheme)) + 
  geom_boxplot()  + facet_grid(~corpus)

##  ACCESS INTERACTION
i_acc <- dative %>% group_by(AccessOfRec , AccessOfTheme, corpus) %>%
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_acc %>% ggplot(aes( x = AccessOfRec , y = nRR , 
                      group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point()

###   MEAN
i_acc %>% ggplot(aes( x = AccessOfRec , y = pRR , 
                      group = AccessOfTheme , fill = AccessOfTheme)) + 
  geom_boxplot()  + facet_grid(~corpus)


##  RECIPIENT INTERACTION
i_rec <- dative %>% group_by(DefinOfRec , AccessOfRec, corpus) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_rec %>% ggplot(aes( x = DefinOfRec , y = nRR , 
                      group = AccessOfRec , color = AccessOfRec)) + 
  geom_line() + geom_point()

###   MEAN
i_rec %>% ggplot(aes( x = DefinOfRec , y = pRR , 
                      group = AccessOfRec , fill = AccessOfRec)) + 
  geom_boxplot()  + facet_grid(~corpus) 

##  THEME INTERACTION
i_thm <- dative %>% group_by(AccessOfTheme , DefinOfTheme) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_thm %>% ggplot(aes( x = DefinOfTheme , y = nRR , 
                      group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point()

###   MEAN
i_thm %>% ggplot(aes( x = DefinOfTheme , y = pRR , 
                      group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point() + facet_grid(~corpus)

### CROSS INTERACTIONS
##  BETWEEM AoR AND DoT
i_AORDOT <- dative %>% group_by(AccessOfRec , DefinOfTheme) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_AORDOT %>% ggplot(aes( x = DefinOfTheme , y = nRR , 
                      group = AccessOfRec , color = AccessOfRec)) + 
  geom_line() + geom_point()

###   MEAN
i_AORDOT %>% ggplot(aes( x = DefinOfTheme , y = pRR , 
                      group = AccessOfRec , color = AccessOfRec)) + 
  geom_line() + geom_point()


##  BETWEEN AoT AND DoR
i_AOTDOR <- dative %>% group_by(AccessOfTheme , DefinOfRec) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_AOTDOR %>% ggplot(aes( x = DefinOfRec , y = nRR , 
                         group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point()

###   MEAN
i_AOTDOR %>% ggplot(aes( x = DefinOfRec , y = pRR , 
                         group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point()
##  BETWEEN everything
i_all <- dative %>% group_by(AccessOfTheme , DefinOfRec, AccessOfRec, DefinOfTheme) %>% 
  dplyr::summarise(pRR = mean(RR) , nRR = length(RR))
###   HOW MANY DATA POINTS 
i_AOTDOR %>% ggplot(aes( x = DefinOfRec , y = nRR , 
                         group = AccessOfTheme , color = AccessOfTheme)) + 
  geom_line() + geom_point()

###   MEAN
i_all %>% ggplot(aes( x = AccessOfTheme , y = pRR , 
                         group = DefinOfRec , color = DefinOfRec)) + 
  geom_line() + geom_point() + facet_wrap(DefinOfTheme~AccessOfRec)

# MODEL ####

contrasts(dative$DefinOfRec) <- contr.sum(2)
contrasts(dative$DefinOfTheme) <- contr.sum(2)
contrasts(dative$AccessOfTheme) <- contr.helmert(3)
contrasts(dative$AccessOfRec) <- contr.helmert(3)
contrasts(dative$RealizationOfRecipient) <- contr.sum(2)
contrasts(dative$corpus) <- contr.sum(2)
contrasts(dative$DefinOfRec) 
contrasts(dative$DefinOfTheme)
contrasts(dative$AccessOfTheme)
contrasts(dative$AccessOfRec)

model0 <- brm(formula = RealizationOfRecipient ~ 1 + DefinOfTheme + AccessOfTheme + DefinOfRec + AccessOfRec  
                      , data = dative 
                      , family = bernoulli("logit") 
                      , chains = 1 
                      , core = 1)

model0.1 <- brm(formula = RealizationOfRecipient ~ 1 + DefinOfTheme * AccessOfTheme + DefinOfRec * AccessOfRec  
                      , data = dative 
                      , family = bernoulli("logit") 
                      , chains = 1 
                      , core = 1)

model0.2 <- brm(formula = RealizationOfRecipient ~ 1 + DefinOfTheme * AccessOfTheme * DefinOfRec * AccessOfRec  
                      , data = dative 
                      , family = bernoulli("logit") 
                      , chains = 1 
                      , core = 1)

model1 <- brm(formula = RealizationOfRecipient ~ 1 + DefinOfTheme * AccessOfTheme + DefinOfRec * AccessOfRec  
                      , data = dative 
                      , family = bernoulli("logit") 
                      , chains = 1 
                      , core = 1)
model2 <- brm(formula = RealizationOfRecipient ~ 1 + DefinOfTheme * AccessOfTheme + DefinOfRec + AccessOfRec + corpus + corpus:DefinOfTheme + corpus:DefinOfRec  
                      , data = dative 
                      , family = bernoulli("logit")
)


library(papaja)
library(lazerhawk)

summary(model, waic = TRUE , LOO = TRUE)
plot(marginal_effects(model))
head(dative)
a <- brms_SummaryTable(model, astrology = TRUE, hype = TRUE , pander = TRUE) %>% as.data.frame()

fixef(model, summary = TRUE , robust = FALSE)
hypothesis(model, "Intercept > 0")
hypothesis(model, "DefinOfRec1:DefinOfTheme1 > 0")