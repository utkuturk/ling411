
# load the 'dative' data.frame from the package 'languageR'

# display the first 10 lines of the data.frame
head(dative, 10)
head(dative, n = 15)
# find out how many rows are in the data.frame
nrow(dative)
# from here on, we will be interested only in AnimacyOfRec, AnimacyOfTheme, and RealizationOfRecipient
# ... let's recode the RealizationOfRecipient column
dative$RealizationOfRecipient_isNP <- ifelse(dative$RealizationOfRecipient == "NP" , T, F)
# ... and drop all other columns, and save the result in 'dative_essential'
##dative_essential <- dplyr::select(dative, AnimacyOfRec, RealizationOfRecipient_isNP)
##dative_essential <- dative %>% dplyr::select(AnimacyOfRec, AnimacyOfTheme, RealizationOfRecipient_isNP)
dative_essential <- dative %>% dplyr::select(., AnimacyOfRec, AnimacyOfTheme, RealizationOfRecipient_isNP)
# let's get an overview of the structure of the new object
summary(dative_essential)
# let's aggregate the new structure up to counts, and save it in 'dative_counts'
dative_essential %>% group_by(AnimacyOfTheme, AnimacyOfRec) %>% summarize(n_isNP = sum(RealizationOfRecipient_isNP))

dative_essential %>% group_by(AnimacyOfTheme, AnimacyOfRec) %>% 
                     summarize( N_isNP = sum(RealizationOfRecipient_isNP),
                                N_isPP = sum(!RealizationOfRecipient_isNP),
                                Nall1 = sum(RealizationOfRecipient_isNP) + sum(!RealizationOfRecipient_isNP),
                                Nall2 = length(RealizationOfRecipient_isNP),
                                N_total = N_isNP + N_isPP
                     )
# let's get the proportions of NP realizations (i) overall, (ii) by AnimacyOfRec, (iii) by AnimacyOfTheme, and (iv) by both
# ... let's compute them from 'dative_essential'
#(i)
mean(dative_essential$RealizationOfRecipient_isNP)
#(ii)
perc_is_NP_byRec <- dative_essential %>% group_by(AnimacyOfRec) %>% summarize( Proportion_ofNP = mean(RealizationOfRecipient_isNP))
#(iii)
perc_is_NP_byTheme <- dative_essential %>% group_by(AnimacyOfTheme) %>% summarize( Proportion_ofNP = mean(RealizationOfRecipient_isNP))
#(iv)
perc_is_NP_byRecAndTheme <- dative_essential %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% summarize(Proportion_ofNP = mean(RealizationOfRecipient_isNP))

# ... let's compute them from 'dative_counts'


# let's visualize them in barplots


ggplot(perc_is_NP_byRec, aes(AnimacyOfRec, Proportion_ofNP)) + geom_bar(stat = "identity")

ggplot(perc_is_NP_byRecAndTheme, aes(AnimacyOfRec, Proportion_ofNP, fill = AnimacyOfTheme, group = AnimacyOfTheme)) + geom_bar(stat = "identity")

ggplot(perc_is_NP_byRecAndTheme, aes(AnimacyOfRec, Proportion_ofNP, fill = AnimacyOfTheme, group = AnimacyOfTheme)) + geom_bar(stat = "identity" , position = position_dodge())

# (optional: let's viualize the sample sizes, too, in separate)


### now let's find out what we should believe in, regarding the rates of NP realizations

# ... overall
# ...... prior, likelihood, unstandardized posterior, proper posterior

# ... only for animate themes
# ...... prior, likelihood, unstandardized posterior, proper posterior






library(languageR)
library(magrittr)
library(dplyr)
library(ggplot2)


data(dative)
help(dative)

dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient))

dative %>% group_by(AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfTheme, p_realization_NP)) + geom_bar(stat = "identity")

dative %>% group_by(AnimacyOfRec) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP)) + geom_bar(stat = "identity")


dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP, fill = AnimacyOfTheme)) + geom_bar(stat = "identity", position = position_dodge())


dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       N = length(RealizationOfRecipient)) %>% 
            ggplot(aes(AnimacyOfRec, p_realization_NP, group = AnimacyOfTheme)) + geom_point() + geom_line() + scale_y_continuous(labels = scales::percent)



# tests for everything
# tests for AnimacyOfTheme == 'animate'

dative %>% group_by() %>% 
            summarize( p_realization_NP = mean(RealizationOfRecipient == "NP"), 
                       n_realization_NP = sum(RealizationOfRecipient == "NP"),
                       N = length(RealizationOfRecipient))

p_grid <- seq(0, 1, .01)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(2414, size = 3263, prob = p_grid)
unstd_posterior <- likelihood * prior
posterior <- unstd_posterior / sum(unstd_posterior)
plot(posterior)

