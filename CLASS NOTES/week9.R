library(dplyr)
library(magrittr)
library(rethinking)
library(languageR)

data(dative)

dative$RealizationRec_isNP <- ifelse(dative$RealizationOfRecipient=="NP", 1, 0)
dative %<>% mutate(cAnimacyRec_animate = ifelse(AnimacyOfRec=="animate", 1, 0), 
                   cAnimacyTheme_animate = ifelse(AnimacyOfTheme=="animate", 1, 0) )
dative_essential <- dative %>% dplyr::select(RealizationRec_isNP, cAnimacyRec_animate, cAnimacyTheme_animate)

dative_aggregated <- dative_essential %>% group_by(cAnimacyRec_animate, cAnimacyTheme_animate)  %>% 
  summarize( mean = mean(RealizationRec_isNP) , 
             N_Total = length(RealizationRec_isNP)) 
dative_aggregated$NPs <- dative_aggregated$mean * dative_aggregated$N_Total

# we may want to test if there is a correlation between them.
# we know the Animacy Th and Animacy Rec have an effect on Realization.
# but do these two modify each other?
# we need to have an interaction of them as a parameter.

dative_aggregated$thRecInt <- with(dative_aggregated, cAnimacyRec_animate * cAnimacyTheme_animate)
dative_aggregated

# model, now we add the interaction too 
modelTreatmentContrastDef <- alist(
  NPs ~ dbinom( N_Total , p_predicted ) ,
  logit( p_predicted ) <- intercept + 
    slopeTh * cAnimacyTheme_animate + 
    slopeRec * cAnimacyRec_animate + 
    slopeInt * thRecInt ,
  intercept ~ dnorm( 0 , 1 ) ,
  slopeTh ~ dnorm( 0 , 1 ) ,
  slopeRec ~ dnorm( 0 , 1 ) ,
  slopeInt ~ dnorm( 0 , 1 )
)
m10 <- map( modelTreatmentContrastDef , data = as.list( dative_aggregated ) )
precis( m10 , corr = T) %>% plot()


# slope of theme is only there when the recipient is inanimate. 
# when the recipient is animate, the difference between the R+Th- and R+Th+ is slopeTh plus slopeInt