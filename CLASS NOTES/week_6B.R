install.packages("readr")
library(readr)
# read in, and format the frequency data
df <- readr::read_delim("../DATABASE/word_forms_stems_and_frequencies_full.txt", delim = "\t" , comment = "#",
                        col_names = c("wordform", "stem", "morph", "frequency"))
#we have to define what delim is.
#we have a comment in it with hash. so we have to define that too.
#define column names
head(df)
library(dplyr)
morphNoun <- dplyr::filter(df, morph == "Noun+A3sg+P3sg+Nom")

#we want to select the top ten thousand, first we need to order them
library(magrittr)
df %<>% dplyr::arrange(frequency)
### df <- dplyr::arrange(df, frequency)
## but this gave us the least frequent ones at the top. we want the top ones first.
df %<>% dplyr::arrange(desc(frequency))
head(df)
#do nrow for the rows.
nrow(df)

#sum for token frequency

sum(df$frequency)
df$frequency_per_million <- 10^6 * df$frequency / sum(df$frequency)
# extract a subset to work with
df_analysis <- head(df, 10000)
#df_analysis <- df[1:10000]
#deternmine the length
install.packages("stringr")
library(stringr)
df_analysis$wordform_length <- stringr::str_length(df_analysis$wordform)
df_analysis$system_length <- stringr::str_length(df_analysis$stem)

head(df_analysis)


# create a log-frequency column
df_analysis$log_frequency_per_million <- log(df_analysis$frequency_per_million)

idx_sample <- sample(1:nrow(df_analysis), 100)
library(ggplot2)

ggplot(df_analysis[idx_sample,], aes(wordform_length, frequency_per_million)) + geom_point()

# not this one, but we need a log one, that s the meaningful one.
ggplot(df_analysis, aes(wordform_length, log_frequency_per_million)) + geom_point()

# fit a linear model for log(frequency) ~ length
library(rethinking)
data_model_definition_freq_len <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million,
  wordform_length = df_analysis$wordform_length
)

data_model_definition_freq_len_subset <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million[idx_sample],
  wordform_length = df_analysis$wordform_length[idx_sample]
)

data_model_definition_freq_len_subset_standardized <- list(
  log_frequency_per_million = df_analysis$log_frequency_per_million[idx_sample],
  wordform_length = scale(df_analysis$wordform_length[idx_sample], center = T, scale = F)
)

model_definition_freq_len <- alist(
  log_frequency_per_million ~ dnorm(mean = mu_predicted, sd = sigma),
  mu_predicted <- intercept + slope * wordform_length,
  intercept ~ dunif(0, 11),
  slope ~ dunif(-10 , 0),
  sigma ~ dunif(0,10)
)

m <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len)
m_subset <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len_subset)
m_subset_scale <- rethinking::map(model_definition_freq_len, data = data_model_definition_freq_len_subset_standardized)


summary(m)
precis(m)

precis(m_subset_scale, corr = T)
precis(m_subset, corr = T)


m_subsetscaleposterior <- extract.samples(m_subset_scale)
plot(m_subsetscaleposterior)


summary(m_subset)
precis(m_subset)



# hmmm, maybe all these morphemes are obscuring things?

### let's determine the root frequencies then

# let's determine which elements are multimorphemic (grepl)

# let's determine how many morphemes are in each word form (gregexpr)

# let's run another linear model

# what's with all the correlations???


# let's return to Alice's data

### so is there an effect of animacy or what

# model 1

# model 2

# model 3
