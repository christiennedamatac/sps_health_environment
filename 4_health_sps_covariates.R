# main associations and covariate testing
sum_questionnaires_clean_mean <- read_csv("data/all_subjects_by_questionnaire/sum_questionnaires_clean_mean.csv")
health_outcomes <- c('ASR_sum','ATQ_sum','PSS_sum','SLS_sum_log','IDS_sum_log', 'STS_sum', "UBO_sum", 'HEC_sum_log', 'OCM_sum')

# reorder columns
col_order <- c('ASR_sum','SPS_sum', 'sex_M1_F2', 'BIG_neuroticism_sum', 'BIG_openness_sum')
data <- sum_questionnaires_clean_mean[, col_order]

# adult adhd self-report screener
mod_summaries_ASR <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(ASR_sum ~ ., data[ , c('ASR_sum', predictors_i)]))
}
mod_summaries                            # view results

# autistic trait questionnaire
mod_summaries_ATQ <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(ATQ_sum ~ ., data[ , c('ATQ_sum', predictors_i)]))
}
mod_summaries                            # view results

# perceived stress scale
mod_summaries_PSS <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(PSS_sum ~ ., data[ , c('PSS_sum', predictors_i)]))
}
mod_summaries                            # view results

# satisfaction with life scale
mod_summaries_SLS <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(SLS_sum_log ~ ., data[ , c('SLS_sum_log', predictors_i)]))
}
mod_summaries                            # view results

# inventory of depressive symptoms
mod_summaries_IDS <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(IDS_sum_log ~ ., data[ , c('IDS_sum_log', predictors_i)]))
}
mod_summaries                            # view results

# state and trait anxiety 
mod_summaries_STS_sum <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(STS_sum ~ ., data[ , c('STS_sum', predictors_i)]))
}
mod_summaries                            # view results

# utracht burnout scale
mod_summaries_UBO_sum <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(UBO_sum ~ ., data[ , c('UBO_sum', predictors_i)]))
}
mod_summaries                            # view results

# health complaints
mod_summaries_HEC_sum_log <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(HEC_sum_log ~ ., data[ , c('HEC_sum_log', predictors_i)]))
}
mod_summaries                            # view results

# over-the-counter medication use
mod_summaries_OCM_sum <- list()                  # create empty list  
for(i in 2:ncol(data)) {                 # head of for-loop
  predictors_i <- colnames(data)[2:i]    # create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # store regression model summary in list
    lm(OCM_sum ~ ., data[ , c('OCM_sum', predictors_i)]))
}
mod_summaries                            # view results
