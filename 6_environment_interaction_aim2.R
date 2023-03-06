# import data
data <- sum_questionnaires_clean_mean

health_outcomes <- c('ASR_sum','ATQ_sum','PSS_sum','SLS_sum_log','IDS_sum_log', 'STS_sum', "UBO_sum", 'HEC_sum_log', 'OCM_sum')
environments <- c('CTQ_new_sum', 'DDH_severity', 'DDU_severity', 'LIE_3_sum', 'MDP_sum')

out <- vector('list', length(health_outcomes))

for (h in seq_along(health_outcomes)){
  for (e in seq_along(environments)){
    out[[h]] <- summary(lm(paste(health_outcomes[h],  '~', 'SPS_sum', '+', environments[e], '+', 'SPS_sum*',environments[e]), data = data))
  }
}
out

# adjusted p-values with false-discovery rate
p <- ()
p.adjust(p, method = 'fdr', n = length(p))
