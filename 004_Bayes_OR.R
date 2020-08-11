## Abraham Apfel
## July 3, 2019


# Bayes model with odds ratio relative to mean

# Create model matrix from original data
X <- fastDummies::dummy_cols(dat, select_columns = c("Site_Name_Formatted", "trial_indication"))
X <- dplyr::select(X, -Site_Name_Formatted, -trial_indication)

# tmp <- bayesglm(QC_pass_fail ~ 0+ ., family = binomial, data = X)



# Transform model matrix into contrast of Site - mean across all sites
Site_dummies <- str_subset(colnames(X), "Site_Name_Formatted")
X <- mutate_at(X, Site_dummies, list(star = ~. - 1/length(Site_dummies)))

# Checks
# X[1:10, 635:642]

# 1-.00166113
# 1/.00166113

# Remove original model matrix covariates
X <- dplyr::select(X, -Site_dummies)

# Run model

set.seed(1432)
bayes_mod <- bayesglm(QC_pass_fail ~ 0+ ., family = binomial, data = X)

# Sample posterior distribution to yield median and 95% Credible Interval
simulates <- coef(sim(bayes_mod, n.sims = 10000))
simulates <- data.frame(simulates[,str_subset(colnames(simulates), "Site_Name_Formatted")])
estimates <- sapply(simulates, quantile, c(0.025, 0.5, 0.975))
estimates <- t(estimates)
estimates <- data.frame(estimates)
estimates <- rownames_to_column(estimates, var = "Site_Name_Formatted")

colnames(estimates) <- c("Site_Name_Formatted", "lower", "fit", "upper")

# Convert ln(OR) to log2(OR) 
estimates <- mutate(estimates, lower = lower/log(2), fit = fit/log(2), upper = upper/log(2))

estimates$Site_Name_Formatted <- str_replace_all(estimates$Site_Name_Formatted, "X.Site_Name_Formatted_", "")
estimates$Site_Name_Formatted <- str_replace_all(estimates$Site_Name_Formatted, "Site_Name_Formatted_", "") # 4 Sites didn't have any spaces and thus had different format
estimates$Site_Name_Formatted <- str_replace_all(estimates$Site_Name_Formatted, "_star.", "")
estimates$Site_Name_Formatted <- str_replace_all(estimates$Site_Name_Formatted, "_star", "") # Ditto
estimates$Site_Name_Formatted <- str_replace_all(estimates$Site_Name_Formatted, "[:punct:]", " ")


# Get prediction dataset with one observation per site
sites <- dat[,"Site_Name_Formatted", drop = F]
sites <- unique(sites)
sites <- inner_join(sites, data.frame(table(Site_Name_Formatted = dat$Site_Name_Formatted)))

estimates <- inner_join(sites, estimates)                    
estimates$Site_Name_Formatted[!(estimates$Site_Name_Formatted %in% sites$Site_Name_Formatted)]

estimates$Site_Name_Formatted <- factor(estimates$Site_Name_Formatted)
estimates <- arrange(estimates, desc(Freq))
estimates$names <- paste(estimates$Site_Name_Formatted, " (N = ", estimates$Freq, ")", sep = "")
estimates$names <- factor(estimates$names, levels = rev(estimates$names))


#making the forest plots
iter.size = 25
p.idx = 1
n = length(levels(estimates$Site_Name_Formatted))

start = 1
end = start + iter.size - 1
p = list()

span = c()
while(end < n) { 
  p[[p.idx]] <- estimates %>%
    dplyr::filter(names %in% estimates$names[start:end]) %>%
    ggplot(aes(y = fit, x = names)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),  
                  position = position_dodge(width = 1), size = 3) +
    geom_point(size = 5, position=position_dodge(width = 1), color = "red")  +
    coord_flip() + 
    geom_hline(yintercept = 0, color = "red", lty = 2) +
    ylab("Estimated log2(Odds Ratio) of Success Relative to Mean (w/ 95% Credible Interval)") +
    xlab("Site") +
    scale_y_continuous(breaks = seq(-6, 8, 1), limits = c(-6, 8)) +
    ggtitle("Estimates of Success from Bayesian Model Adjusting for Indication") +
    theme_bw(20)
  
  span[p.idx] <- length(start:end)
  p.idx = p.idx + 1
  start = end + 1
  end = min(end + iter.size, n)
}


#final plot
p[[p.idx]] <- estimates %>%
  dplyr::filter(names %in% estimates$names[start:end]) %>%
  ggplot(aes(y = fit, x = names)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),  
                position = position_dodge(width = 1), size = 3) +
  geom_point(size = 5, position=position_dodge(width = 1), color = "red")  +
  coord_flip() + 
  geom_hline(yintercept = 0, color = "red", lty = 2) +
  ylab("Estimated log2(Odds Ratio) of Success Relative to Mean (w/ 95% Credible Interval)") +
  xlab("Site") +
  scale_y_continuous(breaks = seq(-6, 8, 1), limits = c(-6, 8)) +
  ggtitle("Estimates of Success from Bayesian Model Adjusting for Indication") +
  theme_bw(20)
span[p.idx] <- length(start:end)


pdf(file.path(results, "Bayes_model_OR.pdf"), height = 16, width = 24)
for (i in seq_len(length(p))) {
  print(p[[i]])
}
dev.off()

