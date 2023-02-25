### Data Prep script ----

# Data prep/cleaning ----
dd <-  dd_raw %>%
  # Reducing status to 0/1 from 1/2
  mutate(across(status , ~. - 1)) %>%
  # Setting levels of ph.ecog variable
  mutate(across(ph.ecog, ~factor(., levels = c(0:4)))) %>%
  # Setting levels of ph.karno variable
  mutate(across(ph.karno, ~factor(., levels = seq(50, 100, 10)))) %>%
  # Setting levels of pat.karno variable
  mutate(across(pat.karno, ~factor(., levels = seq(30, 100, 10)))) %>%
  # Converting sex to factor, keeping as 1/2
  mutate(across(sex, ~factor(as.character(.), levels = c(1, 2), 
                                          labels = c("Male", "Female")))) %>%
  # Make survival time in the same units as age, to include age in 
  # survival analysis accounting for left-truncation
  mutate(across(time, ~./365.25))

# Exploratory Analysis ----
## Examine presence of missing values
n_nas <- data.frame(
  do.call('rbind', apply(dd, 2, function(x){table(is.na(x))}))
)

names(n_nas) <- c("Non-Missing", "Missing")

# Output NAs as a table
n_nas %>%
  kable(format = 'latex') %>%
  kable_styling ()

# Note: Will not impute missing values

# Creating boxplot to examine distributions of variables, 
dd %>%
  select(sex, all_of(cont_demo_vars),
         all_of(cat_demo_vars)) %>%
  # Converting sex to character
  mutate(across(sex ,as.character)) %>%
  # Converting other variables to numeric
  mutate(across(age:pat.karno, as.numeric)) %>%
  # Making data long to use facet wrap
  pivot_longer(names(.)[-1]) %>%
  # Passing to ggplot
  ggplot(aes(x = value, fill = sex, group = sex)) +
  geom_histogram(position = 'dodge') + 
  facet_wrap(. ~ name, scales = 'free')

# It appears that Males had slightly higher levels of 
# self reported Karnofsky scores compared
# to females

# Plotting demographic and score variables by survival status
dd %>%
  select(status, all_of(cont_demo_vars),
         all_of(cat_demo_vars)) %>%
  select(-sex) %>%
  mutate(across(status ,as.character)) %>%
  mutate(across(age:pat.karno, as.numeric)) %>%
  pivot_longer(names(.)[-1]) %>%
  ggplot(aes(x = value, fill = status, group = status)) +
  geom_histogram(position = 'dodge') + 
  facet_wrap(. ~ name, scales = 'free')

# Looping over continuous variables, using t.test to determine 
# if there is a difference between males and females in these variables
t_test_ls <- lapply(cont_demo_vars, function(var){
  test_dd <- dd %>%
    select(sex, out = one_of(var)) 
    
  res <- t.test(out ~ sex, data = test_dd, paired= FALSE)
  dat <- data.frame(
    var = var, 
    p_val = res$p.value
  )
  row.names(dat) <- NULL
  dat
  })

# Combining list into dataframe
test_dd <- do.call('rbind', t_test_ls)


### Fairly even distributions across continuous variables
#' between sex. If anything, age, meal.cal, and wt.loss have 
#' some difference between groups, but these seem to make 
#' some intuitive sense

# Defining performance metric variables
perform_metrics <- c("ph.ecog", 
                     "ph.karno", 
                     "pat.karno")

# Using chi-squared test to determine if there are differences between males/females 
# in these variables
chi_sq_metric_by_sex <- lapply(perform_metrics, function(metric) {
  
  tab <- dd %>%
    select(sex, all_of(metric)) %>%
    mutate(across(all_of(metric), factor)) %>%
    group_by_at(names(.)) %>%
    dplyr::summarise(n = n()) %>%
    filter(!is.na(get(metric))) %>%
    spread(sex, n, fill = 0) %>%
    as.data.frame()
  print(metric)
  row.names(tab) <- as.character(tab[, 1])
  tab[, 1] <- NULL
  
  res <- chisq.test(tab)
  data.frame(
    var = metric, 
    p_val = res$p.value
  )
  
})

chi_sq_metric_by_sex

chi_sq_metric_by_status <- lapply(perform_metrics, function(metric) {
  
  tab <- dd %>%
    select(status, all_of(metric)) %>%
    mutate(across(all_of(metric), factor)) %>%
    group_by_at(names(.)) %>%
    dplyr::summarise(n = n()) %>%
    filter(!is.na(get(metric))) %>%
    spread(status, n, fill = 0) %>%
    as.data.frame()
  print(metric)
  row.names(tab) <- as.character(tab[, 1])
  tab[, 1] <- NULL
  
  res <- chisq.test(tab)
  data.frame(
    var = metric, 
    p_val = res$p.value
  )
  
})

chi_sq_metric_by_status

# Questions ----
## Question 1 ----
#' How many different combinations of ph.ecog and ph.karno are in the data? Based on the
#' definitions used by the two scoring systems, are the results aligned? What do you think
#' explains these results?

# Using summarize to find combinations of these variables
sum_mat <- dd_raw %>%
  group_by(ph.ecog, ph.karno) %>%
  dplyr::summarise(n = n()) 

# Determining which rows had at least one observation
unique_combinations <-   sum_mat %>%
  filter(n > 0) %>%
  arrange(desc(n))

# Looking at scatterplot in order to compare ph.ecog and ph.karno
dat <- dd %>%
  select(ph.ecog, ph.karno) %>%
  na.omit()

# Determine spearman rank correlation coefficient between the two variables
cor_val <- cor(as.numeric(dat$ph.ecog), as.numeric(dat$ph.karno), 
           method = 'spearman')

# Plot ph.ecog and ph.karno as a scatterplot with correlation coefficient
q1_plot <- dd %>%
  select(ph.ecog, ph.karno) %>%
  ggplot(aes(x = ph.ecog, y = ph.karno)) + 
  geom_point(position = position_jitter(width = .1, height = 1)) + 
  geom_label(x = "3", y = "100", label = round(cor_val ,2))

# Creating barplot comparing overall distribution of the two measures
dd %>%
  select(sex, ph.karno, ph.ecog) %>%
  pivot_longer(names(.)[-1]) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(stat = 'count') +
  facet_wrap(. ~ name, scales = 'free')

## Question 2 ----
#' Provide a Kaplan–Meier plot for overall survival stratified by sex. Include axis labels, a 
#' median survival line, and an at risk table below the plot. Add any other features you think 
#' are helpful.

# Fitting original model, non-stratified and without left truncation
fit_overall <- survfit(Surv(time, status) ~ 1, data = dd)

# Fitting original model, stratified and without left truncation
fit_0 <- survfit(Surv(time , status) ~ sex, 
               data = dd)

# Using ggsurvplot to plot Kaplan Meier curves for model without left truncation
kap_plot_no_lt <- ggsurvplot(fit_0, 
                             data = dd,
                             conf.int = TRUE,
                             risk.table = TRUE,
                             pval = TRUE,  
                             surv.median.line = 'h')

# Getting the median survival times from the survival model summary tables
sum_overall <- data.frame(t(summary(fit_overall)$table))
sum_grps <- data.frame(summary(fit_0)$table)

# Combining into a table
sum_both <- rbind(sum_overall, sum_grps)

### Median survival times ----
med_table <- data.frame(
  group = c("Overall", row.names(sum_grps)), 
  med_time = sum_both$median
 ) %>%
  kable(col.names = c("Group", "Median Survival Time (years)"),
        digits = 2, 
        format = 'latex')


## Question 3 ----
#' A potential source of bias is if patients may enter the study after time 0 and are excluded if 
#' they died after time 0 but prior to entering the study (left truncation). Why is this a 
#' problem, and how would you address it?

# Fit model accounting for left truncation
fit_left_trunc <- survfit(Surv(time, 
                               age+time,  
                               status) ~ sex, data = dd)

# Use score test from coxph model, to get p-value equivalent to log-rank test
# between strata
fit0 <- coxph(Surv(time,
                   age+time,
                   status) ~ factor(sex),
              data = dd)

# Testing proportional hazards assumption
test.ph <- cox.zph(fit0)

# Using assertthat to stop the code if the proportional hazards assumption is not met
assertthat::assert_that(!any(test.ph$table[,'p'] <= 0.05), 
                        msg = "Proportional Hazards Assumption Not met")

# getting summary object of fit
sum <- summary(fit0)

# getting p_val from score test
p_val <- round(sum$sctest[3], 3)

# Including kaplan meier curves from model including left truncation, 
# including p-value where test statistic comes from coxph score test
kap_plot_left_trunc <- ggsurvplot(fit_left_trunc, 
           conf.int = TRUE,
           risk.table = TRUE,
           pval = round(p_val, 3))


## Question 4 -----
#' Are there any other interesting patterns in the data? Visualize or quantify one or more of
#' these
#' 


### Looking at agreement between patient and physician ----
cont_tab <- dd %>%
  # Creating pseudo_id to use in pivot_wider
  mutate(id = 1:n()) %>%
  # selecting variables of interest
  select(id, ph.karno, pat.karno) %>%
  pivot_longer(names(.)[-1]) %>%
  filter(!is.na(value)) %>%
  # Setting overall factor level for both scores
  mutate(value = factor(value, seq(30, 100, 10))) %>%
  pivot_wider() %>%
  # Removing NAs
  na.omit() 

# Plotting patient and physician scores as histogram
karno_bar_plot <- cont_tab %>%
  # Making long to summarize by variable
  pivot_longer(names(.)[-1]) %>%
  group_by(name, value) %>%
  # calculating count by group
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  group_by(name) %>%
  # calculating percentage by group
  mutate(pct = n/sum(n)) %>%
  # passing to ggplot
  ggplot(aes(x = value, y = pct, fill = name)) + 
  geom_histogram(stat = 'identity', position = 'dodge') + 
  # Adding labels
  xlab('score') +
  scale_fill_discrete('Rater')

# Calculating spearman rank correlation coefficient between patient and physician
cor_val <- cor(as.numeric(cont_tab$ph.karno), 
           as.numeric(cont_tab$pat.karno), 
           method = 'spearman')

# Plotting measures as a scatterplot, with correlation coefficient
corr_plot <- dd %>%
  ggplot(aes(x = ph.karno, 
             y = pat.karno)) + 
  geom_point(position = position_jitter(width = 0.5, height = 0.5)) +
  geom_text(x = "50", y = "40", color = 'red', label = round(cor_val, 2)) + 
  ggtitle("Patient vs. Physician Karnofsky score")


### Looking at performance score metrics ----
# Chi-square analysis revealed no relationship between sex and the performance rating, 
# but there was a relationship between survival and performance rating (either by patient or physician)
# Additionally, there was not a relationship between kaplan meier curves stratified 
# by sex, after accounting for left-truncation

# independent variables to use in univariate coxph models
ind_vars <- c(
  "sex", 
  "ph.ecog", 
  "ph.karno", 
  "pat.karno", 
  "meal.cal", 
  "wt.loss"
)


# Checking that there are at least 1 observation in each of dead/censored
# in each level of the performance scores
# Since there is only one observation in the censored group in ph.ecog == 3 group,
# This value will be set to NA so it is not included in the coxph analysis

dd %>%
  select(status, one_of(perform_metrics)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(names(.)[-1]) %>%
  group_by(status, value, name) %>%
  dplyr::summarise(n = sum(!is.na(value))) %>%
  spread(status, n) %>%
  arrange(name) %>%
  filter(is.na(`0`) | is.na(`1`))

test_dd <- dd %>%
  mutate(ph.ecog = factor(ph.ecog, levels = c(0:2)))

hr_cox_ls <- lapply(ind_vars, function(x) {
  
  # Subset data to only include variables of interest
  sub_dd <- test_dd %>%
    select(status, age, time,  one_of(x)) 
  
  # Creating formula
  f <- as.formula(paste0("Surv(time, age+time, status) ~ ", factor(x)))
  
  # running coxph model
  res <- coxph(f, 
               data = sub_dd)
  
  # testing proportional hazards assumption
  test.ph <- cox.zph(res)
  
  # Breaking script if assumption not met
  assertthat::assert_that(!any(test.ph$table[,'p'] <= 0.05), 
                          msg = "Proportional Hazards Assumption Not met")
  
  # Getting summary table
  sum <- summary(res) 
  
  # Getting hazard ratio
  hr <- sum$coefficients[, 2]
  
  # Getting standard error
  se <- sum$coefficients[, 3]
  
  # calculating upper and lower bounds
  lb <- hr-qnorm(1-0.05/2)*se
  ub <- hr+qnorm(1-0.05/2)*se
  
  data.frame(
    var = x, 
    lvl = names(res$coefficients),
    hr = hr, 
    lb = lb, 
    ub = ub, 
    hr_pval = sum$coefficients[, 5]
    )
  
})

# Turning list into dataframe, changing labels of the variables and levels
hr_cox_dd <- do.call('rbind', hr_cox_ls) %>%
  mutate(lvl = gsub(paste0(var, collapse = '|'), "",lvl), 
         var = ifelse(lvl == '', var, paste(var, lvl, sep = '=')), 
         var = factor(var))

# Passing data to a forrest plot, with 95% confidence intervals around hazard ratio estimate
hr_plot <-
  ggplot(data = hr_cox_dd, aes(x = hr, y = var)) +
  geom_errorbar(aes(xmin = lb, xmax = ub)) + 
    geom_vline(xintercept = 1, color = 'red', linetype = 'dashed') + 
    geom_text(x = 5, aes(label = paste0("p=", round(hr_pval, 2)))) +
    scale_x_continuous(limits = c(-3,6)) + 
  xlab("Hazard Ratio (95% CI)") + 
  ylab("Variable (level)")

### Log-Rank tests for performance metrics ----
#' Univariate analyses of the different performance metrics showed 
#' that no hazard ratios were significantly different than 1
#' Log-rank tests will be used to test for overall differences between 
#' survival curves.
#' Coxph score test will be used to calculate test statistic

log_rank_test_ls <- lapply(perform_metrics, function(x) {
  
  # Defining formula
  f <- as.formula(paste0("Surv(time, age+time, status) ~ factor(", x, ")"))
  
  # Running coxph model
  fit0 <- coxph(f, 
                data = test_dd,
                # iter = 0,
                na.action = na.exclude)
  
  # Testing proportional hazards assumption
  test.ph <- cox.zph(fit0)

  # Breaking script if assumption not met  
  assertthat::assert_that(!any(test.ph$table[,'p'] <= 0.05), 
                          msg = "Proportional Hazards Assumption Not met")
  
  # Getting summary table
  sum <- summary(fit0)
  
  # Getting p-value from score test
  p_val <- round(sum$sctest[3], 3)
  
  data.frame(
    var = x, 
    p_val = p_val
  )
  
})

# Turning list to a dataframe
log_rank_test_dd <- do.call('rbind', log_rank_test_ls)

# Creating plots for each variable, with p-value from test dataframe
p_1 <- ggsurvplot(survfit(Surv(time, 
                               age+time,
                               status) ~ ph.ecog, data = test_dd), 
                  conf.int = TRUE,
                  pval = log_rank_test_dd$p_val[1])

p_2 <- ggsurvplot(survfit(Surv(time, 
                               age+time,  
                               status) ~ ph.karno, data = test_dd), 
                  conf.int = TRUE,
                  pval = log_rank_test_dd$p_val[2])


p_3 <- ggsurvplot(survfit(Surv(time, 
                               age+time,  
                               status) ~ pat.karno, data = test_dd), 
                  conf.int = TRUE,
                  pval = log_rank_test_dd$p_val[3])


### Performance scores continuous variables ----
#' "Median" split on performance metrics since there are many groups in the
#' pat.karno and ph.karno variables

med_dd <-  dd %>%
  # Selecting variables
  select(status, age, time, all_of(perform_metrics)) %>%
  # Turning performance metrics to numeric
  mutate(across(ph.ecog:pat.karno, as.numeric)) %>%
  pivot_longer(ph.ecog:pat.karno) %>%
  group_by(name) %>%
  # Getting median value by each variable
  dplyr::summarise(med = median(value, na.rm = TRUE)) %>%
  group_by(name) %>%
  # Getting the actual value of the reference level
  mutate(med_val = levels(dd[, name])[med]) %>%
  ungroup() 

# Creating test dataframe, with new median split variable
test_dd <- dd %>%
  select(status, age, time, all_of(perform_metrics)) %>%
  mutate(across(ph.ecog:pat.karno, as.numeric)) %>%
  # Creating pseudo-id column to use in pivot_wider
  mutate(id = 1:n()) %>%
  pivot_longer(ph.ecog:pat.karno)  %>%
  # Pulling in medians
  left_join(med_dd, by = c("name")) %>%
  mutate(value = as.numeric(value)) %>%
  # Creating median split variable
  mutate(med_split = case_when(
    value < med ~ paste('Less than', med_val),
    value >= med ~ paste('Greater than or equal to', med_val)
    )) %>%
  select(id, status, age, time, name, med_split) %>%
  pivot_wider(values_from = 'med_split')

# looping over performance metrics, testing difference between variables
log_rank_test_ls <- lapply(perform_metrics, function(x) {
  
  # Subset data to variables of interest
  sub_dd <- test_dd %>%
    select(id:time, x = all_of(x))
  
  # Fitting coxph model
  fit0 <- coxph(Surv(time, 
                     age+time,
                     status) ~ factor(x), 
                data = sub_dd,
                na.action = na.exclude)
  
  # Testing coxph assumption
  test.ph <- cox.zph(fit0)
  assertthat::assert_that(!any(test.ph$table[,'p'] <= 0.05), 
                          msg = "Proportional Hazards Assumption Not met")
  
  # Getting p-value
  sum <- summary(fit0)
  p_val <- round(sum$sctest[3], 3)
  
  dat <- data.frame(
    var = x, 
    p_val = p_val
  )
  dat
  })

# turning list to dataframe
log_rank_test_dd <- do.call('rbind', log_rank_test_ls)
  
# Creating plots on the median split variables, with p-value added
p_1_med_split <- ggsurvplot(survfit(Surv(time, 
                          age+time,
                          status) ~ ph.ecog, 
                     data = test_dd),
                     pval = subset(log_rank_test_dd, 
                                   var == "ph.ecog", 
                                   p_val)[[1]],
                     conf.int = TRUE)

p_2_med_split <- ggsurvplot(survfit(Surv(time, 
                               age+time,
                               status) ~ ph.karno, 
                          data = test_dd),
                  pval = subset(log_rank_test_dd, 
                                var == "ph.karno", 
                                p_val)[[1]],
                  conf.int = TRUE)

p_3_med_split <- ggsurvplot(survfit(Surv(time, 
                               age+time,
                               status) ~ pat.karno, 
                          data = test_dd),
                  pval = subset(log_rank_test_dd, 
                                var == "pat.karno", 
                                p_val)[[1]],
                  conf.int = TRUE)

