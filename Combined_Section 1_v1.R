library(tidyverse)
library(lubridate)
library(moments)    # skewness, kurtosis
library(MASS)       # fitdistr (for MASS::fitdistr explicitly)
library(copula)     # copulas
library(writexl)    # optional: export to Excel
library(tseries)    # Jarque-Bera test

set.seed(123)

# Data Prep

btc_raw <- read_csv(
  "/Users/rangiho/Desktop/2025/Y4S1/EC4305/Bitcoin_Prices.csv",
  col_types = cols(
    Date = col_date(format = "%Y-%m-%d"),
    `BTC-USD` = col_double()
  )
)

btc_daily <- btc_raw %>%
  rename(
    date    = Date,
    BTC_USD = `BTC-USD`
  ) %>%
  arrange(date) %>%
  mutate(
    log_return = log(BTC_USD / lag(BTC_USD))
  ) %>%
  filter(!is.na(log_return)) %>%
  transmute(
    date,
    adj_close = BTC_USD,
    log_return
  )

glimpse(btc_daily)

moon_raw <- read_csv("/Users/rangiho/Desktop/2025/Y4S1/EC4305/Moon_Phases.csv")

# Expect columns: datetime (e.g. "2015-01-04 23:53:00"), phaseid
moon_new <- moon_raw %>%
  mutate(
    dt = ymd_hms(datetime)
  ) %>%
  filter(phaseid == 1) %>%    # new moon only
  mutate(
    newmoon_date = as_date(dt)
  ) %>%
  distinct(newmoon_date) %>%
  arrange(newmoon_date) %>%
  rename(date = newmoon_date)

glimpse(moon_new)

mindate <- min(btc_daily$date, na.rm = TRUE)
maxdate <- max(btc_daily$date, na.rm = TRUE)

calendar <- tibble(
  date = seq(from = mindate, to = maxdate, by = "day")
)

daily_moon <- calendar %>%
  left_join(moon_new %>% mutate(is_newmoon = 1L), by = "date") %>%
  mutate(
    is_newmoon = replace_na(is_newmoon, 0L),
    nm_date    = if_else(is_newmoon == 1L, date, as.Date(NA))
  )

daily_moon <- daily_moon %>%
  arrange(date) %>%
  mutate(nm_prev = nm_date) %>%
  fill(nm_prev, .direction = "down")

daily_moon <- daily_moon %>%
  arrange(desc(date)) %>%
  mutate(nm_next = nm_date) %>%
  fill(nm_next, .direction = "down") %>%
  arrange(date)

daily_moon <- daily_moon %>%
  filter(!is.na(nm_prev), !is.na(nm_next))

daily_moon <- daily_moon %>%
  mutate(
    cycle_frac = as.numeric(date - nm_prev) /
      as.numeric(nm_next - nm_prev),
    cycle_frac = if_else(is.nan(cycle_frac), 0, cycle_frac)
  ) %>%
  dplyr::select(date, cycle_frac)

glimpse(daily_moon)

#merging both
btc_moon <- btc_daily %>%
  inner_join(daily_moon, by = "date") %>%
  arrange(date)

glimpse(btc_moon)

# optional: to preview
# saveRDS(btc_moon, "btc_moon_master.rds")
# write_csv(btc_moon, "btc_moon_master.csv")
# write_xlsx(btc_moon, "btc_moon_master.xlsx")

# 2

data <- btc_moon %>%
  arrange(date) %>%
  mutate(
    up_day    = if_else(log_return > 0, 1L, 0L),
    abs_ret   = abs(log_return),
    sq_ret    = log_return^2,
    large_2   = if_else(abs_ret > 0.02, 1L, 0L),
    large_5   = if_else(abs_ret > 0.05, 1L, 0L),
    large_10  = if_else(abs_ret > 0.10, 1L, 0L)
  )

# 3) basic estimation: mean, variance, CIs

# 3.1 Point estimates 
summary_stats <- data %>%
  summarise(
    n          = n(),
    mean_ret   = mean(log_return),
    sd_ret     = sd(log_return),
    var_ret    = var(log_return),
    skew_ret   = skewness(log_return),
    kurt_ret   = kurtosis(log_return)
  )

print("Unconditional summary statistics for daily log returns:")
print(summary_stats)

# 3.2 CI for mean return (t-interval) 
n <- nrow(data)
xbar <- mean(data$log_return)
s    <- sd(data$log_return)
alpha <- 0.05
tcrit <- qt(1 - alpha/2, df = n - 1)

ci_mean <- c(
  xbar - tcrit * s / sqrt(n),
  xbar + tcrit * s / sqrt(n)
)

cat("95% CI for mean daily log return:\n",
    "(", ci_mean[1], ", ", ci_mean[2], ")\n")

# 3.3 CI for variance (chi-square)
chisq_low  <- qchisq(1 - alpha/2, df = n - 1)
chisq_high <- qchisq(alpha/2,     df = n - 1)
ci_var <- c(
  (n - 1) * s^2 / chisq_low,
  (n - 1) * s^2 / chisq_high
)
cat("95% CI for variance of daily log return:\n",
    "(", ci_var[1], ", ", ci_var[2], ")\n")

# 3.4 Proportion of up days + CI 
p_hat <- mean(data$up_day)
zcrit <- qnorm(1 - alpha/2)
se_p  <- sqrt(p_hat * (1 - p_hat) / n)
ci_p  <- c(
  p_hat - zcrit * se_p,
  p_hat + zcrit * se_p
)

cat("Estimated P(R_t > 0) =", round(p_hat, 4), "\n")
cat("Approx. 95% CI for P(R_t > 0): (",
    ci_p[1], ", ", ci_p[2], ")\n")

# 4) DISTRIBUTION TESTS (normality etc.)

# use subsample for Shapiro-Wilk if n is huge
set.seed(123)
sample_size <- min(5000, n)
sample_ret  <- sample(data$log_return, size = sample_size)

cat("\nShapiro-Wilk normality test (subsample):\n")
print(shapiro.test(sample_ret))

cat("\nJarque-Bera test for normality:\n")
print(jarque.bera.test(data$log_return))

# KS test against fitted normal (using sample mean/sd)
mu_hat <- xbar
sd_hat <- s

cat("\nKolmogorov-Smirnov test vs Normal(mean, sd):\n")
print(ks.test(data$log_return, "pnorm", mean = mu_hat, sd = sd_hat))

# 5. Conditional Probailities by cycle_frac

# 5.1 Quartile bins of cycle_frac 
data <- data %>%
  mutate(
    cycle_quartile = cut(
      cycle_frac,
      breaks = quantile(cycle_frac, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("Q1_early", "Q2", "Q3", "Q4_late")
    )
  )

quartile_prob <- data %>%
  group_by(cycle_quartile) %>%
  summarise(
    n_days       = n(),
    p_up         = mean(up_day),
    mean_ret     = mean(log_return),
    var_ret      = var(log_return),
    mean_abs_ret = mean(abs_ret),
    p_large_2    = mean(large_2),
    p_large_5    = mean(large_5),
    p_large_10   = mean(large_10),
    .groups = "drop"
  )

print("Conditional probabilities & moments by cycle_quartile:")
print(quartile_prob)

# 5.2 Law of total probability on P(Up) 
ltp_quartile <- data %>%
  group_by(cycle_quartile) %>%
  summarise(
    n_days = n(),
    p_group = n_days / n,
    p_up_given_group = mean(up_day),
    .groups = "drop"
  ) %>%
  mutate(
    contribution = p_group * p_up_given_group
  )

print("Law of total probability decomposition of P(Up):")
print(ltp_quartile)
cat("Sum of contributions (should ≈ P(Up)) =",
    sum(ltp_quartile$contribution), "\n")

# 5.3 Law of total variance using quartiles 
overall_var <- var(data$log_return)

var_within <- data %>%
  group_by(cycle_quartile) %>%
  summarise(
    n_k      = n(),
    var_k    = var(log_return),
    mean_k   = mean(log_return),
    weight_k = n_k / n,
    .groups = "drop"
  )

E_var_cond <- sum(var_within$weight_k * var_within$var_k)
Var_E_cond <- sum(var_within$weight_k * (var_within$mean_k - xbar)^2)

cat("Overall Var(R)             =", overall_var, "\n")
cat("E[Var(R|Q)]                =", E_var_cond, "\n")
cat("Var(E[R|Q])                =", Var_E_cond, "\n")
cat("E[Var] + Var[E] (check)    =", E_var_cond + Var_E_cond, "\n")

#6 tail risk: VaR & ES (overall and by lunar windows)

VaR_ES <- function(x, alpha = 0.95) {
  q <- quantile(x, probs = 1 - alpha, na.rm = TRUE)
  es <- mean(x[x <= q], na.rm = TRUE)
  tibble(
    alpha = alpha,
    VaR   = as.numeric(q),
    ES    = es
  )
}

overall_tail_95 <- VaR_ES(data$log_return, alpha = 0.95)
overall_tail_99 <- VaR_ES(data$log_return, alpha = 0.99)

cat("\nOverall VaR/ES:\n")
print(overall_tail_95)
print(overall_tail_99)

# define new/full-like windows from cycle_frac
data <- data %>%
  mutate(
    is_new_window  = if_else(cycle_frac <= 0.1 | cycle_frac >= 0.9, 1L, 0L),
    is_full_window = if_else(cycle_frac >= 0.4 & cycle_frac <= 0.6, 1L, 0L),
    moon_window = case_when(
      is_full_window == 1L ~ "Full_like",
      is_new_window  == 1L ~ "New_like",
      TRUE                 ~ "Middle_cycle"
    )
  )

tail_by_window <- data %>%
  group_by(moon_window) %>%
  group_modify(~ bind_rows(
    VaR_ES(.x$log_return, alpha = 0.95),
    VaR_ES(.x$log_return, alpha = 0.99)
  )) %>%
  ungroup()

cat("\nVaR/ES by moon_window:\n")
print(tail_by_window)

# 7) EVENT-WINDOW PROBABILITIES (3-day blocks)

data <- data %>%
  arrange(date) %>%
  mutate(
    any_large5_next3 = purrr::map_int(
      seq_len(n()),
      ~ as.integer(any(large_5[.x:min(.x + 2, n())] == 1L))
    )
  )

p_large5_any_3day <- mean(data$any_large5_next3)
cat("\nP(at least one |R|>5% in a 3-day window) ≈",
    round(p_large5_any_3day, 4), "\n")

p_large5_full3 <- data %>%
  filter(moon_window == "Full_like") %>%
  summarise(p = mean(any_large5_next3)) %>%
  pull(p)

p_large5_nonfull3 <- data %>%
  filter(moon_window != "Full_like") %>%
  summarise(p = mean(any_large5_next3)) %>%
  pull(p)

cat("P(large move in 3-day window | Full_like)   ≈",
    round(p_large5_full3, 4), "\n")
cat("P(large move in 3-day window | Non-full)   ≈",
    round(p_large5_nonfull3, 4), "\n")

# 8) DEPENDENCE & TRANSITION PROBABILITIES

# ACF plots (if running interactively)
acf(data$log_return, main = "ACF of log returns")
acf(data$abs_ret,    main = "ACF of absolute log returns")

# Transition probabilities for up/down
data_dep <- data %>%
  arrange(date) %>%
  mutate(up_lag = lag(up_day)) %>%
  filter(!is.na(up_lag))

trans_tab <- table(
  PrevUp = data_dep$up_lag,
  CurrUp = data_dep$up_day
)
cat("\nTransition counts (PrevUp -> CurrUp):\n")
print(trans_tab)

trans_probs <- prop.table(trans_tab, margin = 1)
cat("Transition probabilities P(CurrUp | PrevUp):\n")
print(trans_probs)

# 9) DENSITY ESTIMATION & NORMAL / t FITS + MGF

dens_all <- density(data$log_return, na.rm = TRUE)
plot(dens_all, main = "Kernel density of BTC log returns",
     xlab = "log_return")

# Normal fit (MLE)
fit_norm <- MASS::fitdistr(data$log_return, densfun = "normal")
cat("\nNormal fit (mean, sd):\n")
print(fit_norm)

qqnorm(data$log_return, main = "Q-Q plot vs Normal")
qqline(data$log_return)

# t-distribution fit
fit_t <- MASS::fitdistr(data$log_return, densfun = "t")
cat("\nt-distribution fit (df, mean, sd):\n")
print(fit_t)

logLik_norm <- logLik(fit_norm)
logLik_t    <- logLik(fit_t)
AIC_norm    <- AIC(fit_norm)
AIC_t       <- AIC(fit_t)

cat("Normal: logLik =", round(as.numeric(logLik_norm), 1),
    " AIC =", round(AIC_norm, 1), "\n")
cat("t-dist: logLik =", round(as.numeric(logLik_t), 1),
    " AIC =", round(AIC_t, 1), "\n")

# 9.1 MGF estimates 
t_grid <- seq(-1, 1, by = 0.1)

# empirical MGF: M_hat(t) = mean(exp(tX))
mgf_emp <- sapply(t_grid, function(tt) mean(exp(tt * data$log_return)))

# MGF under fitted Normal (with MLE parameters)
mu_mle <- fit_norm$estimate["mean"]
sd_mle <- fit_norm$estimate["sd"]
mgf_norm <- exp(mu_mle * t_grid + 0.5 * (sd_mle^2) * t_grid^2)

plot(t_grid, mgf_emp, type = "b", pch = 16,
     xlab = "t", ylab = "MGF",
     main = "Empirical vs Normal-theoretical MGF of log returns")
lines(t_grid, mgf_norm, lty = 2)
legend("topleft",
       legend = c("Empirical MGF", "Normal MGF"),
       lty = c(1, 2), pch = c(16, NA))

# 10) SIMPLE REGRESSION WITH SINUSOIDAL LUNAR TERMS
# note: didn't show this in the report

data_reg <- data %>%
  mutate(
    sin_cycle = sin(2 * pi * cycle_frac),
    cos_cycle = cos(2 * pi * cycle_frac)
  )

lm_basic <- lm(log_return ~ sin_cycle + cos_cycle, data = data_reg)
cat("\nRegression of log_return on sin/cos(cycle_frac):\n")
print(summary(lm_basic))

# 11 copulas

df_cop <- data %>%
  dplyr::select(abs_ret, cycle_frac) %>%
  drop_na()

# Pseudo-observations (empirical marginals -> U[0,1])
U <- pobs(as.matrix(df_cop))   # 2-column matrix in [0,1]^2

# Empirical Kendall's tau
tau_emp <- cor(df_cop$abs_ret, df_cop$cycle_frac, method = "kendall")
cat("\nEmpirical Kendall's tau(|R|, cycle_frac) =", round(tau_emp, 4), "\n")

# Gaussian copula 
gauss_cop <- normalCopula(param = 0.0, dim = 2)
fit_gauss <- fitCopula(gauss_cop, U, method = "ml")

cat("\nGaussian copula fit:\n")
print(fit_gauss)
tau_gauss <- tau(fit_gauss@copula)
cat("Implied Kendall's tau (Gaussian) =", round(tau_gauss, 4), "\n")

# Clayton copula 
clay_cop <- claytonCopula(param = 1, dim = 2)
fit_clay <- fitCopula(clay_cop, U, method = "ml")

cat("\nClayton copula fit:\n")
print(fit_clay)
tau_clay <- tau(fit_clay@copula)
cat("Implied Kendall's tau (Clayton) =", round(tau_clay, 4), "\n")

# Gumbel copula 
gumb_cop <- gumbelCopula(param = 1.1, dim = 2)
fit_gumb <- fitCopula(gumb_cop, U, method = "ml")

cat("\nGumbel copula fit:\n")
print(fit_gumb)
tau_gumb <- tau(fit_gumb@copula)
cat("Implied Kendall's tau (Gumbel) =", round(tau_gumb, 4), "\n")

# Compare log-likelihoods / AIC 
loglik_gauss <- fit_gauss@loglik
loglik_clay  <- fit_clay@loglik
loglik_gumb  <- fit_gumb@loglik

k_gauss <- length(coef(fit_gauss))
k_clay  <- length(coef(fit_clay))
k_gumb  <- length(coef(fit_gumb))

AIC_gauss <- -2 * loglik_gauss + 2 * k_gauss
AIC_clay  <- -2 * loglik_clay  + 2 * k_clay
AIC_gumb  <- -2 * loglik_gumb  + 2 * k_gumb

cat("\nCopula log-likelihoods & AIC:\n")
cat("Gaussian: logLik =", round(loglik_gauss, 1),
    " AIC =", round(AIC_gauss, 1), "\n")
cat("Clayton : logLik =", round(loglik_clay, 1),
    " AIC =", round(AIC_clay, 1), "\n")
cat("Gumbel  : logLik =", round(loglik_gumb, 1),
    " AIC =", round(AIC_gumb, 1), "\n")
