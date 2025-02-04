```{r, results='asis'}
#| echo: false
suppressPackageStartupMessages(library(tseries))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(rugarch))
suppressPackageStartupMessages(library(rmgarch))
suppressPackageStartupMessages(library(reshape2))
if (!require("zoo")) install.packages("zoo", dependencies=TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
if (!require("tseries")) install.packages("tseries", dependencies=TRUE)
if (!require("rugarch")) install.packages("rugarch", dependencies=TRUE)
if (!require("rmgarch")) install.packages("rmgarch", dependencies=TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies=TRUE)
if (!require("car")) install.packages("car", dependencies=TRUE)
library(zoo)
library(lubridate)
library(reshape2)
library(moments)
library(knitr)
library(tseries)  # 
library(rugarch)
library(car)
library(kableExtra)

data <- read.csv("homework2.csv")
data$Unadjusted_Prices <- data$PRC
data$Adjusted_Prices <- data$PRC / data$CFACPR
library(ggplot2)
 
Tickers <- dcast(data, date ~ PERMNO, value.var = "TICKER")
Tickers <- tail(Tickers, 1)
Prices <- dcast(data, date ~ PERMNO, value.var = "Adjusted_Prices")
names(Prices) <- Tickers
names(Prices)[1] <- "date"

simpleReturns <- dcast(data, date ~ PERMNO, value.var = "RET")
names(simpleReturns) <- Tickers
names(simpleReturns)[1] <- "date"

Returns <- log(1 + simpleReturns[, 2:dim(simpleReturns)[2]])
Returns$date <- simpleReturns$date
Returns <- Returns[, c(dim(Returns)[2], 1:(dim(Returns)[2] - 1))]


date.ts <- ymd(Returns$date)

#| echo: false

```{r, results='asis'}
#| echo: false
library(gridExtra)
library(grid)
library(ggplot2)


prices_plot <- ggplot(data.frame(Date = date.ts, Tesla = Prices[, 2], Nvidia = Prices[, 3]), aes(x = Date)) +
  geom_line(aes(y = Tesla, color = "Tesla"), linewidth = 1) +
  geom_line(aes(y = Nvidia, color = "Nvidia"), linewidth = 1) +
  labs(title = "Prices: Tesla and Nvidia", y = "Price", x = "Date", color = "Stock") +
  scale_color_manual(values = c("Tesla" = "blue", "Nvidia" = "red")) +
  theme_minimal()


returns_plot <- ggplot(data.frame(Date = date.ts, Tesla = Returns[, 2], Nvidia = Returns[, 3]), aes(x = Date)) +
  geom_line(aes(y = Tesla, color = "Tesla"), linewidth = 1) +
  geom_line(aes(y = Nvidia, color = "Nvidia"), linewidth = 1) +
  labs(title = "Returns: Tesla and Nvidia", y = "Returns", x = "Date", color = "Stock") +
  scale_color_manual(values = c("Tesla" = "blue", "Nvidia" = "red")) +
  theme_minimal()

# Plots Side by Side
grid.arrange(prices_plot, returns_plot, ncol = 2)

library(knitr)
library(kableExtra)
library(htmltools) 
# Summary Statistics
y <- Returns$TSLA
summary_tesla <- c(
  mean(y, na.rm = TRUE),
  sd(y, na.rm = TRUE),
  skewness(y, na.rm = TRUE),
  kurtosis(y, na.rm = TRUE)
)

z <- Returns$NVDA
summary_nvidia <- c(
  mean(z, na.rm = TRUE),
  sd(z, na.rm = TRUE),
  skewness(z, na.rm = TRUE),
  kurtosis(z, na.rm = TRUE)
)

#Summary Statistics One Table
combined_summary <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Skewness", "Kurtosis"),
  Tesla = summary_tesla,
  Nvidia = summary_nvidia
)

kable(
  combined_summary,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Statistics for Tesla and Nvidia Returns",
  col.names = c("Statistic", "Tesla", "Nvidia"),
  digits = 4
) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

{r}
#| echo: false
library(knitr)


# GARCH(1,1) normal distribution Tesla
spec_tesla_normal <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)
garch_tesla_normal <- invisible(ugarchfit(spec = spec_tesla_normal, data = Returns$TSLA))

# GARCH(1,1) Student's t-distribution Tesla
spec_tesla_t <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)
garch_tesla_t <- invisible(ugarchfit(spec = spec_tesla_t, data = Returns$TSLA))

# GARCH(1,1) normal distribution Nvidia
spec_nvidia_normal <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)
garch_nvidia_normal <- invisible(ugarchfit(spec = spec_nvidia_normal, data = Returns$NVDA))

# GARCH(1,1)  Student's t-distribution Nvidia
spec_nvidia_t <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)
garch_nvidia_t <- invisible(ugarchfit(spec = spec_nvidia_t, data = Returns$NVDA))

# Extract log-likelihoods
loglik_tesla_normal <- likelihood(garch_tesla_normal)
loglik_tesla_t <- likelihood(garch_tesla_t)
loglik_nvidia_normal <- likelihood(garch_nvidia_normal)
loglik_nvidia_t <- likelihood(garch_nvidia_t)

# Likelihood Ratio Test Statistics
lr_stat_tesla <- 2 * (loglik_tesla_t - loglik_tesla_normal)
p_value_tesla <- pchisq(lr_stat_tesla, df = 1, lower.tail = FALSE)

lr_stat_nvidia <- 2 * (loglik_nvidia_t - loglik_nvidia_normal)
p_value_nvidia <- pchisq(lr_stat_nvidia, df = 1, lower.tail = FALSE)

# Table
lr_test_table <- data.frame(
  Stock = c("Tesla", "Nvidia"),
  Normal_LogLikelihood = c(loglik_tesla_normal, loglik_nvidia_normal),
  T_LogLikelihood = c(loglik_tesla_t, loglik_nvidia_t),
  LR_Statistic = c(lr_stat_tesla, lr_stat_nvidia),
  P_Value = c(p_value_tesla, p_value_nvidia)
)


kable(
  lr_test_table,
  col.names = c("Stock", "Normal Log-Likelihood", "t-Log-Likelihood", "LR Statistic", "P-Value"),
  caption = "Likelihood Ratio Test: Tesla and Nvidia",
  digits = 4,
  format = "latex",
  booktabs = TRUE
)
{r}
#| echo: false
#| results: hide
library(rugarch)
library(car)  # For qqPlot
suppressPackageStartupMessages(library(xts))  
suppressPackageStartupMessages(library(rmgarch))  



# GARCH for Tesla: GARCH(1,1) Student t-distribution
spec_tesla_t <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  # Student's t-distribution
)
garch_tesla_t <- ugarchfit(spec = spec_tesla_t, data = Returns$TSLA)

# Extract Conditional Volatility
volatility_tesla <- sigma(garch_tesla_t)

# Residuals Tesla
residuals_tesla <- Returns$TSLA/garch_tesla_t@fit$sigma

# Univariate GARCH Nvidia: GARCH(1,1) Student t-distribution
spec_nvidia_t <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  # Student's t-distribution
)
garch_nvidia_t <- ugarchfit(spec = spec_nvidia_t, data = Returns$NVDA)

# Extract Conditional Volatility
volatility_nvidia <- sigma(garch_nvidia_t)

# Residuals Nvidia
residuals_nvidia <- Returns$NVDA/garch_nvidia_t@fit$sigma

library(ggplot2)

# Univariate Conditional Volatility
data_univariate <- data.frame(
  Time = 1:length(volatility_tesla),
  Tesla = volatility_tesla,
  Nvidia = volatility_nvidia
)

ggplot(data_univariate, aes(x = Time)) +
  geom_line(aes(y = Tesla, color = "Tesla"), linewidth = 1) +
  geom_line(aes(y = Nvidia, color = "Nvidia"), linewidth = 1) +
  labs(title = "Univariate Conditional Volatility: Tesla and Nvidia",
       x = "Time", y = "Volatility") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()






par(mfrow = c(1, 2))  

#QQ Plot
qqPlot(residuals_nvidia, main = "QQ Plot: Nvidia Residuals")


qqPlot(residuals_tesla, main = "QQ Plot: Tesla Residuals")

invisible(qqPlot(residuals_nvidia, main = "QQ Plot: Nvidia Residuals"))
invisible(qqPlot(residuals_tesla, main = "QQ Plot: Tesla Residuals"))





par(mfrow = c(1, 2))  # 1 row, 2 columns

# ACF Squared Residuals
acf(residuals_nvidia^2, lag.max = 500, main = "Nvidia")

acf(residuals_tesla^2, lag.max = 500, main = "Tesla")

invisible(acf(residuals_nvidia^2, lag.max = 500, main = "Nvidia"))
invisible(acf(residuals_tesla^2, lag.max = 500, main = "Tesla"))


{r}
#| echo: false
#| results: hide
library(rmgarch)
library(xts)
suppressPackageStartupMessages(library(xts))  
suppressPackageStartupMessages(library(rmgarch)) 


theme_set(theme_gray() + 
            theme(legend.text = element_text(size = 7),
                  legend.title = element_text(size = 7))) 


returns_matrix <- cbind(Returns$TSLA, Returns$NVDA)
colnames(returns_matrix) <- c("Tesla", "Nvidia")

# SUnivariate GARCH Model 
uni_spec_t <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"  # Student's t-distribution
)

# DCC-GARCH Model
dcc_spec_t <- dccspec(
  uspec = multispec(replicate(2, uni_spec_t)),  # Apply univariate specs to both series
  dccOrder = c(1, 1),
  distribution = "mvt"  # Multivariate Student's t-distribution
)



dcc_fit_t <- dccfit(dcc_spec_t, data = returns_matrix)



H = dcc_fit_t@mfit$H
dim(H)
H[,,1]
rhoDCC = H[1,2,] / sqrt(H[1,1,]*H[2,2,])





volatility_dcc <- sigma(dcc_fit_t)


dcc_corr <- rcor(dcc_fit_t)


multivariate_volatility <- data.frame(
  Time = seq_len(nrow(volatility_dcc)),
  Tesla = as.numeric(volatility_dcc[, 1]),
  Nvidia = as.numeric(volatility_dcc[, 2])
)

multivariate_volatility_long <- melt(multivariate_volatility, id.vars = "Time", variable.name = "Stock", value.name = "Volatility")


suppressMessages(library(xts)) 
suppressMessages(library(rmgarch)) 
library(ggplot2)

# Multivariate Conditional Volatility Data
data_multivariate <- data.frame(
  Time = 1:nrow(volatility_dcc),
  Tesla = volatility_dcc[, 1],
  Nvidia = volatility_dcc[, 2]
)




# Dynamic Correlation
plot(dcc_corr[1, 2, ], type = "l", col = "purple", 
     main = "Dynamic Correlation: Tesla and Nvidia", xlab = "Index", ylab = "Correlation")

l
conditional_covariance <- rcov(dcc_fit_t) 


cov_tesla_nvidia <- conditional_covariance[1, 2, ]   


time_series_cov <- ts(cov_tesla_nvidia)

# Plot conditional covariance
plot(time_series_cov, type = "l", col = "purple",
     main = "Conditional Covariance: Tesla and Nvidia",
     xlab = "Time", ylab = "Covariance", lwd = 2)

{r}
#| echo: false
library(knitr)
library(kableExtra)


loglik_tesla <- likelihood(garch_tesla_t)  # Tesla Univariate Log-Likelihood
loglik_nvidia <- likelihood(garch_nvidia_t)  # Nvidia Univariate Log-Likelihood
loglik_dcc <- likelihood(dcc_fit_t)  # Multivariate DCC-GARCH Log-Likelihood


loglik_table <- data.frame(
  Model = c("Tesla (Univariate)", "Nvidia (Univariate)", "DCC-GARCH (Multivariate)"),
  Log_Likelihood = c(loglik_tesla, loglik_nvidia, loglik_dcc)
)


kable(loglik_table, 
      col.names = c("Model", "Log-Likelihood"),
      caption = "Log-Likelihood Results for Univariate and Multivariate Models") %>%
  kable_styling(font_size = 10, full_width = FALSE, position = "center")



lr_stat <- 2 * (loglik_dcc - (loglik_tesla + loglik_nvidia))  # LR Statistic
p_value <- 1 - pchisq(lr_stat, df = 2)  # Degrees of freedom = 2


lrt_table <- data.frame(
  Test = "Likelihood Ratio Test",
  LR_Statistic = lr_stat,
  P_Value = p_value
)


kable(lrt_table, 
      col.names = c("Test", "Likelihood Ratio Statistic", "P-Value"),
      caption = "Likelihood Ratio Test Results") %>%
  kable_styling(font_size = 10, full_width = FALSE, position = "center")

```{r, fig.width=5, fig.height=3, echo=FALSE}

weights <- c(0.5, 0.5)

# Simulate market crash
stress_shock <- -0.20  # -20% return
tesla_returns_stress <- Returns$TSLA
nvidia_returns_stress <- Returns$NVDA


tesla_returns_stress[1] <- tesla_returns_stress[1] + stress_shock
nvidia_returns_stress[1] <- nvidia_returns_stress[1] + stress_shock


returns_matrix_stress <- cbind(tesla_returns_stress, nvidia_returns_stress)
colnames(returns_matrix_stress) <- c("Tesla", "Nvidia")


dcc_fit_stress <- dccfit(dcc_spec_t, data = returns_matrix_stress)


volatility_dcc_stress <- sigma(dcc_fit_stress)
conditional_covariance_stress <- rcov(dcc_fit_stress)


portfolio_var_stress <- numeric(length = nrow(returns_matrix_stress))
for (i in 1:nrow(returns_matrix_stress)) {
  portfolio_var_stress[i] <- weights[1]^2 * volatility_dcc_stress[i, 1]^2 +
    weights[2]^2 * volatility_dcc_stress[i, 2]^2 +
    2 * weights[1] * weights[2] * conditional_covariance_stress[1, 2, i]
}
portfolio_var_stress <- -qnorm(0.05) * sqrt(portfolio_var_stress)


portfolio_var_baseline <- numeric(length = nrow(returns_matrix))
for (i in 1:nrow(returns_matrix)) {
  portfolio_var_baseline[i] <- weights[1]^2 * volatility_dcc[i, 1]^2 +
    weights[2]^2 * volatility_dcc[i, 2]^2 +
    2 * weights[1] * weights[2] * conditional_covariance[1, 2, i]
}
portfolio_var_baseline <- -qnorm(0.05) * sqrt(portfolio_var_baseline)

# Plot 
plot(portfolio_var_baseline, type = "l", col = "blue", lwd = 3, lty = 2,
     main = NULL,  # Remove the title
     xlab = "Time", ylab = "Portfolio VaR",
     ylim = range(c(portfolio_var_baseline, portfolio_var_stress)))
lines(portfolio_var_stress, col = "red", lwd = 2)
legend("topright", legend = c("Baseline (Dashed)", "Stressed (Market Crash)"),
       col = c("blue", "red"), lty = c(2, 1), lwd = c(3, 2), cex = 0.8) 


```


