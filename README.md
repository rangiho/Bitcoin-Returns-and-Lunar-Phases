**README: Bitcoin Returns and Lunar Phases**

**Overview**

This project investigates whether Bitcoin’s daily returns and volatility exhibit any statistical relationship with the lunar cycle. Motivated by prior behavioural-finance studies that document lunar effects on stock markets, we test this hypothesis on Bitcoin, an asset known for extreme volatility and sentiment-driven trading.

Using daily BTC-USD data (2015–2024) and lunar phase timestamps from the US Naval Observatory, we construct a continuous lunar-cycle fraction variable and apply a wide range of econometric, time-series, and dependence-analysis tools.

All code and results are open-sourced for transparency and replication.

**Methods**

The analysis follows several layers of statistical investigation:

**1. Distribution & Normality Tests**

Summary statistics show Bitcoin returns are heavy-tailed and highly leptokurtic.

Shapiro-Wilk, Jarque-Bera, and Kolmogorov–Smirnov tests reject normality.

Kernel density and Q-Q plots confirm extreme tail behaviour.

**2. Moment-Generating Function (MGF) Analysis**

Empirical and Normal MGFs match for small |t|, but heavy tails appear outside this range.

**3. Conditional Return Behaviour by Lunar Phase**

Returns are grouped into lunar quartiles.

Means, variances, and tail probabilities show no systematic differences.

Law of Total Probability and Total Variance decompositions confirm independence.

**4. Short-Horizon Dependence**

Transition matrices show minimal serial dependence in return signs.

Autocorrelation is near zero, consistent with weak-form efficiency.

**5. Tail-Risk Analysis**

Value-at-Risk (VaR) and Expected Shortfall (ES) computed across lunar windows.

Differences across New-like, Middle, and Full-like phases are minimal.

**6. Dependence via Copulas**

Gaussian, Clayton, and Gumbel copulas estimated using |returns|.

Kendall’s τ ≈ 0.01, indicating essentially no dependence.

**Key Findings**

Based on all statistical evidence, there is no meaningful relationship between Bitcoin returns or volatility and the lunar cycle.
Specifically:

Return distributions are invariant across lunar phases.

Volatility and extreme-move probabilities remain constant throughout the cycle.

Both linear and nonlinear dependence measures confirm independence.

This suggests Bitcoin’s behaviour is better explained by market microstructure and sentiment dynamics rather than astrophysical phenomena.

**Contact**

You can reach me at rangi.ho@gmail.com if you would like to verify the results or request a copy of the paper.
