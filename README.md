# Volatility Smiles in Options and Warrants Markets — An Empirical Analysis Using EUREX and EUWAX Data

**Author:** Anil Mujagic
**Institution:** Heinrich-Heine-Universität Düsseldorf, Chair of Statistics and Econometrics (Prof. Dr. Florian Heiß)
**Supervisor:** Dr. Daniel Brunner
**Type:** Bachelor's Thesis
**Date:** November 2024

## Overview

This thesis empirically compares the volatility smile of exchange-traded options (EUREX) and bank-issued warrants (EUWAX) on the DAX Performance Index. It investigates whether the two product types — despite their structural similarity — display systematically different implied volatility patterns, and whether commodity price movements (natural gas, oil, gold, silver) help explain implied volatility beyond the classic Black-Scholes framework.

## Methodology

- **Data:** DAX call/put options (EUREX, products ODAX/ODX1/ODX2/ODX4/ODX5) and DAX call/put warrants from 11 major issuers (EUWAX), 30.05.2022–15.03.2023, strikes covering a moneyness range of 0.7–1.3.
- **Maturity buckets:** Two sub-samples — short-dated (1–3 days) and longer-dated (17–21 days) contracts — to test maturity effects on the smile.
- **Implied volatility:** Computed via the Black-Scholes model, solved iteratively for volatility; the Euro Short-Term Rate (€STR) is used as the risk-free rate to capture the 2022 ECB rate-hike cycle.
- **Commodity data:** Four London-listed Exchange-Traded Commodities (Invesco Physical Gold, WisdomTree Natural Gas, WisdomTree Physical Silver, WisdomTree WTI Crude Oil) as macro-financial controls.
- **Smile model:** Quadratic moneyness regression (σ = β₀ + β₁M + β₂M²), extended with interaction terms for market (EUREX vs. EUWAX), option type (call vs. put), and a structural dummy for the Nord Stream pipeline attack (26.09.2022), interacted with commodity prices.
- **Estimation:** OLS via minimization of squared residuals; separate models for the short and long maturity samples.

## Key Findings

- **Shape differences by option type:** Calls display the classic U-shaped volatility smile; puts on both markets tend toward a volatility smirk/skew, consistent with hedging-driven demand for OTM puts.
- **Market differences (EUREX vs. EUWAX):** Short-dated call warrants are priced with significantly lower implied volatility in the ATM/ITM range than comparable EUREX options — consistent with issuers strategically steering demand — while deep OTM warrants are priced higher. These differences largely disappear at longer maturities.
- **Maturity effect:** Short-dated contracts (1–3 days) show markedly higher and steeper implied volatility than longer-dated contracts (17–21 days); the smile flattens as maturity increases.
- **Commodity effects:** Gold prices are positively correlated with implied volatility on both markets, consistent with its "safe haven" role; silver is consistently negatively correlated, linked to its industrial-demand component. Natural gas and oil show sign reversals in their correlation with implied volatility around the Nord Stream pipeline attack, reflecting shifting market uncertainty about energy supply.
- **Overall:** The results confirm a violation of the Black-Scholes constant-volatility assumption and show that issuer pricing discretion, option type, time to maturity, and macro-financial (commodity) conditions all significantly shape the volatility smile — with the most pronounced market-specific effects concentrated in short-dated call products.

## References

- Black, F. and Scholes, M. (1973). The Pricing of Options and Corporate Liabilities. *Journal of Political Economy*, 81(3), 637–654.
- Hull, J. C. (2014). *Options, Futures, and Other Derivatives* (9th Edition).
- Merton, R. C. (1976). Option Pricing When Underlying Stock Returns Are Continuous. *Journal of Financial Economics*, 3, 125–144.
- Rubinstein, M. (1994). Implied Binomial Trees. *The Journal of Finance*, 49(3), 771–818.
- Pena, I., Rubio, G., and Serna, G. (1999). Why do we smile? On the determinants of the implied volatility function. *Journal of Banking & Finance*, 23(8), 1151–1179.
- Bollen, N. P. B. and Whaley, R. E. (2004). Does Net Buying Pressure Affect the Shape of Implied Volatility Functions? *The Journal of Finance*, 59(2), 711–753.
- Han, Q., Liang, J., and Wu, B. (2016). Cross Economic Determinants of Implied Volatility Smile Dynamics: Three Major European Currency Options. *European Financial Management*, 22(5), 817–852.
- Derman, E. and Miller, M. B. (2016). *The Volatility Smile: An Introduction for Students and Practitioners*.
- Dumas, B., Fleming, J., and Whaley, R. E. (1996). Implied Volatility Functions: Empirical Tests. *The Journal of Finance*, 53(6), 2059–2106.
- Bates, D. S. (2000). Post-'87 crash fears in the S&P 500 futures option market. *Journal of Econometrics*, 94(2), 181–238.
