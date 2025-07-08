
All raw datasets are download from: https://www.sciencedirect.com/science/article/pii/S0169207019301530 


** Dataset details
The forecasts are calculated with one observation per month of online data. The PriceStats indices were transformed for this paper following the steps below:
1) Lag the time series for 14 days.
2) Keep the last observation of each month.
3) Calculate the inflation rate:
100 * (ln(index[t]) - ln(index[t-1]))

No change is made to these raw data files before loading into R for further data manlpulation.