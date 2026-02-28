# ------------------------------------------------------------------------------
# Objetive: to automatically analyse which values of RME are superior.

# We begin this analysis after the KO routine has been applied.

# This script was made specifically for macrophage polarization.
# ------------------------------------------------------------------------------

# Kolmogorov-Smirnov Test: compare data with the uniform distribution.

# Create vectors for statistical analysis.

cells<-c(CellsKO[,2]) ; is.vector(cells) 

signal<-c(SignalKO[,2]);  is.vector(signal)

all<-c(cells, signal); is.vector(all)

# Compare in terms of uniform dist.

ks_cells<- ks.test(cells, "punif"); print(ks_cells) 

ks_signal<- ks.test(signal, "punif"); print(ks_signal)

ks_all<- ks.test(all, "punif"); print(ks_all)

# Interpretation: if p-value < 0.05, the data is not uniform.
# ------------------------------------------------------------------------------

# Anderson-Darling Test: this test works like the KS, but is more sensible
# to deviations in the tails of the distribution.

# Install and load nortest package.

install.packages("nortest")   

library(nortest)

# Check for normality.

ad_cells<- ad.test(cells); print(ad_cells)

ad_signal<-ad.test(signal); print(ad_signal)

ad_all<-ad.test(all); print(ad_all)

# Interpretation: if p-value < 0.05, the data is not normal.
# ------------------------------------------------------------------------------

# Discovering which values are superior for CellsKO.

cells<-sort(cells, decreasing = T)

p90c <- quantile(cells, 0.90)  # 90th percentile.
p95c <- quantile(cells, 0.95)  # 95th percentile.

# Values above the 90th percentile. 

superiores_p90c <- cells[cells > p90c]
print(superiores_p90c)

# Values above the 95th percentile. 

superiores_p95c <- cells[cells > p95c]
print(superiores_p95c)
# ------------------------------------------------------------------------------

# Discovering which values are superior for signalKO.

signal<-sort(signal, decreasing = T)

p90s <- quantile(signal, 0.90)  # 90th percentile.
p95s <- quantile(signal, 0.95)  # 95th percentile.

# Values above the 90th percentile.

superiores_p90s <- signal[signal > p90s]
print(superiores_p90s)

# Values above the 95th percentile.

superiores_p95s <- signal[signal > p95s]
print(superiores_p95s)
# ------------------------------------------------------------------------------

# Discovering which values are superior for allKO.

all<-sort(all, decreasing = T)

p90a <- quantile(all, 0.90)  # 90th percentile.
p95a <- quantile(all, 0.95)  # 95th percentile.

# Values above the 90th percentile.

superiores_p90a <- all[all > p90a]
print(superiores_p90a)

# Values above the 95th percentile.

superiores_p95a <- all[all > p95a]
print(superiores_p95a)
# ------------------------------------------------------------------------------

# Outlier detection using Tukey's Method (IQR - Interquartile Range).

# Analysis for CellsKO.

Q1c <- quantile(cells, 0.25)  # 1st quartile (25%).
Q3c <- quantile(cells, 0.75)  # 3rd quartile (75%).
IQR_valuec <- Q3c - Q1c       # Interquartile Range.

# Define the upper limit to detect extreme outliers.

limite_superiorc <- Q3c + 1.5 * IQR_valuec

# Values considered higher. 

superiores_iqrc <- cells[cells > limite_superiorc]
print(superiores_iqrc)

# Analysis for SignalKO.

Q1s <- quantile(signal, 0.25)  # 1st quartile (25%).
Q3s <- quantile(signal, 0.75)  # 3rd quartile (75%).
IQR_values <- Q3s - Q1s        # Interquartile Range.

# Define the upper limit to detect extreme outliers.

limite_superiors <- Q3s + 1.5 * IQR_values

# Values considered higher.

superiores_iqrs <- signal[signal > limite_superiors]
print(superiores_iqrs)

# Analysis for allKO.

Q1a <- quantile(all, 0.25)  # 1st quartile (25%). 
Q3a <- quantile(all, 0.75)  # 3rd quartile (75%).
IQR_valuea <- Q3a - Q1a     # Interquartile Range.

# Define the upper limit to detect extreme outliers.

limite_superiora <- Q3a + 1.5 * IQR_valuea

# Values considered higher.

superiores_iqra <- all[all > limite_superiora]
print(superiores_iqra)
# ------------------------------------------------------------------------------

# Normality test in order to use other measures.

# Shapiro-Wilk Test (best for small/medium samples).

shapiro.test(cells)

shapiro.test(signal)

shapiro.test(all)

# Interpretation: if p-value < 0.05, the data is not normal.
# ------------------------------------------------------------------------------

# Graphical analysis.

hist(cells, breaks = 10, probability = TRUE, col = "lightblue", main = "Cells KO Histogram")
lines(density(cells), col = "red", lwd = 2)  # Add density curve.

hist(signal, breaks = 10, probability = TRUE, col = "lightblue", main = "Signal KO Histogram")
lines(density(signal), col = "red", lwd = 2)  # Add density curve.

hist(all, breaks = 10, probability = TRUE, col = "lightblue", main = "All KO Histogram")
lines(density(all), col = "red", lwd = 2)  # Add density curve.
# ------------------------------------------------------------------------------

# Kolmogorov-Smirnov Test (KS) for normality.

ks.test(cells, "pnorm", mean = mean(cells), sd = sd(cells))

ks.test(signal, "pnorm", mean = mean(signal), sd = sd(signal))

ks.test(all, "pnorm", mean = mean(all), sd = sd(all))

# Interpretation: if p-value < 0.05, the data is not normal.
# ------------------------------------------------------------------------------

# Lilliefors Test.

lillie.test(cells)

lillie.test(signal)

lillie.test(all)

# Interpretation: if p-value < 0.05, the data is not normal.
