# ------------------------------------------------------------------------------
# Objetive: to automatically analyse which values of RME are superior.
# We begin this analysis after the KO routine has been applied.
# This script was made specifically for macrophage polarization.
# ------------------------------------------------------------------------------

# 1. DATA SETUP ----------------------------------------------------------------

# Extracting numerical values (ensure these are correct)
cells_rme <- CellsKO$RME
signal_rme <- SignalKO$RME 
all_rme <- c(cells_rme, signal_rme)

# Creating a unified table to retrieve names for the "all" group later
Tabela_Cells <- data.frame(Nome = rownames(CellsKO), RME = cells_rme, Tipo = "Cells")
Tabela_Signal <- data.frame(Nome = SignalKO[,1], RME = signal_rme, Tipo = "Signals")
Tabela_All <- rbind(Tabela_Cells, Tabela_Signal)

# ------------------------------------------------------------------------------
# 2. INITIAL UNIFORMITY AND NORMALITY TESTS ------------------------------------

# Kolmogorov-Smirnov Test: compare data with the uniform distribution.
ks_cells <- ks.test(cells_rme, "punif"); print(ks_cells) 
ks_signal <- ks.test(signal_rme, "punif"); print(ks_signal)
ks_all <- ks.test(all_rme, "punif"); print(ks_all)
# Interpretation: if p-value < 0.05, the data is not uniform.

# Anderson-Darling Test: this test works like the KS, but is more sensible
# to deviations in the tails of the distribution.

# Install and load nortest package.
# install.packages("nortest") # Leave commented if already installed
library(nortest)

# Check for normality.
ad_cells <- ad.test(cells_rme); print(ad_cells)
ad_signal <- ad.test(signal_rme); print(ad_signal)
ad_all <- ad.test(all_rme); print(ad_all)
# Interpretation: if p-value < 0.05, the data is not normal.

# ------------------------------------------------------------------------------
# 3. DISCOVERING SUPERIOR VALUES BY PERCENTILE ---------------------------------

# -- Discovering which values are superior for CellsKO --
p90c <- quantile(cells_rme, 0.90) # 90th percentile.
p95c <- quantile(cells_rme, 0.95) # 95th percentile.

# Values above the 90th percentile. 
cat("\n--- Cells above 90th Percentile ---\n")
print(rownames(CellsKO[cells_rme > p90c, ]))

# Values above the 95th percentile. 
cat("\n--- Cells above 95th Percentile ---\n")
print(rownames(CellsKO[cells_rme > p95c, ]))


# -- Discovering which values are superior for signalKO --
p90s <- quantile(signal_rme, 0.90) # 90th percentile.
p95s <- quantile(signal_rme, 0.95) # 95th percentile.

# Values above the 90th percentile.
cat("\n--- Signals above 90th Percentile ---\n")
print(SignalKO[signal_rme > p90s, 1]) # Extract column 1, which contains the names

# Values above the 95th percentile.
cat("\n--- Signals above 95th Percentile ---\n")
print(SignalKO[signal_rme > p95s, 1])


# -- Discovering which values are superior for allKO --
p90a <- quantile(all_rme, 0.90) # 90th percentile.
p95a <- quantile(all_rme, 0.95) # 95th percentile.

# Values above the 90th percentile.
cat("\n--- Union (Signals and Cells) above 90th Percentile ---\n")
# Using Tabela_All here. Displaying Name and Type to identify each entry.
print(Tabela_All[Tabela_All$RME > p90a, c("Nome", "Tipo")])

# Values above the 95th percentile.
cat("\n--- Union (Signals and Cells) above 95th Percentile ---\n")
print(Tabela_All[Tabela_All$RME > p95a, c("Nome", "Tipo")])

# ------------------------------------------------------------------------------
# 4. OUTLIER DETECTION USING TUKEY'S METHOD (IQR) ------------------------------
# Outlier detection using Tukey's Method (IQR - Interquartile Range).

# -- Analysis for CellsKO --
Q1c <- quantile(cells_rme, 0.25) # 1st quartile (25%).
Q3c <- quantile(cells_rme, 0.75) # 3rd quartile (75%).
IQR_valuec <- Q3c - Q1c          # Interquartile Range.

# Define the upper limit to detect extreme outliers.
limite_superiorc <- Q3c + 1.5 * IQR_valuec

# Values considered higher. 
cat("\n--- Cells Outliers (Tukey's Method) ---\n")
print(rownames(CellsKO[cells_rme > limite_superiorc, ]))


# -- Analysis for SignalKO --
Q1s <- quantile(signal_rme, 0.25) # 1st quartile (25%).
Q3s <- quantile(signal_rme, 0.75) # 3rd quartile (75%).
IQR_values <- Q3s - Q1s           # Interquartile Range.

# Define the upper limit to detect extreme outliers.
limite_superiors <- Q3s + 1.5 * IQR_values

# Values considered higher.
cat("\n--- Signals Outliers (Tukey's Method) ---\n")
print(SignalKO[signal_rme > limite_superiors, 1])


# -- Analysis for allKO --
Q1a <- quantile(all_rme, 0.25)  # 1st quartile (25%). 
Q3a <- quantile(all_rme, 0.75)  # 3rd quartile (75%).
IQR_valuea <- Q3a - Q1a         # Interquartile Range.

# Define the upper limit to detect extreme outliers.
limite_superiora <- Q3a + 1.5 * IQR_valuea

# Values considered higher.
cat("\n--- Union Outliers (Tukey's Method) ---\n")
print(Tabela_All[Tabela_All$RME > limite_superiora, c("Nome", "Tipo")])

# ------------------------------------------------------------------------------
# 5. OTHER TESTS AND GRAPHICAL ANALYSIS ----------------------------------------

# Normality test in order to use other measures.

# Shapiro-Wilk Test (best for small/medium samples).
shapiro.test(cells_rme)
shapiro.test(signal_rme)
shapiro.test(all_rme)
# Interpretation: if p-value < 0.05, the data is not normal.

# Graphical analysis.
hist(cells_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "Cells KO Histogram")
lines(density(cells_rme), col = "red", lwd = 2) # Add density curve.

hist(signal_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "Signal KO Histogram")
lines(density(signal_rme), col = "red", lwd = 2) # Add density curve.

hist(all_rme, breaks = 10, probability = TRUE, col = "lightblue", main = "All KO Histogram")
lines(density(all_rme), col = "red", lwd = 2) # Add density curve.

# Kolmogorov-Smirnov Test (KS) for normality.
ks.test(cells_rme, "pnorm", mean = mean(cells_rme), sd = sd(cells_rme))
ks.test(signal_rme, "pnorm", mean = mean(signal_rme), sd = sd(signal_rme))
ks.test(all_rme, "pnorm", mean = mean(all_rme), sd = sd(all_rme))
# Interpretation: if p-value < 0.05, the data is not normal.

# Lilliefors Test.
lillie.test(cells_rme)
lillie.test(signal_rme)
lillie.test(all_rme)
# Interpretation: if p-value < 0.05, the data is not normal.