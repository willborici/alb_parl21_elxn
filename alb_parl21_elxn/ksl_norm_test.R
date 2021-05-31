# ksl_norm_test.R
# Kolmogorov-Smirnov-Lilliefors test for party and candidate votes.
# CC BY-SA. W.A. Borici, 2021.
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Output files

# Normality Tests based on the empirical probabilities of:
# 1. Parties (PS, PD, Other Parties)
# 2. Relative turnout
# 3. Relative invalid ballots
# at the adminisrative unit level (for all 383 admin units)
# Run the Kolmogorov-Smirnov Normality test with Lilliefors criteria on
# each of pPS, pPD, pOP.
# ----

# PS
resultPS <- NormTestsWithVisuals(partyVotesP$pPS, "PS", 0)

# Print results - graph and text - in files
resultPS[[1]]
resultPS[[2]]
resultPS[[3]]
resultPS[[4]]
resultPS[[5]]
# Compute Lilliefors critical value for reference for n > 50
D_crit <-0.895/((0.83 + length(partyVotesP$pPS)) / 
                  sqrt(length(partyVotesP$pPS)) - 0.01)
D_crit
resultPS[[6]]

# ----
# PD
# First, some visuals
resultPD <- NormTestsWithVisuals(partyVotesP$pPD, "PD", 0)

# Print results - graph and text - in files
resultPD[[1]]
resultPD[[2]]
resultPD[[3]]
resultPD[[4]]
resultPD[[5]]
resultPD[[6]]

# ----
# Share PS/PD - a simple metric of inter-party competitiveness
resultPSPD <- NormTestsWithVisuals(partyVotesP$pPSPD, "PS/PD ratio", 0)

# Print results - graph and text - in files
resultPSPD[[1]]
resultPSPD[[2]]
resultPSPD[[3]]
resultPSPD[[4]]
resultPSPD[[5]]
resultPSPD[[6]]


# ----
# Other Parties
resultOP <- NormTestsWithVisuals(partyVotesP$pOP, "Other Parties", 0)

# Print results - graph and text - in files
resultOP[[1]]
resultOP[[2]]
resultOP[[3]]
resultOP[[4]]
resultOP[[5]]
resultOP[[6]]

# ----
# Turnout
resultTurnout <- NormTestsWithVisuals(partyVotesP$pTurnout, "Turnout", 0)

# Print results - graph and text - in files
resultTurnout[[1]]
resultTurnout[[2]]
resultTurnout[[3]]
resultTurnout[[4]]
resultTurnout[[5]]
resultTurnout[[6]]

# ----
# Turnout
resultInvalid <- NormTestsWithVisuals(partyVotesP$pInvalid, 
                                      "Invalid Ballots", 0)

# Print results - graph and text - in files
resultInvalid[[1]]
resultInvalid[[2]]
resultInvalid[[3]]
resultInvalid[[4]]
resultInvalid[[5]]
resultInvalid[[6]]

# END NORMALITY TEST at ADMIN UNIT

# Normality Tests based on the empirical probabilities of:
# 1. Parties (PS, PD, Other Parties)
# 2. Relative turnout
# 3. Relative invalid ballots
# at the municipality level (for all 61 Albanian municipalities)

# Run the Kolmogorov-Smirnov Normality test with Lilliefors criteria on
# each of pPS, pPD, pOP.
# ----
# PS
resultPS <- NormTestsWithVisuals(votesGroupedByMunicipality$pPS, "PS", 0)

# Print results - graph and text - in files
resultPS[[1]]
resultPS[[2]]
resultPS[[3]]
resultPS[[4]]
resultPS[[5]]
resultPS[[6]]

# ----
# PD
# First, some visuals
resultPD <- NormTestsWithVisuals(votesGroupedByMunicipality$pPD, "PD", 0)

# Print results - graph and text - in files
resultPD[[1]]
resultPD[[2]]
resultPD[[3]]
resultPD[[4]]
resultPD[[5]]
resultPD[[6]]

# ----
# Other Parties
resultOP <- NormTestsWithVisuals(votesGroupedByMunicipality$pOP, 
                                 "Other Parties", 0)

# Print results - graph and text - in files
resultOP[[1]]
resultOP[[2]]
resultOP[[3]]
resultOP[[4]]
resultOP[[5]]
resultOP[[6]]

# ----
# Turnout
resultTurnout <- NormTestsWithVisuals(votesGroupedByMunicipality$pTurnout, 
                                      "Turnout", 0)

# Print results - graph and text - in files
resultTurnout[[1]]
resultTurnout[[2]]
resultTurnout[[3]]
resultTurnout[[4]]
resultTurnout[[5]]
resultTurnout[[6]]

# ----
# Turnout
resultInvalid <- NormTestsWithVisuals(votesGroupedByMunicipality$pInvalid, 
                                      "Invalid Ballots", 0)

# Print results - graph and text - in files
resultInvalid[[1]]
resultInvalid[[2]]
resultInvalid[[3]]
resultInvalid[[4]]
resultInvalid[[5]]
resultInvalid[[6]]

# END NORMALITY TEST at MUNICIPALITY LEVEL

# ----
# We have assumed normal distribution based on the i.i.d. assumption of
# party vote percentage set. Below, we analyze what possible other
# distributions could best fit the various data sets:

# PS - fits Weibull
resultPS <- FitOtherDistributions(partyVotesP$pPS, "PS", FALSE)
resultPS
# plot the weibull:
x <- partyVotesP$pPS
crv <- function(x) dweibull(x, shape = 5.2020632, scale = 0.5345796)
hist(partyVotesP$pPS, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Weibull fit for PS vote share"), 
     xlab = "PS vote share")
curve(Vectorize(crv)(x), from = 0, to = 1, add=TRUE, lwd=2, col="navy")

# PD - fits normal
resultPD <- FitOtherDistributions(partyVotesP$pPD, "PD", FALSE)
resultPD
# plot the normal:
x <- partyVotesP$pPD
crv <- function(x) dnorm(x, mean = 0.3894273, sd = 0.1164794)
hist(partyVotesP$pPD, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Normal fit for PD vote share"), 
     xlab = "PD vote share")
curve(Vectorize(crv)(x), from = 0, to = 1, add=TRUE, lwd=2, col="navy")

# Share PS/PD - fits log-normal
resultPSPD <- FitOtherDistributions(partyVotesP$pPSPD, "PS/PD", FALSE)
resultPSPD
# plot the log-normal:
x <- partyVotesP$pPSPD
crv <- function(x) dlnorm(x, meanlog = 0.2512484, sdlog = 0.5823473)
hist(partyVotesP$pPSPD, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Normal fit for PS-to-PD vote share ratio"), 
     xlab = "PS/PD vote share ratio")
curve(Vectorize(crv)(x), from = 0, to = 10, add=TRUE, lwd=2, col="navy")

# Other Parties -  fits beta
resultOP <- FitOtherDistributions(partyVotesP$pOP, "Other Parties", FALSE)
resultOP
# plot the beta:
x <- partyVotesP$pOP
crv <- function(x) dbeta (x, shape1 = 1.940916, shape2 = 14.430417)
hist(partyVotesP$pOP, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Beta fit for other parties' vote share"),
     xlab = "Other parties' vote share")
curve(Vectorize(crv)(x), from = 0, to = 1, add=TRUE, lwd=2, col="navy")

# Turnout - fits Weibull
resultTurnout <- FitOtherDistributions(partyVotesP$pTurnout, "Turnout", FALSE)
resultTurnout
# plot the weibull:
x <- partyVotesP$pTurnout
crv <- function(x) dweibull(x, shape = 5.6563431, scale = 0.5028294)
hist(partyVotesP$pTurnout, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Weibull fit for voter turnout"), xlim = xrange, 
     xlab = "Turnout")
curve(Vectorize(crv)(x), from = 0, to = 1, add=TRUE, lwd=2, col="navy")


# Invalid Ballots - fits Normal
resultInvalid <- FitOtherDistributions(partyVotesP$pInvalid, 
                                       "Invalid Ballots", FALSE)
resultInvalid

# Repeat fittings above by municipality
# ----
# PS - fits Weibull
resultPS <- FitOtherDistributions(votesGroupedByMunicipality$pPS, "PS", FALSE)
resultPS

# PD - fits normal
resultPD <- FitOtherDistributions(votesGroupedByMunicipality$pPD, "PD", FALSE)
resultPD

# Other Parties -  fits beta
resultOP <- FitOtherDistributions(votesGroupedByMunicipality$pOP, 
                                  "Other Parties", FALSE)
resultOP

# Turnout - fits Weibull
resultTurnout <- FitOtherDistributions(votesGroupedByMunicipality$pTurnout, 
                                       "Turnout", FALSE)
resultTurnout

# Invalid Ballots - fits Normal
resultInvalid <- FitOtherDistributions(votesGroupedByMunicipality$pInvalid, 
                                       "Invalid Ballots", FALSE)
resultInvalid

