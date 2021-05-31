# data_description.R
# Describe data visually and numerically using various statistical functions.
# CC BY-SA. W.A. Borici, 2021. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Output file locations
partyVotesRawByAdminUnit <- here("out/party_votes_raw_admin_unit.csv")
partyVotesByAdminUnit <- here("out/party_votes_admin_unit.csv")
partyVotePByAdminUnit <- here("out/party_votes_p_admin_unit.csv")
descripStatsFile <- here("out/descript_stat.csv")
partyVotesByDistrictFile <- here("out/party_votes_by_district.csv")
partyVotesByMunicipalityFile <- here("out/party_votes_by_municipality.csv")
partyVotesRawByPollingStation <- here("out/party_votes_raw_polling_station.csv")


# Create a total columns for each administrative unit: TotalEligibleVotes as
# the sum of reported party votes, and TotalVotes as the latter sum plus
# invalid ballots. We will use the partyVotesRaw data set in our normality test 
# for party votes at each administrative unit, among other analyses.
partyVotesRaw <- within(partyVotesRaw, 
                        TotalEligibleVotes <- PS + PD + LSI + `Other Parties`)
partyVotesRaw <- within(partyVotesRaw, 
                        TotalVotes <- PS + PD + LSI + `Other Parties` 
                        + `Invalid Ballots`)
# Add turnout counts as Eligible Voters - Total Votes
partyVotesRaw <- within(partyVotesRaw, 
                        Turnout <- TotalVotes / `Eligible Voters`)
# write the data set out
write_csv(partyVotesRaw, partyVotesRawByAdminUnit, na = "NA", append = FALSE)

# Combine LSI + Other Parties into one vector due to their relative
# insignificance to PS and PD - the two major parties
partyVotes <- data.frame(partyVotesRaw$District,
                         partyVotesRaw$Municipality,
                         partyVotesRaw$`Administrative Unit`,
                         partyVotesRaw$PS,
                         partyVotesRaw$PD,
                         partyVotesRaw$LSI + partyVotesRaw$`Other Parties`,
                         partyVotesRaw$`Invalid Ballots`,
                         partyVotesRaw$`Eligible Voters`,
                         partyVotesRaw$TotalEligibleVotes,
                         partyVotesRaw$TotalVotes,
                         partyVotesRaw$Turnout)
names(partyVotes) <- c("District", "Municipality", "Administrative Unit", "PS", 
                       "PD", "Other Parties", "Invalid Ballots", 
                       "Eligible Voters", "Total Eligible Votes", 
                       "Total Votes", "Turnout")
# write the data set out
write_csv(partyVotes, partyVotesByAdminUnit, na = "NA", append = FALSE)

# Summarize data set
summary(partyVotes)
# describe() requires package `psych`
describe(partyVotes)

# Compute empirical probabilities for eligible votes for each party,
# invalid ballot/total votes , turnout/ eligible voters
partyVotesP <- data.frame(partyVotes$District,
                          partyVotes$Municipality,
                          partyVotes$`Administrative Unit`,
                          partyVotes$PS / partyVotes$`Total Eligible Votes`,
                          partyVotes$PD / partyVotes$`Total Eligible Votes`,
                          partyVotes$PS / partyVotes$PD,
                          partyVotes$`Other Parties` / 
                            partyVotes$`Total Eligible Votes`,
                          partyVotes$`Invalid Ballots` / 
                            partyVotes$`Total Votes`,
                          partyVotes$`Eligible Voters`,
                          partyVotes$`Total Eligible Votes`,
                          partyVotes$`Total Votes`,
                          partyVotes$Turnout)
names(partyVotesP) <- c("District", "Municipality", "Administrative Unit", "pPS", 
                       "pPD", "pPSPD", "pOP", "pInvalid", 
                       "Eligible Voters", "Total Eligible Votes", 
                       "Total Votes", "pTurnout")
# write the data set out
write_csv(partyVotesP, partyVotePByAdminUnit, na = "NA", append = FALSE)
# Quick look at some stats
summary(partyVotesP)

# Plot densities of PS, PD, OP, Turnout, and Invalid:
# in a 3x2 grid:
par(mfrow = c(3, 2))

# build a common x-axis range for the party plots
xrange <- range(c(partyVotesP$pPS, partyVotesP$pPD, partyVotesP$pOP) )

hist(partyVotesP$pPS, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Histogram of PS vote share"), xlim = xrange, 
     xlab = "PS vote share")
lines(density(partyVotesP$pPS), col = "navy", lwd = 2)
rug(partyVotesP$pPS, col = "gray")

hist(partyVotesP$pTurnout, breaks = "FD", freq = FALSE, col = "#56bd70",
     main = paste("Histogram of turnout rates"),
     xlab = "Turnout rate")
lines(density(partyVotesP$pTurnout), col = "navy", lwd = 2)
rug(partyVotesP$pTurnout, col = "gray")

hist(partyVotesP$pPD, breaks = "FD", freq = FALSE, col = "orange", 
     main = paste("Histogram of PD vote share"), xlim = xrange,
     xlab = "PD vote share")
lines(density(partyVotesP$pPD), col = "navy", lwd = 2)
rug(partyVotesP$pPD, col = "gray")

hist(partyVotesP$pInvalid, breaks = "FD", freq = FALSE, col = "#FE6F5E", 
     main = paste("Histogram of invalid ballot rates"), 
     xlab = "Invalid ballot rate")
lines(density(partyVotesP$pInvalid), col = "navy", lwd = 2)
rug(partyVotesP$pInvalid, col = "gray")

hist(partyVotesP$pOP, breaks = "FD", freq = FALSE, col = "orange", 
     main = paste("Histogram of other parties' vote shares"),
     xlim = xrange, xlab = "Other parties' vote share")
lines(density(partyVotesP$pOP), col = "navy", lwd = 2)
rug(partyVotesP$pOP, col = "gray")

# plot a normal curve for reference
plot(dnorm, -3, 3, col = "navy", lwd = 3, 
     main = "Standard Normal Distribution",
     xlab = "Z",
     ylab = "Density"
)

# reset chart area
par(mfrow = c(1, 1))

# Group aggregated votes by district to look at the constituency level
votesGroupedByDistrict <- partyVotes %>% group_by(District) %>% 
  summarize(PS = sum(PS), PD = sum(PD), `Other Parties` = sum(`Other Parties`),
            `Invalid Ballots` = sum(`Invalid Ballots`),
            `Eligible Voters` = sum(`Eligible Voters`),
            `Total Eligible Votes` = sum(`Total Eligible Votes`),
            meanVotes = mean(`Total Votes`),
            medianVotes = median(`Total Votes`),
            `Total Votes` = sum(`Total Votes`), 
            Turnout = sum(`Total Votes`) / sum(`Eligible Voters`))
# Add some computed empirical probabilities:
votesGroupedByDistrict <- within(votesGroupedByDistrict, 
                                 pPS <- PS / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pPD <- PD / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pOP <- `Other Parties` / 
                                   `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pInvalid <- `Invalid Ballots` 
                                 / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pTurnout <- Turnout)
# Print the district-level data set for later reference
write_csv(votesGroupedByDistrict, partyVotesByDistrictFile, na = "NA", 
          append = FALSE)

# Some insights
# ----
# Summary Table - create empty frame with two columns
summaryTable <- data.frame(character(0), numeric(0))
names(summaryTable) <- c("Data", "Result")

# Add data results to the frame:
newRow <- c("Eligible Voters", sum(partyVotes$`Eligible Voters`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Total Votes Cast", sum(partyVotes$`Total Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Total Eligible Votes", sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Invalid Ballots", 1.0 - sum(partyVotes$`Total Eligible Votes`) / 
              sum(partyVotes$`Total Votes`))       
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Turnout", sum(partyVotes$`Total Votes`) / 
              sum(partyVotes$`Eligible Voters`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS Tally", sum(partyVotes$PS)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS %-age", sum(partyVotes$PS) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS Seats", 74) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD Tally", sum(partyVotes$PD)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD %-age", sum(partyVotes$PD) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD Seats", 59) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties Tally", sum(partyVotes$`Other Parties`)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties %-age", sum(partyVotes$`Other Parties`) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties Seats", 7) 
summaryTable[nrow(summaryTable)+1, ] <- newRow

view(summaryTable)
write_csv(summaryTable, descripStatsFile, na = "NA", append = FALSE)
# ----
# Largest and smallest turnouts were in districts:
minTurnout <- votesGroupedByDistrict[which(votesGroupedByDistrict$pTurnout == 
                               min(votesGroupedByDistrict$pTurnout)), ]
maxTurnout <- votesGroupedByDistrict[which(votesGroupedByDistrict$pTurnout == 
                               max(votesGroupedByDistrict$pTurnout)), ]
view(minTurnout)
view(maxTurnout)

# ----
# Largest and smallest relative invalid ballot proportions were in districts:
minInvalid <- votesGroupedByDistrict[which(votesGroupedByDistrict$pInvalid == 
                               min(votesGroupedByDistrict$pInvalid)), ]
maxInvalid <- votesGroupedByDistrict[which(votesGroupedByDistrict$pInvalid == 
                               max(votesGroupedByDistrict$pInvalid)), ]

# Total invalid ballots & proportion
totalInvalidBallots <- sum(votesGroupedByDistrict$`Invalid Ballots`)
pInvalidBallots <- sum(votesGroupedByDistrict$`Invalid Ballots`) / 
                      sum(votesGroupedByDistrict$`Total Votes`)
view(minInvalid)
view(maxInvalid)
totalInvalidBallots
pInvalidBallots
# ----

# Is the observed ~5% invalid ballot unusual, given prior election results?
# Let's test the hypotheses:
#   H_0: We expect around 2% invalid ballots (population proportion mean)
#        (The observed 5% invalid ballot proportion is not unusual.)
#   H_A: We got more than 2% invalid ballots
#        (The observed 5% invalid ballot proportion is unusual, given 
#        previous elections' results averaging at ~2%.)
# We'll use a one-sample prop-test against the historical control.

# We expect 2% of total votes being invalid ballots based on prior elections
expected <- 0.02

# test if z-scores can be used
if (min(sum(partyVotes$`Invalid Ballots`) * pInvalidBallots,
    sum(partyVotes$`Invalid Ballots`) * (1 - pInvalidBallots)) >= 15) {
  paste ("Use z-scores")
}
# standard deviation
stdev <- sqrt(pInvalidBallots * (1 - pInvalidBallots) / 
                sum(partyVotes$`Invalid Ballots`))

# z-score
z <- (pInvalidBallots - expected) / stdev

# P(b >= z) = 1 - P(b < z):
pValue <- 1 - pnorm(z, mean = 0, sd = 1, lower.tail = TRUE)
pValue

# 2021 election sample confidence interval @95%
alfa <- 0.05
lInt <- pInvalidBallots - abs(qnorm(alfa/2)) * 
                            sqrt(pInvalidBallots * (1 - pInvalidBallots) / 
                            length(partyVotesP$pInvalid))
rInt <- pInvalidBallots + abs(qnorm(alfa/2)) * 
                            sqrt(pInvalidBallots * (1 - pInvalidBallots) / 
                            length(partyVotesP$pInvalid))
paste("C.I. at 95%: (", round(lInt, 4), ", ", round(rInt, 4), 
      "); Historical control:", expected, sep = "")

# ----

# Group aggregated votes by municipality to look at that level
votesGroupedByMunicipality <- partyVotes %>% group_by(Municipality) %>% 
  summarize(PS = sum(PS), PD = sum(PD), `Other Parties` = sum(`Other Parties`),
            `Invalid Ballots` = sum(`Invalid Ballots`),
            `Eligible Voters` = sum(`Eligible Voters`),
            `Total Eligible Votes` = sum(`Total Eligible Votes`),
            `Total Votes` = sum(`Total Votes`), 
            Turnout = sum(`Total Votes`) / sum(`Eligible Voters`))
# Add some computed empirical probabilities:
votesGroupedByMunicipality <- within(votesGroupedByMunicipality, 
                                 pPS <- PS / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pPD <- PD / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pOP <- `Other Parties` / 
                                   `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pInvalid <- `Invalid Ballots` 
                                 / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pTurnout <- Turnout)

# Print the municipality-level data set for later reference
write_csv(votesGroupedByMunicipality, partyVotesByMunicipalityFile, na = "NA", 
          append = FALSE)

# ----
# control chart for variability
qic(partyVotesP$pPS, chart="i", agg.fun = c("mean"), 
    title = "PS Vote Share Control Chart",
    xlab = "Administrative Unit",
    ylab = "Vote Share",
    print.summary = TRUE)
qic(partyVotesP$pPD, chart="i", agg.fun = c("mean"), 
    title = "PD Vote Share Control Chart",
    xlab = "Administrative Unit",
    ylab = "Vote Share",
    print.summary = TRUE)

################################################
# Process raw granular data into proportions and 
# compute other variables, such as turnout %-age
# some turnout tallies are zero; control zero-division
fPartyVotes <- data.frame(str_to_sentence(rawQVVotes$District),
                          rawQVVotes$Municipality,
                          rawQVVotes$AdministrativeUnit,
                          paste("P-", rawQVVotes$PollingStation, sep = ""),
                          rawQVVotes$ListedVoters,
                          rawQVVotes$ListedWomenVoters,
                          rawQVVotes$ListedWomenVoters / 
                            rawQVVotes$ListedVoters,
                          rawQVVotes$ListedVoters - 
                            rawQVVotes$ListedWomenVoters,
                          1.0 - rawQVVotes$ListedWomenVoters / 
                            rawQVVotes$ListedVoters,
                          rawQVVotes$VotesCast,
                          rawQVVotes$VotesCast / rawQVVotes$ListedVoters,
                          rawQVVotes$WomenVoted,
                          rawQVVotes$WomenVoted / rawQVVotes$VotesCast,
                          rawQVVotes$VotesCast - rawQVVotes$WomenVoted,
                          1.0 - rawQVVotes$WomenVoted / rawQVVotes$VotesCast,
                          rawQVVotes$ValidBallots,
                          rawQVVotes$ValidBallots / rawQVVotes$VotesCast,
                          rawQVVotes$InvalidBallots,
                          1.0 - rawQVVotes$ValidBallots / rawQVVotes$VotesCast,
                          rawQVVotes$PSD,
                          rawQVVotes$PSD / rawQVVotes$ValidBallots,
                          rawQVVotes$PBK,
                          rawQVVotes$PBK / rawQVVotes$ValidBallots,
                          rawQVVotes$PLDSH,
                          rawQVVotes$PLDSH / rawQVVotes$ValidBallots,
                          rawQVVotes$BD,
                          rawQVVotes$BD / rawQVVotes$ValidBallots,
                          rawQVVotes$ABEOK,
                          rawQVVotes$ABEOK / rawQVVotes$ValidBallots,
                          rawQVVotes$LSI,
                          rawQVVotes$LSI / rawQVVotes$ValidBallots,
                          rawQVVotes$NTH,
                          rawQVVotes$NTH / rawQVVotes$ValidBallots,
                          rawQVVotes$LRE,
                          rawQVVotes$LRE / rawQVVotes$ValidBallots,
                          rawQVVotes$PDAN,
                          rawQVVotes$PDAN / rawQVVotes$ValidBallots,
                          rawQVVotes$ADR,
                          rawQVVotes$ADR / rawQVVotes$ValidBallots,
                          rawQVVotes$LN,
                          rawQVVotes$LN / rawQVVotes$ValidBallots,
                          rawQVVotes$PS,
                          rawQVVotes$PS / rawQVVotes$ValidBallots,
                          rawQVVotes$Other,
                          rawQVVotes$Other / rawQVVotes$ValidBallots
                          )

names(fPartyVotes) <- c("District",
                        "Municipality",
                        "AdministrativeUnit",
                        "PollingStation",
                        "EligibleVoters",
                        "EligibleWomenVoters",
                        "pEligibleWomenVoters",
                        "EligibleMenVoters",
                        "pEligibleMenVoters",
                        "VotesCast",
                        "pTurnout",
                        "VotingWomen",
                        "pVotingWomen",
                        "VotingMen",
                        "pVotingMen",
                        "ValidBallots",
                        "pValidBallots",
                        "InvalidBallots",
                        "pInvalidBallots",
                        "PSD",
                        "pPSD",
                        "PBK",
                        "pPBK",
                        "PLDSH",
                        "pPLDSH",
                        "BD",
                        "pBD",
                        "ABEOK",
                        "pABEOK",
                        "LSI",
                        "pLSI",
                        "NTH",
                        "pNTH",
                        "LRE",
                        "pLRE",
                        "PD",
                        "pPD",
                        "ADR",
                        "pADR",
                        "LN",
                        "pLN",
                        "PS",
                        "pPS",
                        "Other",
                        "pOther"
                        )

# print the output
write_csv(fPartyVotes, partyVotesRawByPollingStation, na = "NA", append = FALSE)

# Remove any NA records from further statistical analyses
fPartyVotesAll <- fPartyVotes # save a copy for all 5199 stations just in case
fPartyVotes %>% drop_na() -> fPartyVotes
