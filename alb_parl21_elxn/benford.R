# benford.R
# Checking conformity with Benford's Law of leading and second digits for 
# reported figures -- party votes and invalid ballots -- at the admin unit.
# CC BY-SA. W.A. Borici, 2021.
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.


# output suspects
benford_PS_Leading_Suspects <- "out/benford_ps_leading_digit_suspects.csv"
benford_PD_Leading_Suspects <- "out/benford_pd_leading_digit_suspects.csv"
benford_OP_Leading_Suspects <- "out/benford_others_leading_digit_suspects.csv"
benford_IB_Leading_Suspects <- "out/benford_invalid_leading_digit_suspects.csv"

# ----
# Plot chart
PlotBenford(NULL, "Benford's Law", FALSE)

###############################
# LEADING DIGITS
# --------------
# Leading-digit PS
benfordPS <- benford(partyVotes$PS, number.of.digits = 1)

# Leading-digit plots
df <- data.frame(benfordPS[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "PS", TRUE)

# plot a big picture of analysis
plot(benfordPS)

# View suspects
partyVotesPS <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PS)
suspectsPS <- getSuspects(benfordPS, partyVotesPS)
names(suspectsPS) <- c("District", "Municipality", "Administrative Unit", 
                      "Vote Tally")
write_csv(suspectsPS, benford_PS_Leading_Suspects, na = "NA", append = FALSE)

# Leading-digit PD
benfordPD <- benford(partyVotes$PD, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordPD[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "PD", TRUE)

plot(benfordPD)
partyVotesPD <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PD)
suspectsPD <- getSuspects(benfordPD, partyVotesPD)
names(suspectsPD) <- c("District", "Municipality", "Administrative Unit", 
                       "Vote Tally")
write_csv(suspectsPD, benford_PD_Leading_Suspects, na = "NA", append = FALSE)

# Leading-digit Other Parties
benfordOP <- benford(partyVotes$`Other Parties`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordOP[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "OP", TRUE)

plot(benfordOP)
partyVotesOP <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Other Parties`)
suspectsOP <- getSuspects(benfordOP, partyVotesOP)
names(suspectsOP) <- c("District", "Municipality", "Administrative Unit", 
                       "Vote Tally")
write_csv(suspectsOP, benford_OP_Leading_Suspects, na = "NA", append = FALSE)

# Leading-digit Invalid Ballots
benfordIB <- benford(partyVotes$`Invalid Ballots`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordIB[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Invalid Ballots", TRUE)

plot(benfordIB)
partyVotesIB <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Invalid Ballots`)
suspectsIB <- getSuspects(benfordIB, partyVotesIB)
names(suspectsIB) <- c("District", "Municipality", "Administrative Unit", 
                       "Vote Tally")
write_csv(suspectsIB, benford_IB_Leading_Suspects, na = "NA", append = FALSE)

# Total eligible vote count
benfordTEV <- benford(partyVotes$`Total Eligible Votes`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordTEV[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Total Eligible", TRUE)

plot(benfordTEV)
partyVotesTEV <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Total Eligible Votes`)
suspectsIB <- getSuspects(benfordTEV, partyVotesTEV)
view(suspectsTEV)

# Total tally
benfordTV <- benford(partyVotes$`Total Votes`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordTV[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Total", TRUE)

plot(benfordTV)
partyVotesTV <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Total Votes`)
suspectsIB <- getSuspects(benfordTV, partyVotesTV)
view(suspectsTV)

# Plot histogram of total votes (log) and overlay party vote frequency lines
# to analyze distributions in context
# totV <- data.frame(log10(partyVotes$`Total Votes`))
# names(totV) <- c("LogV")
psV <- data.frame(log10(partyVotes$PS))
names(psV) <- c("LogV")
pdV <- data.frame(log10(partyVotes$PD))
names(pdV) <- c("LogV")
opV <- data.frame(log10(partyVotes$`Other Parties`))
names(opV) <- c("LogV")
ibV <- data.frame(log10(partyVotes$`Invalid Ballots`))
names(ibV) <- c("LogV")

# Combine log-votes into a vector for plotting frequency lines against
# total log-vote histogram:
combined <- vector("numeric")
combined <- append(combined, psV$LogV)
combined <- append(combined, pdV$LogV)
combined <- append(combined, opV$LogV)
combined <- append(combined, ibV$LogV)
totV <- data.frame(combined)
names(totV) <- c("LogV")

nrBins <- 40
gp <- ggplot() +
  geom_histogram(bins = nrBins, totV , mapping = aes(x = LogV,
                                                     color = paste("Total log-vote histogram")),
                 fill = "white") +
  geom_freqpoly(bins = nrBins, psV, mapping = aes(x = LogV,
                                                  color = "PS log-votes"), size = 0.9) +
  geom_freqpoly(bins = nrBins, pdV, mapping = aes(x = LogV,
                                                  color = "PD log-votes"), size = 0.9) +
  geom_freqpoly(bins = nrBins, opV, mapping = aes(x = LogV,
                                                  color = "OP log-votes"), size = 0.6) +
  geom_freqpoly(bins = nrBins, ibV, mapping = aes(x = LogV,
                                                  color = "IB log-votes"), size = 0.3) +
  scale_color_manual(values=c("red2", "navy", "orange", "green4", "grey", "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  xlab(paste("Log-vote")) +
  ylab("Frequency") +
  ggtitle(paste("Log-plot of total votes, party votes, and invalid ballots")) +
  theme(legend.title=element_blank(), legend.position = "bottom")

# plot gp
gp
# view plot data for analysis -- stored in $data[[i]]
histData <- ggplot_build(gp) # contains the histogram data by ggplot
# [[1]]: histogram; [[2]] -- [[5]]: party lines & invalid ballots
# to reverse-engineer vote counts, find the xmin-xmax range,
# filter the corresponding vector (e.g. psV or pdV, etc.) based on that range,
# and compute 10^(log-value) to get the original count
dx <- histData$data[[1]]
#view(dx)
#write_csv(ibV, file = "out/hist_benford.csv")


# FIRST TWO DIGITS
# --------------
# Two-digit PS
benfordPS <- benford(partyVotes$PS, number.of.digits = 2)
plot(benfordPS)
partyVotesPS <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PS)
suspectsPS <- getSuspects(benfordPS, partyVotesPS)
view(suspectsPS)

# Two-digit PD
benfordPD <- benford(partyVotes$PD, number.of.digits = 2)
plot(benfordPD)
partyVotesPD <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PD)
suspectsPD <- getSuspects(benfordPD, partyVotesPD)
view(suspectsPD)

# Two-digit Other Parties
benfordOP <- benford(partyVotes$`Other Parties`, number.of.digits = 2)
plot(benfordOP)
partyVotesOP <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Other Parties`)
suspectsOP <- getSuspects(benfordOP, partyVotesOP)
view(suspectsOP)

# Two-digit Invalid Ballots
benfordIB <- benford(partyVotes$`Invalid Ballots`, number.of.digits = 2)
plot(benfordIB)
partyVotesIB <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Invalid Ballots`)
suspectsIB <- getSuspects(benfordIB, partyVotesIB)
view(suspectsIB)

#####
# Testing Benford's Law w/ K-S:
# ----
# PS
benfordPS <- DiscreteBenfordKSTestWithVisuals(partyVotes$PS, "PS")
# Print results - graph and text - in files

benfordPS[[1]]
benfordPS[[2]]

# PD
benfordPD <- DiscreteBenfordKSTestWithVisuals(partyVotes$PD, "PD")
# Print results - graph and text - in files

benfordPD[[1]]
benfordPD[[2]]

# OP
benfordOP <- DiscreteBenfordKSTestWithVisuals(partyVotes$`Other Parties`, 
                                                "Other Parties")
# Print results - graph and text - in files

benfordOP[[1]]
benfordOP[[2]]

# IB
benfordIB <- DiscreteBenfordKSTestWithVisuals(partyVotes$`Invalid Ballots`, 
                                                "Invalid Ballots")
# Print results - graph and text - in files

benfordIB[[1]]
benfordIB[[2]]
