# women_vote.R
# Analyze women vote gap & other differences using the 5199 data points.
# CC BY-SA. W.A. Borici, 2021.
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

districtLevelStats <- here("out/women_men_district.csv")

# Summarize men & women voters by district for the major political groupings
votesByDistrict <- fPartyVotes %>% group_by(District) %>% 
  summarize(PS = sum(PS), 
            PD = sum(PD), 
            Others = sum(PSD, PBK, PLDSH, BD, ABEOK, LSI, NTH, LRE,
                           ADR, LN, Other),
            InvalidBallots = sum(InvalidBallots),
            RegisteredMen = sum(EligibleMenVoters),
            RegisteredWomen = sum(EligibleWomenVoters),
            VotingWomen = sum(VotingWomen),
            VotingMen = sum(VotingMen),
            UnaccountedBallots = sum(VotingWomen) + sum(VotingMen) - (sum(PS) +
               sum(PD) + 
                 sum(PSD, PBK, PLDSH, BD, ABEOK, LSI, NTH, LRE, ADR, LN, Other) 
               + sum(InvalidBallots)),
            pTurnout = (sum(VotingWomen) + sum(VotingMen)) /
              (sum(EligibleMenVoters) + sum(EligibleWomenVoters)),
            pVotingWomen = sum(VotingWomen) / sum(EligibleWomenVoters),
            pVotingMen = sum(VotingMen) / sum(EligibleMenVoters)
            )

write_csv(votesByDistrict, districtLevelStats, na = "NA", append = FALSE)

# Men and women who voted
m = sum(votesByDistrict$VotingMen)
w = sum(votesByDistrict$VotingWomen)
pM = m / (m + w)
pW = 1.0 - pM
paste("Voting men: ", m, " (", round(pM * 100, 2), "%)", sep = "")
paste("Voting women: ", w, " (", round(pW * 100, 2), "%)", sep = "")

# Overall turnout for men and women
rM <- sum(votesByDistrict$RegisteredMen)
rW <- sum(votesByDistrict$RegisteredWomen)
tM <- m / rM
tW <- w / rW
paste("Registered men: ", rM, " (", round(tM * 100, 2), "%)", sep = "")
paste("Registered women: ", rW, " (", round(tW * 100, 2), "%)", sep = "")
pRM <- rM / (rM + rW)
pRW <- 1.0 - pRM
pRM
pRW

# Plot district-level turnout via a group bar plot
# wrangle the vectors into a new frame,
# and update Voters column values to user-friendly names for the legend
districtData <- data.frame(votesByDistrict$District, 
                 votesByDistrict$RegisteredMen,
                 votesByDistrict$RegisteredWomen,
                 votesByDistrict$VotingMen,
                 votesByDistrict$VotingWomen)
names(districtData) <- c("District", 
                         "Registered Men", 
                         "Registered Women",
                         "Voting Men", 
                         "Voting Women"
                         )
districtData %>% pivot_longer(cols = -District, 
                              names_to = "Voters",
                              values_to = "Tally (thousands)"
) -> districtData

# Pivot turnout data into a separate frame for labeling bars
# with the trick of zeroing out non-proportions so labels are not displayed
# on geom_text, except for proportion labels
turnoutData <- data.frame(votesByDistrict$District, 
                          0, 
                          0,
                          votesByDistrict$pVotingMen,
                          votesByDistrict$pVotingWomen)
names(turnoutData) <- c("District", 
                        "Registered Men", 
                        "Registered Women",
                        "Voting Men", 
                        "Voting Women")
turnoutData %>% pivot_longer(cols = -District, 
                             names_to = "Voters",
                             values_to = "Tally (thousands)"
                             ) -> turnoutData
strTunrout <-c("Registered Men", 
               "Registered Women", 
               "Voting Men",
               "Voting Women" 
               )

# factor levels to preserve order for proper bar-label matching:
districtData$Voters <- factor(districtData$Voters)
turnoutData$Voters <- factor(turnoutData$Voters)

ggplot(districtData, aes(x = District, y = `Tally (thousands)`/1000, fill = Voters)) +
  theme_minimal()+
  geom_bar(stat='identity', position='dodge') + 
  coord_flip() +
  geom_text(data = subset(turnoutData, Voters %in% strTunrout),
    aes(label = ifelse(`Tally (thousands)` == 0, 
                       "", 
                       paste(round(`Tally (thousands)` * 100, 2), 
                             "%", sep = ""
                             )
                       )
        ),
    position = position_dodge(width = .95),  # center around the right two bars
    vjust = 0.5,    # nudge above top of bar
    hjust = 1.1,
    size = 3,
    show.legend = FALSE) +
  xlab(paste("District: Registered and voting men and women with turnout %-ages")) +
  ylab("Tally (thousands)") +
  scale_fill_manual(values = c("#cdd9e4", "#8392a1", "#ffa600", "#0066cc")) +
  ggtitle(paste("Men and women tournout: ", 
                "2021 Albanian parliamentary elections")) +
  theme(legend.title=element_blank(), legend.position = "bottom") + 
  theme(plot.title = element_text(size=12))

# ----
# plot only district turnout rates and the national turnout rate
districtData <- data.frame(votesByDistrict$District, 
                           votesByDistrict$pTurnout,
                           votesByDistrict$pVotingMen,
                           votesByDistrict$pVotingWomen)
names(districtData) <- c("District", 
                         "District Turnout",
                         "Men Turnout", 
                         "Women Turnout"
)
districtData %>% pivot_longer(cols = -District, 
                              names_to = "Voters",
                              values_to = "Turnout"
) -> districtData

view(votesByDistrict)

nationalTurnout <- (sum(votesByDistrict$VotingMen) + sum(votesByDistrict$VotingWomen)) / 
  (sum(votesByDistrict$RegisteredMen) + sum(votesByDistrict$RegisteredWomen))

ggplot(districtData, aes(x = District, y = Turnout, fill = Voters)) +
  theme_minimal()+
  geom_bar(stat='identity', position='dodge') + 
  geom_hline(aes(yintercept = nationalTurnout, 
                 linetype = "National Turnout"), color="yellow3") +
  coord_flip() +
  geom_text(aes(label = paste(round(Turnout * 100, 2), "%", sep = "")),
            position = position_dodge(width = .9),  # center around the right two bars
            vjust = 0.5,    # nudge above top of bar
            hjust = -0.05,
            size = 3,
            show.legend = FALSE) +
  xlab(paste("District")) +
  ylab("Turnout %-age") +
  scale_fill_manual(values = c("#c3c7e8", "#a35b9c", "#9bbb7e")) +
  ggtitle(paste("Men and women tournout: ", 
                "2021 Albanian parliamentary elections")) +
  theme(legend.title=element_blank(), legend.position = "bottom") + 
  theme(plot.title = element_text(size=12))+
  guides(linetype=guide_legend(override.aes=list(colour = c("yellow3"))))

#----
# Normality tests for turnout for men, women, and the difference w-m
turnoutM <- NormTestsWithVisuals(fPartyVotes$pVotingMen, "Men Turnout", 0)
turnoutW <- NormTestsWithVisuals(fPartyVotes$pVotingWomen, "Women Turnout", 0)
turnoutD <- NormTestsWithVisuals(
                  fPartyVotes$pVotingWomen - fPartyVotes$pVotingMen 
                  , "Women-Men Turnout Difference", 0)

par(mfrow = c(2, 2))
# basic plots for visual reference:
hist(fPartyVotes$pVotingMen, breaks = "FD", freq = FALSE, col = "#ffa600", 
     main = paste("Histogram of men turnout proportions"),
     xlab = "Turnout (men)")
lines(density(fPartyVotes$pVotingMen), col = "navy", lwd = 2)
rug(fPartyVotes$pVotingMen, col = "gray")

hist(fPartyVotes$pVotingWomen, breaks = "FD", freq = FALSE, col = "#0066cc", 
     main = paste("Histogram of women turnout proportions"),
     xlab = "Turnout (women)")
lines(density(fPartyVotes$pVotingWomen), col = "navy", lwd = 2)
rug(fPartyVotes$pVotingWomen, col = "gray")

hist(fPartyVotes$pVotingWomen - fPartyVotes$pVotingMen, 
     breaks = "FD", freq = FALSE, col = "#56bd70", 
     main = paste("Histogram of women/men turnout proportion differences"),
     xlab = "Turnout difference")
lines(density(fPartyVotes$pVotingWomen - fPartyVotes$pVotingMen), 
      col = "navy", lwd = 2)
rug(fPartyVotes$pVotingWomen - fPartyVotes$pVotingMen, col = "gray")

par(mfrow = c(1, 1))

# Remove the 161 outliers from the normality test & re-plot histograms:
wT <- fPartyVotes$pVotingWomen[fPartyVotes$pVotingWomen != 0]
mT <- fPartyVotes$pVotingMen[fPartyVotes$pVotingMen != 1]

turnoutM <- NormTestsWithVisuals(mT, "Men Turnout (161 outliers removed)", 0)
turnoutW <- NormTestsWithVisuals(wT, "Women Turnout (161 outliers removed)", 0)
turnoutD <- NormTestsWithVisuals(wT - mT, 
                                "Women-Men Turnout Difference (no outliers)", 0)

par(mfrow = c(2, 2))
# basic plots for visual reference:
hist(mT, breaks = "FD", freq = FALSE, col = "#ffa600", 
     main = paste("Histogram of men turnout proportions"),
     xlab = "Turnout (men)")
lines(density(mT), col = "navy", lwd = 2)
rug(mT, col = "gray")

hist(wT, breaks = "FD", freq = FALSE, col = "#0066cc", 
     main = paste("Histogram of women turnout proportions"),
     xlab = "Turnout (women, no outliers)")
lines(density(wT), col = "navy", lwd = 2)
rug(wT, col = "gray")

hist(wT - mT, 
     breaks = "FD", freq = FALSE, col = "#56bd70", 
     main = paste("Histogram of women/men turnout proportion differences"),
     xlab = "Turnout difference (no outliers)")
lines(density(wT - mT), 
      col = "navy", lwd = 2)
rug(wT - mT, col = "gray")

par(mfrow = c(1, 1))

#----
#### Research question ###
# Was there any significant difference between the turnout rates of men
# and women voters for each of the 12 districts?
# H_0: p_w - p_m = 0
# H_A: p_w - p_m != 0

# run various dichotomous-variable comparison tests
# for each of the 12 districts:
resTDiff <- NULL # to store final results
for (i in 1:length(votesByDistrict$District)) {
  
  restDiff1 <- NULL # temp data frame for binding results to final tibble
  
  thisDistrict <- votesByDistrict$District[i] # current district
  
  r <- CompareTurnouts(votesByDistrict, thisDistrict, 4)
  r1 <- r[[1]][[1]] # turnout difference in percentage points: pw - pm
  r21 <- r[[2]][[1]][[3]] # prop-test p-value
  r22 <- r[[2]][[1]][[6]] # prop-test c.i.
  r3 <- r[[2]][[2]] # LRT p-value
  r4 <- r[[2]][[3]][[3]] # Chi-sq p-value
  
  resTDiff1 <- data.frame(District <- thisDistrict,
                         TurnoutDiff <- r1,
                         PropTestPVal <- r21,
                         PropTestCI <- r22,
                         LRTPval <- r3,
                         ChiSqPval <- r4
  )
  names(resTDiff1) <- c("District", 
                       "TurnoutDiff", 
                       "Prop.Test p-val", 
                       "Prop.Test C.I.",
                       "LRT p-val",
                       "Chi-Sq p-val")
  
  resTDiff <- rbind(resTDiff, resTDiff1)
}

view(resTDiff)

# Because the above analysis assumes simple random samples and includes
# no knowledge of confidence intervals for population estimates, let's try
# bootstrapping the 95% C.I. for women and men turnouts using fPartyVotes:

fun.stat <- function(vecData, idx){
  
  vd <- vecData[idx]
  
  return(mean(vd))
}
set.seed(42)

# Change the district name to bootstrap turnout data for that district
dataFreq <- subset(fPartyVotes, District=="Kukes")
#view(dataFreq)

womenBtstr <- boot(data = dataFreq$pVotingWomen, 
                   statistic = fun.stat, 
                   R = 1000)
menBtstr <- boot(data =  dataFreq$pVotingMen, 
                 statistic = fun.stat, 
                 R = 1000)
# plot(womenBtstr)
# plot(menBtstr)


# Bootstrap the CI's at 95%:
boot.ci(boot.out = womenBtstr, 
        type = c("norm", "basic",
                 "perc"))
boot.ci(boot.out = menBtstr, 
        type = c("norm", "basic",
                 "perc"))

#################################################################
# Plot some historical turnout data as time series, where available
# ----
# Data extracted partly from CEC, partly from OSCE
# Year	Overall Turnout
# 2021	46.29%
# 2017	46.75%
# 2013	53.46%
# 2009	50.50%
# 2005	49.23%
# 2001	53.60%
# 1997	72.60%

turnoutEoE <- data.frame(Year <- 2021,
                         Turnout <- 0.4629)
names(turnoutEoE) <- c("Year", "Turnout")

tmp <- data.frame(2017,
                  0.4675)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

tmp <- data.frame(2013,
                  0.5346)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

tmp <- data.frame(2009,
                  0.505)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

tmp <- data.frame(2005,
                  0.4923)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

tmp <- data.frame(2001,
                  0.536)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

tmp <- data.frame(1997,
                  0.726)
names(tmp) <- c("Year", "Turnout")
turnoutEoE <- rbind(turnoutEoE, tmp)

# plot the series and the 2021 women & men turnouts -- the only available data
x_axis_labels <- turnoutEoE$Year
ggplot(turnoutEoE) +
  geom_line(aes(x = Year, y = Turnout), size = 1, color = "orange2") +
  geom_point(aes(x = Year, y = Turnout), size = 2, color = "navy") +
  geom_text(aes(label = paste(Turnout*100, "%", sep = ""), 
                              x = Year, y = Turnout, hjust = 0, vjust = -0.7)) +
  xlab(paste("Parliamentary election year")) +
  ylab("Turnout %-age") +
  ggtitle(paste("Albanian parliamentary election turnout over the years")) +
  theme(plot.title = element_text(size=12))+
  geom_point(aes(x = 2021, y = 0.4461, color = "Women turnout"), 
             color = "red", shape = 2) +
  geom_text(label = "W: 44.61%", x = 2021, y = 0.4461, hjust = 0.5, vjust = 1.4) +
  geom_point(aes(x = 2021, y = 0.4801, color = "Men turnout"), 
             color = "blue", shape = 6) +
  geom_text(label = "M: 48.01%", x = 2021, y = 0.4801, hjust = 0.5, vjust = -0.6) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  ylim(0.4, 0.8) +
  theme(legend.title=element_blank(), legend.position = "bottom")
  