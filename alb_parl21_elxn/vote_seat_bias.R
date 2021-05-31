# vote_seat_bias.R
# Analyze vote-seat curves and election bias.
# CC BY-SA. W.A. Borici, 2021. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

#
# Vote-Seat share analysis
# ----
# Convert seats to shares (%-ages)
voteSeatShare <- within(voteSeatShare, 
                        TotalSeats <- (`PS Seats` + `PD Seats` + `OP Seats`))
voteSeatShare <- within(voteSeatShare, 
                        PSSeatShare <- `PS Seats` / TotalSeats)
voteSeatShare <- within(voteSeatShare, 
                        PDSeatShare <- `PD Seats` / TotalSeats)
voteSeatShare <- within(voteSeatShare, 
                        OPSeatShare <- `OP Seats` / TotalSeats)

# Theoretical s-v curves, where parameters a and b are computed
# empirically from the Albanian election results of 2021 and 2017
svPSCurve = function(x){
  # compute experimental consts a and b from the last two elections:
  vsPS1 <- voteSeatShare %>% filter(`Election Year` == 2017)
  vsPS2 <- voteSeatShare %>% filter(`Election Year` == 2021)
  s1 <- vsPS1$PSSeatShare
  v1 <- vsPS1$`PS Vote Share`
  s2 <- vsPS2$PSSeatShare
  v2 <- vsPS2$`PS Vote Share`
  
  b <- log(s1*(1-s2) / s2*(1-s1)) / log(v1*(1-v2) / v2*(1-v1))
  a <- (s1/(1-s1)) / (v1/(1-v1))^b
  
  a*x^b / (a*x^b + (1-x)^b)
}
#plot(svPSCurve, 0, 1)

# PD Curve
svPDCurve = function(x){
  # compute experimental consts a and b from the last two elections:
  vsPS1 <- voteSeatShare %>% filter(`Election Year` == 2017)
  vsPS2 <- voteSeatShare %>% filter(`Election Year` == 2021)
  s1 <- vsPS1$PDSeatShare
  v1 <- vsPS1$`PD Vote Share`
  s2 <- vsPS2$PDSeatShare
  v2 <- vsPS2$`PD Vote Share`
  
  b <- log(s1*(1-s2) / s2*(1-s1)) / log(v1*(1-v2) / v2*(1-v1))
  a <- (s1/(1-s1)) / (v1/(1-v1))^b
  
  a*x^b / (a*x^b + (1-x)^b)
}
#plot(svPDCurve, 0, 1)

# Other Parties curve
svOPCurve = function(x){
  # compute experimental consts a and b from the last two elections:
  vsPS1 <- voteSeatShare %>% filter(`Election Year` == 2017)
  vsPS2 <- voteSeatShare %>% filter(`Election Year` == 2021)
  s1 <- vsPS1$OPSeatShare
  v1 <- vsPS1$`OP Vote Share`
  s2 <- vsPS2$OPSeatShare
  v2 <- vsPS2$`OP Vote Share`
  
  b <- log(s1*(1-s2) / s2*(1-s1)) / log(v1*(1-v2) / v2*(1-v1))
  a <- (s1/(1-s1)) / (v1/(1-v1))^b
  
  a*x^b / (a*x^b + (1-x)^b)
}
#plot(svOPCurve, 0, 1)

# Next, plot the vote-seat share for each party & year to infer the curve.
# x-axis = percentage of votes
# y-axis = percentage of shares
# plot three curves - one for each party

# create the three data sets we wish to plot
# x = vote share, y = seat share
psVS <- data.frame(voteSeatShare$`PS Vote Share`, voteSeatShare$PSSeatShare)
names(psVS) <- c("vPS", "sPS")
pdVS <- data.frame(voteSeatShare$`PD Vote Share`, voteSeatShare$PDSeatShare)
names(pdVS) <- c("vPD", "sPD")
opVS <- data.frame(voteSeatShare$`OP Vote Share`, voteSeatShare$OPSeatShare)
names(opVS) <- c("vOP", "sOP")

ggplot() +
  geom_point(psVS, mapping = aes(x = vPS, y = sPS, color = "PS V-S"), size=3) +
  geom_point(pdVS, mapping = aes(x = vPD, y = sPD, color = "PD V-S"), size=3) +
  geom_point(opVS, mapping = aes(x = vOP, y = sOP, color = "OP V-S"), size=3) +
  geom_function(fun = svPSCurve, aes(color = "PS V-S")) +
  geom_function(fun = svPDCurve, aes(color = "PD V-S")) +
  geom_function(fun = svOPCurve, aes(color = "OP V-S")) +
  xlim(0, 1) +
  ylim(0, 1) +
  scale_shape_manual(values = c(3, 16, 17)) +
  scale_color_manual(values = c("orange2", "navy", "red2")) +
  xlab(paste("Vote Percentage")) +
  ylab("Seat Percentage") +
  ggtitle(paste("Albanian vote-seat (V-S) share curves: \n", 
                "1997-2021 parliamentary elections \n",
                "- Dots represent observed vote-seat shares \n",
                "- Curves represent the vote-seat share models")) +
  theme(legend.title=element_blank(), legend.position = "bottom") + 
  theme(plot.title = element_text(size=12))
