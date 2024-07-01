# fun.R
# Function definitions.
# CC BY-SA. W.A. Borici, 2021. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Execute normality tests & related plots
# for continuous (isDiscrete = 0) or discrete data
NormTestsWithVisuals <- function(vecData, lblData, isDiscrete) {
  # Density chart
  grDensity <- ggdensity(vecData, 
            main = paste("Density plot of " , lblData , " Votes"),
            xlab = paste(lblData , 
                         " vote proportions"))
  
  # QQ chart
  grQQ <- ggqqplot(vecData)
  
  # Plot the K-S test results as S-curves
  # EDF vector:
  vecEDFlength <- length(vecData) # number of variables in vecData
  
  # Create vecEDF as the EDF of KS Test
  vecEDF <- vector(mode = "numeric", length = vecEDFlength)
  for (i in 1:vecEDFlength) {
    vecEDF[i] <- as.double(i)/as.double(vecEDFlength)
  }
  
  # sort data ascending
  vecData <- sort(vecData, decreasing = FALSE)
  # Build CDF of our data
  vecDataFx <- pnorm(vecData, mean(vecData), sd(vecData))
  
  # create a data set with the three vectors we wish to plot
  # vecData should be on our x-axis and the rest are our CDF curves
  frmEDF <- data.frame(vecData, vecEDF)
  names(frmEDF) <- c("p", "cdf")
  frmFx <- data.frame(vecData, vecDataFx)
  names(frmFx) <- c("p", "cdf")
  
  # Find the x_0 and y_0, y_1 coordinates for D_max = max|Fx - EDF|
  maxD <- abs(frmEDF$cdf[1] - frmFx$cdf[1])
  pos <- 1
  for (i in 2:vecEDFlength) {
    if (abs(frmEDF$cdf[i] - frmFx$cdf[i]) > maxD) {
      maxD = abs(frmEDF$cdf[i] - frmFx$cdf[i])
      pos <- i
    }
  }
  # max D is at position pos
  x0 <- vecData[pos]
  y0 <- frmEDF$cdf[pos]
  y1 <- frmFx$cdf[pos]  
  
  # Plot the curves & D_max
  grKS <- ggplot() +
    geom_line(frmEDF, mapping = aes(x = p, y = cdf, color = "EDF")) +
    geom_line(frmFx, mapping = aes(x = p, y = cdf, color = 
                                     paste("F(", lblData, ")"))) +
    scale_color_manual(values=c("navy", "orange")) +
    xlab(paste(lblData, " Vote Proportions")) +
    ylab("CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1], linetype = "Dmax"),
                 linetype = "solid", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=2) +
    ggtitle(paste("K-S Normality Test: ", lblData)) +
    theme(legend.title=element_blank(), legend.position = "bottom")
  
  resultKS <- vector(mode = "numeric", length = length(vecData))
  resultKSL <- vector(mode = "numeric", length = length(vecData))
  if (isDiscrete == 0) { # continuous distributions
    # Run the default KS test
    resultKS <- ks.test(vecData, "pnorm", mean(vecData), sd(vecData))
    
    # Run the Lilliefors normality test
    resultKSL <- LillieTest(vecData) 
    
  } else { # discrete distributions
    # Run the ks.boot function from lib Matching for the discrete KS test
    resultKS <- ks.boot(vecData, pnorm(length(vecData)), nboots = 1000, 
                        alternative = "two.sided")
  }
  
  # Run the Shapiro normality test, only for sample sizes 3 to 5000
  resultShap <- NULL
  if (vecEDFlength >= 3 & vecEDFlength <= 5000) {
    resultShap <- shapiro.test(vecData)
  }
  
  # Save results in a return list
  # NB: if discrete, resultKSL returns an empty vector
  results <- list(grDensity, grQQ, grKS, resultKS, resultKSL, resultShap)
  
  return(results)
}

# Execute two-sample discrete K-S test & related plots for Benford's Law
# sample1 - vector of integers
DiscreteBenfordKSTestWithVisuals <- function(sample1, lblData) {
  
  sample1 <- ExtractLeadingDigitVector(sample1)
  
  # Group sample1 by values
  sample1 <- tabulate(sample1, nbins = 9)
  # sort sample1 asc
  sample1 <- sort(sample1, decreasing = FALSE)
  # compute proportions
  sum1 <- as.double(sum(sample1))
  for (i in 1:9) {
    sample1[i] = sample1[i]/sum1
  }

  # save relative frequencies in a vector to use as x-axis in ggplot
  xsample1 <- sample1

  # build CDF
  for (i in 2:9) {
    sample1[i] = sample1[i] + sample1[i-1]
  }
  
  # Build CDF per Benford's Law
  sample2 <- vector(mode = "numeric", length = 9)
  for (i in 1:9) {
    sample2[i] = as.double(log10(1+1/i)) # distributions
  }
  # sort sample2 asc
  sample2 <- sort(sample2, decreasing = FALSE)
  sum2 = as.double(sum(sample2))
  # Benford's CDF
  for (i in 2:9) {
    sample2[i] = sample2[i] + sample2[i-1]
  }
  
  Fsample1 <- sample1
  Fsample2 <- sample2

  # Find the x_0 and y_0, y_1 coordinates for D_max = max|Fx - EDF|
  maxD <- abs(Fsample1[1] - Fsample2[1])
  pos <- 1
  for (i in 2:length(Fsample1)) {
    if (abs(Fsample1[i] - Fsample2[i]) > maxD) {
      maxD = abs(Fsample1[i] - Fsample2[i])
      pos <- i
    }
  }
  # max D is at position pos
  x0 <- xsample1[pos]
  y0 <- Fsample1[pos]
  y1 <- Fsample2[pos]  
  
  # create frames in order to work with ggplot
  frame1 <- data.frame(Fsample1)
  frame2 <- data.frame(Fsample2)
  # Plot the curves & D_max
  grKS <- ggplot() +
    geom_line(frame1, mapping = aes(x = xsample1, y = Fsample1, color = 
                                      paste("F(", lblData, ")"))) +
    geom_line(frame2, mapping = aes(x = xsample1, y = Fsample2, color =
                                     "F(Benford's Law)")) +
    scale_color_manual(values=c("navy", "orange")) +
    xlab(paste(lblData, " Vote Proportions")) +
    ylab("CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "solid", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=2) +
    ggtitle(paste("Discrete Two-Sample K-S Test: ", lblData)) +
    theme(legend.title=element_blank(), legend.position = "bottom")

  # Run the ks.boot function from lib Matching for two-sample discrete KS test
  resultKS <- ks.boot(Fsample1, Fsample2, nboots = 1000, 
                      alternative = "two.sided")

  # Save results in a return list
  results <- list(grKS, resultKS)
  
  return(results)
}

# Extract & return a vector of leading digits from a given integer vector
ExtractLeadingDigitVector <- function(vecData) {
  
  vecLeading <- vector(mode = "integer", length = length(vecData))
  
  for (i in 1:length(vecData)) {
    vecLeading[i] = as.integer(substr(vecData[i], 1, 1))
  }

  return(vecLeading)
}

# Check for possible distribution candidates for a given vector
FitOtherDistributions <- function(vecData, lblData, isDiscrete) {
  datPlots <- descdist(vecData, discrete = isDiscrete)
  
  # do not test log-norm if there are zero-valued entries
  dtlognorm <- NULL
  if (!0 %in% vecData) {
    dtlognorm <- fitdist(vecData, distr = "lnorm")
  }
  dtnorm <- fitdist(vecData, distr = "norm")
  dtweibull <- fitdist(vecData, distr = "weibull")
  # ignore beta-fit for PS/PD ratio:
  dtbeta <- NULL
  if (lblData != "PS/PD") {
    dtbeta <- fitdist(vecData, distr = "beta")
  }
  # plot(dtlognorm)
  # plot(dtnorm)
  # plot(dtweibull)
  # plot(dtbeta)
  
  # find best fit
  dtDistr <- list(dtlognorm, dtnorm, dtweibull, dtbeta)
  
  # Check best fit using Akaike values
  bestFit <- dtDistr[[1]]
  pos <- 1
  for (i in 2:length(dtDistr)) {
    if (dtDistr[[i]]$aic < bestFit$aic) {
      bestFit <- dtDistr[[i]]
      pos <- i
    }
  }
  
  results <- list(datPlots, bestFit, pos, bestFit$aic)
  
  return (results)
}

# Plot observed frequencies against Benford's distribution
# df: observed frequencies in a data frame
# lblData: optional label for chart title, etc.
# useLines: if TRUE, plot lines, else plot histogram + Benford curve
PlotBenford <- function(df, lblData, useLines) {
  # Generate Benford table, for reference
  dgt <- vector(mode = "numeric", length = 9)
  bnfrd <- vector(mode = "numeric", length = 9)
  for (i in 1:9) {
    dgt[i] = i # digits
    bnfrd[i] = as.double(log10(1+1/i)) # distribution
  }
  benfordDistrib <- data.frame(dgt, bnfrd)
  names(benfordDistrib) <- c("Digit", "Probability")
  
  # If is null df, then compute the Benford curve/ histogram only
  if (!is.null(df)) {
    # Compute observed frequencies of leading digit
    # Group sample1 by values
    bfreq <- tabulate(df$LeadingDigit, nbins = 9)
    # compute proportions
    sum1 <- as.double(sum(bfreq))
    for (i in 1:9) {
      bfreq[i] = bfreq[i] / sum1
    }
    
    # Plot observed frequencies as bars overlaying Benford's curve for reference:
    dg <- data.frame(dgt, bfreq)
    names(dg) <- c("Digit", "Probability")
  } else{
    dg <- benfordDistrib
  }
  
  if (useLines == FALSE) {
    gbf <- ggplot() +
      geom_col(dg, mapping = aes(x = Digit, y = Probability, 
                                 color = paste(lblData, 
                                 " leading digit frequency")),
               fill = "orange") +
      geom_line(benfordDistrib, mapping = aes(x = Digit, y = Probability, 
                                              color = "Benford's Law")) +
      scale_color_manual(values=c("navy", "orange")) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      scale_x_continuous("Digit", labels = as.character(dgt), breaks = dgt) +
      xlab(paste("Digit")) +
      ylab("Probability") +
      ggtitle(paste("Benford's Law:", lblData, " Votes")) +
      theme(legend.title=element_blank(), legend.position = "bottom")
  } else {
    gbf <- ggplot() +
      geom_line(dg, mapping = aes(x = Digit, y = Probability, 
                                  color = paste(lblData, 
                                                " leading digit frequency"))) +
      geom_line(benfordDistrib, mapping = aes(x = Digit, y = Probability, 
                                              color = "Benford's Law")) +
      scale_color_manual(values=c("navy", "orange")) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      scale_x_continuous("Digit", labels = as.character(dgt), breaks = dgt) +
      xlab(paste("Digit")) +
      ylab("Probability") +
      ggtitle(paste("Benford's Law:", lblData, " Votes")) +
      theme(legend.title=element_blank(), legend.position = "bottom")
  }
  
  return (gbf)
}

# Function to compare two proportions, given a vector vecData & a district
# and a selection of tests as per the following schedule:
# 1: Proportion test
# 2: Likelihood ration test
# 3: Chi-squared two-by-two test
# 4: Run all tests
CompareTurnouts <- function(vecData, district, whichTest) {
  # proportions:
  pW <- vecData$pVotingWomen[which(vecData["District"] == district)]
  pM <- vecData$pVotingMen[which(vecData["District"] == district)]
  tW <- vecData$VotingWomen[which(vecData["District"] == district)]
  tM <- vecData$VotingMen[which(vecData["District"] == district)]
  nW <- vecData$RegisteredWomen[which(vecData["District"] == district)]
  nM <- vecData$RegisteredMen[which(vecData["District"] == district)]
  
  rs <- list() # store test results here
  if (whichTest == 1) {
    # prop test w/o Yates' continuity
    rs <- prop.test(x = c(tW, tM), n = c(nW, nM), correct = FALSE)
  } else if (whichTest == 2) {
    # LRT test
    nom = c(tW, tM, nW - tW, nM - tM, nW + nM)
    denom = c(nW, nM, tW + tM, nW - tW + nM - tM) 

    LRT_Stat = 2.0 * log(exp(sum(nom * log(nom)) - sum(denom * log(denom))))
    
    rs <- pchisq(LRT_Stat, 1, lower.tail = FALSE)
  } else if (whichTest == 3) {
    # Chi-sq 2x2 test
    m = matrix(c(tW, nW - tW, tM, nM - tM), nrow = 2)
    rs <- chisq.test(m, correct = FALSE)
  } else if (whichTest == 4) {
    # store all tests in a list
    t1 <- paste("Prop Test:\n", 
                prop.test(x = c(tW, tM), n = c(nW, nM), correct = FALSE))
    
    nom = c(tW, tM, nW - tW, nM - tM, nW + nM)
    denom = c(nW, nM, tW + tM, nW - tW + nM - tM) 
    t2 <- paste("LRT Test: \n", 
                pchisq(2 * log(exp(sum(nom * log(nom)) - 
                                     sum(denom * log(denom)))), 
                 1, lower.tail = FALSE))
    
    t3 <- paste("ChiSq Test:\n",
                chisq.test(matrix(c(tW, nW - tW, tM, nM - tM), nrow = 2), 
                     correct = FALSE))
    
    rs = list(t1, t2, t3)
  } else {
    rs <- t.test(vecData$pVotingWomen, vecData$pVotingMen, alternative="two.sided", var.equal=FALSE)
  }
  
# 
  # proportion test step by step
#   # interval standard error:             
#   sdInt <- sqrt(pW * (1.0 - pW) / nW + pM * (1.0 - pM) / nM) 
#   
#   # Confidence interval at stated alpha:
#   alfa <- 0.05
#   lInt <- (pW - pM) - abs(qnorm(alfa/2)) * sdInt
#   rInt <- (pW - pM) + abs(qnorm(alfa/2)) * sdInt
#   CI <- paste("(", round(lInt, 4), ", ", 
#               round(rInt, 4), ")", sep = "")
#   
#   # Compute pooled proportion, given the turnout for women and for men
#   pooledP <- (vecData$VotingWomen[which(vecData["District"] == district)] + 
#                 vecData$VotingMen[which(vecData["District"] == district)]) /
#     (nW + nM)
#   
#   # compute z-score standard dev
#   sdZ <- (pW - pM - 0) / 
#     sqrt(pooledP * (1.0 - pooledP) * (1.0 / nW + 1.0 / nM ))
#   
#   # p-value;
#   pVal <- 0
#   if (sdZ < 0) {
#     pVal <- (2 * (1 - pnorm(sdZ, lower.tail=FALSE)))
#   } else {
#     pVal <- (2 * pnorm(sdZ, lower.tail=FALSE))
#   }  
#   
  # return the percentage-point difference and the test results
  return (list(pW - pM, rs))
}

# Given map data, an admin unit, return neighbors of that unit
GetNeighbors <- function(mapData, adminUnit, byID = NULL, neighbor){
  gid3 <- byID
  if (is_null(byID)) {
    gid3 <- mapData[which(mapData$NAME_3 == adminUnit), ]$GID_3
  }
  aDF <- NULL
  if (is_null(byID)) {
    aDF <- mapData[which(mapData$NAME_3 == adminUnit), ]
  } else {
    aDF <- mapData[which(mapData$GID_3 == byID), ]
  }
  for (i in 1:length(neighbor[gid3, ])) {
    if (neighbor[gid3, i] == TRUE) {
      b <- mapData[which(mapData$GID_3 == rownames(neighbor)[i]), ]
      aDF <- rbind(aDF, b)
    }
  }
  
  return (aDF)
}

# Given neighbor map data of the adminUnit, plot the unit + neighbors
# in a turnout heatmap
PlotNeighborsWithTurnout <- function(mapData, adminUnit, centr) {
  minT <- min(mapData$pTurnout)
  maxT <- max(mapData$pTurnout)
  abF <- fortify(mapData)
  abF <- inner_join(abF, mapData@data, by = c("id" = "GID_3"))
  trnout <- abF$pTurnout
  gg <- ggplot() + 
    geom_polygon(data = abF, aes(x = long, 
                                 y = lat, 
                                 group = group,
                                 fill = trnout), 
                 color = "gray", size = 0.2) +
    scale_fill_distiller(name="Turnout %-age\n", 
                         palette = "Reds",
                         trans = "reverse",
                         guide = "colorbar",
                         #  breaks = scales::pretty_breaks(n = 3),
                         breaks = c(minT, (maxT + minT)/2, maxT)
    ) +
    labs(title = paste("2021 Albanian parliamentary elections: \n",
                       "Turnout heatmap for", adminUnit)) +
    geom_point(data = centr, aes(x = c1, y = c2), color = "yellow", size = 3) +
    geom_text(data = centr, 
              aes(label = paste(str_to_title(AdministrativeUnit), ": ", 
                                round(pTurnout, 4) * 100, "%", sep = ""), 
                  x = c1, y = c2 + 0.006)) +
    coord_fixed(1.5) +
    theme(#aspect.ratio = 1,
      legend.direction = "vertical",
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  
  return (gg)
}


# Given neighbor map data of the adminUnit, plot the unit + neighbors
# in a PS vs. Others heatmap, where PS votes are positive and 
# others' votes are negative
PlotNeighborsWithVoteShares <- function(mapData, adminUnit, centr) {
  abF <- fortify(mapData)
  abF <- inner_join(abF, mapData@data, by = c("id" = "GID_3"))
  trnout <- abF$pPS
  gg <- ggplot() + 
    geom_polygon(data = abF, aes(x = long, 
                                 y = lat, 
                                 group = group,
                                 fill = trnout), 
                 color = "gray", size = 0.2) +
    scale_fill_distiller(name = "Mean PS vote share\n", 
                         palette = "Reds",
                         trans = "reverse",
                         guide = "colorbar",
                         breaks = scales::pretty_breaks(n = 4)
    ) +
    labs(title = paste("2021 Albanian parliamentary elections: \n",
                       "Turnout (t) & PS vote (v) heatmap for ", adminUnit,
                       " and neighbors", sep = "")) +
    geom_point(data = centr, aes(x = c1, y = c2), color = "yellow", size = 3) +
    geom_text(data = centr, 
              aes(label = paste(str_to_title(AdministrativeUnit), "\n", 
                                "t: ", round(pTurnout, 4) * 100, "%\n",
                                "v: ", round(pPS, 4) * 100, "%\n",
                                sep = ""), 
                  x = c1, y = c2 + 0.006)) +
    coord_fixed(1.5) +
    theme(#aspect.ratio = 1,
      legend.direction = "vertical",
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  
  return (gg)
}


# Universal information-theoretic distances:
# metric = 0: normalized distance
#   1 - (mutual information)/(joint entropy)
#   Source: https://arxiv.org/pdf/q-bio/0311039.pdf
# metric = 1: symmetric Kullback-Leibler distance
Divergence <- function(X, Y, cX = NULL, cY = NULL, metric = 1) {
  if (metric == 0) {
    return (1.0 - mi.empirical(rbind(cX, cY)) / Entropy(X, Y))
  } else {
    return (KL.empirical(X, Y) + KL.empirical(Y, X))
  }
}
