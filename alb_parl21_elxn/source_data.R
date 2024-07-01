# source_data.R
# Load source data from CSV files located in the ./data directory.
# CC BY-SA. W.A. Borici, 2021.
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# File locations
partyVotesFile <- here('data/alb_parl21_elxn_party_votes.csv')
voteSeatShareFile <- here('data/vote-seat-share.csv')
rawPartyData <- here('data/party_votes_au.csv') # raw party data
rawElxnData <- here('data/elxn_data_all.csv') # results at A.-U. level
finalElxnData <- here('data/elxn_data_final.csv') # output of preprocessed data

# Read CSV data into tibbles for votes aggregated at the admin unit
partyVotesRaw <- read_csv(partyVotesFile)
voteSeatShare <- read_csv(voteSeatShareFile)

# Read CSV data into tibbles for votes at the polling-station level
rawPartyVotes <- read_csv(rawPartyData)
rawQVVotes <- read_csv(rawElxnData)

# Pre-processing: Populate party votes from rawPartyVotes into the last 13
# fields of rawQVVotes by joining on the PollingStation field of both sets
noPartyResults <- nrow(rawPartyVotes)

# for each record in rawQVVotes, populate party votes from rawPartyVotes
# records to populate are numbers 11 to 23
i <- 1
for (j in 1:noPartyResults) {
  # match by polling station to copy corresponding votes
  i <- as.numeric(which(rawQVVotes$PollingStation == 
               rawPartyVotes$PollingStation[j]))
  
  if (rawPartyVotes[j, "Party"] == "PSD") {
    rawQVVotes[i, "PSD"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "PBK") {
    rawQVVotes[i, "PBK"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "PLDSH") {
    rawQVVotes[i, "PLDSH"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "BD") {
    rawQVVotes[i, "BD"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "ABEOK") {
    rawQVVotes[i, "ABEOK"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "LSI") {
    rawQVVotes[i, "LSI"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "NTH") {
    rawQVVotes[i, "NTH"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "LRE") {
    rawQVVotes[i, "LRE"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "PDAN") {
    rawQVVotes[i, "PDAN"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "ADR") {
    rawQVVotes[i, "ADR"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "LN") {
    rawQVVotes[i, "LN"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "PS") {
    rawQVVotes[i, "PS"] <- rawPartyVotes[j, "Votes"]
  } else if (rawPartyVotes[j, "Party"] == "Other") {
    rawQVVotes[i, "Other"] <- rawPartyVotes[j, "Votes"]
  }
}

# export the joined data for later reference
# write the data set out
write_csv(rawQVVotes, finalElxnData, na = "NA", append = FALSE)
