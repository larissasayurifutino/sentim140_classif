rm(list = ls()); gc()
library(stringr)


# This data was downloaded from: http://help.sentiment140.com/for-students, 'Google Drive link.
diret <- "DATA"
sent_file <- read.csv(file = file.path(diret, "training.1600000.processed.noemoticon.csv"), header = F)
# Based on the format of the training data
names(sent_file) <- c("target", "id", "date", "flag", "user", "text")
# target: the polarity of the tweet (0 = negative, 2 = neutral, 4 = positive)
# ids: The id of the tweet ( 2087)
# date: the date of the tweet (Sat May 16 23:58:44 UTC 2009)
# flag: The query (lyx). If there is no query, then this value is NO_QUERY.
# user: the user that tweeted (robotickilldozr)
# text: the text of the tweet (Lyx is cool)

# Aparently there are duplicated entries
length(which(duplicated(sent_file$"id")))
length(which(duplicated(sent_file$"text")))

View(sent_file[which(duplicated(sent_file$"text")),])

# Processing
sent_file$"text" <- as.character(sent_file$"text")
sent_file$"text" <- enc2utf8(sent_file$"text")
TEXTO <- str_trim(sent_file$"text")

tCom <- Sys.time(); tCom
# Lower case
txtLow <- tolower(TEXTO)
rm(TEXTO)
# Replace usernames by a single pattern
txtLowUserPattern <- str_replace_all(string = txtLow, pattern = "@.", replacement = " USER ")
rm(txtLow)
# Replace links (usage of links) by a single pattern
txtLowUserPatternLinkPattern <- str_replace_all(string = txtLowUserPattern, pattern = "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", replacement = " LINK ")
rm(txtLowUserPattern)
# Repeated letters
txtMinUserPadraoLinkPadraoRepLetters <- str_replace_all(string = txtLowUserPatternLinkPattern, pattern = "([[:alpha:]])\\1+", replacement = "\\1")
rm(txtLowUserPatternLinkPattern)
# Punctuation
txtMinUserPadraoLinkPadraoRepLettersPunct <- str_remove_all(string = txtMinUserPadraoLinkPadraoRepLetters, pattern = "[[:punct:]]+")
rm(txtMinUserPadraoLinkPadraoRepLetters)
# Single letter
txtMinUserPadraoLinkPadraoRepLettersPunctSingleLet <- str_remove_all(string = txtMinUserPadraoLinkPadraoRepLettersPunct, pattern = "\\b\\w{1,2}\\b")
rm(txtMinUserPadraoLinkPadraoRepLettersPunct)
# Double space
txtMinUserPadraoLinkPadraoRepLettersPunctSingleLetDoubleSpace <- str_squish(txtMinUserPadraoLinkPadraoRepLettersPunctSingleLet)
rm(txtMinUserPadraoLinkPadraoRepLettersPunctSingleLet)
tFim <- Sys.time(); tFim


# How much time did it take?
time_dif <- tFim - tCom 
time_dif


sent_file$"text" <- txtMinUserPadraoLinkPadraoRepLettersPunctSingleLetDoubleSpace
rm(txtMinUserPadraoLinkPadraoRepLettersPunctSingleLetDoubleSpace)

# Remove duplicated entries
sent_file <- sent_file[-which(duplicated(sent_file$"id")),]
sent_file <- sent_file[-which(duplicated(sent_file$"text")),]

sentiment140 <- sent_file
rm(sent_file); gc(reset = TRUE)

saveRDS(sentiment140, file = "DATA/sentiment140.rds")
