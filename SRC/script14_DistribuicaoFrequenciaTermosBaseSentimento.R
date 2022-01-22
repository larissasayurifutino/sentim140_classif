rm(list = ls()); gc()
library(dplyr)
library(stopwords)

cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
load(file = file.path(cam, "df.RData"))


h_att <- df %>%
  group_by(term) %>%
  summarise(freq = n_distinct(document))


h_att <- h_att[order(h_att$freq, decreasing = TRUE),]
save(h_att, file = file.path(cam, "distFreqDadosSent.RData"))


h_att$"term" <- tolower(h_att$"term")
sw <- stopwords(language = "en", source = "smart")

posStopWords <- which(h_att$"term" %in% sw)
h_att <- h_att[-posStopWords,]

save(h_att, file = file.path(cam, "distFreqDadosSentSemSW.RData"))

###

# xx <- df %>% filter(term == "user") %>% select(document, count, target)
# head(xx)
# 
# 
# dfFake <- data.frame(document = df$document,
#                      target = factor(df$target),
#                      term = 0)
# 
# 
# yy <- merge(dfFake, xx, by = c("document", "target"), all.x = TRUE)
# yy$countDef <- yy$term + yy$count
# 
# 
# ww <- FSelectorRcpp::information_gain(x =  data.frame(att = yy$countDef), 
#                                       y = yy$target)
# 
# ww
