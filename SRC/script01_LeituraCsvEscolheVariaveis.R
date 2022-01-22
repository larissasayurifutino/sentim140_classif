rm(list = ls()); gc()
library(stringr)

diret <- "/home/leste/Downloads/sentiment140"
arq <- read.csv(file = file.path(diret, "training.1600000.processed.noemoticon.csv"), header = F)
names(arq) <- c("target", "id", "date", "flag", "user", "text")
# target: the polarity of the tweet (0 = negative, 2 = neutral, 4 = positive)
# ids: The id of the tweet ( 2087)
# date: the date of the tweet (Sat May 16 23:58:44 UTC 2009)
# flag: The query (lyx). If there is no query, then this value is NO_QUERY.
# user: the user that tweeted (robotickilldozr)
# text: the text of the tweet (Lyx is cool)

length(which(duplicated(arq$id)))
length(which(duplicated(arq$text)))

View(arq[which(duplicated(arq$text)),])

# Removendo
arq$"text" <- as.character(arq$"text")
arq$"text" <- enc2utf8(arq$"text")
TEXTO <- str_trim(arq$"text")
tCom <- Sys.time(); tCom
# Minusculo
txtMin <- tolower(TEXTO)
rm(TEXTO)
# Substituir referencia a outros usuarios (usernames) por unico padrao
txtMinUserPadrao <- str_replace_all(string = txtMin, pattern = "@.", replacement = " USER ")
rm(txtMin)
# Substituir referencia a links (usage of links) por unico padrao
txtMinUserPadraoLinkPadrao <- str_replace_all(string = txtMinUserPadrao, pattern = "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", replacement = " LINK ")
rm(txtMinUserPadrao)
# Letras repetidas
txtMinUserPadraoLinkPadraoLetrasRepet <- str_replace_all(string = txtMinUserPadraoLinkPadrao, pattern = "([[:alpha:]])\\1+", replacement = "\\1")
rm(txtMinUserPadraoLinkPadrao)
# Pontuacao
txtMinUserPadraoLinkPadraoLetrasRepetPont <- str_remove_all(string = txtMinUserPadraoLinkPadraoLetrasRepet, pattern = "[[:punct:]]+")
rm(txtMinUserPadraoLinkPadraoLetrasRepet)
# Letra unica
txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnic <- str_remove_all(string = txtMinUserPadraoLinkPadraoLetrasRepetPont, pattern = "\\b\\w{1,2}\\b")
rm(txtMinUserPadraoLinkPadraoLetrasRepetPont)
# Espaços duplos
txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnicEspDup <- str_squish(txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnic)
rm(txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnic)
tFim <- Sys.time(); tFim

arq$"text" <- txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnicEspDup
rm(txtMinUserPadraoLinkPadraoLetrasRepetPontLetUnicEspDup)

arq <- arq[-which(duplicated(arq$"id")),]
arq <- arq[-which(duplicated(arq$"text")),]

sentiment140 <- arq
rm(arq)

saveRDS(sentiment140, file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/sentiment140.rds")
