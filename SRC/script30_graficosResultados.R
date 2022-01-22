rm(list = ls()); gc(reset = TRUE)

library(dplyr)
library(ggplot2)

## -------------------------------------------------------- ##
## -- funcaoDescritivasPorNumClustersECustoResultadosSVM -- ##
## -------------------------------------------------------- ##

funcaoDescritivasPorNumClustersECustoResultadosSVM <- function(bBase){
  bBase <- bBase %>% 
    mutate(ModelTime = difftime(ModelEndtime, ModelBegtime, units = "secs"),
           Predictiontime = difftime(PredictionEndtime, PredictionBegtime, units = "secs"))
  
  
  df <- bBase %>%
    group_by(numGroups, Cost) %>%
    #group_by(Part, numGroups, Cost) %>%
    summarise(Scenarios = n(),
              NumObs = sum(numTestInst, na.rm = TRUE),
              NumObsClCrime = sum(numCrimeTestDB, na.rm = TRUE),
              
              LabelNonCrime_ModelNonCrime = sum(LabelNonCrime_ModelNonCrime, na.rm = TRUE),
              LabelNonCrime_ModelCrime = sum(LabelNonCrime_ModelCrime, na.rm = TRUE),
              LabelCrime_ModelNonCrime = sum(LabelCrime_ModelNonCrime, na.rm = TRUE),
              LabelCrime_ModelCrime = sum(LabelCrime_ModelCrime, na.rm = TRUE),
              
              ModelTime = max(ModelTime, na.rm = TRUE),
              Predictiontime = max(Predictiontime, na.rm = TRUE)) 
  
  
  df <- df %>%
    mutate(Precision = LabelCrime_ModelCrime / (LabelCrime_ModelCrime + LabelNonCrime_ModelCrime),
           Recall = LabelCrime_ModelCrime / (LabelCrime_ModelCrime +  LabelCrime_ModelNonCrime),
           F1 = (2*Recall * Precision) / (Recall + Precision))
  
  df$"Accuracy" <- (df$"LabelNonCrime_ModelNonCrime" + df$"LabelCrime_ModelCrime")/df$"NumObs"
  
  # Ordenacao
  df <- df[with(df, order(numGroups, Cost)),]
  #df <- df[with(df, order(Part, numGroups, Cost)),]
  
  names(df) <- c("numGroups", "Cost", "Scenarios", # c("Part", "numGroups", "Cost", "Scenarios",
                 "numTestInst", "numCrimeTestDB",
                 "LabelNonCrime_SVMNonCrime", "LabelNonCrime_SVMCrime", "LabelCrime_SVMNonCrime", "LabelCrime_SVMCrime",
                 "Modeltime", "Predictiontime",
                 "Precision", "Recall", "F1", "Accuracy")  
  
  return(df)
}

## -- Caminhos e Arquivos ----
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
load(file = file.path(cam, "dfResultados.RData"))

dfResultado <- dfResultados; rm(dfResultados)
names(dfResultado)

# Ordenacao
dfResultado <- dfResultado[with(dfResultado, order(Cost, Base)),]

## Arquivo com Resultados (resumo) ----
dfResultadoResumo <- funcaoDescritivasPorNumClustersECustoResultadosSVM(bBase = dfResultado)

save(dfResultadoResumo, file = file.path(cam, "dfResultadoResumo.RData"))

## -- Graficos ----
COLUNASDFRESULTADOAUX <- c("Cost", "F1")
resAux <- dfResultado[, COLUNASDFRESULTADOAUX]
ordem <- order(resAux$"Cost", decreasing = FALSE)
resAux <- resAux[ordem, ]

df <- data.frame("0.1" = resAux$F1[which(resAux$Cost == 0.1)],
                 "1" = resAux$F1[which(resAux$Cost == 1)],
                 "10" = resAux$F1[which(resAux$Cost == 10)])

ggplot(resAux, aes(x = 1, y = F1)) +
  geom_point(width = 1) + 
  facet_wrap( ~ Cost)

# Boxplot 01 ----
gBoxplot <- ggplot(resAux, aes(x = 1, y = F1, colour = factor(Cost))) +
  geom_boxplot() + xlab("") + ylim(c(0.7, 0.85)) +
  scale_colour_manual(labels = c("0.1", "1", "10"), 
                      values = c("#999999", "#E69F00", "#56B4E9")) +
  guides(colour = guide_legend(title = "Cost")) + 
  theme(axis.text.x = element_text(colour = "white", size = 0, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(colour = "grey22", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(colour = "grey22", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(colour = "grey22", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        #legend.position = "bottom",
        strip.text.x = element_text(size = 11, angle = 0))

# Salvando ----
nomeArqSalvar <- "grafResultados"
camSalvarGrafico <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/02_Graficos"

png(filename = file.path(camSalvarGrafico, paste0("grafBoxPlot_", nomeArqSalvar, ".png")), width = 578, height = 228)
print(gBoxplot)
dev.off()




write.table(x = df, 
          file = "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/dfResultadoTabLatex.csv", 
          quote = FALSE, 
          row.names = FALSE, 
          sep = ";", 
          dec = ".")
