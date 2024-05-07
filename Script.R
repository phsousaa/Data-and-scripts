# Libraries
library (tidyverse)
library (sjPlot)
library (nnet)
library (ggstatsplot)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#ANÁLISE GERAL
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
pesquisas = c ("Lopes da Silva, 2020", "Nunes, 2014", "Nascimento, 2014", "Rocha, 2013",
               "Cavalcante, 2007", "Serra, 2018", "Reimann e Yacovenco, 2011",
               "Rocha, 2012", "Reimann e Yacovenco, 2014")
dadostodos = data.matrix (rbind (
  c (1819, 278, 7),
  c (616, 214, 13),
  c (1751, 478, 34),
  c (5279, 324, 4),
  c (1343, 568, 115),
  c (1009, 133, 17),
  c (721, 216, 42),
  c (940, 117, 4),
  c (1754, 478, 34)
))



rownames (dadostodos) = pesquisas
colnames (dadostodos) = VD
View (dadostodos)

dadostodos = as.data.frame.table (dadostodos)
dadostodos = countsToCases(as.data.frame (dadostodos))
names (dadostodos) = c ('pesquisas', 'VD')
dadostodos$pesquisas = factor (dadostodos$pesquisas, levels = c ("Lopes da Silva, 2020", "Nunes, 2014", "Nascimento, 2014", "Rocha, 2013",
                                                                 "Cavalcante, 2007", "Serra, 2018", "Reimann e Yacovenco, 2011",
                                                                 "Rocha, 2012", "Reimann e Yacovenco, 2014"))
head(dadostodos)
ggbarstats (x = VD, y =pesquisas, data=dadostodos,
            bf.message = F,
            xlab = "Pesquisas",
            legend.title = "Negação")
