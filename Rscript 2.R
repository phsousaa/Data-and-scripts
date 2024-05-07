#ANÁLISE DA VARIÁVEL GÊNERO NO USO DA NEGAÇÃO DUPLA COM VALOR DE REFERÊNCIA "NEGAÇÃO PRÉ-VERBAL"

#Cavalcante (2007)
VD = c ("Negação Pré-verbal", "Dupla Negação")
genero = c ("masculino", 'feminino')
cav2007 = data.matrix (rbind (
  c (585, 205),
  c (759, 363)
))
rownames (cav2007) = genero
colnames (cav2007) = VD
View (cav2007)

cav2007.t = t(cav2007)
barplot (cav2007.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

cav2007 = as.data.frame.table (cav2007)
cav2007 = countsToCases(as.data.frame (cav2007))
names (cav2007) = c ('genero', 'VD')
cav2007$genero = factor (cav2007$genero, levels = c ('masculino', 'feminino'))
head(cav2007)
ggbarstats (x = VD, y =genero, data=cav2007,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (cav2007)

cav2007$VD = relevel (cav2007$VD, ref='Negação Pré-verbal')
modelo = glm (VD ~ genero, data=cav2007, family = 'binomial')
summary (modelo)
tab_model (modelo, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# #Lopes da Silva (2020)
VD = c ("Negação Pré-verbal", "Dupla Negação")
genero = c ("masculino", 'feminino')
lopes2020 = data.matrix (rbind (
  c (817, 124),
  c (1002, 154)
))
rownames (lopes2020) = genero
colnames (lopes2020) = VD
View (lopes2020)

lopes2020.t = t(lopes2020)
barplot (lopes2020.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

lopes2020 = as.data.frame.table (lopes2020)
lopes2020 = countsToCases(as.data.frame (lopes2020))
names (lopes2020) = c ('genero', 'VD')
lopes2020$genero = factor (lopes2020$genero, levels = c ('masculino', 'feminino'))
head(lopes2020)
ggbarstats (x = VD, y =genero, data=lopes2020,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (lopes2020)

lopes2020$VD = relevel (lopes2020$VD, ref='Negação Pré-verbal')
modelo2 = glm (VD ~ genero, data=lopes2020, family = 'binomial')
summary (modelo2)
tab_model (modelo2, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nunes (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação")
genero = c ("masculino", 'feminino')
nun2014 = data.matrix (rbind (
  c (264, 109),
  c (352, 105)
))
rownames (nun2014) = genero
colnames (nun2014) = VD
View (nun2014)

nun2014.t = t(nun2014)
barplot (nun2014.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nun2014 = as.data.frame.table (nun2014)
nun2014 = countsToCases(as.data.frame (nun2014))
names (nun2014) = c ('genero', 'VD')
nun2014$genero = factor (nun2014$genero, levels = c ('masculino', 'feminino'))
head(nun2014)
ggbarstats (x = VD, y =genero, data=nun2014,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (nun2014)

nun2014$VD = relevel (nun2014$VD, ref='Negação Pré-verbal')
modelo3 = glm (VD ~ genero, data=nun2014, family = 'binomial')
summary (modelo3)
tab_model (modelo3, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#Nascimento (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação")
genero = c ("masculino", 'feminino')
nasc2014 = data.matrix (rbind (
  c (881, 238),
  c (870, 240)
))
rownames (nasc2014) = genero
colnames (nasc2014) = VD
View (nasc2014)

nasc2014.t = t(nasc2014)
barplot (nasc2014.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nasc2014 = as.data.frame.table (nasc2014)
nasc2014 = countsToCases(as.data.frame (nasc2014))
names (nasc2014) = c ('genero', 'VD')
nasc2014$genero = factor (nasc2014$genero, levels = c ('masculino', 'feminino'))
head(nasc2014)
ggbarstats (x = VD, y =genero, data=nasc2014,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (nasc2014)

nasc2014$VD = relevel (nasc2014$VD, ref='Negação Pré-verbal')
modelo4 = glm (VD ~ genero, data=nasc2014, family='binomial')
summary (modelo4)
tab_model (modelo4, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Rocha (2013)
VD = c ("Negação Pré-verbal", "Dupla Negação")
genero = c ("masculino", 'feminino')
roc2013 = data.matrix (rbind (
  c (2434, 146),
  c (2845, 178)
))
rownames (roc2013) = genero
colnames (roc2013) = VD
View (roc2013)

roc2013.t = t(roc2013)
barplot (roc2013.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

roc2013 = as.data.frame.table (roc2013)
roc2013 = countsToCases(as.data.frame (roc2013))
names (roc2013) = c ('genero', 'VD')
roc2013$genero = factor (roc2013$genero, levels = c ('masculino', 'feminino'))
head(roc2013)
ggbarstats (x = VD, y =genero, data=roc2013,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (roc2013)

roc2013$VD = relevel (roc2013$VD, ref='Negação Pré-verbal')
modelo5 = glm (VD ~ genero, data=roc2013, family = 'binomial')
summary (modelo5)
tab_model (modelo5, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#METANÁLISE
df <-
  structure(
    list(
      term = structure(
        1:5,
        .Label = c("Sudeste \n (Nunes, 2014)", "Nordeste \n (Lopes da Silva, 2020)", "Nordeste \n (Cavalcante, 2007)", 
                   "Sudeste \n (Nascimento, 2014)", "Sudeste \n (Rocha, 2013)"),
        class = "factor"
      ),
      estimate = c(
        -0.3251,
        0.01256,
        0.31100,
        0.02093,
        0.04215
      ),
      std.error = c(
        0.1591,
        0.12954,
        0.10325,
        0.10321,
        0.11502
      ),
      statistic = c(
        -2.043,
        0.097,
        3.012,
        0.203,
        0.366
      ),
      p.value = c(
        0.0411,
        0.923,
        0.00259,
        0.839,
        0.714
      ),
      df.residual = c(
        828,
        2095,
        1910,
        2227,
        5601
      )
    ),
    row.names = c(NA, -5L),
    class = c("tbl_df", "tbl", "data.frame")
  )
### gerar a visualização gráfica:
ggcoefstats(
  x = df,
  statistic = "z",
  sort = "ascending",
  ylab = "Amostras",
  xlab = "Coeficiente de regressão",
  caption = "Meta-análise"
)




#ANÁLISE DA VARIÁVEL GÊNERO NO USO DA NEGAÇÃO PÓS-VERBAL COM VALOR DE REFERÊNCIA "NEGAÇÃO DUPLA"

# Cavalcante (2007)
VD = c ("Dupla Negação", "Negação Pós-verbal")
genero = c ("masculino", 'feminino')
cav2007.2 = data.matrix (rbind (
  c (205, 61),
  c (363, 53)
))
rownames (cav2007.2) = genero
colnames (cav2007.2) = VD
View (cav2007.2)

cav2007.2.t = t(cav2007.2)
barplot (cav2007.2.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

cav2007.2 = as.data.frame.table (cav2007.2)
cav2007.2 = countsToCases(as.data.frame (cav2007.2))
names (cav2007.2) = c ('genero', 'VD')
cav2007.2$genero = factor (cav2007.2$genero, levels = c ('masculino', 'feminino'))
head(cav2007.2)
ggbarstats (x = VD, y =genero, data=cav2007.2,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (cav2007.2)

cav2007.2$VD = relevel (cav2007.2$VD, ref='Dupla Negação')
modelo.1 = glm (VD ~ genero, data=cav2007.2, family='binomial')
summary (modelo.1)
tab_model (modelo.1, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nunes (2014)
VD = c ("Dupla Negação", "Negação Pós-verbal")
genero = c ("masculino", 'feminino')
nun2014.2 = data.matrix (rbind (
  c (109, 8),
  c (105, 5)
))
rownames (nun2014.2) = genero
colnames (nun2014.2) = VD
View (nun2014.2)

nun2014.2.t = t(nun2014.2)
barplot (nun2014.2.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nun2014.2 = as.data.frame.table (nun2014.2)
nun2014.2 = countsToCases(as.data.frame (nun2014.2))
names (nun2014.2) = c ('genero', 'VD')
nun2014.2$genero = factor (nun2014.2$genero, levels = c ('masculino', 'feminino'))
head(nun2014.2)
ggbarstats (x = VD, y =genero, data=nun2014.2,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (nun2014.2)

nun2014.2$VD = relevel (nun2014.2$VD, ref='Dupla Negação')
modelo.2 = glm (VD ~ genero, data=nun2014.2, family='binomial')
summary (modelo.2)
tab_model (modelo.2, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nascimento (2014)
VD = c ("Dupla Negação", "Negação Pós-verbal")
genero = c ("masculino", 'feminino')
nasc2014.2 = data.matrix (rbind (
  c (238, 18),
  c (240, 16)
))
rownames (nasc2014.2) = genero
colnames (nasc2014.2) = VD
View (nasc2014.2)

nasc2014.2.t = t(nasc2014.2)
barplot (nasc2014.2.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nasc2014.2 = as.data.frame.table (nasc2014.2)
nasc2014.2 = countsToCases(as.data.frame (nasc2014.2))
names (nasc2014.2) = c ('genero', 'VD')
nasc2014.2$genero = factor (nasc2014.2$genero, levels = c ('masculino', 'feminino'))
head(nasc2014.2)
ggbarstats (x = VD, y =genero, data=nasc2014.2,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (nasc2014.2)

nasc2014.2$VD = relevel (nasc2014.2$VD, ref='Dupla Negação')
modelo.3 = glm (VD ~ genero, data=nasc2014.2, family='binomial')
summary (modelo.3)
tab_model (modelo.3, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#METANÁLISE
df2 <-
  structure(
    list(
      term = structure(
        1:3,
        .Label = c("Sudeste \n (Nunes, 2014)", "Nordeste \n (Cavalcante, 2007)", "Sudeste \n (Nascimento, 2014)"),
        class = "factor"
      ),
      estimate = c(
        -0.4326,
        -0.7120,
        -0.1262
      ),
      std.error = c(
        0.5862,
        0.2071,
        0.3556
      ),
      statistic = c(
        -0.738,
        -3.438,
        -0.355
      ),
      p.value = c(
        0.461,
        0.000587,
        0.723
      ),
      df.residual = c(
        225,
        680,
        510
      )
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )
### gerar a visualização gráfica:
ggstatsplot::ggcoefstats(
  x = df2,
  statistic = "z",
  sort = "ascending",
  ylab = "Amostras",
  xlab = "Coeficiente de regressão",
  caption = "Meta-análise"
)
