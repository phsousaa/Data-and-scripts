#ANÁLISE DA VARIÁVEL ESCOLARIDADE
# Cavalcante (2007)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
escolaridade = c ("Semi-Alfabetizados", 'Analfabetos')
cav2007esc = data.matrix (rbind (
  c (884, 302, 64),
  c (460, 266, 50)
))
rownames (cav2007esc) = escolaridade
colnames (cav2007esc) = VD
View (cav2007esc)

cav2007esc.t = t(cav2007esc)
barplot (cav2007esc.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

cav2007esc = as.data.frame.table (cav2007esc)
cav2007esc = countsToCases(as.data.frame (cav2007esc))
names (cav2007esc) = c ('escolaridade', 'VD')
cav2007esc$escolaridade = factor (cav2007esc$escolaridade, levels = c ('Semi-Alfabetizados', 'Analfabetos'))
head(cav2007esc)
tail (cav2007esc)
ggbarstats (x = VD, y =escolaridade, data=cav2007esc,
            bf.message = F,
            xlab = "Escolaridade",
            legend.title = "Negação")
table (cav2007esc)

cav2007esc$VD = relevel (cav2007esc$VD, ref='Negação Pré-verbal')
mod.esc1 = multinom (VD ~ escolaridade, data=cav2007esc)
summary (mod.esc1)
tab_model (mod.esc1, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#Lopes da Silva (2020)
VD = c ("Negação Pré-verbal", "Dupla Negação")
escolaridade = c ("Ensino Médio", 'Ensino Superior')
lopes2020esc = data.matrix (rbind (
  c (852, 136),
  c (967, 142)
))
rownames (lopes2020esc) = escolaridade
colnames (lopes2020esc) = VD
View (lopes2020esc)

lopes2020esc.t = t(lopes2020esc)
barplot (lopes2020esc.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

lopes2020esc = as.data.frame.table (lopes2020esc)
lopes2020esc = countsToCases(as.data.frame (lopes2020esc))
names (lopes2020esc) = c ('escolaridade', 'VD')
lopes2020esc$escolaridade = factor (lopes2020esc$escolaridade, levels = c ('Ensino Médio', 'Ensino Superior'))
head(lopes2020esc)
ggbarstats (x = VD, y =escolaridade, data=lopes2020esc,
            bf.message = F,
            xlab = "Escolaridade",
            legend.title = "Negação")
table (lopes2020esc)

lopes2020esc$VD = relevel (lopes2020esc$VD, ref='Negação Pré-verbal')
mod.esc2 = glm (VD ~ escolaridade, data=lopes2020esc, family = 'binomial')
summary (mod.esc2)
tab_model (mod.esc2, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nunes (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
escolaridade = c ("5-8 anos", '9-11 anos')
nun2014esc = data.matrix (rbind (
  c (324, 131, 10),
  c (292, 83, 3)
))
rownames (nun2014esc) = escolaridade
colnames (nun2014esc) = VD
View (nun2014esc)

nun2014esc.t = t(nun2014esc)
barplot (nun2014esc.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nun2014esc = as.data.frame.table (nun2014esc)
nun2014esc = countsToCases(as.data.frame (nun2014esc))
names (nun2014esc) = c ('escolaridade', 'VD')
nun2014esc$escolaridade = factor (nun2014esc$escolaridade, levels = c ('5-8 anos', '9-11 anos'))
head(nun2014esc)
ggbarstats (x = VD, y =escolaridade, data=nun2014esc,
            bf.message = F,
            xlab = "Gênero",
            legend.title = "Negação")
table (nun2014esc)

nun2014esc$VD = relevel (nun2014esc$VD, ref='Negação Pré-verbal')
mod.esc3 = multinom (VD ~ escolaridade, data=nun2014esc)
summary (mod.esc3)
tab_model (mod.esc3, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nascimento (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
escolaridade = c ("Ensino Fundamental", 'Ensino Médio', "Ensino Universitário")
nasc2014esc = data.matrix (rbind (
  c (544, 178, 16),
  c (695, 166, 6),
  c (512, 134, 12)
))
rownames (nasc2014esc) = escolaridade
colnames (nasc2014esc) = VD
View (nasc2014esc)

nasc2014esc.t = t(nasc2014esc)
barplot (nasc2014esc.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nasc2014esc = as.data.frame.table (nasc2014esc)
nasc2014esc = countsToCases(as.data.frame (nasc2014esc))
names (nasc2014esc) = c ('escolaridade', 'VD')
nasc2014esc$escolaridade = factor (nasc2014esc$escolaridade, levels = c ('Ensino Fundamental', 'Ensino Médio', "Ensino Universitário"))
head(nasc2014esc)
ggbarstats (x = VD, y =escolaridade, data=nasc2014esc,
            bf.message = F,
            xlab = "Escolaridade",
            legend.title = "Negação")
table (nasc2014esc)

nasc2014esc$VD = relevel (nasc2014esc$VD, ref='Negação Pré-verbal')
mod.esc4 = multinom (VD ~ escolaridade, data=nasc2014esc)
summary (mod.esc4)
tab_model (mod.esc4, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Rocha (2013)
VD = c ("Negação Pré-verbal", "Dupla Negação")
escolaridade = c ("Ensino Médio", 'Ensino Superior')
roc2013esc = data.matrix (rbind (
  c (2491, 202),
  c (2788, 122)
))
rownames (roc2013esc) = escolaridade
colnames (roc2013esc) = VD
View (roc2013esc)

roc2013esc.t = t(roc2013esc)
barplot (roc2013esc.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,3000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

roc2013esc = as.data.frame.table (roc2013esc)
roc2013esc = countsToCases(as.data.frame (roc2013esc))
names (roc2013esc) = c ('escolaridade', 'VD')
roc2013esc$escolaridade = factor (roc2013esc$escolaridade, levels = c ('Ensino Médio', 'Ensino Superior'))
head(roc2013esc)
ggbarstats (x = VD, y =escolaridade, data=roc2013esc,
            bf.message = F,
            xlab = "Escolaridade",
            legend.title = "Negação")
table (roc2013esc)

roc2013esc$VD = relevel (roc2013esc$VD, ref='Negação Pré-verbal')
mod.esc5 = glm (VD ~ escolaridade, data=roc2013esc, family = 'binomial')
summary (mod.esc5)
tab_model (mod.esc5, transform = NULL)