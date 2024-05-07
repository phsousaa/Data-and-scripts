#ANÁLISE DA VARIÁVEL FAIXA ETÁRIA
# Cavalcante (2007)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
faixa.etaria = c ("20-40 anos", '41-60 anos', "+60 anos")
cav2007fe = data.matrix (rbind (
  c (411, 182, 45),
  c (556, 198, 22),
  c (377, 188, 47)
))
rownames (cav2007fe) = faixa.etaria
colnames (cav2007fe) = VD
View (cav2007fe)

cav2007fe.t = t(cav2007fe)
barplot (cav2007fe.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

cav2007fe = as.data.frame.table (cav2007fe)
cav2007fe = countsToCases(as.data.frame (cav2007fe))
names (cav2007fe) = c ('faixa.etaria', 'VD')
cav2007fe$faixa.etaria = factor (cav2007fe$faixa.etaria, levels = c ("20-40 anos", '41-60 anos', "+60 anos"))
head(cav2007fe)
ggbarstats (x = VD, y =faixa.etaria, data=cav2007fe,
            bf.message = F,
            xlab = "Faixa Etária",
            legend.title = "Negação")
table (cav2007fe)

cav2007fe$VD = relevel (cav2007fe$VD, ref='Negação Pré-verbal')
mod.fe1 = multinom (VD ~ faixa.etaria, data=cav2007fe)
summary (mod.fe1)
tab_model (mod.fe1, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#Lopes da Silva (2020)
VD = c ("Negação Pré-verbal", "Dupla Negação")
faixa.etaria = c ("-30 anos", '+30 anos')
lopes2020fe = data.matrix (rbind (
  c (953, 124),
  c (866, 154)
))
rownames (lopes2020fe) = faixa.etaria
colnames (lopes2020fe) = VD
View (lopes2020fe)

lopes2020fe.t = t(lopes2020fe)
barplot (lopes2020fe.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")
library (ggstatsplot)
lopes2020fe = as.data.frame.table (lopes2020fe)
lopes2020fe = countsToCases(as.data.frame (lopes2020fe))
names (lopes2020fe) = c ('faixa.etaria', 'VD')
lopes2020fe$faixa.etaria = factor (lopes2020fe$faixa.etaria, levels = c ("-30 anos", '+30 anos'))
head(lopes2020fe)
ggbarstats (x = VD, y =faixa.etaria, data=lopes2020fe,
            bf.message = F,
            xlab = "Faixa Etária",
            legend.title = "Negação")
table (lopes2020fe)

lopes2020fe$VD = relevel (lopes2020fe$VD, ref='Negação Pré-verbal')
mod.fe2 = glm (VD ~ faixa.etaria, data=lopes2020fe, family = 'binomial')
summary (mod.fe2)
tab_model (mod.fe2, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nunes (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
faixa.etaria = c ("15-25 anos", '26-50 anos')
nun2014fe = data.matrix (rbind (
  c (235, 116, 3),
  c (381, 98, 10)
))
rownames (nun2014fe) = faixa.etaria
colnames (nun2014fe) = VD
View (nun2014fe)

nun2014fe.t = t(nun2014fe)
barplot (nun2014fe.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nun2014fe = as.data.frame.table (nun2014fe)
nun2014fe = countsToCases(as.data.frame (nun2014fe))
names (nun2014fe) = c ('faixa.etaria', 'VD')
nun2014fe$faixa.etaria = factor (nun2014fe$faixa.etaria, levels = c ("15-25 anos", '26-50 anos'))
head(nun2014fe)
ggbarstats (x = VD, y =faixa.etaria, data=nun2014fe,
            bf.message = F,
            xlab = "Faixa Etária",
            legend.title = "Negação")
table (nun2014fe)

nun2014fe$VD = relevel (nun2014fe$VD, ref='Negação Pré-verbal')
mod.fe3 = multinom (VD ~ faixa.etaria, data=nun2014fe)
summary (mod.fe3)
tab_model (mod.fe3, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Nascimento (2014)
VD = c ("Negação Pré-verbal", "Dupla Negação", "Negação Pós-verbal")
faixa.etaria = c ("15-25 anos", '26-49 anos', '>49 anos')
nasc2014fe = data.matrix (rbind (
  c (566, 164, 15),
  c (557, 146, 7),
  c (628, 168, 12)
))
rownames (nasc2014fe) = faixa.etaria
colnames (nasc2014fe) = VD
View (nasc2014fe)

nasc2014fe.t = t(nasc2014fe)
barplot (nasc2014fe.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue", "azure4"),
         cex.names = 0.8,
         ylim= c (0,1000),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

nasc2014fe = as.data.frame.table (nasc2014fe)
nasc2014fe = countsToCases(as.data.frame (nasc2014fe))
names (nasc2014fe) = c ('faixa.etaria', 'VD')
nasc2014fe$faixa.etaria = factor (nasc2014fe$faixa.etaria, levels = c ("15-25 anos", '26-49 anos', '>49 anos'))
head(nasc2014fe)
ggbarstats (x = VD, y =faixa.etaria, data=nasc2014fe,
            bf.message = F,
            xlab = "Faixa Etária",
            legend.title = "Negação")
table (nasc2014fe)

nasc2014fe$VD = relevel (nasc2014fe$VD, ref='Negação Pré-verbal')
mod.fe4 = multinom (VD ~ faixa.etaria, data=nasc2014fe)
summary (mod.fe4)
tab_model (mod.fe4, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Rocha (2013)
VD = c ("Negação Pré-verbal", "Dupla Negação")
faixa.etaria = c ("18-35 anos", '36-55 anos', '>56 anos')
roc2013fe = data.matrix (rbind (
  c (1917, 101),
  c (1683, 106),
  c (1679, 117)
))
rownames (roc2013fe) = faixa.etaria
colnames (roc2013fe) = VD
View (roc2013fe)

roc2013fe.t = t(roc2013fe)
barplot (roc2013fe.t,
         beside = T,
         legend = T,
         cex.axis = 0.8,
         col = c ('aquamarine2', "aliceblue"),
         cex.names = 0.8,
         ylim= c (0,2500),
         args.legend = list (x='topleft',
                             cex = 0.8, bty = 'n'),
         ylab = "Frequência de Uso")

roc2013fe = as.data.frame.table (roc2013fe)
roc2013fe = countsToCases(as.data.frame (roc2013fe))
names (roc2013fe) = c ('faixa.etaria', 'VD')
roc2013fe$faixa.etaria = factor (roc2013fe$faixa.etaria, levels = c ("18-35 anos", '36-55 anos', '>56 anos'))
head(roc2013fe)
ggbarstats (x = VD, y =faixa.etaria, data=roc2013fe,
            bf.message = F,
            xlab = "Faixa Etária",
            legend.title = "Negação")
table (roc2013fe)

roc2013fe$VD = relevel (roc2013fe$VD, ref='Negação Pré-verbal')
mod.fe5 = glm (VD ~ faixa.etaria, data=roc2013fe, family = 'binomial')
summary (mod.fe5)
tab_model (mod.fe5, transform = NULL)
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------