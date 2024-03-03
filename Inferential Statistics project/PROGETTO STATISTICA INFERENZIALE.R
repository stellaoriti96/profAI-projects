# PUNTO 1
setwd("C:\\Users\\Dell\\Desktop\\profession ai\\progetto Statistica Inferenziale")
data=read.csv("neonati.csv")
nrow(data)
head(data[1:5])
head(data[6:10])
data=na.omit(data) # rimuovo le osservazioni con valori mancanti
nrow(data) # nessuna osservazione presenta valori mancanti

# PUNTO 2
# Il dataset iniziale contiene 2500 ossevazioni di 10 variabili relative a dei neonati.
# L'obiettivo dell'analisi statistica è trovare un modello per prevedere il peso alla nascita dei neonati.
# Le variabili sono principalmente caratteristiche del neonato e della madre:
#
# età della madre -> quantitativa continua
# numero di gravidanze sostenute -> quantitativa discreta
# madre fumatrice (0=NO, 1=SI')-> qualitativa nominale dicotomica
# numero di settimane di gestazione -> quantitava continua
# tipo di parto (naturale o cesareo) -> qualitativa nomimale
# 
# peso in grammi del neonato -> quantitativa continua
# lunghezza in mm del neonato -> quantitativa continua
# diametro in mm del cranio del neonato -> quantitativa continua
# sesso del neonato -> qualitativa nominale dicotomica
# 
# ospedale (1,2,3) -> qualitativa nominale


# PUNTO 3
attach(data)

data_quant=subset(data, select = -c(Fumatrici,Tipo.parto,Sesso,Ospedale))

attach(data_quant)

summary(data_quant[1:3]) 
summary(data_quant[4:ncol(data_quant)]) 
# osservo che il minimo di Anni.madre è 0, valore sicuramente errato

data[Anni.madre=="0",] # alla riga 1380 il dato Anni.madre è 0, per cui si elimina l'osservazione

data=subset(data, Anni.madre != 0) # Costruisco la distribuzione di frequenza per osservare i valori assunti dalla variabile
library(ggplot2)
ggplot(data=data)+
  geom_bar(aes(x=Anni.madre),
           stat="count",
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Anni madre",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,46,1))+
  scale_y_continuous(expand = c(0, 0))
# Alcuni valori della variabile Anni.madre non sono plausibili; altri lo sono poco

ggplot(data=data)+
  geom_bar(aes(x=N.gravidanze),
           stat="count",
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Numero di gravidanze",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_y_continuous(expand = c(0, 0))
# Alcuni valori per la variabile N.gravidanze sono poco plausibili

# https://www.istat.it/it/files/2023/10/Report-natalita-26-ottobre-2023.pdf
# Supponiamo che il dataset sia stato ottenuto su donne italiane o residenti in Italia, nel 2022.
# Il numero medio di figli per donna in Italia nel 2022 era pari a 1,87 per le donne straniere e 1,18 per le donne italiane: rimuoviamo le osservazioni in cui le donne hanno più di 4 figli
# Il tasso di fecondità in Italia nel 2022 al di sotto dei 20 anni è molto basso: 
# rimuoviamo le osservazioni per cui l'età della madre è sotto 20; inoltre, supponiamo 
# che tutte le madri abbiano la prima gravidanza non prima dei 20 anni e 
# non più di una gravidanza all'anno

data=subset(data, Anni.madre >= 20 & N.gravidanze<=4)
lista_indexes_to_remove=list()
for (i in 0:2){
  indexes_to_remove=which(data$Anni.madre==as.numeric(20)+i & data$N.gravidanze>as.numeric(0)+i)
  lista_indexes_to_remove[[i+1]]=c(indexes_to_remove)
  if (length(c(indexes_to_remove))>0){
    data=data[-c(indexes_to_remove),]
  }
}
lista_indexes_to_remove


summary(data)
# https://www.health.ny.gov/community/pregnancy/why_is_40_weeks_so_important.htm#:~:text=Pregnancy%20lasts%20for%20about%20280,between%2029%20and%2033%20weeks.
# https://www.salute.gov.it/portale/donna/dettaglioContenutiDonna.jsp?area=Salute+donna&id=4478&menu=nascita
# Un neonato prematuro nasce indicativamente dopo un numero di settimanane di gestazione tra 23 e 37, 
# per cui il minimo valore osservato (26) risulta plausibile.
# Un neonato post-termine nasce indicativamente dopo un numero di settimane superiore o uguale a 42,
# per cui il massimo valore osservato (43) risulta plausibile.

# https://media.tghn.org/articles/newbornsize.pdf
# L'articolo riportato mostra:
# Pesi del neonato tra circa 1 e 5,5 kg
# Lunghezze del neonato tra circa 38 e 56 cm
# Circonferenze del cranio del neonato tra circa 26 e 40 cm
# L'articolo riporta i dati di bambini da diversi stati, Italia compresa.
# Ipotizzando che anche il dataset della nostra analisi possa contenere osservazioni di bambini stranieri 
# nati in Italia, consideriamo i nostri valori plausibili rispetto ai range forniti da questo altro studio:
# il nostro range non è sempre interno al range fornito dall'articolo, ma si ipotizza che i valori al di fuori siano corretti
data_quant=subset(data, select = -c(Fumatrici,Tipo.parto,Sesso,Ospedale))
attach(data)
summary(data_quant[1:3]) 
summary(data_quant[4:ncol(data_quant)]) 

plot(density(Peso)) # la variabile risposta (Peso) sembra distribuita secondo una normale con una coda sinistra più lunga
abline(v=quantile(Peso,probs=c(0.25,0.75)),
       col=2) 
quantile(Peso,probs=c(0.25,0.75)) # la metà del valori si colloca nel range 3000-3620 g

plot(density(Lunghezza)) # la variabile Lunghezza sembra distribuita secondo una normale con una coda sinistra più lunga
abline(v=quantile(Lunghezza,probs=c(0.25,0.75))
       ,col=2) 
quantile(Lunghezza,probs=c(0.25,0.75)) # la metà del valori si colloca nel range 480-510 mm

plot(density(Cranio)) # la variabile Cranio sembra distribuita secondo una normale con una coda sinistra più lunga
abline(v=quantile(Cranio,probs=c(0.25,0.75)),
       col=2) 
quantile(Cranio,probs=c(0.25,0.75)) # la metà del valori si colloca nel range 330-350 mm


data_cl=data.frame(matrix(nrow =nrow(data), ncol = 0))
data_cl$Anni.madre.cl=cut(data$Anni.madre, breaks=seq(from=min(Anni.madre)-1,to=max(Anni.madre),length.out=6))
ggplot(data=data_cl)+ 
  geom_bar(aes(x=Anni.madre.cl),
           stat="count",
           col="black",
           fill="deepskyblue")+
  labs(title="Distribuzione di frequenza",
       x="Anni madre suddivisi in classe",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
# La maggior parte delle madri hanno età compresa tra i 25 e i 30 anni, e, con minor frequenza, nella fascia 30-35 anni

moments::skewness(Anni.madre)
# La skewness positiva della variabile Anni.madre conferma che sono più frequenti valori bassi

ggplot(data=data)+
  geom_bar(aes(x=N.gravidanze),
           stat="count",
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Numero di gravidanze",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(min(N.gravidanze),max(N.gravidanze),1))+
  scale_y_continuous(expand = c(0, 0))
# All'aumentare del numero di gravidanze diminuisce il numero di ossevazioni: cioè la maggior parte della madri hanno avuto pochi o nessun figlio

moments::skewness(N.gravidanze)
# La skewness positiva della variabile N.gravidanze conferma che sono più frequenti valori bassi

data_cl$Gestazione_cl=cut(data$Gestazione, breaks=c(23,37,41))
ggplot(data=data_cl)+
  geom_bar(aes(x=Gestazione_cl),
           stat="count",
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Mesi di gestazione",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0))
# La maggior parte delle donne oggetto di studio partorisce dopo un numero di mesi di gestazione normale (37-41).
# La minoranza delle donne ha partotito neonati prematuri (23-37 mesi di gestazione) 
# Nessuna delle madri osservate ha partorito post-termine


ggplot(data=data)+
  geom_bar(aes(x=Fumatrici,
               y=after_stat(count/sum(count)*100)),
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Madri e fumo",
       y="Frequenze assolute (%)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=c(0,1),
                     labels=c("NON fumatrici","Fumatrici"))+
  scale_y_continuous(expand = c(0, 0))
# La maggior parte delle madri sono non fumatrici

ggplot(data=data)+
  geom_bar(aes(x=Sesso,y=after_stat(count/sum(count)*100)),
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Sesso",
       y="Frequenze assolute (%)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0))
# I due sessi sono quasi equamente distribuiti nel dataset

ggplot(data=data)+
  geom_bar(aes(x=Ospedale,y=after_stat(count/sum(count)*100)),
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="Ospedale",
       y="Frequenze assolute (%)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0))
# I tre ospedali sono quasi equamente distribuiti nel dataset




# PUNTO 4

# https://www.ospedalebambinogesu.it/da-0-a-30-giorni-come-si-presenta-e-come-cresce-80012/#:~:text=In%20media%20il%20peso%20nascita,pari%20mediamente%20a%2050%20centimetri.
# Secondo dati dell'ospedale Bambino Gesù:
# Peso medio neonati -> 3300 kg (i maschi pesano circa 150 grammi in più)
# Lunghezza media neonati -> 50 cm
# Non ci sono particolari differenze per quanto riguarda la lunghezza tra maschi e femmine.
# Lunghezza e peso possono essere diversi non solo in base al sesso, 
# ma anche a fattori ereditari o ambientali. Ad esempio, se la mamma fuma 
# in gravidanza, c'è un rischio aumentato di avere un neonato di basso peso (inferiore a 2500 grammi).

# Poichè voglio saggiare l'ipotesi che la media del campione non differisca da un determinato valore e non conosco la deviazione standard della popolazione utilizzo un t-test
# H0: mu_cap-mu=0
t.test(x=Peso,
       mu=3300,
       conf.level=0.95,
       alternative="two.sided")
# Accetto l'ipotesi nulla in quanto i criteri sotto sono verificati:
# p-value>0.025
# mu all'interno dell'intervallo di confidenza

# Poichè voglio saggiare l'ipotesi che la media del campione non differisca da un determinato valore e non conosco la deviazione standard della popolazione utilizzo un t-test
# H0: mu_cap-mu=0
t.test(x=Lunghezza,
       mu=500,
       conf.level=0.95,
       alternative="two.sided")
# Rifiuto l'ipotesi nulla in quanto i criteri sotto NON sono verificati:
# p-value>0.025
# mu all'interno dell'intervallo di confidenza



# PUNTO 5
boxplot(Peso~Sesso,data=data) 
summary(Peso[Sesso=="F"])
summary(Peso[Sesso=="M"])
mean(Peso[Sesso=="M"])-mean(Peso[Sesso=="F"])
# la media del Peso è più alta per i maschi che per le femmine del campione, ma non sappiamo se in modo significativo
# H0:mu_peso_maschi-mu_peso_femmine=0
t.test(data=data, 
       Peso~Sesso,
       paired=FALSE)
# Rifiuto l'ipotesi nulla in quanto i criteri sotto NON sono verificati:
# p-value>0.025
# 0 all'interno dell'intervallo di confidenza
# Come suggerisce il test sul campione e le statistiche trovate in rete, i neonati maschi pesano mediamente più delle neonate femmine



boxplot(Lunghezza~Sesso,data=data) 
summary(Lunghezza[Sesso=="F"])
summary(Lunghezza[Sesso=="M"])
mean(Lunghezza[Sesso=="M"])-mean(Lunghezza[Sesso=="F"])
# la media della Lunghezza è più alta per i maschi che per le femmine del campione, ma non sappiamo se in modo significativo
# H0:mu_lunghezza_maschi-mu_lunghezza_femmine=0
t.test(data=data, 
       Lunghezza~Sesso,
       paired=FALSE)
# Rifiuto l'ipotesi nulla in quanto i criteri sotto NON sono verificati:
# p-value>0.025
# 0 all'interno dell'intervallo di confidenza
# Il test sul campione suggerisce che esista una differenza nella lunghezza media dei neonati tra maschi e femmine,
# nonostante le statistiche trovate in rete dicano che il sesso non determini una differenza.
# Tale differenza potrebbe dipendere dal caso o dal non aver effettuato un corretto campionamento.
# In caso di non corretto campionamento, potremmo avere tante neonate da madri fumatrici 
# (e che ipotizziamo abbiano fumato in gravidanza), per cui le neonate potrebbero avere un peso 
# ancora minore rispetto a quello che avrebbero con madri non fumatrici e questo potrebbe tradursi in una 
# minore lunghezza: da come riportato sotto, vediamo che il numero di neonate da madri fumatrici,
# così come il numero di neonati di madri fumatrici è in realtà simile.
table(Fumatrici,Sesso)
ggplot(data=data, 
       aes(x=Sesso, 
           fill=factor(Fumatrici)))+ 
  geom_bar(position ="fill",
           color="black")+
  labs(title="Distribuzione delle tipologie di parti",
       x="Sesso",
       y="Madri e fumo (%)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand=c(0,0),
                     labels = scales::percent_format())+
  scale_fill_manual(name="",
                    values = c("purple","yellow"),
                    labels=c("Non fumatrici","Fumatrici"))

# https://www.healthychildren.org/English/ages-stages/baby/Pages/First-Month-Physical-Appearance-and-Growth.aspx
# Il diametro medio del cranio di un neonato è di circa 35 cm.
# Rispetto alle femmine, i maschi hanno in media un diametro maggiore 1 cm.
boxplot(Cranio~Sesso,data=data)
summary(Cranio[Sesso=="F"])
summary(Cranio[Sesso=="M"])
mean(Cranio[Sesso=="M"])-mean(Cranio[Sesso=="F"])
# la media del diametro del Cranio è più alta per i maschi che per le femmine del campione, , ma non sappiamo se in modo significativo 
# H0:mu_cranio_maschi-mu_cranio_femmine=0
t.test(data=data, 
       Cranio~Sesso,
       paired=FALSE)
# Rifiuto l'ipotesi nulla in quanto i criteri sotto NON sono verificati:
# p-value>0.025
# 0 all'interno dell'intervallo di confidenza
# Come da info trovate online, anche in questo campione il diametro del cranio dei 
# neonati maschi risulta leggermente maggiore del diametro del cranio delle neonate femmine,
# con una differenza minore di 1 cm.


# PUNTO 6
ggplot(data=data, 
       aes(x=Ospedale, 
           fill=Tipo.parto))+ 
  geom_bar(position = "fill", 
           stat = "count",
           color="black")+
  labs(title="Distribuzione delle tipologie di parti",
       x="Ospedale",
       y="Numero di parti (%)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand=c(0,0),labels = scales::percent_format())+
  scale_fill_manual(name="Tipo di parto",
                    labels=c("Cesareo","Naturale"),
                    values = c("purple","yellow"))
# Le differenze tra le percentuali di parti cesarei nei 3 ospedali non risultano marcate nel campione 

#TEST CHI-QUADRATO PER CONFRONTO DI PROPORZIONI TRA GRUPPI
# H0: le probabilità di avere parto CESAREO in un determinato ospedale è la stessa per i 3 ospedali
data$Tipo.parto_d=ifelse(Tipo.parto=="Ces",1,0)
case.vector=tapply(data$Tipo.parto_d,Ospedale,sum) # numero di casi favorevoli (parto cesareo) per ogni ospedale
total.vector=tapply(data$Tipo.parto_d,Ospedale,length) # numero di casi totali per ogni ospedale
prop.test(x=case.vector, 
          n=total.vector,
          conf.level = 0.95)
# Non si rifiuta l'ipotesi nulla, ovvero la probabilità di avere un parto cesareo è la stessa nei 3 ospedali,
# in quanto il p-value è maggiore di 0.05.
data=subset(data, select = -Tipo.parto_d)


# ANALISI MULTIDIMENSIONALE

# PUNTO 1
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data_quant[,c(4,1:3,5,6)],upper.panel = panel.smooth,lower.panel = panel.cor) # matrice degli scatterplots e dei coefficienti di correlazione tra le variabili quantitative
# https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/pearsons-correlation-coefficient/#:~:text=High%20degree%3A%20If%20the%20coefficient,to%20be%20a%20small%20correlation.
# Degree of correlation:
# Perfect: If the value is near ± 1, then it said to be a perfect correlation: as one variable increases, the other variable tends to also increase (if positive) or decrease (if negative).
# High degree: If the coefficient value lies between ± 0.50 and ± 1, then it is said to be a strong correlation.
# Moderate degree: If the value lies between ± 0.30 and ± 0.49, then it is said to be a medium correlation.
# Low degree: When the value lies below + .29, then it is said to be a small correlation.
# No correlation: When the value is zero.
# Correlazioni medio-alte: 
# Lunghezza-Peso -> 0.79 (tendenza lineare)
# Cranio-Peso -> 0.70 (tendenza lineare; sembra ci siano due pendenze)
# Gestazione-Peso -> 0.59 (tendenza quasi lineare; pendenza variabile)
# Cranio-Gestazione -> 0.46 (retta in parte quasi orizzontale o legame quadratico)
# Lunghezza-Gestazione -> 0.62 (retta in parte quasi orizzontale o legame quadratico)
# Cranio-Lunghezza -> 0.60 (tendenza a legame quadratico)


range(Peso)
range_peso=max(Peso)-min(Peso)
data_cl$Peso.cl=cut(Peso, breaks=seq(from=min(Peso)-1,to=max(Peso),length.out=9)) # divido il peso in classi con un range di circa 500 (g)
ggplot(data=data_cl)+
  geom_bar(aes(x=Peso.cl),
           stat="count",
           col="black",
           fill="deepskyblue")+
  labs(title="Distribuzione di frequenza",
       x="Peso suddiviso in classe",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
tabella_sesso_peso=table(Sesso,data_cl$Peso.cl)
test.indipendenza.1=chisq.test(tabella_sesso_peso)
test.indipendenza.1
# poichè il p-value è minore di 0.05 si rifiuta l'ipotesi nulla di indipendenza delle variabili Sesso e Peso.cl
plot(density(Peso[Sesso=="F"]),col="purple",main="")
lines(density(Peso[Sesso=="M"]),col="blue")
title(main="Densità di probabilità - Maschi (blu) vs Femmine (viola)")
# si osserva che per valori più alti del peso, le osservazioni più probabili sono di neonati maschi


tabella_parto_peso=table(Tipo.parto,data_cl$Peso.cl)
test.indipendenza.2=chisq.test(tabella_parto_peso)
test.indipendenza.2
# poichè il p-value è maggiore di 0.05 si accetta l'ipotesi nulla di indipendenza delle variabili Tipo.parto e Peso.cl
plot(density(Peso[Tipo.parto=="Nat"]),col="purple",main="")
lines(density(Peso[Tipo.parto=="Ces"]),col="blue")
title(main="Densità di probabilità - Cesareo (blu) vs Naturale (viola)")
# non si osserva un legame tra il peso e il tipo di parto, in quanto le curve quasi si sovrappongono


# https://academic.oup.com/aje/article/165/8/849/184757?login=false
# Peso normale alla nascita: 2500-4000 g
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8791242/
# Smoking during pregnancy is associated with a considerable reduction in birth weight 
# in different geographic areas, with the range of weight reduction ranging from 
# 77.7 to 232.7 g.
tabella_fumatrice_peso=table(Fumatrici,data_cl$Peso.cl)
test.indipendenza.3=chisq.test(tabella_fumatrice_peso)
test.indipendenza.3
# poichè il p-value è maggiore di 0.05 si accetta l'ipotesi nulla di indipendenza delle variabili Fumatrici e Peso.cl
# Questo risultato potrebbe essere dovuto al caso, o a un non corretto campionamento o ancora potrebbe significare
# che alcune o tutte le madri fumatrici NON abbiano fumato in gravidanza
plot(density(Peso[Fumatrici=="1"]),col="red",main="")
lines(density(Peso[Fumatrici=="0"]),col="green")
title(main="Densità di probabilità - Fumatrici (rosso) vs Non fumatrici (verde)")
abline(v=c(2500,4000))
abline(v=3400,col="blue")
# Le densità di probabilità per pesi bassi o alti sono quasi sovrapponibili (possibile assenza di relazione tra peso basso o alto e madre fumatrice).
# Tuttavia, nella zona normopeso, il picco delle madri fumatrici è spostato più a sinistra, indicando che valori più bassi del peso sono più probabili per madri fumatrici
nonfumatrici_normopeso=sum(table(subset(data,Peso>=2500 & Peso <=4000 & Fumatrici==0)$Peso))
fumatrici_normopeso=sum(table(subset(data,Peso>=2500 & Peso <=4000 & Fumatrici==1)$Peso))
n_nonfumatrici=nrow(subset(data,Fumatrici==0))
n_fumatrici=nrow(subset(data,Fumatrici==1))
p_nonfumatrici_normopeso=nonfumatrici_normopeso/n_nonfumatrici
p_fumatrici_normopeso=fumatrici_normopeso/n_fumatrici
p_nonfumatrici_normopeso
p_fumatrici_normopeso
# Le proporzioni di neonati normopeso con madri non fumatrici o fumatrici 
# sono simili nel campione (possibile assenza di relazione tra normopeso e madre fumatrice). 
#TEST CHI-QUADRATO PER CONFRONTO DI PROPORZIONI TRA GRUPPI
# H0: la probabilità di avere neonati normopeso in una determinata categoria di madre (non fumatrice/fumatrice) è la stessa per le due categorie
case.vector=c(nonfumatrici_normopeso,fumatrici_normopeso) # numero di casi favorevoli (normopeso) per ogni categoria di madre (non fumatrice/fumatrice)
total.vector=c(n_nonfumatrici,n_fumatrici) # numero di casi totali per ogni categoria di madre (non fumatrice/fumatrice)
prop.test(x=case.vector, 
          n=total.vector,
          conf.level = 0.95)
# poichè il p-value è maggiore di 0.05 si accetta l'ipotesi nulla


nonfumatrici_normopeso=sum(table(subset(data,Peso>=2500 & Peso <=3400 & Fumatrici==0)$Peso))
fumatrici_normopeso=sum(table(subset(data,Peso>=2500 & Peso <=3400 & Fumatrici==1)$Peso))
n_nonfumatrici=nrow(subset(data,Fumatrici==0))
n_fumatrici=nrow(subset(data,Fumatrici==1))
p_nonfumatrici_normopeso=nonfumatrici_normopeso/n_nonfumatrici
p_fumatrici_normopeso=fumatrici_normopeso/n_fumatrici
p_nonfumatrici_normopeso
p_fumatrici_normopeso
# Le proporzioni di neonati con peso tra 2500 e 3400 g sono diverse considerando madri non fumatrici o fumatrici 
# (possibile relazione tra peso dato e madre fumatrice): prevale il numero di neonati da madri fumatrici
#TEST CHI-QUADRATO PER CONFRONTO DI PROPORZIONI TRA GRUPPI
# H0: le probabilità di avere neonati con peso tra 2500 e 3400 g per una determinata categoria di madre (non fumatrice/fumatrice) è la stessa per le due categorie
case.vector=c(nonfumatrici_normopeso,fumatrici_normopeso) # numero di casi favorevoli (peso 2500-3400) per ogni categoria di madre (non fumatrice/fumatrice)
total.vector=c(n_nonfumatrici,n_fumatrici) # numero di casi totali per ogni categoria di madre (non fumatrice/fumatrice)
prop.test(x=case.vector, 
          n=total.vector,
          conf.level = 0.95)
# poichè il p-value è minore di 0.05 si rifiuta l'ipotesi nulla



nonfumatrici_normopeso=sum(table(subset(data,Peso>=3400 & Peso <=4000 & Fumatrici==0)$Peso))
fumatrici_normopeso=sum(table(subset(data,Peso>=3400 & Peso <=4000 & Fumatrici==1)$Peso))
n_nonfumatrici=nrow(subset(data,Fumatrici==0))
n_fumatrici=nrow(subset(data,Fumatrici==1))
p_nonfumatrici_normopeso=nonfumatrici_normopeso/n_nonfumatrici
p_fumatrici_normopeso=fumatrici_normopeso/n_fumatrici
p_nonfumatrici_normopeso
p_fumatrici_normopeso
# Le proporzioni di neonati con peso tra 3400 e 4000 g sono diverse considerando madri non fumatrici o fumatrici 
# (possibile relazione tra peso dato e madre fumatrice): prevale il numero di neonati da madri NON fumatrici
#TEST CHI-QUADRATO PER CONFRONTO DI PROPORZIONI TRA GRUPPI
# H0: le probabilità di avere neonati con peso tra 3400 e 4000 g in una determinata categoria di madre (non fumatrice/fumatrice) è la stessa per le due categorie
case.vector=c(nonfumatrici_normopeso,fumatrici_normopeso) # numero di casi favorevoli (peso 2500-3400) per ogni categoria di madre (non fumatrice/fumatrice)
total.vector=c(n_nonfumatrici,n_fumatrici) # numero di casi totali per ogni categoria di madre (non fumatrice/fumatrice)
prop.test(x=case.vector, 
          n=total.vector,
          conf.level = 0.95)
# poichè il p-value è minore di 0.05 si rifiuta l'ipotesi nulla


tabella_ospedale_peso=table(Ospedale,data_cl$Peso.cl)
test.indipendenza.4=chisq.test(tabella_ospedale_peso)
test.indipendenza.4
# poichè il p-value è maggiore di 0.05 si accetta l'ipotesi nulla di indipendenza delle variabili Ospedale e Peso.cl
plot(density(Peso[Ospedale=="osp3"]),col="purple",main="")
lines(density(Peso[Ospedale=="osp2"]),col="blue")
lines(density(Peso[Ospedale=="osp1"]),col="green")
title(main="Densità di probabilità - osp1 (verde) vs osp2 (blu) vs osp3 (viola)")
# le 3 curve quasi si sovrappongono (anche se si osserva un picco più alto rispetto all'ospedale 3):
# non si osserva un legame tra lo specifico ospedale e il peso del neonato


# PUNTO 2
mod1=lm(Peso~.,data=data)
summary(mod1)
# Il test di ipotesi sui coefficienti beta con ipotesi nulla beta=0 mostra, considerando un livello di 
# significatività pari a 0.05, in base ai valori del p-value, che:
# N.gravidanze, Gestazione, Lunghezza, Cranio, Tipo parto, Ospedale ("osp3"), Sesso spiegano la risposta
# Adjusted R-squared:  0.7245

# PUNTO 3
# mod2: elimino le variabili non significative del modello 1
mod2=lm(Peso~N.gravidanze+Gestazione+Lunghezza+Cranio+Tipo.parto+Ospedale+Sesso)
summary(mod2)
library(car)
# https://rpubs.com/CPEL/multlinearregression
# For each variable, GVIF (generalized VIF), DF, and GVIF normalized by DF is produced. 
# The accepted VIF cutoffs are 5 or 10, depending on who you ask. 
# This is to say that if your VIF value is above 5 (or 10), you may consider that variable 
# is collinear and needs to be removed.
# If you run this without any categorical variables, the output will look different. 
# It produces VIFs (instead of GVIFs), which do not need to be normalized by DF, 
# so you can just compare them to your cutoff of 5 or 10.
vif(mod2)
# Multicollinearità: nessuna variabile X dà problemi al modello in quanto GVIF^(1/(2*Df)) < 5
# Adjusted R-squared:  0.7245 (non cambia, preferisco il modello 2, più semplice)

# mod3: elimino le variabili che, in base al punto 1, sono risultate indipendenti rispetto a Y
mod3=lm(Peso~Gestazione+Lunghezza+Cranio+Sesso)
summary(mod3)
# Adjusted R-squared:  0.7222  (non si riduce molto, preferisco il modello 3, più semplice)

# mod4: aggiungo l'effetto quadratico di Gestazione, in base allo scatterplot del punto 1
mod4=update(mod3,~.+I(Gestazione^2))
summary(mod4)
# Adjusted R-squared:  0.7225  (cresce poco, escludiamo questo modello più complesso)

# valuto se il sesso possa avere una qualche influenza sulla composizione corporea e 
# quindi il contributo che la lunghezza possa dare al peso a seconda del sesso 
mod5=update(mod3,~.+Sesso*Lunghezza)
summary(mod5)
# l'interazione delle variabili sesso e lunghezza non è significativa 
# (p-value del coefficiente relativo all'interazione Sesso-Lunghezza maggiore di 0.05)

# valuto se il sesso possa avere una qualche influenza sulla composizione corporea e 
# quindi il contributo che il diametro della testa possa dare al peso a seconda del sesso 
mod6=update(mod3,~.+Sesso*Cranio)
summary(mod6)
# l'interazione delle variabili sesso e cranio non è significativa
# (p-value del coefficiente relativo all'interazione Sesso-Cranio maggiore di 0.05)

AIC(mod1,mod2,mod3,mod4,mod5,mod6) # il modello migliore secondo l'AIC è il modello 2 (AIC minimo)
BIC(mod1,mod2,mod3,mod4,mod5,mod6) # il modello migliore secondo l'AIC è il modello 3 (BIC minimo)
# si sceglie tra i due modelli il modello più semplice, mod3

n=nrow(data)
stepwise.mod=MASS::stepAIC(mod1, # tutta la procedura di ricerca del modello migliore si può effetturare con la funzione stepAIC
                           direction="both", # si parte dal modello 1, con procedura mixed e usando il criterio BIC 
                           k=log(n))
summary(stepwise.mod)
# Adjusted R-squared:  0.7232
# questo metodo restituisce un modello che rispetto al modello 3 considera anche la variabile
# N.Gravidanze: tuttavia l'R^2 aggiustato cresce molto poco, quindi si seleziona ancora il modello 3

# PUNTO 5
par(mfrow=c(2,2))
plot(mod3)
# RESIDUALS VS FITTED: Non tutti i residui hanno media nulla (linea rossa) attorno ai valori stimati
# Q-Q RESIDUALS: I quantili delle distribuzione dei residui cadono all'incirca in corrispondenza dei
# quantili di una distribuzione normale (sulla retta)
# SCALE-LOCATION: La varianza dei residui è all'incirca costante (linea rossa quasi orizzontale) lungo i valori stimati
# RESIDUALS VS LEVERAGE: Individuato un punto levegare (osservazione 1437) che potrebbe avere influenza sul modello,
# perchè la sua distanza di Cook sembra superare la soglia di 1

shapiro.test(residuals(mod3)) 
# poichè il p-value è minore di 0.05 si rifiuta l'ipotesi nulla di distribuzione normale dei residui

library(lmtest)
bptest(mod3)
# poichè il p-value è minore di 0.05 si rifiuta l'ipotesi nulla di omoschedasticità dei residui

dwtest(mod3)
# poichè il p-value è maggiore di 0.05 non si rifiuta l'ipotesi nulla di indipendenza dei residui

# trovo i punti ad alto leverage considerando solo quelli il cui
# leverage è 2 volte più grande del leverage medio
lev=hatvalues(mod3)
plot(lev)
p=sum(lev)
soglia=2*p/n
abline(h=soglia,col=2)
high_lev=lev[lev>soglia]
length(high_lev)
which.max(lev)
# vengono identificati 122 osservazioni con alto leverage.
# L'osservazione con leverage massimo è la 1437

# test l'ipotesi nulla che le osservazioni non siano outlier
# https://quantoid.net/files/702/lecture9.pdf
outlierTest(mod3) 
# 3 outliers (p<0.05), tra cui l'osservazione 1437


# Effetto di ciascuna osservazione su ogni coefficiente beta: 
# le osservazioni per cui il DFBETAS è fuori dalla soglia sono possibili valori influenti
dfbetas=data.frame(dfbetas(mod3))
thresh= 2/sqrt(n)
par(mfrow=c(1,1))
plot(dfbetas$X.Intercept., type='h')
abline(h = thresh, col ="red")
abline(h = -thresh, col ="red")
plot(dfbetas$Gestazione, type='h')
abline(h = thresh, col ="red")
abline(h = -thresh, col ="red")
plot(dfbetas$Lunghezza, type='h')
abline(h = thresh, col ="red")
abline(h = -thresh, col ="red")
plot(dfbetas$Cranio, type='h')
abline(h = thresh, col ="red")
abline(h = -thresh, col ="red")
plot(dfbetas$SessoM, type='h')
abline(h = thresh, col ="red")
abline(h = -thresh, col ="red")
# possibili osservazioni influenti sono presenti per ogni coefficiente

# distanza di cook
cook=cooks.distance(mod3)
plot(cook)
soglia1=0.5
soglia2=1
abline(h=soglia1, col="red")
abline(h=soglia2, col="red")
# solamente un'osservazione supera la distanza di cook
max_cook=max(cook)
influencials=cook[cook>1 & cook==max(cook)] # considero come dato influente quello con massima distanza di Cook (maggiore di 1)
influencials
# l'osservazione 1437 ha distanza di cook maggiore di 1
rows_of_influencials=names(influencials)

# https://rpubs.com/christianthieme/769935
# rimuovo le osservazioni che hanno distanza di cook maggiore di 1 fino a quando ce ne sono e
# non creando più di 10 modelli di regressione
loops=0
list_of_influencials=list(rows_of_influencials)
while (length(influencials)!=0){
  loops=loops+1
  if (loops==1){
    data_reduction=subset(data,row.names(data)!=c(rows_of_influencials))
  } else {
    data_reduction=subset(data_reduction,row.names(data_reduction)!=c(rows_of_influencials))
  }
  mod_x=lm(Peso~Gestazione+Lunghezza+Cranio+Sesso,data=data_reduction)
  cook=cooks.distance(mod_x)
  max_cook=max(cook)
  influencials=cook[cook>1 & cook==max(cook)]
  rows_of_influencials=names(influencials)
  list_of_influencials=append(list_of_influencials,rows_of_influencials)
  if (loops>10) {
    break
  }
}
list_of_influencials
nrow(data)-nrow(data_reduction)
# 2 osservazioni influenti sono state rimosse dal dataset
mod3.1=mod_x
summary(mod3.1)
# Adjusted R-squared:  0.7334 (aumentato rispetto al modello 3, quindi è un possibile modello migliore)


# https://stats.oarc.ucla.edu/r/dae/robust-regression/
# https://rpubs.com/DragonflyStats/Robust-Regression-Weighting
# https://datascienceplus.com/robust-regressions-dealing-with-outliers-in-r/
# https://en.wikipedia.org/wiki/Robust_regression
# Gli approcci robusti sono metodi di regressione alternativi al metodo dei minimi quadrati,
# usati quando le assunzioni richieste da questo metodo non sono rispettate (per esempio ci sono outliers)
# Aggiusto il modello pesando ogni osservazione in base a quanto è un outlier:
# più è outlier, minore sarà il suo peso
library(MASS)
mod3.2=rlm(Peso~Gestazione+Lunghezza+Cranio+Sesso) #robust fitting of linear model using an M estimator
summary(mod3.2)
hweights=data.frame(Peso=data$Peso,  #Observation 
                       resid = mod3.2$resid, #Residual
                       weight = mod3.2$w) #Weight
hweights=hweights[order(mod3.2$w), ] 
head(hweights)
hweights["1437",]
hweights["1551",]
# notare che il modello mod3.1 ha il peso più basso in corrispondenza
# dell'osservazione 1437, ma peso 1 in corrispondenza dell'osservazione 1551:
# la mia interpretazione è che l'osservazione 1551 diventa un outlier solo quando l'osservazione
# 1437 viene rimossa

# PUNTO 6
AIC(mod3,mod3.2)
BIC(mod3,mod3.2)
# secondo i criteri AIC e BIC, il modello 3.2 è peggiore rispetto al modello 3,
# ma abbiamo già osservato che il modello 3.1 è migliore rispetto al modello 3

mod3$coefficients
mod3.1$coefficients
mod3.2$coefficients
# analizzo i coefficienti per verificare quale eventuale modello stima il valore più basso/alto 
# di peso dei neonati, poichè, a seconda delle esigenze dello studio, può essere più utile stimare un valore
# più alto piuttosto che più basso.
# I coefficienti non crescono/decrescono tutti allo stesso modo da un modello a un altro
# (per esempio, il coefficiente per la variabile Gestazione decresce dal modello 3 al modello 3.2,
# mentre il coefficiente per la variabile Lunghezza cresce), per cui in generale
# il modello che fornisce il peso più alto/basso può variare a seconda dei dati specifici.


# PUNTO 7
plot(x=Gestazione[Sesso=="F"],
     y=Lunghezza[Sesso=="F"])
new_born_len_med =median(Lunghezza[Sesso=="F" & Gestazione==39]) # 495 mm
new_born_len_med
new_born_len_min =min(Lunghezza[Sesso=="F" & Gestazione==39]) # 440 mm
new_born_len_min
new_born_len_max =max(Lunghezza[Sesso=="F" & Gestazione==39]) # 550 mm
new_born_len_max
# nel nostro dataset, la lunghezza mediana di una neonata nata alla 39esima settimana è di 495 mm
plot(x=Gestazione[Sesso=="F"],
     y=Cranio[Sesso=="F"])
new_born_head_med=median(Cranio[Sesso=="F" & Gestazione==39]) # 340 mm
new_born_head_med
new_born_head_min=min(Cranio[Sesso=="F" & Gestazione==39]) # 300 mm
new_born_head_min
new_born_head_max=max(Cranio[Sesso=="F" & Gestazione==39]) # 390 mm
new_born_head_max
# nel nostro dataset, il diametro mediano del cranio di una neonata nata alla 39esima settimana è di 340 mm
new_born_med=data.frame(Gestazione=39,Sesso="F",Lunghezza=new_born_len_med,Cranio=new_born_head_med)
new_born_estim_weigth_med=predict(mod3.1, newdata = new_born_med) # 3250 g
print(new_born_estim_weigth_med)
new_born_min=data.frame(Gestazione=39,Sesso="F",Lunghezza=new_born_len_min,Cranio=new_born_head_min)
new_born_estim_weigth_min=predict(mod3.1, newdata = new_born_min) # 2252 g
print(new_born_estim_weigth_min)
new_born_max=data.frame(Gestazione=39,Sesso="F",Lunghezza=new_born_len_max,Cranio=new_born_head_max)
new_born_estim_weigth_max=predict(mod3.1, newdata = new_born_max) # 4349 g
print(new_born_estim_weigth_max)
lett_median_weigth=3000
(new_born_estim_weigth_med-lett_median_weigth)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 6% del range
(new_born_estim_weigth_min-new_born_estim_weigth_med)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 24% del range
(new_born_estim_weigth_max-new_born_estim_weigth_med)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 27% del range
# https://media.tghn.org/articles/newbornsize.pdf
# I dati reali ricavati su neonati ed esposti nell'articolo sopra, mostrano che il 
# 50esimo percentile (mediana) delle neonate nate alla 39esima settimana di gestazione
# hanno una lunghezza attorno a 49 cm, circonferenza della testa attorno a 33/34 cm e peso
# attorno a 3 kg. Dunque, i valori di lunghezza e circonferenza della testa IPOTIZZATI 
# al fine di stimare il peso, sono coerenti con i dati della letteratura. 

# Il peso stimato risulta superiore al peso mediano della letteratura, ma solamente di una quantità pari al 6 % del range.

# Se la lunghezza e il diametro del cranio sono pari ai valori minimi del range (per neonate alla 39esima settimana) allora il peso scende del 25 % rispetto al range (tanto)

# Se la lunghezza e il diametro del cranio sono pari ai valori massimi del range (per neonate alla 39esima settimana) allora il peso sale del 27 % rispetto al range (tanto)

# Dunque, avere queste due variabili (lunghezza e diametro del cranio) è importante per stimare il peso in maniera più accurata

# https://www.statology.org/how-to-interpret-residual-standard-error/#:~:text=Residual%20standard%20error%20%3D%20%E2%88%9A%CE%A3,total%20number%20of%20model%20parameters.
# https://library.virginia.edu/data/articles/getting-started-with-gamma-regression
# Creo un modello lineare generalizzato suppongendo una distribuzione degli errori di tipo Gamma e una link function di tipo "inverse"
mod3.3=glm(Peso~Gestazione+Lunghezza+Cranio+Sesso,family=Gamma(link="inverse"))
summary(mod3.3)
AIC(mod3,mod3.3)
BIC(mod3,mod3.3)
# secondo i criteri AIC e BIC, il modello 3.3 è mogliore rispetto al modello 3

new_born_estim_weigth_med=predict(mod3.3, newdata = new_born_med,type = "response") # 3194 g
new_born_estim_weigth_med
(new_born_estim_weigth_med-lett_median_weigth)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 5% del range
new_born_estim_weigth_min=predict(mod3.3, newdata = new_born_min,type = "response") # 2419 g
new_born_estim_weigth_min
new_born_estim_weigth_max=predict(mod3.3, newdata = new_born_max,type = "response") # 4949 g
new_born_estim_weigth_max
(new_born_estim_weigth_min-new_born_estim_weigth_med)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 19% del range
(new_born_estim_weigth_max-new_born_estim_weigth_med)/(max(Peso)-min(Peso))*100 # differenza tra le due stime pari a circa il 43% del range



panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data_quant[,c(4,6)],upper.panel = panel.smooth,lower.panel = panel.cor)
# la pendenza sembra cambiare attorno al diametro del cranio di 325 mm, quindi
# si potrebbero costruire due modelli separati per i due range di diametro del cranio




panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data_quant[,c(4,3)],upper.panel = panel.smooth,lower.panel = panel.cor)
# la pendenza sembra cambiare alla settimana 37, quindi
# si potrebbero costruire due modelli separati per i due range di settimane di gestazione


plot(density(Peso[Fumatrici=="1"]),col="red",main="")
lines(density(Peso[Fumatrici=="0"]),col="green")
title(main="Densità di probabilità - Fumatrici (rosso) vs Non fumatrici (verde)")
abline(v=c(2500,4000))
abline(v=3400,col="blue")
# come osservato in precedenza, sembra che la variabile madre fumatrice/non fumatrice
# abbia un effetto sulla variabile peso.
# E' possibile che modelli differenti da quelli definiti, che contengono la variabile
# Fumatrice possano tener conto di questo effetto

# PUNTO 8
# https://www.statology.org/plot-multiple-linear-regression-in-r/
avPlots(mod3,
        main="Regular linear model - Linear Regression contribution for each variable")


avPlots(mod3.1,
        main="Regular linear model without outliers - Linear Regression contribution for each variable")

avPlots(mod3.2,
        main="Robust linear model - Linear Regression contribution for each variable")

avPlots(mod3.3,
        main="Generealized linear model without outliers - Linear Regression contribution for each variable")
