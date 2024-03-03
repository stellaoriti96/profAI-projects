setwd("C:/Users/Dell/Desktop/profession ai/progetto Statistica Descrittiva")

data=read.csv("realestate_texas.csv")

attach(data)


# Calcolo della moda per tutte le variabili
library(DescTools)
modes=apply(data[,c("year","city","months_inventory","month")],2,Mode) # applico la funzione Mode alle colonne del dataframe data
View(modes)

# Calcolo degli indici di posizione media e quartili per tutte le variabili quantitative
data_quant=data[,c("year","sales","volume","median_price","listings","months_inventory")]
pos_indexes=summary(data_quant)
View(pos_indexes)

means=apply(subset(data_quant,select=-year),2,mean)
View(means)

# Boxplot della variabile median_price rispetto alla variabile city
city_ordered_median_price=with(data,reorder(city,median_price,median)) # applico la funzione reorder al dataframe, ordinando i gruppi della variabile city secondo la mediana di median_price 
View(city_ordered_median_price)
city_xlab=paste(levels(factor(city_ordered_median_price)),"\n(N=",table(city),")",sep="")
library(ggplot2)
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data)+ 
  geom_boxplot(aes(x=city_ordered_median_price,
                   y=median_price,
                   fill=city_ordered_median_price))+
  scale_x_discrete(labels=city_xlab)+
  labs(title=expression(~italic("Median price")~"distribution with respect to"~italic("city")~"variable"),
       x="City",
       y="Median price")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="City")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio


# Range delle variabili quantitative
range_function=function(x){
  max(x)-min(x)
}
ranges=apply(data_quant,2,range_function)  
View(ranges)

tapply(median_price, city, range_function) # applico la funzione range_function alla variabile median_price raggruppata secondo la variabile city


# Varianza delle variabili quantitative
variances=apply(subset(data_quant,select=-year),2,var)
View(variances)

# Deviazione standard delle variabili quantitative
st_deviations=sqrt(variances)
View(st_deviations)

# Coefficiente di variazione delle variabili quantitative
CV=st_deviations/means*100
View(CV)
col_names=colnames(subset(data_quant, select = -year))
View(col_names)
data2=data.frame(col_names,CV) # creo un dataframe con le variabili col_names e CV
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data2,aes(x=factor(col_names),y=CV,col=factor(col_names)))+
  geom_point(aes(size=CV))+
  geom_text(aes(label = round(CV)),
            vjust=-1)+
  labs(title="Coefficients of Variation",
       x="",
       y="CV (%)")+
  scale_y_continuous(breaks=seq(0,55,5))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")             
dev.off() # chiudo il plottaggio 

# Range interquartile
iqr=apply(data_quant,2,IQR)
View(iqr)

# Boxplot della variabile sales rispetto alla variabile city
city_ordered_sales=with(data,reorder(city,sales,median)) # applico la funzione reorder al dataframe, ordinando i gruppi della variabile city secondo la mediana di sales
city_xlab=paste(levels(factor(city_ordered_sales)),"\n(N=",table(city),")",sep="")
View(city_xlab)
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data)+ 
  geom_boxplot(aes(x=city_ordered_sales,
                   y=sales,
                   fill=city_ordered_sales))+
  scale_x_discrete(labels=city_xlab)+
  labs(title=expression(~italic("Sales")~"distribution with respect to"~italic("city")~"variable"),
       x="City",
       y="Sales")+
  scale_fill_discrete(name="City")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio

tapply(sales, city, range_function) # applico la funzione range_function alla variabile sales raggruppata secondo la variabile city

# Boxplot della variabile sales rispetto alla variabile year
year_xlab=paste(levels(factor(year)),"\n(N=",table(year),")",sep="")
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data)+ 
  geom_boxplot(aes(x=factor(year),
                   y=sales,
                   fill=factor(year)))+
  scale_x_discrete(labels=year_xlab)+
  labs(title=expression(~italic("Sales")~"distribution with respect to"~italic("year")~"variable"),
       x="Year",
       y="Sales")+
  scale_fill_discrete(name="Year")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio

# Boxplot della variabile sales rispetto alla variabile year e alla variabile city
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data)+
  geom_boxplot(aes(x=factor(city),
                   y=sales,
                   fill=factor(year)))+
  scale_x_discrete(labels=city_xlab)+
  labs(title=expression(~italic("Sales")~"distribution with respect to"~italic("year")~"variable"),
       x="City",
       y="Sales")+
  scale_fill_discrete(name="Year")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio

table(city,year) 

# Grafico a barre sovrapposte per ogni anno, 
# per confrontare il totale delle vendite nei vari mesi, 
# sempre considerando le città.
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(year),y=sales))+
  geom_col(aes(fill=factor(month)),
           col="black",
           position="stack")+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Sales")~"stacked bar plots for each city"),
       x="Year",
       y="Sales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="Month",labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
dev.off() # chiudo il plottaggio

table(year,month)  

# Grafico a barre sovrapposte per ogni anno, 
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(year),y=sales))+
  geom_col(aes(fill=factor(month)),
           col="black",
           position="stack")+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Sales")~"stacked bar plots for each city"),
       x="Year",
       y="Sales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="Month",labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
dev.off() # chiudo il plottaggio

# Grafico a barre sovrapposte per ogni mese
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(month),y=sales))+
  geom_col(aes(fill=factor(year)),
           col="black",
           position="stack")+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Sales")~"stacked bar plots for each city"),
       x="Month",
       y="Sales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="Year")
dev.off() # chiudo il plottaggio

  
# Indice di GINI
gini.index=function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J=length(table(x))
  
  gini=1-sum(fi2)
  gini.normalizzato=gini/((J-1)/J)
  
  return(c(gini.normalizzato,J)) # restituisce l'indice di Gini e il numero di modalità per ogni variabile
}
gini_modal=apply(data[,c("city","month")],2,gini.index)
View(gini_modal)

N=dim(data)[1]  # numero di osservazioni

p_1=table(city)/N # la probabilità che presa una riga a caso questa riporti la città di Beaumont è di 60/240=1/4
View(p_1)

p_2=table(month)/N # la probabilità che presa una riga a caso questa riporti il mese di Luglio è di 20/240=1/12
View(p_2)

p_3=table(year,month)/N # la probabilità che presa una riga a caso questa riporti la il mese di dicembre 2014 è di 4/240=1/60
View(p_3)

data$mean=volume/sales  # variabile mean (prezzo medio) aggiunta al dataframe
data$efficacy=volume/months_inventory # variabile efficacy aggiunta al dataframe

# Grafico a barre per ogni anno, 
# per confrontare l'efficacia degli annunci di vendita nelle diverse città
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(year),y=efficacy))+
  geom_col(fill=year)+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Efficacy")~"vs"~italic("year")~"bar plot for each"~italic("city")),
       x="Year",
       y="Efficacy")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio

# Grafico a barre per ogni anno, 
# per confrontare la variabile volume nelle diverse città
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(year),y=volume))+
  geom_col(fill=year)+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Volume")~"vs"~italic("year")~"bar plot for each"~italic("city")),
       x="Year",
       y="Volume")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio


# Grafico a barre per ogni anno, 
# per confrontare la variabile months_inventory nelle diverse città
tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data,
       aes(x=factor(year),y=months_inventory))+
  geom_col(fill=year)+
  facet_wrap(~factor(city))+
  labs(title=expression(~italic("Months of inventory")~"vs"~italic("year")~"bar plot for each"~italic("city")),
       x="Year",
       y="Months of inventory")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio


# INDICI DI FORMA
library("moments")
sk=skewness(subset(data_quant, select = -year)) #skewness
View(sk)

variable_max_sk=which.max(sk)
View(variable_max_sk)

kurt=kurtosis(subset(data_quant, select = -year)) # kurtosis
View(kurt)

# DISTRIBUZIONI DI FREQUENZA
data_qual=data[,c("city","month")]
freq_abs=apply(data_qual,2,table)  

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data_qual)+ # distribuzione di frequenza della variabile city
  geom_bar(aes(x=city),
           stat="count",
           col="black",
           fill="purple")+
  labs(title="Distribuzione di frequenza",
       x="City",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(expand = c(0, 0))
dev.off() # chiudo il plottaggio

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
ggplot(data=data_qual)+ # distribuzione di frequenza della variabile month
  geom_bar(aes(x=month),
           stat="count",
           col="black",
           fill="orange")+
  labs(title="Distribuzione di frequenza",
       x="Month",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_y_continuous(expand = c(0, 0))
dev.off() # chiudo il plottaggio

# DIVISIONE IN CLASSI DI UNA VARIABILE QUANTITATIVA: SALES 
data_quant$sales_cl=cut(sales, breaks=seq(from=min(sales)-1,to=max(sales),length.out=8))

tiff(file="my_plot.tiff", units="in",width=9,height=6,res=400) # creo una figura tiff
ggplot(data=data_quant)+ # distribuzione di frequenza della variabile month
  geom_bar(aes(x=sales_cl),
           stat="count",
           col="black",
           fill="deepskyblue")+
  labs(title="Distribuzione di frequenza",
       x="Sales classes",
       y="Frequenze assolute")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off() # chiudo il plottaggio

gini_sales_cl=gini.index(data_quant$sales_cl) # un valore dell'indice di Gini molto alto indica che
View(gini_sales_cl)

# Line chart di una variabile a mia scelta per fare confronti fra città e periodi storici
tiff(file="my_plot.tiff", units="in",width=12,height=3,res=400) # creo una figura tiff
ggplot(data=data, 
       aes(x=month, y=volume, group=city)) +
  geom_line(aes(color = city)) + 
  geom_point(aes(color = city)) + 
  facet_wrap(~year,nrow=1)+
  labs(title="Total volume vs time",
       x="Month",
       y="Volume")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,12,1))
dev.off() # chiudo il plottaggio

# Media e deviazione standard di alcune variabili a mia scelta, condizionatamente alla città, agli anni e ai mesi. 
library(dplyr)

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(city) %>%
  summarise(mean_value=mean(sales),
            st_dev_value=sqrt(var(sales))) %>%
  ggplot(aes(x=city, y=mean_value))+ 
  geom_bar(aes(fill=city),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "City",
       y = "Sales")+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(year) %>%
  summarise(mean_value=mean(sales),
            st_dev_value=sqrt(var(sales))) %>%
  ggplot(aes(x=year, y=mean_value))+ 
  geom_bar(aes(fill=year),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "Year",
       y = "Sales")+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(month) %>%
  summarise(mean_value=mean(sales),
            st_dev_value=sqrt(var(sales))) %>%
  ggplot(aes(x=month, y=mean_value))+ 
  geom_bar(aes(fill=month),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "Month",
       y = "Sales")+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio

  



tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(city) %>%
  summarise(mean_value=mean(months_inventory),
            st_dev_value=sqrt(var(months_inventory))) %>%
  ggplot(aes(x=city, y=mean_value))+ 
  geom_bar(aes(fill=city),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "City",
       y = "Months of inventory")+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(year) %>%
  summarise(mean_value=mean(months_inventory),
            st_dev_value=sqrt(var(months_inventory))) %>%
  ggplot(aes(x=year, y=mean_value))+ 
  geom_bar(aes(fill=year),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "Year",
       y = "Months of inventory")+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio

tiff(file="my_plot.tiff", units="in",width=7,height=6,res=400) # creo una figura tiff
data %>%
  group_by(month) %>%
  summarise(mean_value=mean(months_inventory),
            st_dev_value=sqrt(var(months_inventory))) %>%
  ggplot(aes(x=month, y=mean_value))+ 
  geom_bar(aes(fill=month),
           stat="identity") + 
  geom_errorbar(aes(ymin=mean_value-st_dev_value, ymax=mean_value+st_dev_value), width=.2)+
  theme_classic() +
  labs(x = "Month",
       y = "Months of inventory")+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)
dev.off() # chiudo il plottaggio
