select -- numero di clienti
count(distinct id_cliente)
from banca.cliente;


select *
from banca.cliente;

select *
from banca.conto;

select *
from banca.tipo_conto;

select *
from banca.tipo_transazione;

select *
from banca.transazioni;

-- CALCOLO DELL'ETA'
create temporary table banca.tabella_eta as 
select
id_cliente,
data_nascita,
timestampdiff(year, date(data_nascita), current_date()) as eta
from banca.cliente;


select * from banca.tabella_eta;

-- NUMERO DI TRANSAZIONI IN USCITA (SEGNO -)
select -- tutte le transazioni non conteggiate
*
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
left join 
banca.tipo_transazione as tipo_transazione 
on transazioni.id_tipo_trans=tipo_transazione.id_tipo_transazione;

select -- tutte le transazioni conteggiate
cliente.id_cliente,
count(*) as num_trans
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
left join 
banca.tipo_transazione as tipo_transazione 
on transazioni.id_tipo_trans=tipo_transazione.id_tipo_transazione
group by id_cliente;

create temporary table banca.num_transazioni_uscita as
select -- transazioni in uscita conteggiate
cliente.id_cliente,
count(case when tipo_transazione.segno="-" then transazioni.importo end) as num_trans_uscita
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
left join 
banca.tipo_transazione as tipo_transazione 
on transazioni.id_tipo_trans=tipo_transazione.id_tipo_transazione
group by id_cliente;

select * from banca.num_transazioni_uscita;

-- NUMERO DI TRANSAZIONI IN INGRESSO (SEGNO +)
create temporary table banca.num_transazioni_entrata as
select -- transazioni in entrata conteggiate
cliente.id_cliente,
count(case when tipo_transazione.segno="+" then transazioni.importo end) as num_trans_entrata
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
left join 
banca.tipo_transazione as tipo_transazione 
on transazioni.id_tipo_trans=tipo_transazione.id_tipo_transazione
group by id_cliente;

select * from banca.num_transazioni_entrata;


-- IMPORTO TRANSATO IN USCITA SU TUTTI I CONTI (IMPORTO < 0)
create temporary table banca.importo_transazioni_uscita as
select 
cliente.id_cliente,
sum(case when transazioni.importo < 0 then transazioni.importo else 0 end) as importo_uscita
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
group by id_cliente;

select * from banca.importo_transazioni_uscita;

-- IMPORTO TRANSATO IN ENTRATA SU TUTTI I CONTI (IMPORTO > 0)
create temporary table banca.importo_transazioni_entrata as
select 
cliente.id_cliente,
sum(case when transazioni.importo > 0 then transazioni.importo else 0 end) as importo_entrata
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto
group by id_cliente;

select * from banca.importo_transazioni_entrata;

-- NUMERO TOTALE DI CONTI POSSEDUTI
create temporary table banca.numero_conti as
select
cliente.id_cliente,
count(id_conto) as num_conti
from 
banca.cliente
left join
banca.conto
on cliente.id_cliente=conto.id_cliente
group by id_cliente;

select * from banca.numero_conti;

-- NUMERO DI CONTI POSSEDUTI PER TIPOLOGIA (UN INDICATORE PER TIPO)
select
*
from
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente 
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto;

create temporary table banca.numero_conti_tipo as
select
cliente.id_cliente,
count(case when tipo_conto.desc_tipo_conto = "Conto Privati" then conto.id_cliente end) as num_conto_privati,
count(case when tipo_conto.desc_tipo_conto = "Conto Base" then conto.id_cliente end) as num_conto_base,
count(case when tipo_conto.desc_tipo_conto = "Conto Business" then conto.id_cliente end) as num_conto_business,
count(case when tipo_conto.desc_tipo_conto = "Conto Famiglie" then conto.id_cliente end) as num_conto_famiglie
from
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente 
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto
group by id_cliente;

select * from banca.numero_conti_tipo;

-- NUMERO DI TRANSAZIONI IN USCITA (IMPORTO < 0) PER TIPOLOGIA (UN INDICATORE PER TIPO)
create temporary table banca.num_transazioni_uscita_tipo as
select
cliente.id_cliente,
count(case when tipo_conto.desc_tipo_conto = "Conto Privati" and transazioni.importo < 0 then conto.id_cliente end) as num_trans_usc_conto_privati,
count(case when tipo_conto.desc_tipo_conto = "Conto Base" and transazioni.importo < 0 then conto.id_cliente end) as num_trans_usc_conto_base,
count(case when tipo_conto.desc_tipo_conto = "Conto Business" and transazioni.importo < 0 then conto.id_cliente end) as num_trans_usc_conto_business,
count(case when tipo_conto.desc_tipo_conto = "Conto Famiglie" and transazioni.importo < 0 then conto.id_cliente end) as num_trans_usc_conto_famiglie
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto 
group by id_cliente;

select * from banca.num_transazioni_uscita_tipo;

-- NUMERO DI TRANSAZIONI IN ENTRATA (IMPORTO > 0) PER TIPOLOGIA (UN INDICATORE PER TIPO)
create temporary table banca.num_transazioni_entrata_tipo as
select
cliente.id_cliente,
count(case when tipo_conto.desc_tipo_conto = "Conto Privati" and transazioni.importo > 0 then conto.id_cliente end) as num_trans_entr_conto_privati,
count(case when tipo_conto.desc_tipo_conto = "Conto Base" and transazioni.importo > 0 then conto.id_cliente end) as num_trans_entr_conto_base,
count(case when tipo_conto.desc_tipo_conto = "Conto Business" and transazioni.importo > 0 then conto.id_cliente end) as num_trans_entr_conto_business,
count(case when tipo_conto.desc_tipo_conto = "Conto Famiglie" and transazioni.importo > 0 then conto.id_cliente end) as num_trans_entr_conto_famiglie
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto 
group by id_cliente;

select * from banca.num_transazioni_entrata_tipo;


-- IMPORTO TRANSATO IN USCITA (IMPORTO < 0) PER TIPOLOGIA (UN INDICATORE PER TIPO)
create temporary table banca.importo_uscita_tipo as
select
cliente.id_cliente,
sum(case when tipo_conto.desc_tipo_conto = "Conto Privati" and transazioni.importo < 0 then transazioni.importo else 0 end) as importo_usc_conto_privati,
sum(case when tipo_conto.desc_tipo_conto = "Conto Base" and transazioni.importo < 0 then transazioni.importo else 0 end) as importo_usc_conto_base,
sum(case when tipo_conto.desc_tipo_conto = "Conto Business" and transazioni.importo < 0 then transazioni.importo else 0 end) as importo_usc_conto_business,
sum(case when tipo_conto.desc_tipo_conto = "Conto Famiglie" and transazioni.importo < 0 then transazioni.importo else 0 end) as importo_usc_conto_famiglie
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto 
group by id_cliente;

select * from banca.importo_uscita_tipo;

-- IMPORTO TRANSATO IN ENTRATA (IMPORTO > 0) PER TIPOLOGIA (UN INDICATORE PER TIPO)
create temporary table banca.importo_entrata_tipo as
select
cliente.id_cliente,
sum(case when tipo_conto.desc_tipo_conto = "Conto Privati" and transazioni.importo > 0 then transazioni.importo else 0 end) as importo_entr_conto_privati,
sum(case when tipo_conto.desc_tipo_conto = "Conto Base" and transazioni.importo > 0 then transazioni.importo else 0 end) as importo_entr_conto_base,
sum(case when tipo_conto.desc_tipo_conto = "Conto Business" and transazioni.importo > 0 then transazioni.importo else 0 end) as importo_entr_conto_business,
sum(case when tipo_conto.desc_tipo_conto = "Conto Famiglie" and transazioni.importo > 0 then transazioni.importo else 0 end) as importo_entr_conto_famiglie
from 
banca.cliente as cliente
left join
banca.conto as conto
on cliente.id_cliente=conto.id_cliente
left join
banca.tipo_conto as tipo_conto
on conto.id_tipo_conto=tipo_conto.id_tipo_conto
left join
banca.transazioni as transazioni
on conto.id_conto=transazioni.id_conto 
group by id_cliente;

select * from banca.importo_entrata_tipo;

-- CREAZIONE TABELLA DENORMALIZZATA
create table banca.tabella_denormalizzata (
select 
cliente.id_cliente,
eta,
num_trans_uscita,
num_trans_entrata,
importo_uscita,
importo_entrata,
num_conti,
num_conto_privati,
num_conto_base,
num_conto_business,
num_conto_famiglie,
num_trans_usc_conto_privati,
num_trans_usc_conto_base,
num_trans_usc_conto_business,
num_trans_usc_conto_famiglie,
num_trans_entr_conto_privati,
num_trans_entr_conto_base,
num_trans_entr_conto_business,
num_trans_entr_conto_famiglie,
importo_usc_conto_privati,
importo_usc_conto_base,
importo_usc_conto_business,
importo_usc_conto_famiglie,
importo_entr_conto_privati,
importo_entr_conto_base,
importo_entr_conto_business,
importo_entr_conto_famiglie
from banca.cliente as cliente
left join
banca.tabella_eta as tabella_eta
on cliente.id_cliente=tabella_eta.id_cliente
left join
banca.num_transazioni_uscita as num_transazioni_uscita
on cliente.id_cliente=num_transazioni_uscita.id_cliente
left join
banca.num_transazioni_entrata as num_transazioni_entrata
on cliente.id_cliente=num_transazioni_entrata.id_cliente
left join
banca.importo_transazioni_uscita as importo_transazioni_uscita
on cliente.id_cliente=importo_transazioni_uscita.id_cliente
left join
banca.importo_transazioni_entrata as importo_transazioni_entrata
on cliente.id_cliente=importo_transazioni_entrata.id_cliente
left join
banca.numero_conti as numero_conti
on cliente.id_cliente=numero_conti.id_cliente
left join
banca.numero_conti_tipo as numero_conti_tipo
on cliente.id_cliente=numero_conti_tipo.id_cliente
left join
banca.num_transazioni_uscita_tipo as num_transazioni_uscita_tipo
on cliente.id_cliente=num_transazioni_uscita_tipo.id_cliente
left join
banca.num_transazioni_entrata_tipo as num_transazioni_entrata_tipo
on cliente.id_cliente=num_transazioni_entrata_tipo.id_cliente
left join
banca.importo_uscita_tipo as importo_uscita_tipo
on cliente.id_cliente=importo_uscita_tipo.id_cliente
left join
banca.importo_entrata_tipo as importo_entrata_tipo
on cliente.id_cliente=importo_entrata_tipo.id_cliente
) ;

select * from banca.tabella_denormalizzata;


select -- numero di righe
count(id_cliente)
from banca.tabella_denormalizzata;









