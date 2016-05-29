library(dplyr)
## leitura dos dados
## Pagamentos de PDV realizados em 2015, obtido portal de transparencia USP
pago <- read.csv2("pagoPDV_2015.csv", as.is=TRUE)
## Folha de Janeiro de 2015
folhaJan15 <- read.csv2("folhaJan2015.csv", as.is=TRUE, row.names=NULL)
## Folha de Fev 2015
folhaFev15 <- read.csv2("folhaFev2015.csv", as.is=TRUE, row.names=NULL)
## Folha de Mar 2015
folhaMar15 <- read.csv2("folhaMar2015.csv", as.is=TRUE, row.names=NULL)
## Folha de Abr 2015
folhaAbr15 <- read.csv2("folhaAbr2015.csv", as.is=TRUE, row.names=NULL)
## Folha de Mai 2015
folhaMai15 <- read.csv2("folhaMai2015.csv", as.is=TRUE, row.names=NULL)
## altera nomes da tabela, para facilitar
names(folhaJan15) <- names(folhaFev15) <- names(folhaMar15) <- names(folhaAbr15) <- names(folhaMai15) <- 
    c("nome", "unidade", "depto", "jornada", "categoria",
      "classe", "ref.MS", "funcao", "funcao.estrutura", "tempo.usp",
      "parcelas.eventuais", "salario.mensal", "salario.liquido")
## retira uma coluna vazia (problema de importação dos dados)
folhaJan15 <- folhaJan15[, -14]
folhaFev15 <- folhaFev15[, -14]
folhaMar15 <- folhaMar15[, -14]
folhaAbr15 <- folhaAbr15[, -14]
folhaMai15 <- folhaMai15[, -14]
## Cruza tabelas de PDV com a folha de Janeiro, quando todos ainda estavam ativos
pdv <- merge(pago, folhaFev15, by="nome")[, -2]
## Há homonimos 
(hom.names <- unique(pdv$nome[duplicated(pdv$nome)]))
## Planilha com os nomes que batem com estes homônimos, RG e data de desligamento,
## obtida do DOESP de 06/fev-2015 (pag 42)
hom.doesp <- read.csv2("homonimosDOESP.csv", as.is=TRUE)
hom.doesp$data.deslig <- as.Date(hom.doesp$data.deslig, "%d-%m-%Y")
## Planilha apenas com os homônimos
homonimos <- pago[pago$nome %in% hom.names,]
## Verificando os homonimos, comparando os valores de folha avulsa no mes de desligamento
## o servidor correto entre os  homonimos terá valor de outros pagamentos similar ao
## que consta pago como despesas de pdv
## Isso foi feito inspecionando-se os valores pagos em cada mês para todos os homonimos, com os comandos abaixo
## Nomes de cada mês
## Fevereiro
n1 <- hom.doesp[hom.doesp$data.deslig<"2015-03-01","nome"]
fo1 <- folhaFev15
tmp1 <- homonimos %>%
    merge(fo1, by="nome") %>%
    filter(parcelas.eventuais>0&nome%in%n1) %>%
    select(nome, unidade, funcao, pago, parcelas.eventuais, salario.mensal)
## Inspeciona e seleciona os servidores corretos
tmp1
tmp1 <- tmp1[c(2, 3, 5, 6, 7, 8, 9, 12, 14, 15),1:3] ## esta é a parte feita manulamente, por inspeção dos valores em tmp1
## Marco
n1 <- hom.doesp[hom.doesp$data.deslig>"2015-03-01"&hom.doesp$data.deslig<"2015-04-01","nome"]
fo1 <- folhaMar15
tmp2 <- homonimos %>%
    merge(fo1, by="nome") %>%
    filter(parcelas.eventuais>0&nome%in%n1) %>%
    select(nome, unidade, funcao, pago, parcelas.eventuais, salario.mensal)
## Inspeciona e seleciona os servidores corretos
tmp2
tmp2 <- tmp2[c(1, 3, 4, 6, 7, 9),1:3]
## Abril
n1 <- hom.doesp[hom.doesp$data.deslig>"2015-04-01"&hom.doesp$data.deslig<"2015-05-01","nome"]
fo1 <- folhaAbr15
tmp3 <- homonimos %>%
    merge(fo1, by="nome") %>%
    filter(parcelas.eventuais>0&nome%in%n1) %>%
    select(nome, unidade, funcao, pago, parcelas.eventuais, salario.mensal)
## Inspeciona e seleciona os servidores corretos
tmp3
tmp3 <- tmp3[c(1, 3, 4, 6, 8, 9),1:3]
## Reune todos os nomes e unidades em uma so planilha
hom.unid <- rbind(tmp1,tmp2,tmp3)
## Indice na planilha de pdv dos homonimos corretos
pdv.i <- which((paste(pdv$nome,pdv$unidade, pdv$funcao) %in% paste(hom.unid$nome,hom.unid$unidade, hom.unid$funcao)))
## Adiciona os homonimos corretos
pdv.final <- rbind(pdv[!(pdv$nome %in% hom.names),], pdv[pdv.i, ])
## Ultima correcao, que teve que ser manual:
## Ha duas pessoa com nomes Maria Aparecida da Silva e duas com nome Jose Antonio da Silva
## Mas na lista de pagamentos há pagamento para un nome só de cada.
## Possivelmente somaram os pagamentos de duas pessoas sob o mesmo nome no sistema USP
## Minha correcao aproximada foi dividir o pagamanto do PDV na proporcao das parcelas eventuais recebida por cada pessoa
## Maria Aparecida da Silva ##
## Soma das parcelas eventuas recebidas pelas duas 
x1 <- 155659.17 + 104060.03
## Correcao do pago por pdv para cada uma destas pessoas, na proporcao do pago em parcelas eevntuais, conforme folha de pagamento
pdv.final$pago[pdv.final$nome=="Maria Aparecida da Silva"&pdv.final$unidade=="HU"] <- 
    pdv.final$pago[pdv.final$nome=="Maria Aparecida da Silva"&pdv.final$unidade=="HU"]*(155659.17/(x1))
pdv.final$pago[pdv.final$nome=="Maria Aparecida da Silva"&pdv.final$unidade=="FAU"] <-
    pdv.final$pago[pdv.final$nome=="Maria Aparecida da Silva"&pdv.final$unidade=="FAU"]*(104060.03/(x1))
## Jose Antonio da Silva ##
## Soma das parcelas eventuas recebidas pelas duas 
x1 <- 103995.18 + 175137.90
## Correcao do pago por pdv para cada uma destas pessoas, na proporcao do pago em parcelas eevntuais, conforme folha de pagamento
pdv.final$pago[pdv.final$nome=="Jose Antonio da Silva"&pdv.final$unidade=="SEF"] <- 
    pdv.final$pago[pdv.final$nome=="Jose Antonio da Silva"&pdv.final$unidade=="SEF"]*(103995.18/(x1))
pdv.final$pago[pdv.final$nome=="Jose Antonio da Silva"&pdv.final$unidade=="FMRP"] <-
    pdv.final$pago[pdv.final$nome=="Jose Antonio da Silva"&pdv.final$unidade=="FMRP"]*(175137.90/(x1))
## Montagem da planilha consolidade por unidade ##
## Consolidando número de pessoas que aderiram, valores pagos e tempo médio de trabalho por unidade
pdv.unid <- pdv.final %>%
    group_by(unidade) %>%
    summarise(N.pdv=n(), pago.pdv=sum(pago), tempo.medio=mean(tempo.usp))
## Salario mensal total de servidores nao docentes da ativa por unidade em Jan/2015
ndocente.J15 <- folhaJan15 %>%
    filter(categoria=="Celetista"|categoria=="Func Aut")%>%
    group_by(unidade) %>%
    summarise(N.nd=n(), sal.nd=sum(salario.mensal))
## Salario mensal total de Celetistas por unidade em Jan/2015
clt.J15 <- folhaJan15 %>%
    filter(categoria=="Celetista")%>%
    group_by(unidade) %>%
    summarise(N.clt=n(), sal.clt=sum(salario.mensal))
## Salario mensal total por unidade de celetistas com 20 ou mais de tempo de trabalho na USP em Jan/2015
clt.20.J15 <- folhaJan15 %>%
    filter(categoria=="Celetista"&tempo.usp>=20)%>%
    group_by(unidade) %>%
    summarise(N.clt.20 = n(), sal.clt.20 = sum(salario.mensal))
## Junta todas as planilhas
pdv.unid <- merge(pdv.unid, ndocente.J15, by="unidade")
pdv.unid <- merge(pdv.unid, clt.J15, by="unidade")
pdv.unid <- merge(pdv.unid, clt.20.J15, by="unidade")
## Calculo de taxas e razões ##
## Proporcoes do n de pessoas e valores do PDV na USP ##
## em relação a total de funcionarios, celetistas e celetistas com pelo menos 20 anos de USP ##
## Total de funcionarios ##
## % dos incluidos no PDV 
(usp.p.nd <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.nd)*100)
## % do total gasto em PDV
(usp.p.pg.nd <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.nd)*100)
## Total de celetistas ##
## % dos incluidos no PDV
(usp.p.clt <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.clt)*100)
## % do pago em PDV 
(usp.p.pg.clt <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.clt)*100)
## Celetistas com pelo menos 20 anos de USP##
## % dos incluidos no PDV
(usp.p.clt.20 <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.clt.20)*100)
## % do pago em PDV 
(usp.p.pg.clt.20 <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.clt.20)*100)
## Adiciona desvios em relacao à proporcao
pdv.unid.p <- mutate(pdv.unid,
                     p.nd = N.pdv/N.nd*100,
                     desvio.nd = N.pdv - (N.nd*usp.p.nd)/100,
                     p.clt = N.pdv/N.clt*100,
                     desvio.clt = N.pdv - (N.clt*usp.p.clt)/100,
                     p.clt.20 = N.pdv/N.clt.20*100,
                     desvio.clt.20 = N.pdv - (N.clt.20*usp.p.clt.20)/100,
                     p.sal.nd = pago.pdv/sal.nd*100,
                     desvio.sal.nd = pago.pdv - (sal.nd*usp.p.pg.nd)/100,
                     p.sal.clt = pago.pdv/sal.clt*100,
                     desvio.sal.clt = pago.pdv - (sal.clt*usp.p.pg.clt)/100,
                     p.sal.clt.20 = pago.pdv/sal.clt.20*100,
                     desvio.sal.clt.20 = pago.pdv - (sal.clt.20*usp.p.pg.clt.20)/100
                     )
## Grava planilhas
write.csv2(pdv.final, "pagospdv.csv")
## write.csv2(homonimos, "homonimosPdv.csv")
write.csv2(pdv.unid.p, "pdvUnidade.csv")
