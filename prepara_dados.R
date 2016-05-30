library(dplyr)
library(tidyr)
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
## Tabela com grupo (superior, tecnico, básico) de cada funcao que consta na folha de Jan/2015
grupos <- read.csv2("funcoesJan15.csv", as.is=TRUE, row.names=1)
## Acrescenta a variavel de grupo à folha de Janeiro/15
folhaJan15g <- (merge(folhaJan15, grupos, by="funcao"))[,c(names(folhaJan15),"grupo")]
folhaJan15g$grupo <- factor(folhaJan15g$grupo, levels=c("basico","tecnico","superior"))
## Acrescenta a variavel de grupo ao arquivo de pdv
nomes <- names(pdv.final)
pdv.final <- (merge(pdv.final, grupos, by="funcao"))[,c(nomes,"grupo")]
pdv.finalg$grupo <- factor(pdv.finalg$grupo, levels=c("basico","tecnico","superior"))
## Montagem da planilha consolidade por unidade ##
## Consolidando número de pessoas que aderiram, valores pagos e tempo médio de trabalho por unidade
pdv.unid <- pdv.final %>%
    group_by(unidade, grupo) %>%
    summarise(N.pdv=n(), pago.pdv=sum(pago)) %>%
    as.data.frame() %>%
    reshape(idvar="unidade", timevar="grupo", direction="wide")
## Substitui NA's por zero em N e valores pagos para as combinacoes de grupos e unidades que nao tiveram casos de pdv
nomes <- c("N.pdv.basico", "pago.pdv.basico", "N.pdv.superior", "pago.pdv.superior", "N.pdv.tecnico", "pago.pdv.tecnico")
pdv.unid[nomes][is.na(pdv.unid[nomes])] <- 0
## Adiciona variaveis de totais
pdv.unid <- mutate(pdv.unid,
                   N.pdv = N.pdv.basico + N.pdv.superior + N.pdv.tecnico,
                   pago.pdv = pago.pdv.basico + pago.pdv.superior + pago.pdv.tecnico)

## Salario mensal total de servidores nao docentes da ativa por unidade em Jan/2015
ndocente.J15 <- folhaJan15g %>%
    filter(categoria=="Celetista"|categoria=="Func Aut")%>%
    group_by(unidade, grupo) %>%
    summarise(N.nd=n(), sal.nd=sum(salario.mensal)) %>%
    as.data.frame() %>%
    reshape(idvar="unidade", timevar="grupo", direction="wide")
## Substitui eventuais NA's por zeros
nomes <- c("N.nd.basico", "sal.nd.basico", "N.nd.superior", "sal.nd.superior", "N.nd.tecnico", "sal.nd.tecnico")
ndocente.J15[nomes][is.na(ndocente.J15[nomes])] <- 0
## Inclui variaveis de totalizacao 
ndocente.J15 <- mutate(ndocente.J15,
                       N.nd = N.nd.basico + N.nd.superior + N.nd.tecnico,
                       sal.nd = sal.nd.basico + sal.nd.superior + sal.nd.tecnico)
## Salario mensal total de Celetistas por unidade em Jan/2015
clt.J15 <- folhaJan15g %>%
    filter(categoria=="Celetista")%>%
    group_by(unidade, grupo) %>%
    summarise(N.clt=n(), sal.clt=sum(salario.mensal)) %>%
    as.data.frame() %>%
    reshape(idvar="unidade", timevar="grupo", direction="wide")
## Substitui eventuais NA's por zeros
nomes <- c("N.clt.basico", "sal.clt.basico", "N.clt.superior", "sal.clt.superior", "N.clt.tecnico", "sal.clt.tecnico")
clt.J15[nomes][is.na(clt.J15[nomes])] <- 0
## Inclui variaveis de totalizacao 
clt.J15 <- mutate(clt.J15,
                       N.clt = N.clt.basico + N.clt.superior + N.clt.tecnico,
                       sal.clt = sal.clt.basico + sal.clt.superior + sal.clt.tecnico)
## Salario mensal total por unidade de celetistas com 20 ou mais de tempo de trabalho na USP em Jan/2015
clt.20.J15 <- folhaJan15g %>%
    filter(categoria=="Celetista"&tempo.usp>=20)%>%
    group_by(unidade, grupo) %>%
    summarise(N.clt.20 = n(), sal.clt.20 = sum(salario.mensal)) %>%
    as.data.frame() %>%
    reshape(idvar="unidade", timevar="grupo", direction="wide")
## Substitui eventuais NA's por zeros
nomes <- c("N.clt.20.basico", "sal.clt.20.basico", "N.clt.20.superior",
           "sal.clt.20.superior", "N.clt.20.tecnico", "sal.clt.20.tecnico")
clt.20.J15[nomes][is.na(clt.20.J15[nomes])] <- 0
## Inclui variaveis de totalizacao 
clt.20.J15 <- mutate(clt.20.J15,
                       N.clt.20 = N.clt.20.basico + N.clt.20.superior + N.clt.20.tecnico,
                       sal.clt.20 = sal.clt.20.basico + sal.clt.20.superior + sal.clt.20.tecnico)
## Junta todas as planilhas
pdv.unid <- merge(pdv.unid, ndocente.J15, by="unidade")
pdv.unid <- merge(pdv.unid, clt.J15, by="unidade")
pdv.unid <- merge(pdv.unid, clt.20.J15, by="unidade")
## Grava planilhas
write.csv2(pdv.final, "pagospdv.csv")
write.csv2(pdv.unid, "pdvUnidade.csv")
