#########################################################
#  PEPE - Painel de Evolução dos Processos Eletrônicos  #
#                                                       #
#  global.r - Funções Globais e Querys Estatísticas     #
#########################################################

# Bibliotecas necessárias
library(RODBC)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(reshape)
library(scales)
library(shinyjs) 
library(V8)
library(htmlwidgets)
library(js)
library(plotly)
library(shinyBS)
library(openxlsx) 

# Dados de acesso ao banco
# Substituir o Driver pelo driver ODBC apropriado
# Recomendado usar um usuario com permissao apenas de leitura à uma base replicada
#
acesso_sei <- odbcDriverConnect('driver={SQL Server};server=[Endereço];database=[Banco_de_dados];uid=[Usuario Leitura];pwd=[Senha]', DBMSencoding="UTF-8")

# Em caso de token inválido, redirecionar para o endereço abaixo
erro_login <- "shinyjs.navigate = function() {window.stop(); window.location.href='/?exp';}"

# Coleta os orgaos do SEI
lista_orgaos <- sqlQuery(acesso_sei, 'select orgao.sigla from orgao WHERE sin_ativo=\'S\' ORDER BY sigla')
lista_orgaos <- as.vector(lista_orgaos)


##### VISAO GERAL ####
# Quantitativo: Total de processos autuados por órgão e unidade
processos_autuados <- sqlQuery(acesso_sei, 'SELECT orgao.sigla,unidade.sigla, count(1) AS total 
                               FROM protocolo, unidade, orgao WHERE sta_protocolo = \'P\'
                               AND id_unidade_geradora=id_unidade
                               AND unidade.id_orgao=orgao.id_orgao
                               GROUP BY orgao.sigla,unidade.sigla')
processos <- data.frame(processos_autuados)


# Quantitativo: Usuarios cadastrados e ativos
nusu <- sqlQuery(acesso_sei, 'SELECT orgao.sigla,COUNT(1) AS TOTAL 
                 FROM usuario, orgao
                 WHERE usuario.sin_ativo=\'S\'
                 AND orgao.id_orgao=usuario.id_orgao
                 GROUP BY orgao.sigla')
usuarios <- data.frame(nusu)

# Quantitativo: Total de Tipos de Processos
ntp <- sqlQuery(acesso_sei, 'select count(distinct tipo_procedimento.nome) from protocolo
                inner join procedimento on protocolo.id_protocolo = procedimento.id_procedimento
                inner join tipo_procedimento on procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento')
tipo_processo <- as.numeric(ntp)


# Lista de tipos de processos por órgão e unidade
lista_tipos_processos <- sqlQuery(acesso_sei, 'SELECT orgao.sigla,unidade.sigla,tipo_procedimento.nome AS Tipo_processo, COUNT(1) AS total
                                  FROM protocolo, procedimento, tipo_procedimento, unidade, orgao
                                  WHERE protocolo.sta_protocolo=\'P\'
                                  AND protocolo.id_protocolo = procedimento.id_procedimento
                                  AND procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento
                                  AND protocolo.id_unidade_geradora=unidade.id_unidade
                                  AND unidade.id_orgao=orgao.id_orgao
                                  GROUP BY orgao.sigla,unidade.sigla,tipo_procedimento.nome
                                  ORDER BY orgao.sigla,total DESC')
lista_tipos_processos <- data.frame(lista_tipos_processos)


# Processos autuados nas últimas dez semanas
evolucao_semanal_apenas_sei <-sqlQuery(acesso_sei, 'SELECT orgao.sigla orgao, unidade.sigla unidade, datepart(wk, dta_geracao) Semana, datepart(YEAR, dta_geracao) Ano, COUNT(id_protocolo) total
                                       FROM protocolo, unidade, orgao, procedimento, tipo_procedimento
                                       WHERE sta_protocolo=\'P\'
                                       AND dta_geracao < DATEADD(wk, DATEDIFF(wk, 6, CURRENT_TIMESTAMP), 6)
                                       AND dta_geracao >= DATEADD(wk, -10, DATEADD(wk, DATEDIFF(wk, 6, CURRENT_TIMESTAMP), 6))
                                       AND id_unidade_geradora=id_unidade
                                       AND unidade.id_orgao=orgao.id_orgao
                                       AND protocolo.id_protocolo = procedimento.id_procedimento
                                       AND procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento
                                       GROUP BY orgao.sigla, unidade.sigla, DATEPART(wk, dta_geracao), datepart(YEAR, dta_geracao)
                                       ORDER BY orgao.sigla, Ano DESC, Semana DESC')
# BUGFIX SQL Server
# Comentar se as datas estiverem incorretas na evolução semanal - No SQL Server, há uma lógica diferente do R para a numeracao das semanas
evolucao_semanal_apenas_sei$Semana <- evolucao_semanal_apenas_sei$Semana-1
# END: BUGFIX SQL Server

# Processos autuados nas últimas dez semanas - Inserção de Datas
data_inicio_semana_sei <- as.Date(paste(evolucao_semanal_apenas_sei$Ano, evolucao_semanal_apenas_sei$Semana, 7, sep="-"), "%Y-%U-%u")
data_fim_semana_sei <- as.Date(paste(evolucao_semanal_apenas_sei$Ano, evolucao_semanal_apenas_sei$Semana, 6, sep="-"), "%Y-%U-%u")
intervalo <- ifelse((format(data_inicio_semana_sei,"%b") == format(data_fim_semana_sei,"%b")),
                    paste (format(data_inicio_semana_sei,"%d"),"-",format(data_fim_semana_sei,"%d/%b")),
                    paste (format(data_inicio_semana_sei,"%d/%b"),"-",format(data_fim_semana_sei,"%d/%b")))
intervalo <- gsub("[[:space:]]", "", intervalo)
evolucao_semanal_apenas_sei <- data.frame(evolucao_semanal_apenas_sei, data_inicio_semana_sei, data_fim_semana_sei, intervalo)

## Estoques de Processo
estoques_sei <- sqlQuery(acesso_sei, 'SELECT year(dth_abertura) ano, month(dth_abertura) mes, day(dth_abertura) dia,
                         orgao.sigla orgao, unidade.sigla unidade,
                         tipo_procedimento.nome AS tipo_processo,
                         CASE atividade.id_tarefa
                         when 1 then \'Entrada\' 
                         when 29 then \'Entrada\' 
                         when 32 then \'Entrada\'
                         when 28 then \'Saida\'
                         when 41 then \'Saida\' end as atividade,
                         COUNT(1) AS qtde, protocolo.id_protocolo protocolo
                         FROM atividade, unidade, orgao, tipo_procedimento, protocolo, procedimento
                         WHERE atividade.id_tarefa IN (1,29,32,28,41)
                         AND protocolo.id_protocolo IN (SELECT protocolo.id_protocolo FROM protocolo,atividade WHERE protocolo.sta_protocolo=\'P\' AND atividade.id_protocolo = protocolo.id_protocolo AND atividade.dth_conclusao IS NULL)
                         AND atividade.id_unidade=unidade.id_unidade
                         AND unidade.id_orgao=orgao.id_orgao
                         AND atividade.id_protocolo=protocolo.id_protocolo
                         AND protocolo.id_protocolo=procedimento.id_procedimento
                         AND tipo_procedimento.id_tipo_procedimento=procedimento.id_tipo_procedimento
                         GROUP BY orgao.sigla, unidade.sigla, tipo_procedimento.nome, (CASE atividade.id_tarefa when 1 then \'Entrada\' when 29 then \'Entrada\' when 32 then \'Entrada\' when 28 then \'Saida\'when 41 then \'Saida\' END),
                         year(dth_abertura), month(dth_abertura), day(dth_abertura), protocolo.id_protocolo
                         ORDER BY Ano, Mes, Dia DESC, orgao.sigla, qtde DESC')
estoques_sei <- data.frame(estoques_sei)

## Correção de acentuação
levels(estoques_sei[,7])[levels(estoques_sei[,7])=="Saida"] <- "Saída"

## Adicionando a variável de data necessária para o filtro
#
estoques_sei_data <- paste(estoques_sei$Dia,"/",estoques_sei$Mes,"/",estoques_sei$Ano,sep="")
estoques_sei_data_formatado <- as.Date(estoques_sei_data, "%d/%m/%Y")
estoques_sei <- data.frame(estoques_sei, estoques_sei_data, estoques_sei_data_formatado)
colnames(estoques_sei)[10] <- ("data")
colnames(estoques_sei)[11] <- ("data_filtro")

#
## criando a base para órgãos e unidades da base de estoques do SEI
#
base_unidade_estoques <- data.frame(estoques_sei$orgao, estoques_sei$unidade)
vetor_unidade_estoques <- unique(base_unidade_estoques, by=c("orgao","unidade"))

# Controle de processos
controle_processos_sei <- sqlQuery(acesso_sei, 'SELECT orgao.sigla AS orgao,
                                   unidade.sigla AS unidade,
                                   protocolo.protocolo_formatado as num_processo,
                                   year(protocolo.dta_geracao) AS ano,
                                   month(protocolo.dta_geracao) AS mes,
                                   day(protocolo.dta_geracao) AS dia,
                                   usuario.nome AS usuario,
                                   tipo_procedimento.nome AS tipo_processo,
                                   CASE protocolo.sta_nivel_acesso_global when 0 then \'Publico\' 
                                   when 1 then \'Restrito\' 
                                   when 2 then \'Sigiloso\' end as nivel_acesso,
                                   protocolo.descricao as descricao,
                                   (SELECT sigla FROM unidade WHERE id_unidade=atividade.id_unidade) as unidade_atual,
                                   (SELECT orgao.sigla FROM orgao,unidade WHERE orgao.id_orgao = unidade.id_orgao and unidade.id_unidade=atividade.id_unidade) as orgao_atual,
                                   DATEDIFF(day, atividade.dth_abertura, isnull(atividade.dth_conclusao, GETDATE())) as dias_unidade,
                                   DATEDIFF(day,protocolo.dta_geracao, isnull(atividade.dth_conclusao, GETDATE())) as tempo_vida,
                                   (SELECT nome FROM usuario WHERE id_usuario=atividade.id_usuario_atribuicao) AS atribuido
                                   FROM protocolo, unidade, orgao, procedimento, tipo_procedimento, usuario, atividade
                                   WHERE protocolo.sta_protocolo=\'P\'
                                   AND protocolo.id_unidade_geradora = unidade.id_unidade
                                   AND unidade.id_orgao = orgao.id_orgao
                                   AND protocolo.id_protocolo = procedimento.id_procedimento
                                   AND procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento
                                   AND protocolo.id_usuario_gerador = usuario.id_usuario
                                   AND atividade.id_protocolo = protocolo.id_protocolo
                                   AND atividade.dth_conclusao IS NULL
                                   ORDER BY orgao.sigla')
controle_processos_sei <- data.frame(controle_processos_sei)

# Correção de acentuação
levels(controle_processos_sei[,9])[levels(controle_processos_sei[,9])=="Publico"] <- "Público"

## Adicionando a variável de data necessária para o filtro
#
controle_processos_sei_data <- paste(controle_processos_sei$dia,"/",controle_processos_sei$mes,"/",controle_processos_sei$ano,sep="")
controle_processos_sei_data_formatado <- as.Date(controle_processos_sei_data, "%d/%m/%Y")

controle_processos_sei <- data.frame(controle_processos_sei, controle_processos_sei_data, controle_processos_sei_data_formatado)
colnames(controle_processos_sei)[16] <- ("data")
colnames(controle_processos_sei)[17] <- ("data_filtro")

# Criando um Frame resumido dos dados
controle_processos_sei_resumida <- data.frame(controle_processos_sei[,3],controle_processos_sei[,16],controle_processos_sei[,8],controle_processos_sei[,9],
                                              controle_processos_sei[,11],controle_processos_sei[,13],controle_processos_sei[,15],controle_processos_sei[,17],
                                              controle_processos_sei[,12])

# alterando o nome das colunas
colnames(controle_processos_sei_resumida)[1] <- ("num_processo")
colnames(controle_processos_sei_resumida)[2] <- ("Data de autuação")
colnames(controle_processos_sei_resumida)[3] <- ("tipo_processo")
colnames(controle_processos_sei_resumida)[4] <- ("Nível de acesso")
colnames(controle_processos_sei_resumida)[5] <- ("unidade_atual")
colnames(controle_processos_sei_resumida)[6] <- ("Dias na unidade")
colnames(controle_processos_sei_resumida)[7] <- ("Atribuído para")
colnames(controle_processos_sei_resumida)[8] <- ("data_filtro")
colnames(controle_processos_sei_resumida)[9] <- ("orgao_atual")

# Processos por mês
evolucao_mensal_sei <-sqlQuery(acesso_sei, "SELECT orgao.sigla orgao, datepart(month, dta_geracao) Mes, datepart(year, dta_geracao) Ano, 
                               CONCAT(datepart(month, dta_geracao),'/',datepart(YEAR, dta_geracao)) periodo, 
                               COUNT(id_protocolo) total_sei 
                               FROM protocolo, unidade, orgao, procedimento, tipo_procedimento 
                               WHERE sta_protocolo='P' 
                               AND id_unidade_geradora=id_unidade 
                               AND unidade.id_orgao=orgao.id_orgao 
                               AND protocolo.id_protocolo = procedimento.id_procedimento
                               AND procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento
                               GROUP BY orgao.sigla, DATEPART(month, dta_geracao), datepart(YEAR, dta_geracao) 
                               ORDER BY orgao.sigla, datepart(YEAR, dta_geracao) DESC, datepart(month, dta_geracao) DESC")
evolucao_mensal_sei <- data.frame(evolucao_mensal_sei)
evolucao_mensal_sei <- evolucao_mensal_sei %>% group_by(orgao,Ano,Mes,periodo) %>% summarise(total_sei=sum(total_sei))


# Filtro de unidade para indicadores gerais
base_unidade_indicadores_gerais <- data.frame(controle_processos_sei$orgao_atual, controle_processos_sei$unidade_atual)
vetor_unidade_indicadores_gerais <- unique(base_unidade_indicadores_gerais, by=c("orgao_atual","unidade_atual"))

# Tempo mediano de vida por tipo de processo
tabela_tempo_vida <- sqlQuery(acesso_sei,'SELECT protocolo.protocolo_formatado as processo,
                              unidade.sigla AS unidade,
                              orgao.sigla AS orgao,
                              tipo_procedimento.nome AS tipo_processo,
                              DATEDIFF(day,protocolo.dta_geracao, isnull(atividade.dth_conclusao, GETDATE())) as tempo_vida
                              FROM protocolo, unidade, orgao, procedimento, tipo_procedimento, atividade
                              WHERE protocolo.sta_protocolo=\'P\'
                              AND protocolo.id_unidade_geradora = unidade.id_unidade
                              AND unidade.id_orgao = orgao.id_orgao
                              AND protocolo.id_protocolo = procedimento.id_procedimento
                              AND procedimento.id_tipo_procedimento = tipo_procedimento.id_tipo_procedimento
                              AND atividade.id_protocolo = protocolo.id_protocolo
                              AND atividade.id_atividade = (SELECT MAX(id_atividade) from atividade WHERE id_protocolo=protocolo.id_protocolo)')
tabela_tempo_vida <- data.frame(tabela_tempo_vida)
tabela_tempo_vida <- data.frame(tabela_tempo_vida[3],tabela_tempo_vida[2],tabela_tempo_vida[4],tabela_tempo_vida[5])


#
# Indicadores da visão geral
#

# Mediana de tempo de vida de um processo
tabela_tipo_processo_mediana <- tabela_tempo_vida %>% group_by(tipo_processo) %>% summarise(mediana = median(tempo_vida))

# Tempo mediano de cada usuário em relação a certo tipo de processo
tabela_tempo_usuario <- data.frame(controle_processos_sei[12],controle_processos_sei[11],controle_processos_sei[8],
                                   controle_processos_sei[15],controle_processos_sei[13])
tabela_tempo_mediana <- tabela_tempo_usuario %>% group_by(tipo_processo) %>% summarise(mediana = median(dias_unidade))

# Folhas economizadas
# Tamanho dos docs externos e internos
# Folha em Doc Externo ~= 335KB 
# Folha em Doc Interno = 37,5KB

tamanho_documento_externo <- sqlQuery(acesso_sei, 'SELECT orgao.sigla orgao,unidade.sigla unidade,SUM(CAST(tamanho AS BIGINT)) folhas
                                      FROM anexo, orgao, unidade
                                      WHERE unidade.id_orgao=orgao.id_orgao
                                      AND anexo.id_unidade=unidade.id_unidade
                                      GROUP BY orgao.sigla, unidade.sigla')
tamanho_documento_externo <- data.frame(tamanho_documento_externo)
folhas_documento_externo <- (tamanho_documento_externo$tamanho/1024)/335
documento_externo <- data.frame(tamanho_documento_externo[,-3],folhas_documento_externo)

tamanho_documento_interno <- sqlQuery(acesso_sei, 'SELECT orgao.sigla orgao,unidade.sigla unidade, 
                                      SUM(DATALENGTH(documento_conteudo.conteudo)) folhas
                                      FROM documento, documento_conteudo, orgao, unidade
                                      WHERE unidade.id_orgao=orgao.id_orgao
                                      AND documento.id_unidade_responsavel=unidade.id_unidade
                                      AND documento.id_documento=documento_conteudo.id_documento
                                      AND sta_documento=\'I\'
                                      GROUP BY orgao.sigla, unidade.sigla')

tamanho_documento_interno <- data.frame(tamanho_documento_interno)
folhas_documento_interno <- (tamanho_documento_interno$tamanho/1024)/37.5
documento_interno <- data.frame(tamanho_documento_interno[,-3], folhas_documento_interno)
folhas_economizadas <- inner_join(documento_externo, documento_interno, by=c("orgao","unidade"))
soma <- folhas_economizadas$folhas.x + folhas_economizadas$folhas.y
folhas_economizadas <- data.frame(folhas_economizadas[,-(3:4)], soma)


odbcClose(acesso_sei)
