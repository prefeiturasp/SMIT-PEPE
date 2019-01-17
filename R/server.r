#########################################################
#  PEPE - Painel de Evolução dos Processos Eletrônicos  #
#                                                       #
#  server.r - Tratamento dos datasets                   #
#########################################################

login <- odbcDriverConnect('driver={MySQL};server=localhost;database=pepe;uid=[usuario];pwd=[senha]')

function(input, output, session) {
  jscode <- "shinyjs.init = function() { $(document).keypress(function(e) { alert('Key pressed: ' + e.which); }); }"
  consulta_validacao <- c()
  
  # Guarda o token, para validar a sessão do usuário
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query[['token']])) {
      js$navigate()
    }
    if (!is.null(query[['token']])) {
      updateTextInput(session, "token", value = query[['token']])
      token <-  query[['token']]
      print("Token recebido")
      query_validacao <- paste('SELECT orgao.sigla_sei FROM auth,permissao,orgao WHERE token=\'',token, '\' AND created_at > DATE_SUB(CURDATE(), INTERVAL 1 DAY) AND permissao.id_user = auth.id_user AND permissao.id_orgao = orgao.id',sep="")
      consulta_validacao <- sqlQuery(login, query_validacao)
      print(consulta_validacao)
      if (nrow(consulta_validacao) == 0) {
        print("caiu na condição importante")
        js$navigate()
      }
    }
    
    base_orgaos_sei <- reactive ({
      print("Entrou no laço dos órgãos")
      consulta <- data.frame(consulta_validacao) #para pegar os órgãos em que o usuário possui acesso
      base <- consulta[,1]
      if (base[1] == '*') {
        base <- c("Todos", t(lista_orgaos))
      }
      return(base)
    })
    
    # Carrega lista de orgaos da instalacao no combo
    output$orgaos_sei <- renderUI({
      selectInput('selecao_orgao', 'Órgão a ser analisado', as.list(base_orgaos_sei()))
    })
    
  })
  
  output$processosBox <- renderValueBox({
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas"){
      num <- nrow(controle_processos_sei_resumida)
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- subset(controle_processos_sei_resumida, unidade_atual == input$selecao_unidade)
      num <- nrow(data)
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- subset(controle_processos_sei_resumida, orgao_atual == input$selecao_orgao)
      num <- nrow(data)
    }
    else {
      data <- subset(controle_processos_sei_resumida, (orgao_atual == input$selecao_orgao & unidade_atual == input$selecao_unidade))
      num <- nrow(data)
    }
    
    num <- format(num, digits=9, decimal.mark=",",big.mark=".",small.mark=".", small.interval=3)
    
    valueBox(
      num,"Processos abertos", icon = icon("folder-open-o"),
      color = "red"
    )
  })
  
  
  output$usuariosBox <- renderValueBox({
    if(input$selecao_orgao == "Todos") {
      num <- sum(usuarios$TOTAL)
    }
    else {
      num_usuarios <- subset(usuarios, sigla == input$selecao_orgao) 
      num <- as.numeric(num_usuarios[1,2])
    }
    num <- format(num, digits=9, decimal.mark=",",big.mark=".",small.mark=".", small.interval=3)
    
    valueBox(
      num, "Total de usuários cadastrados", icon = icon("users"),
      color = "blue"
    )
  })
  
  output$tempomedianoBox <- renderValueBox({
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      mediana <- median(tabela_tempo_vida$tempo_vida)
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- tabela_tempo_vida %>% filter(unidade == input$selecao_unidade)
      mediana <- median(data$tempo_vida)
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao)
      mediana <- median(data$tempo_vida)
    }
    else {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao & unidade == input$selecao_unidade)
      mediana <- median(data$tempo_vida)
    }
    mediana <- format(mediana, digits=9, decimal.mark=",",big.mark=".",small.mark=".", small.interval=3)
    mediana <- paste(mediana,' dias')
    
    valueBox(
      mediana, "Tempo de vida de um processo", icon = icon("clock-o"),
      color = "orange"
    )
  })
  
  base_tipos_processos_mais_autuados <- reactive({
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      data <- lista_tipos_processos %>% group_by(Tipo_processo) %>% summarise (total = sum(total))
    }
    if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- subset(lista_tipos_processos, sigla.1 == input$selecao_unidade)
      data <- data.frame(data$Tipo_processo, data$total)
      
    }
    if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- subset(lista_tipos_processos, sigla == input$selecao_orgao)
      data <- data %>% group_by(Tipo_processo) %>% summarise(total=sum(total))
    }
    if(input$selecao_orgao != "Todos" && input$selecao_unidade != "Todas") {
      data <- subset(lista_tipos_processos, (sigla == input$selecao_orgao & sigla.1 == input$selecao_unidade))
      data <- data.frame(data$Tipo_processo, data$total)
      
    }
    data <- data.frame(data)
    colnames(data)[1] <- ("Tipo_processo")
    colnames(data)[2] <- ("total")
    
    data
    
  })
  
  
  
  output$tipos_processos_mais_autuados <- renderPlotly({
    plot_ly(base_tipos_processos_mais_autuados(), labels = ~Tipo_processo, values = ~total, type='pie', textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Tipo_processo, ': ', total, ' processos'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)
    
  })
  
  
  output$folhasBox <- renderValueBox({
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      num <- round(sum(folhas_economizadas$soma),0)
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- subset(folhas_economizadas, unidade == input$selecao_unidade)
      num <- round(as.numeric(data$soma),0)
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- folhas_economizadas %>% filter(orgao == input$selecao_orgao)
      num <- round(sum(data$soma),0)
    }
    else {
      data <- folhas_economizadas %>% filter(orgao == input$selecao_orgao & unidade == input$selecao_unidade)
      num <- round(as.numeric(data$soma),0)
    }
    
    num <- format(num, digits=9, decimal.mark=",",big.mark=".",small.mark=".", small.interval=3)
    
    valueBox(
      num, "Folhas economizadas", icon = icon("tree-deciduous", lib = "glyphicon"),
      color = "green"
    )
  })
  
  
  base_grafico_semanal <- reactive({
    base <- evolucao_semanal_apenas_sei
    
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      base <- base %>% group_by(data_inicio_semana_sei,intervalo) %>% summarise(total=sum(total))
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      base <- subset(base, unidade == input$selecao_unidade)
      base <- base %>% group_by(data_inicio_semana_sei,intervalo) %>% summarise(total = sum(total))
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      base <- subset(base, orgao == input$selecao_orgao)
      base <- base %>% group_by(data_inicio_semana_sei,intervalo) %>% summarise(total = sum(total))
    }
    else {
      base <- subset(base, (orgao == input$selecao_orgao & unidade == input$selecao_unidade))
      base <- base %>% group_by(data_inicio_semana_sei,intervalo) %>% summarise(total = sum(total))
    }
    #base <- base[order(as.numeric(base$data_inicio_semana_sei)),]
    base <- base[,-1:0]
    base
  })
  
  output$grafico_evolucao_semanal_sei = renderPlotly({
    plot_ly(base_grafico_semanal(), x = ~intervalo, y = ~total, type = "bar", textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            #text = ~paste('Semana: ', intervalo, ' / ', total, ' processos'),
            text = ~paste(' ', total, ' processos'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(
        yaxis = list(title=''),
        xaxis = list(categoryorder = "trace", showgrid = FALSE, zeroline = FALSE, title='',tickfont=list(size=10), tickangle=0)) 
  })
  
  # funções para a tela de controle de estoques e processos do SEI
  
  # primeiro, funções relacionadas aos estoques
  
  # essa função permite que você visualize apenas as unidades de controle de estoques às quais o usuário possui permissão
  
  base_unidades_estoques <- reactive({
    vetor <- vetor_unidade_estoques
    if(input$selecao_orgao != "Todos") {
      vetor <- subset(vetor, estoques_sei.orgao == input$selecao_orgao)
    }
    
    base <- vetor$estoques_sei.unidade
    base <- base[ base != '*'] 
    
    return(base)
  })
  
  output$unidades_estoques <- renderUI({
    
    selectInput("opcao_unidade_estoque","Unidade:", c("Todas", as.list(sort(base_unidades_estoques(),decreasing=F)) ))
  })
  
  #######################################################################
  
  # gráfico de estoques
  
  output$GraficoEstoquesSEI <- renderUI ({
    if(input$visualizarConteudoSubunidades) {
      Base_estoques <- reactive({
        
        data <- estoques_sei
        
        data <- data %>%
          filter(
            data_filtro >= as.Date(input$opcao_data_estoque[1]),
            data_filtro <= as.Date(input$opcao_data_estoque[2])
          )
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        data
      })
      
      Base_saldo_estoques <- reactive({
        
        data <- estoques_sei
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          # primeiro, o saldo acumulado até a data anterior do filtro
          dados_anteriores <- subset(estoques_sei, estoques_sei$data_filtro < input$opcao_data_estoque[1])
          
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          # primeiro, o saldo acumulado até a data anterior do filtro
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$orgao == input$selecao_orgao))
          
          data <- estoques_sei %>% filter(orgao == input$selecao_orgao)
          
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1]))
          dados_anteriores <- dados_anteriores[grep(input$opcao_unidade_estoque, dados_anteriores$unidade), ]
          
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo))
          
          data <- data %>% filter(tipo_processo == input$opcao_estoque_tipo_processo)
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] &
                                                      estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo))
          dados_anteriores <- dados_anteriores[grep(input$opcao_unidade_estoque, dados_anteriores$unidade), ]
          
          data <- data %>% filter(tipo_processo == input$opcao_estoque_tipo_processo)
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] &
                                                      estoques_sei$orgao == input$selecao_orgao))
          dados_anteriores <- dados_anteriores[grep(input$opcao_unidade_estoque, dados_anteriores$unidade), ]
          
          data <- data %>% filter(orgao == input$selecao_orgao)
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo &
                                                      estoques_sei$orgao == input$selecao_orgao))
          
          data <- estoques_sei %>% filter(tipo_processo == input$opcao_estoque_tipo_processo & orgao == input$selecao_orgao)
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo &
                                                      estoques_sei$orgao == input$selecao_orgao))
          dados_anteriores <- dados_anteriores[grep(input$opcao_unidade_estoque, dados_anteriores$unidade), ]
          
          data <- data %>% filter(tipo_processo == input$opcao_estoque_tipo_processo & orgao == input$selecao_orgao)
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if (nrow(dados_anteriores) == 0) {
          aux = 0
        }
        else {
          tabela_aux <- dados_anteriores %>% group_by(atividade,protocolo) %>% summarise(qtde = 1)
          
          #tabela_aux <- dados_anteriores[!duplicated(dados_anteriores$protocolo),]
          tabela_aux <- dados_anteriores %>% group_by(atividade) %>% summarise(qtde = sum(qtde))
          tabela_aux[2,2] <- tabela_aux[2,2] * (-1)
          aux <- as.numeric(tabela_aux %>% summarise(qtde = sum(qtde)))
        }
        
        data <- data %>%
          filter(
            data_filtro >= as.Date(input$opcao_data_estoque[1]),
            data_filtro <= as.Date(input$opcao_data_estoque[2])
          )
        
        data$qtde <- ifelse((data$atividade == "Entrada"), data$qtde, (-1)*data$qtde)
        
        tabela_estoque <- data %>% group_by(data_filtro) %>% summarise(estoque_diario = sum(qtde))
        
        tabela_estoque <- tabela_estoque[order(tabela_estoque$data_filtro),]
        tabela_estoque[1,2] <- tabela_estoque[1,2] + aux
        
        tabela_estoque <- tabela_estoque %>% mutate(estoque = cumsum(estoque_diario))
        
        tabela_estoque
      })
      
      output$grafico_estoques_sei = renderPlotly({
        
        Entrada <- subset(Base_estoques(), atividade == "Entrada")
        Saida <- subset(Base_estoques(), atividade == "Saída")
        
        p <- plot_ly(Entrada, x = ~data_filtro, y = ~qtde, type = 'bar', name="Entrada") %>%
          add_trace(y= ~qtde, data = Saida, name = "Saída", marker = list(color = "#2CA02C")) %>%
          add_lines(y = ~estoque, data = Base_saldo_estoques(), name = "Estoque", yaxis = "y2", line = list(color = "#DD4B39")) %>%
          layout(yaxis = list(title = 'Processos autuados'), legend = list(orientation = 'h'), barmode = 'group',
                 yaxis2 = list(
                   overlaying = "y",
                   side = "right",
                   title = "Estoque de processos acumulados", showgrid = FALSE, zeroline = FALSE
                 ),
                 xaxis = list(title = 'Data'),margin = list(
                   l = 80, r = 90)
          )
        p
      })
    }
    
    else {
      Base_estoques <- reactive({
        
        data <- estoques_sei
        
        data <- data %>%
          filter(
            data_filtro >= as.Date(input$opcao_data_estoque[1]),
            data_filtro <= as.Date(input$opcao_data_estoque[2])
          )
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
          data <- data %>% group_by(data_filtro,atividade) %>% summarise(qtde = sum(qtde))
        }
        
        data
      })
      
      Base_saldo_estoques <- reactive({
        
        data <- estoques_sei
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          # primeiro, o saldo acumulado até a data anterior do filtro
          dados_anteriores <- subset(estoques_sei, estoques_sei$data_filtro < input$opcao_data_estoque[1])
          
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          # primeiro, o saldo acumulado até a data anterior do filtro
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$orgao == input$selecao_orgao))
          
          data <- estoques_sei %>% filter(orgao == input$selecao_orgao)
          
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$unidade == input$opcao_unidade_estoque))
          
          data <- estoques_sei %>% filter(unidade == input$opcao_unidade_estoque)
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo))
          
          data <- estoques_sei %>% filter(tipo_processo == input$opcao_estoque_tipo_processo)
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$unidade == input$opcao_unidade_estoque &
                                                      estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo))
          
          data <- estoques_sei %>% filter(unidade == input$opcao_unidade_estoque & tipo_processo == input$opcao_estoque_tipo_processo)
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$unidade == input$opcao_unidade_estoque &
                                                      estoques_sei$orgao == input$selecao_orgao))
          
          data <- estoques_sei %>% filter(unidade == input$opcao_unidade_estoque & orgao == input$selecao_orgao)
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo &
                                                      estoques_sei$orgao == input$selecao_orgao))
          
          data <- estoques_sei %>% filter(tipo_processo == input$opcao_estoque_tipo_processo & orgao == input$selecao_orgao)
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          dados_anteriores <- subset(estoques_sei, (estoques_sei$data_filtro < input$opcao_data_estoque[1] & estoques_sei$tipo_processo == input$opcao_estoque_tipo_processo &
                                                      estoques_sei$orgao == input$selecao_orgao & estoques_sei$unidade == input$opcao_unidade_estoque))
          
          data <- estoques_sei %>% filter(tipo_processo == input$opcao_estoque_tipo_processo & orgao == input$selecao_orgao & unidade == input$opcao_unidade_estoque)
        }
        
        if (nrow(dados_anteriores) == 0) {
          aux = 0
        }
        
        else {
          tabela_aux <- dados_anteriores %>% group_by(atividade) %>% summarise(qtde = sum(qtde))
          tabela_aux[2,2] <- tabela_aux[2,2] * (-1)
          aux <- as.numeric(tabela_aux %>% summarise(qtde = sum(qtde)))
        }
        
        data <- data %>%
          filter(
            data_filtro >= as.Date(input$opcao_data_estoque[1]),
            data_filtro <= as.Date(input$opcao_data_estoque[2])
          )
        
        data$qtde <- ifelse((data$atividade == "Entrada"), data$qtde, (-1)*data$qtde)
        
        tabela_estoque <- data %>% group_by(data_filtro) %>% summarise(estoque_diario = sum(qtde))
        
        tabela_estoque <- tabela_estoque[order(tabela_estoque$data_filtro),]
        tabela_estoque[1,2] <- tabela_estoque[1,2] + aux
        
        tabela_estoque <- tabela_estoque %>% mutate(estoque = cumsum(estoque_diario))
        
        tabela_estoque
      })
      
      
      output$grafico_estoques_sei = renderPlotly({
        Entrada <- subset(Base_estoques(), atividade == "Entrada")
        Saida <- subset(Base_estoques(), atividade == "Saída")
        
        p <- plot_ly(Entrada, x = ~data_filtro, y = ~qtde, type = 'bar', name="Entrada") %>%
          add_trace(y= ~qtde, data = Saida, name = "Saída", marker = list(color = "#2CA02C")) %>%
          add_lines(y = ~estoque, data = Base_saldo_estoques(), name = "Estoque", yaxis = "y2", line = list(color = "#DD4B39")) %>%
          layout(yaxis = list(side = "left", title = 'Processos autuados'), legend = list(orientation = 'h'), barmode = 'group',
                 yaxis2 = list(
                   overlaying = "y",
                   side = "right",
                   title = "Estoque de processos acumulados", showgrid = FALSE, zeroline = FALSE
                 ),
                 xaxis = list(title = 'Data'),margin = list(
                   l = 80, r = 90)
          )
        p
      })
      
    }
    plotlyOutput('grafico_estoques_sei')
  })
  
  ### FIM GRAFICO DE ESTOQUES
  
  
  
  output$TabelaContProcessosSEIResumida <- renderUI ({
    if(input$visualizarConteudoSubunidades) {
      
      base_controle_processos_sei_resumida <- reactive({
        
        data <- controle_processos_sei_resumida
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,1:7])
        
        colnames(data)[1] <- ("Processo")

        data
      })
      
      output$tabela_controle_processos_sei_resumida <- DT::renderDataTable(DT::datatable({
        base_controle_processos_sei_resumida()
      },
      extensions = 'Buttons', 
      escape = TRUE,
      rownames = FALSE, 
      server = F,
      class = "nowrap row-border",
      options = list(
        destroy = T,
        scrollX = F,
        dom = 'Bfrtip',
        buttons = 
          list(list(extend = 'copy',text = 'Copiar'),
               list(extend = 'print',text = 'Imprimir'),
               list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')
          ) 
        #,columnDefs = list(list(targets = c(0, 1, 4, 6, 8, 9, 11), visible = T), list(targets = '_all', visible = F)),
      ) 
      ))
    }
    
    else {
      
      base_controle_processos_sei_resumida <- reactive({
        
        data <- controle_processos_sei_resumida
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,1:7])
        colnames(data)[1] <- ("Processo")
        colnames(data)[2] <- ("Data")
        colnames(data)[3] <- ("Tipo de Processo")
        colnames(data)[4] <- ("Nível de Acesso")
        colnames(data)[5] <- ("Unidade Atual")
        colnames(data)[6] <- ("Dias na unidade")
        colnames(data)[7] <- ("Atribuído para")
        
        data
      })
      
      
      
      
      output$tabela_controle_processos_sei_resumida <- DT::renderDataTable({
        base_controle_processos_sei_resumida()
      }
      ,rownames = F
      ,server=T
      #,class = "nowrap row-border"
      ,extensions = 'Buttons'
      ,options = list(
        destroy = T,
        escape = T,
        scrollX = F,
        dom = 'Bfrtip',
        #initComplete = JS('function(setting, json) { setTimeout(scrollDT2(), 1000); }'),
        buttons = list(list(extend = 'copy',text = 'Copiar'),
                       list(extend = 'print',text = 'Imprimir'),
                       list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')
        )
        #,columnDefs = list(list(targets = c(0, 1, 4, 6, 8, 9, 11), visible = T))
      )
      )
      
    }
    DT::dataTableOutput("tabela_controle_processos_sei_resumida")
  })
  
  
  output$TabelaContProcessosSEIDownloadXLSX <- renderUI ({
    
    if(input$visualizarConteudoSubunidades) {
      base_controle_processos_sei_download <- reactive({
        data <- controle_processos_sei
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,3],data[,16],data[,1], data[,2],data[,8],data[,7],data[,9],
                           data[,10],data[,11],data[,13],data[,14],data[,15])
        
        colnames(data)[1] <- ("Número do processo")
        colnames(data)[2] <- ("Data de autuação")
        colnames(data)[3] <- ("Órgão gerador")
        colnames(data)[4] <- ("Unidade geradora")
        colnames(data)[5] <- ("Tipo de processo")
        colnames(data)[6] <- ("Usuário gerador")
        colnames(data)[7] <- ("Nível de acesso")
        colnames(data)[8] <- ("Descrição")
        colnames(data)[9] <- ("Unidade atual")
        colnames(data)[10] <- ("Dias em que o processo está na unidade")
        colnames(data)[11] <- ("Tempo de vida")
        colnames(data)[12] <- ("Atribuído para")
        
        data
        
      })
      
      # para baixar as planilhas de controle de processos em .xlsx
      output$downloadTabelaProcessosXLSX <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.xlsx', sep='') 
        },
        content = function(file) {
          data <- base_controle_processos_sei_download()
          write.xlsx(data, file, row.names=FALSE)
        }
      )
      
    }
    
    else {
      base_controle_processos_sei_download <- reactive({
        data <- controle_processos_sei
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,3],data[,16],data[,1], data[,2],data[,8],data[,7],data[,9],
                           data[,10],data[,11],data[,13],data[,14],data[,15])
        
        colnames(data)[1] <- ("Número do processo")
        colnames(data)[2] <- ("Data de autuação")
        colnames(data)[3] <- ("Órgão gerador")
        colnames(data)[4] <- ("Unidade geradora")
        colnames(data)[5] <- ("Tipo de processo")
        colnames(data)[6] <- ("Usuário gerador")
        colnames(data)[7] <- ("Nível de acesso")
        colnames(data)[8] <- ("Descrição")
        colnames(data)[9] <- ("Unidade atual")
        colnames(data)[10] <- ("Dias em que o processo está na unidade")
        colnames(data)[11] <- ("Tempo de vida")
        colnames(data)[12] <- ("Atribuído para")
        
        data
      })
      
      # para baixar as planilhas de controle de processos em .xlsx
      output$downloadTabelaProcessosXLSX <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.xlsx', sep='') 
        },
        content = function(file) {
          data <- base_controle_processos_sei_download()
          write.xlsx(data, file, row.names=FALSE)
        }
      )
      
    }
    
    downloadButton('downloadTabelaProcessosXLSX', 'Download em .xlsx')
  })
  
  output$TabelaContProcessosSEIDownloadCSV <- renderUI ({
    
    if(input$visualizarConteudoSubunidades) {
      base_controle_processos_sei_download <- reactive({
        data <- controle_processos_sei
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,3],data[,16],data[,1], data[,2],data[,8],data[,7],data[,9],
                           data[,10],data[,11],data[,13],data[,14],data[,15])
        
        colnames(data)[1] <- ("Número do processo")
        colnames(data)[2] <- ("Data de autuação")
        colnames(data)[3] <- ("Órgão gerador")
        colnames(data)[4] <- ("Unidade geradora")
        colnames(data)[5] <- ("Tipo de processo")
        colnames(data)[6] <- ("Usuário gerador")
        colnames(data)[7] <- ("Nível de acesso")
        colnames(data)[8] <- ("Descrição")
        colnames(data)[9] <- ("Unidade atual")
        colnames(data)[10] <- ("Dias em que o processo está na unidade")
        colnames(data)[11] <- ("Tempo de vida")
        colnames(data)[12] <- ("Atribuído para")
        
        data
        
      })
      
      # para baixar as planilhas de controle de processos em .csv
      output$downloadTabelaProcessosCSV <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.csv', sep='') 
        },
        content = function(file) {
          write.csv(base_controle_processos_sei_download(), file, fileEncoding = "UTF-16LE",sep=';',dec=',', row.names=FALSE)
        }
      )
      
    }
    
    else {
      base_controle_processos_sei_download <- reactive({
        data <- controle_processos_sei
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data <- data.frame(data[,3],data[,16],data[,1], data[,2],data[,8],data[,7],data[,9],
                           data[,10],data[,11],data[,13],data[,14],data[,15])
        
        colnames(data)[1] <- ("Número do processo")
        colnames(data)[2] <- ("Data de autuação")
        colnames(data)[3] <- ("Órgão gerador")
        colnames(data)[4] <- ("Unidade geradora")
        colnames(data)[5] <- ("Tipo de processo")
        colnames(data)[6] <- ("Usuário gerador")
        colnames(data)[7] <- ("Nível de acesso")
        colnames(data)[8] <- ("Descrição")
        colnames(data)[9] <- ("Unidade atual")
        colnames(data)[10] <- ("Dias em que o processo está na unidade")
        colnames(data)[11] <- ("Tempo de vida")
        colnames(data)[12] <- ("Atribuído para")
        
        data
      })
      
      # para baixar as planilhas de controle de processos em .csv
      output$downloadTabelaProcessosCSV <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.csv', sep='') 
        },
        content = function(file) {
          write.csv(base_controle_processos_sei_download(), file, fileEncoding = "UTF-16LE",sep=';',dec=',', row.names=FALSE)
        }
      )
      
    }
    
    downloadButton('downloadTabelaProcessosCSV','Download em .csv')
  })
  
  output$calculoTotalEstoqueSEI <- renderUI ({
    if(input$visualizarConteudoSubunidades) {
      
      base_controle_processos_sei_resumida <- reactive({
        
        data <- controle_processos_sei_resumida
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        total_estoque <- length(unique(data[,1]))
        total_estoque
      })
      
      output$totalEstoqueSEI <- renderValueBox({
        valueBox( # trocar ícone 27/10/2017
          base_controle_processos_sei_resumida(), "Saldo do estoque", icon = icon("list"),
          color = "red"
        )
        
      })
      
    }
    
    else {
      
      base_controle_processos_sei_resumida <- reactive({
        
        data <- controle_processos_sei_resumida
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[data$unidade_atual == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        total_estoque <- length(unique(data[,1]))
        total_estoque
      })
      
      output$totalEstoqueSEI <- renderValueBox({
        valueBox( # trocar ícone 27/10/2017
          base_controle_processos_sei_resumida(), "Saldo do estoque", icon = icon("list"),
          color = "red"
        )
        
      })
      
    }
    valueBoxOutput("totalEstoqueSEI")
  })
  
  output$TabelaEstoqueSEIDownloadCSV <- renderUI ({
    
    data <- estoques_sei
    
    data <- data %>%
      filter(
        data_filtro >= as.Date(input$opcao_data_estoque[1]),
        data_filtro <= as.Date(input$opcao_data_estoque[2])
      )
    
    if(input$visualizarConteudoSubunidades) {
      base_controle_estoque_sei_download <- reactive({
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
        
      })
      
      # para baixar as planilhas de controle de processos em .csv
      output$downloadTabelaEstoqueCSV <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.csv', sep='') 
        },
        content = function(file) {
          write.csv(base_controle_estoque_sei_download(), file, fileEncoding = "UTF-16LE",sep=';',dec=',', row.names=FALSE)
        }
      )
      
    }
    
    else {
      base_controle_estoque_sei_download <- reactive({
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
      })
      
      # para baixar as planilhas de controle de processos em .csv
      output$downloadTabelaEstoquesCSV <- downloadHandler(
        filename = function() { 
          paste("controle_processos_", input$selecao_orgao, '.csv', sep='') 
        },
        content = function(file) {
          write.csv(base_controle_estoque_sei_download(), file, fileEncoding = "UTF-16LE",sep=';',dec=',', row.names=FALSE)
        }
      )
      
    }
    
    downloadButton('downloadTabelaEstoqueCSV','Download em .csv')
  })
  
  
  output$TabelaEstoqueSEIDownloadXLSX <- renderUI ({
    
    data <- estoques_sei
    
    data <- data %>%
      filter(
        data_filtro >= as.Date(input$opcao_data_estoque[1]),
        data_filtro <= as.Date(input$opcao_data_estoque[2])
      )
    
    if(input$visualizarConteudoSubunidades) {
      base_controle_estoque_sei_download <- reactive({
        
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
        
      })
      
    }
    
    else {
      base_controle_estoque_sei_download <- reactive({
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
      })
    }
    
    # para baixar as planilhas de controle de processos em .xlsx
    output$downloadTabelaEstoqueXLSX <- downloadHandler(
      filename = function() { 
        paste("controle_processos_", input$selecao_orgao, '.xlsx', sep='') 
      },
      content = function(file) {
        write.xlsx(base_controle_estoque_sei_download(), file, row.names=FALSE)
      }
    )
    #downloadButton('downloadTabelaEstoqueXLSX','Download em .xlsx')
  })
  
  
  #logotipo do Processo Eletrônico
  output$logotipo_processo_eletronico <- renderUI({
    #tags$img(src= "//pepe.prefeitura.sp.gov.br/img/processo_eletronico_hor_branco.png", height='40px', width='130px')
    tags$img(src= "//pepe.prefeitura.sp.gov.br/img/pepe_white.svg", height='53px', width='140px')
  })
  
  #logotipo de SMIT
  output$logotipo_smit <- renderUI({
    tags$img(src= "//pepe.prefeitura.sp.gov.br/img/logo_smit.png", height='80px', width='80px')
    
  })
 
  
  ### funcao nova - exportacao xls ###
  output$downloadExp <- function(tipo) {
    
    data <- estoques_sei
    data <- data %>%
      filter(
        data_filtro >= as.Date(input$opcao_data_estoque[1]),
        data_filtro <= as.Date(input$opcao_data_estoque[2]) )
    
    if(input$visualizarConteudoSubunidades) {
      base_controle_estoque_sei_download <- reactive({
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao_atual == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade_atual), ]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[grep(input$opcao_unidade_estoque, data$unidade), ]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
        
      })
      
    }
    
    else {
      base_controle_estoque_sei_download <- reactive({
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao == "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo == "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque == "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        if(input$selecao_orgao != "Todos" && input$opcao_unidade_estoque != "Todas" && input$opcao_estoque_tipo_processo != "Todos") {
          data <- data[data$orgao == input$selecao_orgao,]
          data <- data[data$unidade == input$opcao_unidade_estoque,]
          data <- data[data$tipo_processo == input$opcao_estoque_tipo_processo,]
        }
        
        data
      })
    }
    
    # para baixar as planilhas de controle de processos em .xlsx
    downloadHandler(
      filename = function() { 
        paste("controle_processos_", input$selecao_orgao, '.xlsx', sep='') 
      },
      content = function(file) {
        write.xlsx(base_controle_estoque_sei_download(), file, row.names=FALSE)
      }
    )
  }
  ### fim - exportacao xls ###  
  
  # Filtro de unidades em indicadores gerais
  base_filtro_unidades_indicadores_gerais <- reactive({
    vetor <- vetor_unidade_indicadores_gerais
    if(input$selecao_orgao != "Todos") {
      vetor <- subset(vetor, controle_processos_sei.orgao_atual == input$selecao_orgao)
    }
    base <- vetor$controle_processos_sei.unidade
    base <- base[ base != '*'] 
    
    return(base)
  })
  
  output$filtro_unidades_indicadores_gerais <- renderUI({
    
    selectInput("selecao_unidade","Unidade:", c("Todas", as.list(sort(base_filtro_unidades_indicadores_gerais(),decreasing=F))))
  })
  
  # erika - função para controlar a tabela de tipos de processos com o tempo de vida mediano
  output$tipos_processos_mediana <- DT::renderDataTable(DT::datatable({
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tipo_processo_mediana
      data <- tabela_tipo_processo_mediana[order(-tabela_tipo_processo_mediana$mediana),]
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- tabela_tempo_vida %>% filter(unidade == input$selecao_unidade) %>% group_by(tipo_processo) %>% summarise(mediana = median(tempo_vida))
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao) %>% group_by(tipo_processo) %>% summarise(mediana = median(tempo_vida))
    }
    else {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao & unidade == input$selecao_unidade) %>% group_by(tipo_processo) %>% summarise(mediana = median(tempo_vida))
    }
    
    if(input$selecao_orgao != "Todos" | input$selecao_unidade != "Todas") {
      data <- inner_join(data, tabela_tipo_processo_mediana, by="tipo_processo")
      data <- data[order((data$mediana.x - data$mediana.y)),]
      desvio <- ifelse(((data$mediana.x - data$mediana.y) > 0), paste("Atrasado ", abs(data$mediana.x - data$mediana.y), " dias"), ifelse((data$mediana.x < as.numeric(quantile(tabela_tempo_vida$tempo_vida,0.25)[1])), paste("Adiantado"), paste("Dentro do esperado")))
      data <- data.frame(data[,-3], desvio)
      #data[data=='Adiantado  0  dias'] <- 'Na média'
      colnames(data)[1] <- ("Tipo de processo")
      colnames(data)[2] <- ("Tempo de vida mediano")
      colnames(data)[3] <- ("Diferença com a mediana geral")
    } else {
      colnames(data)[1] <- ("Tipo de processo")
      colnames(data)[2] <- ("Tempo de vida mediano")
    }
    data
  },
  extensions = 'Buttons', escape = TRUE,
  rownames = FALSE, options = list(
    dom = 'Bfrtip',
    #scrollX = T,
    buttons = 
      list(list(extend = 'copy',text = 'Copiar'),
           list(extend = 'print',text = 'Imprimir'),
           list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')
      ), pageLength = nrow(data)
  )
  ), server = FALSE)
  
  # rodrigo - funcoes de data
  som <- function(x) {
    as.Date(format(x, "%Y-%m-01"))
  }
  
  end_day = som(Sys.Date()) - 1
  
  # erika - função para controlar a tabela de usuários, tipos de processos e dias na unidade
  output$usuario_tempo <- DT::renderDataTable(DT::datatable({
    colnames(tabela_tempo_usuario)[1] <- ("orgao")
    tabela_tempo_usuario <- tabela_tempo_usuario %>%  filter(!is.na(atribuido))
    
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_usuario %>% group_by(atribuido, tipo_processo) %>% summarise(mediana = median(dias_unidade))
    }
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- tabela_tempo_usuario %>% filter(unidade_atual == input$selecao_unidade) %>% group_by(atribuido, tipo_processo) %>% summarise(mediana = median(dias_unidade))
    }
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_usuario %>% filter(orgao == input$selecao_orgao) %>% group_by(atribuido, tipo_processo) %>% summarise(mediana = median(dias_unidade))
    }
    else {
      data <- tabela_tempo_usuario %>% filter(orgao == input$selecao_orgao & unidade_atual == input$selecao_unidade) %>% group_by(atribuido, tipo_processo) %>% summarise(mediana = median(dias_unidade))
    }
    
    if(input$selecao_orgao != "Todos" | input$selecao_unidade != "Todas") {
      data <- inner_join(data, tabela_tempo_mediana, by="tipo_processo")
      #data <- data[order((data$mediana.x - data$mediana.y)),]
      
      desvio <- ifelse(((data$mediana.x - data$mediana.y) > 0), paste("Atrasado ", abs(data$mediana.x - data$mediana.y), " dias"), ifelse((data$mediana.x < as.numeric(quantile(tabela_tempo_vida$tempo_vida,0.25)[1])), paste("Adiantado"), paste("Dentro do esperado")))
      data <- data.frame(data[,-4], desvio)
      data <- subset(data, atribuido != " ")
      #data[data=='Adiantado  0  dias'] <- 'Na média'
      colnames(data)[1] <- ("Usuário")
      colnames(data)[2] <- ("Tipo de processo")
      colnames(data)[3] <- ("Tempo mediano")
      colnames(data)[4] <- ("Diferença em relação ao tempo mediano")
    } else {
      data <- data[order(-data$mediana),]
      colnames(data)[1] <- ("Usuário")
      colnames(data)[2] <- ("Tipo de processo")
      colnames(data)[3] <- ("Tempo de vida mediano")
    }
    
    data
    
  },
  extensions = 'Buttons', rownames = FALSE, options = list(
    dom = 'Bfrtip',
    buttons = 
      list(list(extend = 'copy',text = 'Copiar'),
           list(extend = 'print',text = 'Imprimir'),
           list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')
      )
  )
  ), server = FALSE)
  
  base_distribuicao_tempo_vida <-  reactive({
    
    #colnames(tabela_tempo_vida)[4] <- ("Tempo de vida")
    
    if(input$selecao_orgao == "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_vida
    }
    
    else if(input$selecao_orgao == "Todos" && input$selecao_unidade != "Todas") {
      data <- tabela_tempo_vida %>% filter(unidade == input$selecao_unidade)
    }
    
    else if(input$selecao_orgao != "Todos" && input$selecao_unidade == "Todas") {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao)
    }
    
    else {
      data <- tabela_tempo_vida %>% filter(orgao == input$selecao_orgao & unidade == input$selecao_unidade)
    }
    
    data
    
  })
  
  output$grafico_distribuicao_tempo_vida = renderPlotly({
    p <- plot_ly(base_distribuicao_tempo_vida(), y = ~tempo_vida, color = ~tipo_processo, type="box", xaxis = list(showticklabels = FALSE, tickangle = 45, tickfont = list(size = 8)), showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = 'Tempo de Vida'))
    p
  })
}