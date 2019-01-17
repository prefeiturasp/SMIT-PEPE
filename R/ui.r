#########################################################
#  PEPE - Painel de Evolução dos Processos Eletrônicos  #
#                                                       #
#  ui.r - Montagem de interface pelo shinydashboard     #
#########################################################

dashboardPage( 
  # Titulo
  dashboardHeader(title = uiOutput("logotipo_processo_eletronico")),
  
  dashboardSidebar(
    sidebarMenu(
      # Itens do Menu
      uiOutput("orgaos_sei"), #seleciona o órgão a ser analisado
      menuItem("Indicadores gerais", tabName = "dashboard", icon = icon("bar-chart")),
      menuItem("Evolução do projeto", tabName = "widgets", icon = icon("line-chart")),
      menuItem("Controle de estoques", tabName = "estoques_sei", icon = icon("inbox")),
      menuItem("Análise de dados", tabName = "dados_sei", icon = icon("area-chart")),
	  # Link para o Endereço de logout
      menuItem("Sair do Painel", icon = icon("sign-out"), href = paste("logout.php?token=",""), newtab = FALSE),
      uiOutput("logotipo_smit", align="center", style = "padding-top:100px;"),
      
      # Explicação através de tooltip para os dados apresentados
      bsTooltip("processosBox", "É o número de processos não encerrados, neste momento, no órgão selecionado.", placement = "top", trigger = "hover", options = NULL),
      bsTooltip("usuariosBox", "Total de usuários com permissão de acesso ao SEI no órgão selecionado.", placement = "top", trigger = "hover", options = NULL),
      bsTooltip("tempomedianoBox", "Tempo que um processo dura, em média, no órgão selecionado.", placement = "top", trigger = "hover", options = NULL),
      bsTooltip("folhasBox", "Estimativa do número de folhas economizadas com base na quantidade de documentos gerados em meio eletrônico.", placement = "top", trigger = "hover", options = NULL),

      bsTooltip("rkg_semanal", "O Ranking Semanal é calculado com base na última semana completa.<br/><br/>Os rankings são calculados pela proporção entre os processos gerados em meio eletrônico e os processos gerados em papel.<br/><br/>Para constar no Ranking, o órgão precisa ter autuado no mínimo 5 processos na última semana.", placement = "left", trigger = "hover", options = NULL),
      bsTooltip("rkg_mensal", "O Ranking Mensal é calculado com base no último mês completo.<br/><br/>Os rankings são calculados pela proporção entre os processos gerados em meio eletrônico e os processos gerados em papel.<br/><br/>Para constar no Ranking, o órgão precisa ter autuado no mínimo 30 processos no último mês.", placement = "left", trigger = "hover", options = NULL),
      bsTooltip("evol_semanal", "Utilize a barra inferior para filtrar as datas exibidas no relatório.<br/><br/>Este ranking exibe dados até a última semana completa.", placement = "top", trigger = "hover", options = NULL),
      bsTooltip("evol_mensal", "Utilize a barra inferior para filtrar as datas exibidas no relatório.<br/><br/>Este ranking exibe dados em tempo real (com um atraso de até 24h).", placement = "top", trigger = "hover", options = NULL),

      bsTooltip("tempo_de_vida_tipo", "Tempo de vida mediano: É o tempo médio, em dias, da grande maioria de processos de um determinado tipo.<br/><br/>Diferença com a mediana geral: É a comparação dos processos do órgão/unidade selecionado em relação com a mediana, para o mesmo tipo de processo, de toda a prefeitura.", placement = "top", trigger = "hover", options = NULL),
      bsTooltip("grafico_estoques", "As barras azuis representam os processos que o órgão/unidade recebeu.<br/><br/> As barras verdes, os processos remetidos pelo órgão/unidade.<br/><br/> A linha vermelha representa a evolução do estoque de processos do órgão/unidade.", placement = "top", trigger = "hover", options = NULL)
    )
  ),
  
  # Paginas disponiveis (tabItems)
  dashboardBody(
    tabItems(
      # Indicadores Gerais
      tabItem(tabName = "dashboard",
              h2("Indicadores gerais"),
              uiOutput("filtro_unidades_indicadores_gerais"),
              fluidRow(
                
                # Dynamic valueBoxes
                valueBoxOutput("processosBox", width=3),
                #textInput("token", "Token"),
                valueBoxOutput("usuariosBox", width=3),
                valueBoxOutput("tempomedianoBox", width=3),
                valueBoxOutput("folhasBox", width=3),
                box(title = "Tipos de processos mais autuados",  collapsible = TRUE,
                    plotlyOutput("tipos_processos_mais_autuados", height = 500)
                ),
                box(title = "Evolução semanal",  collapsible = TRUE,
                    plotlyOutput('grafico_evolucao_semanal_sei', height = 500)
                ),
                box(title = "Tempo de vida por tipo de processo",  collapsible = TRUE, id='tempo_de_vida_tipo',
                    DT::dataTableOutput("tipos_processos_mediana")
                ),
                box(title = "Dias trabalhados em cada tipo de processos abertos por usuário",  collapsible = TRUE,
                    DT::dataTableOutput("usuario_tempo")
                )
              )
      ),
      
      # Evolução semanal
      tabItem(tabName = "widgets",
              h2("Evolução semanal do projeto"),
              fluidRow(
                box(width=7, title = "Evolução semanal",  collapsible = TRUE,
                    plotlyOutput('grafico_evolucao_projeto_semanal', height = 600), id="evol_semanal"
                )
                ,
                box(
                  width=5,
                  title = tagList(shiny::icon("trophy"), "Ranking Semanal de Migração"), solidHeader = TRUE,  icon = icon("bar-chart"), status = "warning",
                  collapsible = TRUE, DT::dataTableOutput("ranking_orgaos_semanal_output"), id="rkg_semanal"
                )
                
              ),
              h2("Evolução mensal do projeto"),
              fluidRow(
                box(width=7, title = "Mensal a partir de 01/2016",  collapsible = TRUE,
                    plotlyOutput('grafico_evolucao_projeto_mensal', height = "100%"), id="evol_mensal"
                )
                ,
                box(
                  width=5,
                  title = tagList(shiny::icon("trophy"), paste("Ranking Mensal -",format(as.Date(format(Sys.Date(), "%Y-%m-01"))-1, "%b/%y") )), solidHeader = TRUE,  icon = icon("bar-chart"), status = "warning",
                  DT::dataTableOutput("ranking_orgaos_mensal_output"), id="rkg_mensal"
                )
                #,column(4, dateRangeInput("data_selecionada_diaria", "Mês a ser analisado:", format = "dd/mm/yyyy", language = "pt-BR"))
              )
      ),
      
      # Estoques
      tabItem(tabName = "estoques_sei",
              h2("Controle de estoques"),
              fluidRow(
                column(3,
                       uiOutput("unidades_estoques")
                ),
                column(3,
                       selectInput("opcao_estoque_tipo_processo",
                                   "Tipo de processo:",
                                   c("Todos",sort(unique(as.character(estoques_sei$tipo_processo)),decreasing=F) ))
                ),
                
                column(3, 
                       dateRangeInput("opcao_data_estoque", "Intervalo de data a ser analisado:", format = "yy-mm-dd", language = "pt-BR", weekstart = 0, start = (cut(Sys.Date(),"month")), end = (Sys.Date()))
                ),
                column(3,
                       checkboxInput("visualizarConteudoSubunidades", "Visualizar processos de subunidades?", value = FALSE)
                )
              ),
              fluidRow(
                tags$div(
                  tags$br(),
                  tags$hr()
                )
              ),
              fluidRow(
                box(title = "Entradas e saídas de processos eletrônicos", uiOutput("GraficoEstoquesSEI"),
                    collapsible = TRUE, width=12, id="grafico_estoques")
              ),
              fluidRow(
                box(title = "Entrada de processos nas unidades", uiOutput("TabelaContProcessosSEIResumida"),
                    collapsible = TRUE, width = 12)
              )      
      ),
      
      # Análise de Dados Estatísticos
      tabItem(tabName = "dados_sei",
              h2("Análise de dados"),
              fluidRow(
                box(title = "Distribuição do tempo de vida (em dias) por tipo de processo",  collapsible = TRUE, plotlyOutput("grafico_distribuicao_tempo_vida", height = 600), width = 12, height = 800)
              )
          )  
    ),
    
	# Icone do Painel
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
     
    tags$head(tags$script(src="//pepe.prefeitura.sp.gov.br/js/script.js")), # Se necessário, alterar para o endereço da instalação
    
    tags$head(tags$style(HTML('
                              .wrapper { filter: alpha(opacity=0); opacity: 0.0; }
                              td { font-size: 13px; }
                              .shiny-output-error { visibility: hidden; }
                              .shiny-output-error:before { visibility: hidden; }

                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #076C9E;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #076C9E;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #076C9E;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #464646;
                              color: #FFFFFF;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #676767;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #464646;
                              color: #FFFFFF;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #076c9d;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #676767;
                              }

                              .overloading {
   position    : fixed;
   z-index     : 1; /* or higher if necessary */
   top         : 0;
   left        : 0;
   overflow    : hidden;
   text-indent : 100%;
   font-size   : 0;
   background  : #000;
}

.loader {
  position: fixed;
  z-index: 9999;
  overflow: show;
  margin: auto;
  top: 0;
  left: 0;
  bottom: 0;
  right: 0;
  border: 16px solid #f3f3f3; /* Light grey */
  border-top: 16px solid #3498db; /* Blue */
  border-radius: 50%;
  width: 120px;
  height: 120px;
  animation: spin 2s linear infinite;
}

@keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
}
                              ')))
      
  ),
  mainPanel(useShinyjs(), titlePanel("[SEI] Painel Gerencial"), 
            extendShinyjs(text = erro_login, functions = c("navigate"))
  )

)
