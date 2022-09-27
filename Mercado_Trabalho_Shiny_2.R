#   rm(list = ls())  # Selectize para procurar municípios
# Carregamento e preparo ----
# EXEMPLOS
# http://shinyepe.brazilsouth.cloudapp.azure.com:3838/pdgd/
# http://shinyepe.brazilsouth.cloudapp.azure.com:3838/anuario/
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(zoo)
# library(shinycssloaders) ............. INSTALAR
options(editor = "notepad")

# Estoque_Novo_Caged_Consolidacao_Saldo_202004
df_wide <-
  readRDS("./data/Estoque_Novo_Caged_Consolidacao_Saldo_20220926.RDS")

df_long_cnae <-
  pivot_longer(
    df_wide,
    values_to = "estoque",names_to = "mes",
    cols = c("2020.01", "2020.02", "2020.03", "2020.04", "2020.05", 
             "2020.06", "2020.07", "2020.08", "2020.09", "2020.10", "2020.11", 
             "2020.12", "2021.01", "2021.02", "2021.03", "2021.04", 
             "2021.05", "2021.06", "2021.07", "2021.08", "2021.09", "2021.10", 
             "2021.11", "2021.12", "2022.01"))

df_long_cnae <-
  df_long_cnae[, c(
    "subclasse_Cod",
    "subclasse",
    "classe",
    "grupo",
    "divisão",
    "seção",
    "Município",
    "UF_sigla",
    "mes",
    "estoque"
  )]

df_long_cnae <- df_long_cnae[df_long_cnae$estoque > 0,]


# Novo_Caged_microdados_movimentacao_secoes_CNAE23_B_C.RDATA
movimentacao_CNAE_B_C <-
  readRDS(file = "./data/Novo_Caged_microdados_movimentacao_secoes_CNAE23_B_C_202205.Rds")
movimentacao_CNAE_B_C$competência <- movimentacao_CNAE_B_C$competência |> lubridate::as_date()|> zoo::as.yearmon()

df_movimentacao <-
  movimentacao_CNAE_B_C[, c(
    "competênciamov","município","subclasse","saldomovimentação","cbo2002ocupação","horascontratuais","salário","indicadoraprendiz","indicadordeexclusão","trimestre","semestre","competência","subclasse.descrição","classe.descrição","grupo.descrição","divisão.descrição","seção.descrição","grande.Grupamento","UF_sigla","Município","NO_CBO","tipomovimentação.Descrição","graudeinstrução.Descrição","tamestabjan.Descrição","tipoempregador.Descrição","tipoestabelecimento.Descrição")]
colnames(df_movimentacao) <-
  stringi::stri_trans_general(colnames(df_movimentacao), id = "Latin-ASCII")
colnames(df_movimentacao) <-
  c("competenciamov", "municipio", "subclasse_Cod", "saldomovimentacao", "cbo2002ocupacao", "horascontratuais", "salario", "indicadoraprendiz", "indicadordeexclusao", "trimestre", "semestre", "competencia", "subclasse", "classe", "grupo", "divisao", "secao", "grande.Grupamento", "UF_sigla", "Municipio", "NO_CBO", "tipomovimentacao", "graudeinstrucao", "tamestabjan", "tipoempregador", "tipoestabelecimento")
df_movimentacao$trimestre <- as.character(df_movimentacao$trimestre)
df_movimentacao$semestre <- as.character(df_movimentacao$semestre)

# _____ Variáveis de sessão ----  
UF <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")  
TRIMESTRES <- sort(unique(df_movimentacao$trimestre))
MUNICIPIOS <- "Todos"
# _______________________________________________ ----
# USER INTERFACE ----
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Emprego formal no Setor Mineral: Estoques, Salários e Saldos de movimentação"),
  dashboardSidebar(width = 100,
    sidebarMenu(menuItem("Estoques", tabName = "Estoques"),
                menuItem("Variações", tabName = "Variacoes"),
                menuItem("Salários", tabName = "Salarios"),
                menuItem("Síntese", tabName = "sinteseSetorial"))),
  dashboardBody(
     tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
# _____ tab ESTOQUES ---- 
      tabItem(tabName = "Estoques",
              sidebarLayout(
                sidebarPanel = sidebarPanel(fluid = T,
                  fluidRow(#width = 1,
# __________ Mês airDatepickerInput ----                    
      # airMonthpickerInput(inputId = "id.airMonthpickerInput",
      #                     label = "Mês", multiple = T,
      #                     value = movimentacao_CNAE_B_C$competência  ,
      #                     maxDate = tail(movimentacao_CNAE_B_C$competência,1),
      #                     minDate = head(movimentacao_CNAE_B_C$competência,1),
      #                     view = "months",
      #                     minView = "months",
      #                     dateFormat = "yyyy-mm")
),
              fluidRow(
                    column(width = 4,
                           box(title = "UF", width = NULL, collapsible = T, collapsed = T, 
# __________  Uf CheckboxGroupInput ----                               
                               checkboxGroupInput(width = 2,
                                                  inputId = "id.UfCheckboxGroupInput", label = "", 
                                                  choices = UF, selected = c("MG")))),
                    column(width = 8,
                           box(title = "municípios", width = NULL, collapsible = T, collapsed = T, 
                               checkboxGroupInput(inputId = "id.MunicipioCheckboxGroupInput",label = "",
                                                  choices = MUNICIPIOS,selected = c("")))))),
                mainPanel = mainPanel(#width = 10,
# ________ FluidRow 1 ----  
              fluidRow(
                box(title = "Admissões", textOutput(outputId = "id.Admissoes"), width = 2),
                box(title = "Desligamentos", textOutput(outputId = "id.Desligamentos"), width = 2),
                box(title = "Saldo", textOutput(outputId = "id.Saldo"), width = 2)),
# ________ FluidRow 2 ----
          fluidRow(
            column(offset = 1,width = 6,
                   reactableOutput(outputId = "id.TabelaCNAE"))), #),
# ________ FluidRow 3 ----
          fluidRow(
            column(offset = 0, width = 10, 
                   plotOutput(outputId = "id.PlotCNAEGrupos")))))),
# _____ tab VARIAÇÕES ----      
      tabItem(tabName = "Variacoes",
              fluidRow(column(width = 12,h2("Variações")))),
# _____ tab SALÁRIOS ----
      tabItem(tabName = "Salarios",
            fluidRow(column(width = 12,h2("Salários")))),
# _____ tab SÍNTESE SETORIAL ----
      tabItem(tabName = "sinteseSetorial",
              fluidRow(column(width = 12,h2("Síntese Setorial"))))))
)

# SERVER ----

server <- function(input, output, session) {

  MUNICIPIOS <- observe(df_long_cnae[df_long_cnae$UF_sigla %in% "id.UfCheckboxGroupInput", ])
# _____ (info)Box ----
  output$id.Admissoes <- renderText({
    
    id.Admissoes <-
      length(df_movimentacao[df_movimentacao$trimestre %in% input$id.TrimestreSliderTextInput &
                                   df_movimentacao$tipomovimentacao %in%
                                     c(20, 97, 10, 35, 25), ]$tipomovimentacao)})
  
  output$id.Desligamentos <- renderText({
    id.Desligamentos <-
      length(df_movimentacao[df_movimentacao$trimestre %in% input$id.TrimestreSliderTextInput &
                                  !df_movimentacao$tipomovimentacao %in%
                                     c(20, 97, 10, 35, 25), ]$tipomovimentacao)})
  
  output$id.Saldo <- renderText({
    id.Saldo <-
      length(df_movimentacao[df_movimentacao$trimestre %in% input$id.TrimestreSliderTextInput &
                                   df_movimentacao$tipomovimentacao %in%
                                     c(20, 97, 10, 35, 25), ]$tipomovimentacao) -
      length(df_movimentacao[df_movimentacao$trimestre %in% input$id.TrimestreSliderTextInput &
                                  !df_movimentacao$tipomovimentacao %in%
                                     c(20, 97, 10, 35, 25), ]$tipomovimentacao)})
# _____ Reactable CNAE ----
  output$id.TabelaCNAE <- renderReactable({
    
    # considerar uma formma de executar apenas 1 vez (tmb consta em # _____ Plot CNAE Grupos) 
  df_long <- df_long_cnae[df_long_cnae$trimestre %in% input$id.TrimestreSliderTextInput,]
    
    reactable(compact = T, columns = list(secao = colDef(name = "Nível cnae 2.0"),estoque = colDef(name = "Estoque")),highlight = T,data = summarise(group_by(df_long, secao), "estoque" = sum(estoque)),details = function(index) {
         df_divisao <- df_long[df_long$secao == sort(unique(df_long$secao))[index], c("divisao", "grupo", "classe", "subclasse", "estoque")]
         htmltools::div(reactable(compact = T, columns = list(divisao = colDef(name = ""), estoque = colDef(name = "")),highlight = T,data = summarise(group_by(df_divisao, divisao), "estoque" = sum(estoque)),details = function(index) {
                    df_grupo <- df_divisao[df_divisao$divisao == sort(unique(df_divisao$divisao))[index], c("grupo", "classe", "subclasse", "estoque")]
                    htmltools::div(reactable(compact = T, columns = list(grupo = colDef(name = ""), estoque = colDef(name = "")),highlight = T,data = summarise(group_by(df_grupo, grupo), "estoque" = sum(estoque)),details = function(index) {
                              df_classe <- df_grupo[df_grupo$grupo == sort(unique(df_grupo$grupo))[index], c("grupo", "classe", "subclasse", "estoque")]
                              htmltools::div(reactable(compact = T, columns = list(classe = colDef(name = ""),estoque = colDef(name = "")),highlight = T,data = summarise(group_by(df_classe, classe), "estoque" = sum(estoque)),details = function(index) {
                                        df_subclasse <- df_classe[df_classe$classe == sort(unique(df_classe$classe))[index], c("classe", "subclasse", "estoque")]
                                        htmltools::div(reactable(compact = T, columns = list(subclasse = colDef(name = ""),estoque = colDef(name = "")),highlight = T,data = summarise(group_by(df_subclasse, subclasse),"estoque" = sum(estoque))))}))}))}))})})

# _____ Plot CNAE Grupos ----

    output$id.PlotCNAEGrupos <- renderPlot({
    # considerar uma formma de executar apenas 1 vez (tmb consta em # _____ Reactable CNAE) 
  df_long <- df_movimentacao[df_movimentacao$trimestre %in% input$id.TrimestreSliderTextInput,]
    
  df_divisao <- summarise(group_by(df_long, divisao), "saldo" = sum(saldomovimentacao))
    
    ggplot(data = df_divisao) +
      geom_col(mapping = aes(x = as.factor(divisao), y = saldo), fill = "#FFCC00") +
      xlab("") + ylab("") +
      scale_x_discrete(labels = str_wrap(string = as.factor(unique(df_divisao$divisao)), width = 40)) +
      theme(legend.position = "none") +
      coord_flip()})
  }


shinyApp(ui, server)









