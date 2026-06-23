library(jsonlite)
library(purrr)
library(DBI)
library(RMySQL)
library(httr)
library(lubridate)

#### FUNÇÃO PARA AUTENTICAÇÃO ####
get_token <- function(user, password) {
  url_auth <- "https://api-autenticacao.grupoiagsaude.com.br/login"
  proxy <- "http://proxy.prodemge.gov.br:8080"
  
  body <- list(
    userName = user,
    password = password,
    origin   = "API_DRG"
  )
  
  resp <- POST(
    url_auth,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers("Content-Type" = "application/json"),
    use_proxy(proxy)
  )
}


#### FUNÇÃO PARA EXTRAÇÃO DA API DO DRG ####
get_internacao <- function(data_u, token, max_pages = 100) {
  url <- "https://apidrg-exporta-assistencial.drgbrasil.com.br/search"
  proxy <- "http://proxy.prodemge.gov.br:8080"
  all_dados <- list()
  
  for (p in 1:max_pages) {
    body <- list(
      dataUltimaAlteracao = data_u,
      page = p
    )
    
    resp <- POST(
      url,
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", token)
      ),
      use_proxy(proxy)
    )
    
    if (http_status(resp)$category != "Success") {
      stop(paste("Erro na requisição página", p, ":", http_status(resp)$message))
    }
    
    dados <- content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
    
    # Se não veio nada, parar o loop
    if (length(dados) == 0) break
    
    all_dados[[p]] <- dados
  }
  
  return(all_dados)
}

hoje <- Sys.Date()

#### ETAPA 1: CONECTANDO AO BANCO DE DADOS MYSQL (ci_drg) ####
tryCatch(
  expr = {
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = "drg",
                     host="00.00.00.00",
                     user="",
                     password="")
  },
  error = function(e){
    message("ERRO NA ETAPA 1 - CONEXAO COM O BANCO DE DADOS")
    message(conditionMessage(e))
    quit(status = 1)  # Retorna código de erro para o .bat capturar
  }
)



#### ETAPA 2: QUERY PARA VERIFICAR ULTIMA IMPORTAÇÃO BEM SUCEDIDA NO BANCO ####
tryCatch(
  expr = {
    query <- sprintf(
      "SELECT data_codificacao
        FROM atualizacao
        WHERE status='1.0'
        ORDER BY data_codificacao DESC
        LIMIT 1")
    
    ultima_import <- dbGetQuery(con, query)$data_codificacao
    ultima_import <- as.Date(ultima_import)
  },
  error = function(e){
    message("ERRO NA ETAPA 2 - BUSCA DA ULTIMA IMPORTACAO BEM SUCEDIDA NO BANCO DE DADOS")
    message(conditionMessage(e))
    quit(status = 1)
  }
)

tryCatch(
  expr = {
    data_inicio <- ultima_import + 1
    data_fim    <- Sys.Date() - 1
    datas       <- seq(data_inicio, data_fim, by = "day")
    
    response <- list(internacao = list())  # Inicializa com uma lista internacao vazia
  },
  error = function(e){
    message("ERRO NA ETAPA 2 - A ULTIMA ATUALIZACAO BEM SUCEDIDA FOI ONTEM. NAO HA DATAS A SEREM PROCESSADAS.")
    message(conditionMessage(e))
    quit(status = 1)
  }
)


####  ETAPA 3: IMPORTACAO DE DADOS DA API DO DRG PARA RESPONSE - armazena objeto da API em JSON e paresea em Large list ####
tryCatch(
  expr = {
    for (dia in format(datas, "%Y-%m-%d")) {
      # 1. Autenticar
      token <- get_token("981-exporta", "SwGMKRsS")
      
      #2. Importar codificações do dia
      cat("Buscando:", dia, "\n")
      
      resp <- get_internacao(dia, token)
      resp <- unlist(lapply(resp, function(pagina) pagina$items), recursive = FALSE)
      
      if (!is.null(resp) && length(resp) > 0) {
        # Acrescenta os elementos dentro da lista internacao
        response <- c(response, resp)
      }
    }
    
    cat("Total de internacoes:", length(response), "\n")
  },
  error = function(e){
    # Informa na tabela atualizacao do ci_drg que houve erro para as datas buscadas
    df_atualizacao <- data.frame(
    data_codificacao = datas,
    data_importacao  = hoje,
    status           = 0,
    mensagem         = "Erro na importacao de dados da API do DRG-Brasi"
    )
    
    dbWriteTable(con, "atualizacao", df_atualizacao, append = TRUE, row.names = FALSE)
    
    message("ERRO NA ETAPA 3 - IMPORTACAO DOS DADOS DA API DO DRG-BRASIL")
    message(conditionMessage(e))
    quit(status = 1)
  }
)



####  ETAPA 4: LOOP PARA EXPORTACAO DO RESPONSE PARA O BANCO DE DADOS ####
tryCatch(
  expr = {
    for (index in 1:length(response)){
      
      #log
      #print("++++++++++++++++++++++++++++++++")
      print(sprintf("LOOP internacao(id) - %d (%d)", response[[index]]$id, index))
      
      
      #Fazer varredura de lista dentro do condicaoAdquirida LISTA:
      for( indexcondicaoAdquirida in 1:length(pluck(response,index,"condicaoAdquirida"))){
        
        #Fazer o vinculo condicaoAdquirida internacao
        #print("            ++++++++++++++++++++")
        #print(sprintf("inserindo ft_internacao_condicaoAdquirida - %s", response[[index]]$condicaoAdquirida[[indexcondicaoAdquirida]]$dataInicial))
        
        condicaoAdquiridaCOD <- pluck(response, index,"condicaoAdquirida",indexcondicaoAdquirida,"codigo", .default = NA)
      }
      
      
      
      #criar um novo DF para internacao:
      internacaoDf <- data.frame(
        id=pluck(response,  index, "id", .default = NA),
        situacao=pluck(response,  index, "situacao", .default = NA),
        caraterInternacao=pluck(response,  index, "caraterInternacao", .default = NA),
        numeroOperadora=pluck(response,  index, "numeroOperadora", .default = NA),
        numeroRegistro=pluck(response,  index, "numeroRegistro", .default = NA),
        numeroAtendimento=pluck(response,  index, "numeroAtendimento", .default = NA),
        numeroAutorizacao=pluck(response,  index, "numeroAutorizacao", .default = NA),
        dataInternacao=pluck(response,  index, "dataInternacao", .default = NA),
        dataAlta=pluck(response,  index, "dataAlta", .default = NA),
        condicaoAlta=pluck(response,  index, "condicaoAlta", .default = NA),
        internadoOutrasVezes=pluck(response,  index, "internadoOutrasVezes", .default = NA),
        hospitalInternacaoAnterior=pluck(response,  index, "hospitalInternacaoAnterior", .default = NA),
        reinternacao=pluck(response,  index, "reinternacao", .default = NA),
        recaida=pluck(response,  index, "recaida", .default = NA),
        origemReadmissao30Dias=pluck(response,  index, "origemReadmissao30Dias", .default = NA),
        origemRecaida30Dias=pluck(response,  index, "origemRecaida30Dias", .default = NA),
        dataPrevistaAlta=pluck(response,  index, "dataPrevistaAlta", .default = NA),
        permanenciaPrevistaNaInternacao=pluck(response,  index, "permanenciaPrevistaNaInternacao", .default = NA),
        permanenciaPrevistaNaAlta=pluck(response,  index, "permanenciaPrevistaNaAlta", .default = NA),
        permanenciaReal=pluck(response,  index, "permanenciaReal", .default = NA),
        percentil=pluck(response,  index, "percentil", .default = NA),
        procedencia=pluck(response,  index, "procedencia", .default = NA),
        ventilacaoMecanica=pluck(response,  index, "ventilacaoMecanica", .default = NA),
        modalidadeInternacao=pluck(response,  index, "modalidadeInternacao", .default = NA),
        dataCadastro=pluck(response,  index, "dataCadastro", .default = NA),
        usuarioCadastro=pluck(response,  index, "usuarioCadastro", .default = NA),
        dataCadastroAlta=pluck(response,  index, "dataCadastroAlta", .default = NA),
        usuarioCadastroAlta=pluck(response,  index, "usuarioCadastroAlta", .default = NA),
        dataUltimaAlteracao=pluck(response,  index, "dataUltimaAlteracao", .default = NA),
        usuarioUltimaAlteracao=pluck(response,  index, "usuarioUltimaAlteracao", .default = NA),
        leito=pluck(response,  index, "leito", .default = NA),
        condicaoAdquiridaGrave=pluck(response,  index, "condicaoAdquiridaGrave", .default = NA),
        
        #campos Nested
        instituicao=pluck(response,  index, "instituicao", "codigo", .default = NA),
        hospital=pluck(response,  index, "hospital", "codigo", .default = NA),
        beneficiario=pluck(response,  index, "beneficiario", "codigoPaciente", .default = NA),
        cidPrincipal=pluck(response,  index, "cidPrincipal", "codigo", .default = NA),
        drgBrasilRefinado_cod=pluck(response,  index, "drgBrasilRefinado", "codigo", .default = NA),
        drgBrasilRefinado_peso=pluck(response,  index, "drgBrasilRefinado", "peso", .default = NA),
        drgAdmissional=pluck(response,  index, "drgAdmissional", "codigo", .default = NA),
        condicaoAdquirida= condicaoAdquiridaCOD,
        
        #variaveis
        variaveisCaGrave=pluck(response,  index, "variaveis", "caGrave", .default = NA),
        variaveisGAP=pluck(response,  index, "variaveis", "gerenciavelAtencaoPrimaria", .default = NA),
        variaveisGE=pluck(response,  index, "variaveis", "gerenciavelEmergencia", .default = NA)
      )
      #print("parte1")
      
      #### DELETANDO INTERNAÇÕES EXISTENTES ####
      
      #Deletando ft_internacao_procedimento_medico
      if(dbExistsTable(con, "ft_internacao_procedimento_medico")){
        #log
        #print(sprintf("Deletando ft_internacao_procedimento_medico - %d", response[[index]]$id))
        
        #monta a query
        query <- sprintf(
          "DELETE
                FROM ft_internacao_procedimento_medico
                where idinternacao = %d",
          pluck(response,  index,"id", .default = -1)
          
        )
        
        #executa a query
        dbExecute(
          con,
          query
        )
      }
      
      
      
      #Deletando ft_internacao_medico
      if(dbExistsTable(con, "ft_internacao_medico")){
        #log
        #print(sprintf("Deletando ft_internacao_medico - %d", response[[index]]$id))
        
        #monta a query
        query <- sprintf(
          "DELETE
                FROM ft_internacao_medico
                where idinternacao = %d",
          pluck(response,  index,"id", .default = -1)
          
        )
        
        #executa a query
        dbExecute(
          con,
          query
        )
      }
      
      
      #Deletando cidSecundario da internação
      if(dbExistsTable(con, "ft_internacao_cidSecundario")){
        #log
        #print(sprintf("Deletando ft_internacao_cidSecundario - %d", response[[index]]$id))
        
        #monta a query
        query <- sprintf(
          "DELETE
                FROM ft_internacao_cidSecundario
                where idinternacao = %d",
          pluck(response,  index,"id", .default = -1)
          
        )
        
        #executa a query
        dbExecute(
          con,
          query
        )
      }
      
      
      # Deletando ft_internacao_analiseCritica
      if(dbExistsTable(con, "ft_internacao_analiseCritica")){
        #print(sprintf("Deletando ft_internacao_analiseCritica - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_analiseCritica WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      # Deletando ft_internacao_causaExterna
      if(dbExistsTable(con, "ft_internacao_causaExterna")){
        #print(sprintf("Deletando ft_internacao_causaExterna - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_causaExterna WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
    
      # Deletando ft_internacao_procedimento
      if(dbExistsTable(con, "ft_internacao_procedimento")){
        #print(sprintf("Deletando ft_internacao_procedimento - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_procedimento WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      # Deletando ft_internacao_cti
      if(dbExistsTable(con, "ft_internacao_cti")){
        #print(sprintf("Deletando ft_internacao_cti - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_cti WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      # Deletando ft_internacao_condicaoAdquirida
      if(dbExistsTable(con, "ft_internacao_condicaoAdquirida")){
        #print(sprintf("Deletando ft_internacao_condicaoAdquirida - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_condicaoAdquirida WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      # Deletando ft_internacao_sondaVesicalDeDemora
      if(dbExistsTable(con, "ft_internacao_sondaVesicalDeDemora")){
        #print(sprintf("Deletando ft_internacao_sondaVesicalDeDemora - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_sondaVesicalDeDemora WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      # Deletando ft_internacao_suporteVentilatorio
      if(dbExistsTable(con, "ft_internacao_suporteVentilatorio")){
        #print(sprintf("Deletando ft_internacao_suporteVentilatorio - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_suporteVentilatorio WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      #print("parte2")
      
      # Deletando ft_internacao_rn
      if(dbExistsTable(con, "ft_internacao_rn")){
        #print(sprintf("Deletando ft_internacao_rn - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_rn WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      
      # Deletando ft_internacao_altaAdministrativa
      if(dbExistsTable(con, "ft_internacao_altaAdministrativa")){
        #print(sprintf("Deletando ft_internacao_altaAdministrativa - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_altaAdministrativa WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      
      
      # Deletando ft_internacao_cateterVascularCentral
      if(dbExistsTable(con, "ft_internacao_cateterVascularCentral")){
        #print(sprintf("Deletando ft_internacao_cateterVascularCentral - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_cateterVascularCentral WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      
      
      # Deletando ft_internacao_dispositivoTerapeutico
      if(dbExistsTable(con, "ft_internacao_dispositivoTerapeutico")){
        #print(sprintf("Deletando ft_internacao_dispositivoTerapeutico - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_dispositivoTerapeutico WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      
      
      # Deletando ft_internacao_partoAdequado
      if(dbExistsTable(con, "ft_internacao_partoAdequado")){
        #print(sprintf("Deletando ft_internacao_partoAdequado - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_partoAdequado WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
      
      
      
      # Deletando ft_internacao_intercambio
      if(dbExistsTable(con, "ft_internacao_intercambio")){
        #print(sprintf("Deletando ft_internacao_intercambio - %d", response[[index]]$id))
        query <- sprintf(
          "DELETE FROM ft_internacao_intercambio WHERE idinternacao = %d",
          pluck(response,  index, "id", .default = -1)
        )
        dbExecute(con, query)
      }
      
    
      #print("parte3")
      
      #Deletando a internação
      if(dbExistsTable(con, "ft_internacao")){
        
        #log
        #print(sprintf("Deletando ft_internacao - %d", response[[index]]$id))
        
        #monta a query
        query <- sprintf(
          "DELETE
                FROM ft_internacao
                where id = %d",
          pluck(response,  index,"id", .default = -1)
          
        )
        
        #executa a query
        dbExecute(
          con,
          query
        )
      }
      
      #print("parte4")
      
      ######## INSERINDO NOVAS INTERNAÇÕES ########
      
      #### TEBELAS DIMENSÃO (NESTED) ####
      
      #### criando ou incluindo dim_instituicao ####
      instituicaoDf <- data.frame(
        codigo=pluck(response,  index, "instituicao", "codigo", .default = NA),
        nome=pluck(response,  index, "instituicao", "nome", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_instituicao")){
        query <- sprintf(
          "DELETE FROM dim_instituicao WHERE codigo = '%s'",
          pluck(response,  index,"instituicao", "codigo", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_instituicao", instituicaoDf, append = TRUE, row.names = FALSE)}
      
      
      #### criando ou incluindo dim_hospital ####
      hospital <- data.frame(
        codigo=pluck(response,  index, "hospital", "codigo", .default = NA),
        nome=pluck(response,  index, "hospital", "nome", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_hospital")){
        query <- sprintf(
          "DELETE FROM dim_hospital WHERE codigo = '%s'",
          pluck(response,  index,"hospital", "codigo", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_hospital", hospital, append = TRUE, row.names = FALSE)}
      
      
      #### criando ou incluindo dim_beneficiario ####
      beneficiario <- data.frame(
        codigoPaciente=pluck(response,  index, "beneficiario", "codigoPaciente", .default = NA),
        nome=pluck(response,  index, "beneficiario", "nome", .default = NA),
        dataNascimento=pluck(response,  index, "beneficiario", "dataNascimento", .default = NA),
        sexo=pluck(response,  index, "beneficiario", "sexo", .default = NA),
        recemNascido=pluck(response,  index, "beneficiario", "recemNascido", .default = NA),
        particular=pluck(response,  index, "beneficiario", "particular", .default = NA),
        idadeEmAnos=pluck(response,  index, "beneficiario", "idadeEmAnos", .default = NA),
        idadeEmMeses=pluck(response,  index, "beneficiario", "idadeEmMeses", .default = NA),
        idadeEmDias=pluck(response,  index, "beneficiario", "idadeEmDias", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_beneficiario")){
        query <- sprintf(
          "DELETE FROM dim_beneficiario WHERE codigoPaciente = '%s'",
          pluck(response,  index,"beneficiario", "codigoPaciente", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_beneficiario", beneficiario, append = TRUE, row.names = FALSE)}
      
      
      #### criando ou incluindo dim_cidPrincipal ####
      cidPrincipal <- data.frame(
        codigo=pluck(response,  index, "cidPrincipal", "codigo", .default = NA),
        descricao=pluck(response,  index, "cidPrincipal", "descricao", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_cidPrincipal")){
        query <- sprintf(
          "DELETE FROM dim_cidPrincipal WHERE codigo = '%s'",
          pluck(response,  index,"cidPrincipal", "codigo", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_cidPrincipal", cidPrincipal, append = TRUE, row.names = FALSE)}
      
      
      #### criando ou incluindo dim_drgBrasilRefinado ####
      drgBrasilRefinado <- data.frame(
        codigo=pluck(response,  index, "drgBrasilRefinado", "codigo", .default = NA),
        descricao=pluck(response,  index, "drgBrasilRefinado", "descricao", .default = NA),
        tipo=pluck(response,  index, "drgBrasilRefinado", "tipo", .default = NA),
        peso=pluck(response,  index, "drgBrasilRefinado", "peso", .default = NA),
        mdc_codigo=pluck(response,  index, "drgBrasilRefinado", "mdc", "codigo", .default = NA),
        mdc_descricao=pluck(response,  index, "drgBrasilRefinado", "mdc", "descricao", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_drgBrasilRefinado")){
        query <- sprintf(
          "DELETE FROM dim_drgBrasilRefinado WHERE codigo = '%s'",
          pluck(response,  index,"drgBrasilRefinado", "codigo", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_drgBrasilRefinado", drgBrasilRefinado, append = TRUE, row.names = FALSE)}
      
      
      #### criando ou incluindo dim_drgAdmissional ####
      drgAdmissional <- data.frame(
        codigo=pluck(response,  index, "drgAdmissional", "codigo", .default = NA),
        descricao=pluck(response,  index, "drgAdmissional", "descricao", .default = NA)
      )
      
      if(dbExistsTable(con, "dim_drgAdmissional")){
        query <- sprintf(
          "DELETE FROM dim_drgAdmissional WHERE codigo = '%s'",
          pluck(response,  index,"drgAdmissional", "codigo", .default = "")
        )
        dbExecute(con, query)
      }
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(con, "dim_drgAdmissional", drgAdmissional, append = TRUE, row.names = FALSE)}
      

      #### TABELA INTERNAÇÃO ####
      #print(sprintf("inserindo ft_internacao - %d", response[[index]]$id))
      
      if(!is.null(pluck(response,index,"id"))){dbWriteTable(
        con,
        "ft_internacao",
        internacaoDf,
        append = TRUE,
        row.names = FALSE
      )}
      
      #print("parte5")
      
      #### TABELAS FT AUXILIARES (LISTAS NO OBJETO INTERNACAO) ####
      
      #### cria ou inclui ft_internacao_medico ####
      # verificação se a propriedade médico existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"medico"))){
        #Fazer varredura de lista dentro do medico LISTA:
        for( indexMed in 1:length(pluck(response,index,"medico"))){
          
          #Fazer o vinculo medico internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_medico - %s", response[[index]]$medico[[indexMed]]$crm))
          
          medicoInternacao <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            nome=pluck(response,  index, "medico", indexMed, "nome", .default = NA),
            uf=pluck(response,  index, "medico", indexMed, "uf", .default = NA),
            crm=pluck(response,  index, "medico", indexMed, "crm", .default = NA),
            codigoEspecialidade=pluck(response,  index, "medico", indexMed, "codigoEspecialidade", .default = NA),
            especialidade=pluck(response,  index, "medico", indexMed, "especialidade", .default = NA),
            medicoResponsavel=pluck(response,  index, "medico", indexMed, "medicoResponsavel", .default = NA),
            tipoAtuacao=pluck(response,  index, "medico", indexMed, "tipoAtuacao", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_medico",
            medicoInternacao,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      ##print("parte 6.1")
      
      #### cria ou inclui ft_internacao_cidSecundario ####
      # verificação se a propriedade cidSecundario existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"cidSecundario"))){
        #Fazer varredura de lista dentro do cidSecundario LISTA:
        for( indexCidSec in 1:length(pluck(response,index,"cidSecundario"))){
    
          #Fazer o vinculo cidSecundario internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_cidSecundario - %s", response[[index]]$cidSecundario[[indexCidSec]]$codigo))
    
          cidSecundario <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            codigo=pluck(response, index,"cidSecundario",indexCidSec,"codigo", .default = NA),
            descricao=pluck(response,  index, "cidSecundario", indexCidSec, "descricao", .default = NA)
          )
    
          dbWriteTable(
            con,
            "ft_internacao_cidSecundario",
            cidSecundario,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      ##print("parte 6.2")
      
      ### cria ou inclui ft_internacao_procedimento ####
      # verificação se a propriedade procedimento existe na lista, para evitar erros
      if(length(pluck(response,index,"procedimento"))>0){
        #Fazer varredura de lista dentro do procedimento LISTA:
        for( indexProcedimento in 1:length(pluck(response,index,"procedimento"))){
    
          #Fazer o vinculo procedimento internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_procedimento - %s", response[[index]]$procedimento[[indexProcedimento]]$codigo))
    
          procedimento <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            codigo=pluck(response, index,"procedimento",indexProcedimento,"codigo"),
            nome=pluck(response,  index, "procedimento", indexProcedimento, "nome", .default = NA),
            dataExecucao=pluck(response,  index, "procedimento", indexProcedimento, "dataExecucao", .default = NA),
            dataExecucaoFinal=pluck(response,  index, "procedimento", indexProcedimento, "dataExecucaoFinal", .default = NA)
          )
    
          dbWriteTable(
            con,
            "ft_internacao_procedimento",
            procedimento,
            append = TRUE,
            row.names = FALSE
          )
          
          
          # verificação se a propriedade procedimento existe na lista, para evitar erros
          if(length(pluck(response,index,"procedimento",indexProcedimento,"medico")) > 0){
            #Fazer varredura de lista de medico dentro do procedimento :
            for( indexProcedimentoMedico in 1:length(pluck(response,index,"procedimento",indexProcedimento,"medico"))){
      
              #Fazer o vinculo procedimento e medico
              #print("            ++++++++++++++++++++")
              #print(sprintf("inserindo ft_internacao_procedimento_medico - %s", response[[index]]$procedimento[[indexProcedimento]]$medico[[indexProcedimentoMedico]]))
      
              procedimento_medico <- data.frame(
                idinternacao=pluck(response,  index, "id"),
                codigo_procedimento=pluck(response, index,"procedimento",indexProcedimento,"codigo"),
                medico_nome=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "nome", .default = NA),
                medico_uf=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "uf", .default = NA),
                medico_crm=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "crm", .default = NA),
                medico_codigoEspecialidade=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "codigoEspecialidade", .default = NA),
                medico_especialidade=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "especialidade", .default = NA),
                medico_tipoAtuacao=pluck(response, index,"procedimento",indexProcedimento, "medico", indexProcedimentoMedico, "tipoAtuacao", .default = NA)
              )
      
              dbWriteTable(
                con,
                "ft_internacao_procedimento_medico",
                procedimento_medico,
                append = TRUE,
                row.names = FALSE
              )
            }
          }
        }
      }
      
      #print("parte 6.3")
      
      #### cria ou inclui ft_internacao_cti ####
      # verificação se a propriedade cti existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"cti"))){
        #Fazer varredura de lista dentro do cidSecundario LISTA:
        for( indexcti in 1:length(pluck(response,index,"cti"))){
          
          #Fazer o vinculo cti internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_cti - %s", response[[index]]$cti[[indexcti]]$dataInicial))
          
          cti <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            dataInicial=pluck(response, index,"cti",indexcti,"dataInicial", .default = NA),
            dataFinal=pluck(response,  index, "cti", indexcti, "dataFinal", .default = NA),
            condicaoAlta=pluck(response,  index, "cti", indexcti, "condicaoAlta", .default = NA),
            tipo=pluck(response,  index, "cti", indexcti, "tipo", .default = NA),
            permanenciaPrevistaNaAlta=pluck(response,  index, "cti", indexcti, "permanenciaPrevistaNaAlta", .default = NA),
            permanenciaReal=pluck(response,  index, "cti", indexcti, "permanenciaReal", .default = NA),
            leito=pluck(response,  index, "cti", indexcti, "leito", .default = NA),
            medico_crm=pluck(response,  index, "cti", indexcti, "medico", "crm", .default = NA),
            hospital_cod=pluck(response,  index, "cti", indexcti, "hospital", "codigo", .default = NA),
            cidPrincipal=pluck(response,  index, "cti", indexcti, "cidPrincipal", "codigo", .default = NA),
            drgBrasilRefinado_cod=pluck(response,  index, "cti", indexcti, "drgBrasilRefinado", "codigo", .default = NA),
            drgBrasilRefinado_desc=pluck(response,  index, "cti", indexcti, "drgBrasilRefinado", "descricao", .default = NA),
            drgBrasilRefinado_tipo=pluck(response,  index, "cti", indexcti, "drgBrasilRefinado", "tipo", .default = NA),
            drgBrasilRefinado_mdc_cod=pluck(response,  index, "cti", indexcti, "drgBrasilRefinado", "mdc", "codigo", .default = NA),
            drgBrasilRefinado_mdc_desc=pluck(response,  index, "cti", indexcti, "drgBrasilRefinado", "mdc", "descricao", .default = NA)
            
          )
          
          dbWriteTable(
            con,
            "ft_internacao_cti",
            cti,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      #print("parte 6.4")
      
      #### cria ou inclui ft_internacao_condicaoAdquirida ####
      # verificação se a propriedade condicaoAdquirida existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"condicaoAdquirida"))){
        #Fazer varredura de lista dentro do condicaoAdquirida LISTA:
        for( indexcondicaoAdquirida in 1:length(pluck(response,index,"condicaoAdquirida"))){
          
          #Fazer o vinculo condicaoAdquirida internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_condicaoAdquirida - %s", response[[index]]$condicaoAdquirida[[indexcondicaoAdquirida]]$dataInicial))
          
          condicaoAdquirida <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            codigo=pluck(response, index,"condicaoAdquirida",indexcondicaoAdquirida,"codigo", .default = NA),
            descricao=pluck(response,  index, "condicaoAdquirida", indexcondicaoAdquirida, "descricao", .default = NA),
            dataOcorrencia=pluck(response,  index, "condicaoAdquirida", indexcondicaoAdquirida, "dataOcorrencia", .default = NA),
            medico_crm=pluck(response,  index, "condicaoAdquirida", indexcondicaoAdquirida, "medico", "crm", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_condicaoAdquirida",
            condicaoAdquirida,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      #print("parte 6.5")
      
      
      #### cria ou inclui ft_internacao_sondaVesicalDeDemora ####
      # verificação se a propriedade sondaVesicalDeDemora existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"sondaVesicalDeDemora"))){
        #Fazer varredura de lista dentro do sondaVesicalDeDemora LISTA:
        for( indexCateter in 1:length(pluck(response,index,"sondaVesicalDeDemora"))){
          
          #Fazer o vinculo sondaVesicalDeDemora internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_sondaVesicalDeDemora - %s", response[[index]]$sondaVesicalDeDemora[[indexCateter]]$dataInicial))
          
          sondaVesicalDeDemora <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            local=pluck(response, index,"sondaVesicalDeDemora",indexCateter,"local", .default = NA),
            dataInicial=pluck(response,  index, "sondaVesicalDeDemora", indexCateter, "dataInicial", .default = NA),
            dataFinal=pluck(response,  index, "sondaVesicalDeDemora", indexCateter, "dataFinal", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_sondaVesicalDeDemora",
            sondaVesicalDeDemora,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      #print("parte 6.6")
      
      
      #### cria ou inclui ft_internacao_suporteVentilatorio ####
      # verificação se a propriedade suporteVentilatorio existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"suporteVentilatorio"))){
        #Fazer varredura de lista dentro do suporteVentilatorio LISTA:
        for( indexSuporte in 1:length(pluck(response,index,"suporteVentilatorio"))){
    
          #Fazer o vinculo suporteVentilatorio internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_suporteVentilatorio - %s", response[[index]]$suporteVentilatorio[[indexSuporte]]$dataInicial))
    
          suporteVentilatorio <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            tipo=pluck(response, index,"suporteVentilatorio",indexSuporte,"tipo", .default = NA),
            tipoInvasivo=pluck(response, index,"suporteVentilatorio",indexSuporte,"tipoInvasivo", .default = NA),
            local=pluck(response, index,"suporteVentilatorio",indexSuporte,"local", .default = NA),
            dataInicial=pluck(response,  index, "suporteVentilatorio", indexSuporte, "dataInicial", .default = NA),
            dataFinal=pluck(response,  index, "suporteVentilatorio", indexSuporte, "dataFinal", .default = NA)
          )
    
          dbWriteTable(
            con,
            "ft_internacao_suporteVentilatorio",
            suporteVentilatorio,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      #print("parte 6.7")
      
      #### cria ou inclui ft_internacao_sondaVesicalDeDemora ####
      # verificação se a propriedade sondaVesicalDeDemora existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"sondaVesicalDeDemora"))){
        #Fazer varredura de lista dentro do sondaVesicalDeDemora LISTA:
        for( indexSonda in 1:length(pluck(response,index,"sondaVesicalDeDemora"))){
          
          #Fazer o vinculo sondaVesicalDeDemora internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_sondaVesicalDeDemora - %s", response[[index]]$sondaVesicalDeDemora[[indexSonda]]$dataInicial))
          
          sondaVesicalDeDemora <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            local=pluck(response, index,"sondaVesicalDeDemora",indexSonda,"local", .default = NA),
            dataInicial=pluck(response,  index, "sondaVesicalDeDemora", indexSonda, "dataInicial", .default = NA),
            dataFinal=pluck(response,  index, "sondaVesicalDeDemora", indexSonda, "dataFinal", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_sondaVesicalDeDemora",
            sondaVesicalDeDemora,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      #print("parte 6.8")
      
      #### cria ou inclui ft_internacao_analiseCritica ####
      # verificação se a propriedade analiseCritica existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"analiseCritica"))){
        #Fazer varredura de lista dentro do analiseCritica LISTA:
        for( indexAnalise in 1:length(pluck(response,index,"analiseCritica"))){
          
          #Fazer o vinculo analiseCritica internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_analiseCritica - %s", response[[index]]$analiseCritica[[indexAnalise]]$dataAnalise))
          
          analiseCritica <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            dataAnalise=pluck(response, index,"analiseCritica",indexAnalise,"dataAnalise", .default = NA),
            analiseCritica=pluck(response,  index, "analiseCritica", indexAnalise, "analiseCritica", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_analiseCritica",
            analiseCritica,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      
      
      #### cria ou inclui ft_internacao_causaExterna ####
      # verificação se a propriedade causaExterna existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"causaExterna"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"causaExterna"))){
          
          #Fazer o vinculo causaExterna internacao
          #print("            ++++++++++++++++++++")
          #print(sprintf("inserindo ft_internacao_causaExterna - %s", response[[index]]$causaExterna[[indexCausa]]$dataInicial))
          
          causaExterna <- data.frame(
            idinternacao=pluck(response,  index, "id"),
            descricao=pluck(response, index,"causaExterna",indexCausa,"descricao", .default = NA),
            tempo=pluck(response, index,"causaExterna",indexCausa,"tempo", .default = NA),
            dataInicial=pluck(response,  index, "causaExterna", indexCausa, "dataInicial", .default = NA),
            dataFinal=pluck(response,  index, "causaExterna", indexCausa, "dataFinal", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_causaExterna",
            causaExterna,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      #### cria ou inclui ft_internacao_rn ####
      # verificação se a propriedade rn existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"rn"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"rn"))){
          rn <- data.frame(
            idinternacao=pluck(response, index, "id"),
            idadeGestacional=pluck(response, index,"rn",indexCausa,"idadeGestacional", .default = NA),
            comprimento=pluck(response, index,"rn",indexCausa,"comprimento", .default = NA),
            sexo=pluck(response, index, "rn", indexCausa, "sexo", .default = NA),
            nascidoVivo=pluck(response, index, "rn", indexCausa, "nascidoVivo", .default = NA),
            tocotraumatismo=pluck(response, index, "rn", indexCausa, "tocotraumatismo", .default = NA),
            apgar=pluck(response, index, "rn", indexCausa, "apgar", .default = NA),
            apgarQuintoMinuto=pluck(response, index, "rn", indexCausa, "apgarQuintoMinuto", .default = NA),
            alta48horas=pluck(response, index, "rn", indexCausa, "alta48horas", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_rn",
            rn,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      
      #### cria ou inclui ft_internacao_altaAdministrativa ####
      # verificação se a propriedade altaAdministrativa existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"altaAdministrativa"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"altaAdministrativa"))){
          altaAdministrativa <- data.frame(
            idinternacao=pluck(response, index, "id"),
            numeroAtendimento=pluck(response, index,"altaAdministrativa",indexCausa,"numeroAtendimento", .default = NA),
            numeroAutorizacao=pluck(response, index,"altaAdministrativa",indexCausa,"numeroAutorizacao", .default = NA),
            dataAutorizacao=pluck(response, index, "altaAdministrativa", indexCausa, "dataAutorizacao", .default = NA),
            dataAtendimentoInicial=pluck(response, index, "altaAdministrativa", indexCausa, "dataAtendimentoInicial", .default = NA),
            dataAtendimentoFinal=pluck(response, index, "altaAdministrativa", indexCausa, "dataAtendimentoFinal", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_altaAdministrativa",
            altaAdministrativa,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      
      #### cria ou inclui ft_internacao_cateterVascularCentral ####
      # verificação se a propriedade cateterVascularCentral existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"cateterVascularCentral"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"cateterVascularCentral"))){
          cateterVascularCentral <- data.frame(
            idinternacao=pluck(response, index, "id"),
            local=pluck(response, index,"cateterVascularCentral",indexCausa,"local", .default = NA),
            dataInicial=pluck(response, index, "cateterVascularCentral", indexCausa, "dataInicial", .default = NA),
            dataFinal=pluck(response, index, "cateterVascularCentral", indexCausa, "dataFinal", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_cateterVascularCentral",
            cateterVascularCentral,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      
      #### cria ou inclui ft_internacao_dispositivoTerapeutico ####
      # verificação se a propriedade dispositivoTerapeutico existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"dispositivoTerapeutico"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"dispositivoTerapeutico"))){
          dispositivoTerapeutico <- data.frame(
            idinternacao=pluck(response, index, "id"),
            local=pluck(response, index,"cateterVascularCentral",indexCausa,"local", .default = NA),
            dataInicial=pluck(response, index, "cateterVascularCentral", indexCausa, "dataInicial", .default = NA),
            dataFinal=pluck(response, index, "cateterVascularCentral", indexCausa, "dataFinal", .default = NA),
            tipoTerapeutico=pluck(response, index,"dispositivoTerapeutico",indexCausa,"tipoTerapeutico")
          )
          
          dbWriteTable(
            con,
            "ft_internacao_dispositivoTerapeutico",
            dispositivoTerapeutico,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      
      
      #### cria ou inclui ft_internacao_partoAdequado ####
      # verificação se a propriedade partoAdequado existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"partoAdequado"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for (indexCausa in 1:length(pluck(response, index, "partoAdequado"))) {
          partoAdequado <- data.frame(
            idinternacao = pluck(response, index, "id"),
            classificacaoRobson = pluck(response, index, "partoAdequado", indexCausa, "classificacaoRobson", .default = NA),
            numeroFetos = pluck(response, index, "partoAdequado", indexCausa, "numeroFetos", .default = NA),
            antecedenteObstetrico = pluck(response, index, "partoAdequado", indexCausa, "antecedenteObstetrico", .default = NA),
            numeroCesareasAnteriores = pluck(response, index, "partoAdequado", indexCausa, "numeroCesareasAnteriores", .default = NA),
            apresentacaoFetal = pluck(response, index, "partoAdequado", indexCausa, "apresentacaoFetal", .default = NA),
            apresentacaoFetalRn2 = pluck(response, index, "partoAdequado", indexCausa, "apresentacaoFetalRn2", .default = NA),
            apresentacaoFetalRn3 = pluck(response, index, "partoAdequado", indexCausa, "apresentacaoFetalRn3", .default = NA),
            apresentacaoFetalRn4 = pluck(response, index, "partoAdequado", indexCausa, "apresentacaoFetalRn4", .default = NA),
            apresentacaoFetalRn5 = pluck(response, index, "partoAdequado", indexCausa, "apresentacaoFetalRn5", .default = NA),
            inicioTrabalhoParto = pluck(response, index, "partoAdequado", indexCausa, "inicioTrabalhoParto", .default = NA),
            rupturaUterina = pluck(response, index, "partoAdequado", indexCausa, "rupturaUterina", .default = NA),
            laceracaoPerineal = pluck(response, index, "partoAdequado", indexCausa, "laceracaoPerineal", .default = NA),
            transfusaoSanguinea = pluck(response, index, "partoAdequado", indexCausa, "transfusaoSanguinea", .default = NA),
            morteMaterna = pluck(response, index, "partoAdequado", indexCausa, "morteMaterna", .default = NA),
            morteFetalIntraparto = pluck(response, index, "partoAdequado", indexCausa, "morteFetalIntraparto", .default = NA),
            admissaoMaternaUti = pluck(response, index, "partoAdequado", indexCausa, "admissaoMaternaUti", .default = NA),
            retornoMaternoSalaParto = pluck(response, index, "partoAdequado", indexCausa, "retornoMaternoSalaParto", .default = NA),
            indiceSatisfacaoEquipeMedica = pluck(response, index, "partoAdequado", indexCausa, "indiceSatisfacaoEquipeMedica", .default = NA),
            indiceSatisfacaoHospital = pluck(response, index, "partoAdequado", indexCausa, "indiceSatisfacaoHospital", .default = NA),
            contatoPelePele = pluck(response, index, "partoAdequado", indexCausa, "contatoPelePele", .default = NA),
            contatoPelePeleRn2 = pluck(response, index, "partoAdequado", indexCausa, "contatoPelePeleRn2", .default = NA),
            contatoPelePeleRn3 = pluck(response, index, "partoAdequado", indexCausa, "contatoPelePeleRn3", .default = NA),
            contatoPelePeleRn4 = pluck(response, index, "partoAdequado", indexCausa, "contatoPelePeleRn4", .default = NA),
            contatoPelePeleRn5 = pluck(response, index, "partoAdequado", indexCausa, "contatoPelePeleRn5", .default = NA),
            posicaoMaternaParto = pluck(response, index, "partoAdequado", indexCausa, "posicaoMaternaParto", .default = NA),
            medicacaoInducaoParto = pluck(response, index, "partoAdequado", indexCausa, "medicacaoInducaoParto", .default = NA),
            estagioOcitocinaMisoprostol = pluck(response, index, "partoAdequado", indexCausa, "estagioOcitocinaMisoprostol", .default = NA),
            parturienteAcompanhada = pluck(response, index, "partoAdequado", indexCausa, "parturienteAcompanhada", .default = NA),
            presencaDoula = pluck(response, index, "partoAdequado", indexCausa, "presencaDoula", .default = NA),
            episiotomia = pluck(response, index, "partoAdequado", indexCausa, "episiotomia", .default = NA),
            aleitamento = pluck(response, index, "partoAdequado", indexCausa, "aleitamento", .default = NA),
            aleitamentoRn2 = pluck(response, index, "partoAdequado", indexCausa, "aleitamentoRn2", .default = NA),
            aleitamentoRn3 = pluck(response, index, "partoAdequado", indexCausa, "aleitamentoRn3", .default = NA),
            aleitamentoRn4 = pluck(response, index, "partoAdequado", indexCausa, "aleitamentoRn4", .default = NA),
            aleitamentoRn5 = pluck(response, index, "partoAdequado", indexCausa, "aleitamentoRn5", .default = NA),
            tempoClampeamentoCordao = pluck(response, index, "partoAdequado", indexCausa, "tempoClampeamentoCordao", .default = NA),
            tempoClampeamentoCordaoRn2 = pluck(response, index, "partoAdequado", indexCausa, "tempoClampeamentoCordaoRn2", .default = NA),
            tempoClampeamentoCordaoRn3 = pluck(response, index, "partoAdequado", indexCausa, "tempoClampeamentoCordaoRn3", .default = NA),
            tempoClampeamentoCordaoRn4 = pluck(response, index, "partoAdequado", indexCausa, "tempoClampeamentoCordaoRn4", .default = NA),
            tempoClampeamentoCordaoRn5 = pluck(response, index, "partoAdequado", indexCausa, "tempoClampeamentoCordaoRn5", .default = NA),
            analgesia = pluck(response, index, "partoAdequado", indexCausa, "analgesia", .default = NA),
            metodoAnalgesia = pluck(response, index, "partoAdequado", indexCausa, "metodoAnalgesia", .default = NA),
            perimetroCefalicoRn1 = pluck(response, index, "partoAdequado", indexCausa, "perimetroCefalicoRn1", .default = NA),
            perimetroCefalicoRn2 = pluck(response, index, "partoAdequado", indexCausa, "perimetroCefalicoRn2", .default = NA),
            perimetroCefalicoRn3 = pluck(response, index, "partoAdequado", indexCausa, "perimetroCefalicoRn3", .default = NA),
            perimetroCefalicoRn4 = pluck(response, index, "partoAdequado", indexCausa, "perimetroCefalicoRn4", .default = NA),
            perimetroCefalicoRn5 = pluck(response, index, "partoAdequado", indexCausa, "perimetroCefalicoRn5", .default = NA),
            cesariana = pluck(response, index, "partoAdequado", indexCausa, "cesariana", .default = NA),
            numeroPartosAnteriores = pluck(response, index, "partoAdequado", indexCausa, "numeroPartosAnteriores", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_partoAdequado",
            partoAdequado,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      #### cria ou inclui ft_internacao_intercambio ####
      # verificação se a propriedade intercambio existe na lista, para evitar erros
      if(!is.null(pluck(response,index,"intercambio"))){
        #Fazer varredura de lista dentro do causaExterna LISTA:
        for( indexCausa in 1:length(pluck(response,index,"intercambio"))){
          intercambio <- data.frame(
            idinternacao=pluck(response, index, "id"),
            consideracoes=pluck(response, index,"intercambio",indexCausa,"consideracoes", .default = NA),
            data=pluck(response, index,"intercambio",indexCausa,"data", .default = NA),
            usuario=pluck(response, index, "intercambio", indexCausa, "usuario", .default = NA)
          )
          
          dbWriteTable(
            con,
            "ft_internacao_intercambio",
            intercambio,
            append = TRUE,
            row.names = FALSE
          )
        }
      }
      
      #print("parte7")
      
      
    }
    
    #### ATUALIZANDO A TABELA ATUALIZAÇÃO ####
    df_atualizacao <- data.frame(
      data_codificacao = datas,
      data_importacao  = hoje,
      status           = 1,
      mensagem         = "Importacao bem sucedida"
    )
    
    dbWriteTable(con, "atualizacao", df_atualizacao, append = TRUE, row.names = FALSE)
    #print("parte8")
  },
  error = function(e){
    # Informa na tabela atualizacao do ci_drg que houve erro para as datas buscadas
    df_atualizacao <- data.frame(
      data_codificacao = datas,
      data_importacao  = hoje,
      status           = 0,
      mensagem         = "Erro na exportacao dos dados do R para o banco ci_drg"
    )
    
    dbWriteTable(con, "atualizacao", df_atualizacao, append = TRUE, row.names = FALSE)
    
    message("ERRO NA ETAPA 4 - EXPORTACAO DOS DADOS DO R PARA O BANCO CI_DRG")
    message(conditionMessage(e))
    quit(status = 1)
  }
  
)



dbDisconnect(con)


