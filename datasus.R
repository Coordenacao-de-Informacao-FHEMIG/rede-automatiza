library(microdatasus)
library(RMySQL)
library(dplyr)
library(lubridate)

# Configurações do banco de dados
db_config <- list(
  host = "00.00.00.00",
  user = "",
  password = "",
  dbname = "datasus"
)

# Função para registrar a atualização no banco de dados
registrar_atualizacao <- function(conn, ano_mes, bases_ok) {
  # Verificar se já existe registro para este ANOMES
  query_check <- sprintf(
    "SELECT COUNT(*) FROM atualizacao WHERE ANOMES = '%s'",
    ano_mes
  )
  existe <- dbGetQuery(conn, query_check)[1,1] > 0
  
  if (existe) {
    # Atualizar registro existente
    query <- sprintf(
      "UPDATE atualizacao 
       SET ULTIMA_ATUALIZACAO = NOW(),
           BASES_ATUALIZADAS = '%s'
       WHERE ANOMES = '%s'",
      bases_ok, ano_mes
    )
  } else {
    # Inserir novo registro
    query <- sprintf(
      "INSERT INTO atualizacao (ANOMES, ULTIMA_ATUALIZACAO, BASES_ATUALIZADAS) 
       VALUES ('%s', NOW(), '%s')",
      ano_mes, bases_ok
    )
  }
  
  dbExecute(conn, query)
}


# Função para conectar ao MySQL com schema específico
connect_db <- function() {
  tryCatch({
    conn <- dbConnect(
      MySQL(),
      host = db_config$host,
      user = db_config$user,
      password = db_config$password,
      dbname = db_config$dbname
    )
    
    return(conn)
  }, error = function(e) {
    stop(paste("Falha ao conectar ao banco:", e$message))
  })
}

# Função para gerar sequência de meses no formato AAAAMM
generate_month_sequence <- function(base_date, months_back) {
  seq(base_date %m-% months(months_back), base_date, by = "month") %>%
    format("%Y%m") %>%
    as.character()
}

# Função para deletar dados existentes de um mês específico
delete_existing_data <- function(conn, table_name, year_month) {
  year <- substr(year_month, 1, 4)
  month <- substr(year_month, 5, 6)
  
  if (table_name == "SIH_RD") {
    query <- sprintf(
      "DELETE FROM %s WHERE ANO_CMPT = '%s' AND MES_CMPT = '%s'",
      table_name, year, month
    )
  } else if (table_name == "SIH_ER") {
    query <- sprintf(
      "DELETE FROM %s WHERE ANO = '%s' AND MES = '%s'",
      table_name, year, month
    )
  } else if (table_name == "SIH_RJ") {
    query <- sprintf(
      "DELETE FROM %s WHERE ANO_CMPT = '%s' AND MES_CMPT = '%s'",
      table_name, year, month
    )
  } else if (table_name == "SIA_PA") {
    query <- sprintf(
      "DELETE FROM %s WHERE PA_CMP = '%s%s'",
      table_name, year, month
    )
  } else if (table_name == "CNES_LT") {
    query <- sprintf(
      "DELETE FROM %s WHERE COMPETEN = '%s%s'",
      table_name, year, month
    )
  } else if (table_name == "CNES_PF") {
    query <- sprintf(
      "DELETE FROM %s WHERE COMPETEN = '%s%s'",
      table_name, year, month
    )
  } else if (table_name == "CNES_EQ") {
    query <- sprintf(
      "DELETE FROM %s WHERE COMPETEN = '%s%s'",
      table_name, year, month
    )
  } else if (table_name == "CNES_HB") {
    query <- sprintf(
      "DELETE FROM %s WHERE COMPETEN = '%s%s'",
      table_name, year, month
    )
  }
  
  dbExecute(conn, query)
}



# Função para incluir coluna DT_CMPT em todas as tabelas (no formato data hora)
add_dt_cmpt <- function(dados, information_system) {
  if (information_system %in% c("SIH-RD", "SIH-RJ")) {
    # ANO_CMPT + MES_CMPT
    dados <- dados %>%
      mutate(
        DT_CMPT = as.POSIXct(
          sprintf("%s-%02d-01 00:00:00", ANO_CMPT, as.integer(MES_CMPT)),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
    
  } else if (information_system == "SIH-ER") {
    # ANO + MES
    dados <- dados %>%
      mutate(
        DT_CMPT = as.POSIXct(
          sprintf("%s-%02d-01 00:00:00", ANO, as.integer(MES)),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
    
  } else if (information_system == "SIA-PA") {
    # PA_CMP = aaaamm
    dados <- dados %>%
      mutate(
        DT_CMPT = as.POSIXct(
          paste0(substr(PA_CMP, 1, 4), "-", substr(PA_CMP, 5, 6), "-01 00:00:00"),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
    
  } else if (information_system %in% c("CNES-LT", "CNES-PF", "CNES-EQ", "CNES-HB")) {
    # COMPETEN = aaaamm
    dados <- dados %>%
      mutate(
        DT_CMPT = as.POSIXct(
          paste0(substr(COMPETEN, 1, 4), "-", substr(COMPETEN, 5, 6), "-01 00:00:00"),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      )
  }
  
  return(dados)
}



# Função genérica para atualizar tabela
update_table <- function(conn, information_system, table_name, year_month, cnes_fhemig = NULL) {
  year <- substr(year_month, 1, 4)
  month <- substr(year_month, 5, 6)
  
  message(paste("Processando", table_name, "para", month, "/", year))
  
  tryCatch({
    # Buscar dados do DATASUS
    dados <- fetch_datasus(
      year_start = year,
      month_start = month,
      year_end = year,
      month_end = month,
      uf = "MG",
      information_system = information_system,
      vars = NULL
    )
    
    # Filtrar dados da FHEMIG
    if (information_system %in% c("CNES-LT", "CNES-PF", "CNES-EQ", "CNES-HB")) {
      dados_filtrados <- dados %>% filter(CNPJ_MAN == "19843929000100")
    } else if (information_system %in% c("SIH-RD", "SIH-RJ")) {
      dados_filtrados <- dados %>% filter(CNPJ_MANT == "19843929000100")
    } else if (information_system == "SIH-ER") {
      cnes_fhemig <- c("0026948", "0026697", "0026964", "0026913", "2111624", 
                       "0027022", "2105799", "2115662", "3698548", "2726726", 
                       "0026921", "2098946", "2115654", "0027626", "2181770", 
                       "0026999", "0026972", "2775905", "2195429")
      dados_filtrados <- dados %>% filter(CNES %in% cnes_fhemig)
    } else if (information_system == "SIA-PA") {
      dados_filtrados <- dados %>% filter(PA_CNPJMNT == "19843929000100")
    }
    
    if (nrow(dados_filtrados) == 0) {
      message(paste("Nenhum dado encontrado para", table_name, "em", month, "/", year))
      return(0)
    }
    
    
    # Preencher DT_CMPT no R
    dados_filtrados <- add_dt_cmpt(dados_filtrados, information_system)
    
    
    # Deletar dados existentes para evitar duplicatas
    delete_existing_data(conn, table_name, year_month)
    
    # Inserir novos dados
    dbWriteTable(
      conn,
      name = table_name,
      value = dados_filtrados,
      append = TRUE,
      row.names = FALSE
    )
    
    message(paste("Inseridos", nrow(dados_filtrados), "registros em", table_name, "para", month, "/", year))
    return(nrow(dados_filtrados))
    
  }, error = function(e) {
    message(paste("ERRO ao processar", table_name, "para", month, "/", year, ":", e$message))
    return(-1)
  })
}

# Função principal
main <- function() {
  # 1. Determinar os meses a serem processados
  current_date <- Sys.Date()
  
  ultimate_month <- current_date %m-% months(1)
  months_to_process <- format(ultimate_month, "%Y%m")
  
  months_to_process <- c(
    months_to_process,
    generate_month_sequence(ultimate_month %m-% months(1), 5)
  )
  
  months_to_process <- unique(months_to_process) %>% sort()
  
  message("Meses a serem processados: ", paste(months_to_process, collapse = ", "))
  
  conn <- connect_db()
  on.exit(dbDisconnect(conn))
  
  results <- list()
  
  for (month_ym in months_to_process) {
    message("\n", rep("-", 50))
    message("Processando mês: ", month_ym)
    
    # ── BLOCO 1: todos exceto SIA-PA ──────────────────────────────────────────
    message(">> Etapa 1/2: bases SIH e CNES")
    
    month_results <- list(
      SIH_RD  = update_table(conn, "SIH-RD",  "SIH_RD",  month_ym),
      SIH_RJ  = update_table(conn, "SIH-RJ",  "SIH_RJ",  month_ym),
      SIH_ER  = update_table(conn, "SIH-ER",  "SIH_ER",  month_ym),
      CNES_LT = update_table(conn, "CNES-LT", "CNES_LT", month_ym),
      CNES_PF = update_table(conn, "CNES-PF", "CNES_PF", month_ym),
      CNES_EQ = update_table(conn, "CNES-EQ", "CNES_EQ", month_ym),
      CNES_HB = update_table(conn, "CNES-HB", "CNES_HB", month_ym)
    )
    
    # Libera memória antes de carregar o SIA-PA
    gc(verbose = TRUE)
    message("Memória liberada. Iniciando SIA-PA...")
    
    # ── BLOCO 2: SIA-PA ───────────────────────────────────────────────────────
    message(">> Etapa 2/2: SIA-PA")

    month_results$SIA_PA <- update_table(conn, "SIA-PA", "SIA_PA", month_ym)

    # ── Registro de atualização ───────────────────────────────────────────────
    bases_ok     <- names(month_results)[sapply(month_results, function(x) x >= 0)]
    bases_ok_str <- paste(bases_ok, collapse = ", ")
    
    registrar_atualizacao(conn, month_ym, bases_ok_str)
    message("Registro inserido/atualizado para ", month_ym,
            " | BASES_ATUALIZADAS = ", bases_ok_str)
    
    results[[month_ym]] <- month_results
    
    # Libera memória ao fim de cada mês também
    gc()
  }
  
  # Resumo
  message("\n", rep("=", 50))
  message("Resumo da execução:")
  
  for (month_ym in names(results)) {
    message("\nMês: ", month_ym)
    for (nm in names(results[[month_ym]])) {
      message(nm, ": ", results[[month_ym]][[nm]], " registros")
    }
  }
  
  message("\nAtualização concluída!")
}

# Executar o script
main()

