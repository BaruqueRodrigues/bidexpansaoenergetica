# Clear all variables
rm(list=ls())

#Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")

# Check the operating system
if (Sys.info()["sysname"] == "Windows") 
{
  # For Windows, set encoding for writing names with accentuation
  Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")  # Change to the appropriate locale
  
} else if (Sys.info()["sysname"] == "Darwin" || Sys.info()["sysname"] == "Linux") 
{  # macOS (iOS not directly supported in R)
  # For macOS (Darwin), convert names with accentuation to names without accentuation
  #Sys.setlocale(category="LC_CTYPE", locale="en_US.UTF-8")
  Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
} else 
{
  print("Unsupported operating system.")
}

baixa_visita_5_pnadc <- function(ano = 2016,
                                 destination_dir = "data/pnadc/2016/"){
  
  ## Baixando o Dicionário ---------------------------------------------------
  
  #destination_dir <- paste0("data-raw/pnadc/", ano, "/")
  
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  # Caminho onde o arquivo será salvo
  dictionary_path <- paste0('dicionario_PNADC_microdados_', ano, '_visita5_20231220.xls')
  input_path <- paste0('input_PNADC_', ano, '_visita5_20231222.txt')
  
  # Definindo o URL do dicionário
  dictionary_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
    dictionary_path
  )
  
  input_url <-
    paste0(
      "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
      input_path
    )
  
  # Baixando e salvando o arquivo zip
  download.file(dictionary_url,
                paste0(destination_dir,
                       dictionary_path), mode = 'wb', timeout = 300)
  download.file(input_url,
                paste0(destination_dir,
                       input_path), mode = 'wb', timeout = 300)
  
  ## Baixa Microdados --------------------------------
  
  microdados_path <- paste0("PNADC_", ano ,"_visita5_20231222.zip")
  
  url_microdados <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Dados/",
    microdados_path)
  
  download.file(url_microdados,
                paste0(destination_dir,
                       microdados_path),
                mode = 'wb',
                timeout = 300)
  
  # Descompactando o arquivo
  
  unzip(paste0(destination_dir,microdados_path), exdir = destination_dir)
}
