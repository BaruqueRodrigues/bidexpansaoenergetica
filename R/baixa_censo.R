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


baixa_censo <- function(destination_dir = 'data/censo-ibge-2022/'){
  # checa se o diretorio existe
  
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  # url de download do zip
  zip_url <- 'https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios_preliminares/agregados_por_setores_csv/BR/Agregados_preliminares_por_setores_censitarios_BR.zip'
  
  zip_diretorio <- paste0(destination_dir, 'Agregados_preliminares_por_setores_censitarios_BR.zip')
  
  download.file(zip_url,
                zip_diretorio, mode = 'wb')
  
  unzip(zip_diretorio, exdir = destination_dir)
  
  
  
  
  readr::read_csv2(paste0(destination_dir, 'Agregados_preliminares_por_setores_censitarios_BR.csv')
  ) |>
    janitor::clean_names()
  
}


baixa_censo("data/censo-ibge-2022/")












