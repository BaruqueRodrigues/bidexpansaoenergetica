#' Tranform PNADCA
#'
#' Apply data treatment and transformation in dataset of PNADCA Visita 1 and
#' return a DataFrame with columns of interest by household.
#'
#' @param pnad_data A table that contains data of PNADCA Visita 1.
#'
#' @return A DataFrame of selected and transformed columns of PNADC Visita 1 by
#' year. Each row represents a household.
#'
#'
#' @examples
#' \dontrun{
#' transform_pnad(
#'   pnad_data = pnad2019
#' )
#' }
#' @export
transform_pnad <- function(pnad_data){
  data <- dplyr::select(pnad_data,
                        # Key-Variables
                        Ano, Trimestre, UF, UPA, V1008,
                        # Filter for 'Responsável pelo domicílio'
                        V2005,
                        ## Variables used to calculate Indicators
                        #IA0101 e IA0102
                        S010141, S010142,
                        #IA0103 e IA0104
                        contains("S01016"),
                        #IA0202
                        S01015,
                        #IP0101
                        S01023,
                        #IP0501
                        S01025,
                        #IP0502
                        S01028,
                        #IP0503
                        S01021,
                        #IP0504
                        S01029,
                        #IP0601
                        S01024,
                        #IT0201
                        S010311, S010312,
                        ## Variables used to calculate Determinants
                        #DC0101
                        V2007,

                        #DC0201
                        V2010,

                        #DC0301 (old)
                        V2009,

                        #DC0401 (old): VD3004 (new)
                        VD3004,

                        #DC0501 (old): VD4009 (new) ** Changed to VD4008 by EPE suggestion
                        VD4008,

                        #DD01 (old): DF0101, DF0102, DF0103, DF0104, DF0105, DF0201, DF0202, DF0203,
                        #DF0204, DF0205, DF0206, DF0301 (new)
                        V2009,
                        #DF0301
                        VD2003,

                        #DD02 (old): DS0101, DS0102, DS0103, DS0104, DS0105, DS0106 (new)
                        VD5010, VD5011,

                        #DD03 (old)
                        #DS0501 (new)
                        S01018, S01019,

                        #DD04 (old): #DS0201, DS0202, DS0203, DS0204, DS0205, DS0206, DS0207 (new)
                        #DS0201
                        V5002A,
                        #DS0202
                        V5001A,
                        #DS0207
                        V5003A,

                        #DD05 (old): DD0701, DD0702 (new) - Doesn't exist

                        #DD06 (old)
                        #DD0101 (new)
                        S01001,

                        #DD07 (old):
                        #DD0301 (new)
                        S01017,

                        #DD08 (old): DD0401, DD0402, DD0403 (don't have), DD0404 (new)
                        #DD0401 (new)
                        S01010,
                        #DD0402
                        S01003, S01002, S01004,
                        #DD0404
                        S01013,
                        #DD0406
                        contains("S01011"), contains("S01012"),

                        #DD13 (old): DD0201 (new)
                        V1022,

                        #NUMERO_PESSOAS_DOMICILIO
                        VD2003,

                        # Pesos
                        V1032
  )

  if(unique(data$Ano) == 2016){
    data <- dplyr::select(data,-S01016)
  }
  if(unique(data$Ano) %in% c(2017,2018)){
    data <- dplyr::select(data,
                          !c(S01011B,S01011C, S01016))
  }
  if(unique(data$Ano) == 2019){
    data <- dplyr::select(data,
                          !c(S01011B,S01011C, S01016A))
  }
  if(unique(data$Ano) == 2022){
    data <- dplyr::select(data,
                          !c(S01011B,S01011C, S01016A))
  }

  ifelse((unique(data$Ano) %in% c(2019,2022)),
         # Missing values treatment for pnad_dataC 2019 and 2022
         data <- dplyr::mutate(data,
                               # Recodificando a variável UF para números
                               UF = stringr::str_extract(UPA,"^.."),
                               # Tratamneto de NA
                               VD5010 = replace(VD5010, is.na(VD5010),0), # Não se aplica a pessoa por ser empregada doméstica do domicílio
                               VD5011 = replace(VD5011, is.na(VD5011),0), # Não se aplica a pessoa por ser empregada doméstica do domicílio
                               S01015 = replace(S01015, is.na(S01015),"Outra frequência"), # Pessoa que responderam que não tem acesso a rede geral
                               VD4008 = replace(as.character(VD4008), is.na(VD4008),"Aposentado ou Desempregado"), # Pessoas que estão aposentadas ou desempregadas
                               S01012A = replace(S01012A, is.na(S01012A), "Rio, lago ou mar"), #Os NA foram substituídos por "Rio, lago ou mar"
                               S01019 = ifelse((S01017 == "Alugado"),S01019,0), # 12 casos que não declararam o valor do Aluguel
                               S01018 = ifelse((S01017 == "Próprio de algum morador - ainda pagando"),S01018,0) # 18 casos que não declararam o valor da prestação do imóvel
         ),
         # Missing values treatment for pnad_dataC past years
         data <- dplyr::mutate(data,
                               # Recodificando a variável UF para números
                               UF = UPA %>% stringr::str_extract("^.."),
                               # Tratamneto de NA
                               VD5010 = replace(VD5010, is.na(VD5010),0), # Não se aplica a pessoa por ser empregada doméstica do domicílio
                               VD5011 = replace(VD5011, is.na(VD5011),0), # Não se aplica a pessoa por ser empregada doméstica do domicílio
                               S01015 = replace(S01015, is.na(S01015),"Outra frequência"), # Pessoa que responderam que não tem acesso a rede geral
                               VD4008 = replace(as.character(VD4008), is.na(VD4008),"Aposentado ou Desempregado"), # Pessoas que estão aposentadas ou desempregadas
                               S01012 = replace(S01012, is.na(S01012), "Rio, lago ou mar"), #Os NA foram substituídos por "Rio, lago ou mar"
                               S01019 = ifelse((S01017 == "Alugado"),S01019,0), # 12 casos que não declararam o valor do Aluguel
                               S01018 = ifelse((S01017 == "Próprio de algum morador - ainda pagando"),S01018,0) # 18 casos que não declararam o valor da prestação do imóvel
         )
  )
  # Criando variáveis que contém dados dos Determinantes Familiares (DF)
  data <- dplyr::group_by(data, Ano, Trimestre, UF, UPA, V1008)
  data <- dplyr::mutate(data,
                        # Variável que contém dados para DF0101
                        V2009_index_1 = sum(V2009<=12, na.rm = TRUE),
                        # Variável que contém dados para DF0102
                        V2009_index_2 = sum(V2009 > 12 & V2009 <18, na.rm = TRUE),
                        # Variável que contém dados para DF0103
                        V2009_index_3 = sum(V2009 >= 18 & V2009 < 50, na.rm = TRUE),
                        # Variável que contém dados para DF0104
                        V2009_index_4 = sum(V2009 >= 50 & V2009 < 65, na.rm = TRUE),
                        # Variável que contém dados para DF0105
                        V2009_index_5 = sum(V2009 >= 65, na.rm = TRUE)
  )
  data <- dplyr::ungroup(data)
  data <- dplyr::filter(data, V2005 == "Pessoa responsável pelo domicílio")
  data <- dplyr::mutate(data,
                        V2005 = ifelse((V2005 == "Pessoa responsável pelo domicílio"),1,0),
  )
  data <- dplyr::rename(data,
                        "COD_UPA" = UPA,
                        "NUM_DOM" = V1008,
                        "PESO_FINAL" = V1032,
                        "NUMERO_PESSOAS_DOMICILIO" = VD2003)

  data_transformed <- tidyr::drop_na(data, S01018, S01019)

}
