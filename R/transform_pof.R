#' Transform POF
#'
#' Apply data treatment and transformation in tables of interest in POF and
#' returning a dataset with selected tables and columns of interest by Unidades
#' de consumo.
#'
#' @param pof_data A list vector of POF containing each table of POF.
#' @param pof_year A numeric vector that identifies the year of the POF.
#'
#' @return A DataFrame of selected columns of every table of interest in POF
#' by year. Each row represents a 'Unidade de Consumo'.
#'
#' @examples
#' \dontrun{
#' transform_pof(
#'   pof_data = pof2009,
#'   2009
#' )
#' }
#'
#' @export
transform_pof <- function(pof_data, pof_year){

  pof_transformation <- function(pof_data, pof_tables_names, pof_year){
    # If POF year is 2018 --------------------------------------------------------
    if(pof_year == 2018){
      ## Condições de vida --------------------------------------------------------
      if (pof_tables_names == "Condições de Vida") {
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE,
            #[DD0403] Algum problema para além dos problemas estruturais
            V61061, V61062, V61063, V61064, V61065,
            #[DD0405] Avaliação do padrão de vida da familia
            V61042,
            # [DD0601] avaliação: serviço de fornecimento de água?
            V61051,
            # [IA0201]´avaliação: fornecimento de energia de qualidade
            V61052,
            # [DD0602] avaliação: iluminação de rua?
            V61053,
            # [DD0603] avaliação: serviço de coleta de lixo?
            V61054,
            # [DD0604] avaliação: limpeza e manutenção de rua?
            V61055,
            # [DD0605] avaliação: escoamento da água da chuva?
            V61056,
            # [DD0606] avaliação: serviço de esgotamento sanitário?
            V61057,
            # [DD0607] avaliação: serviço de transporte coletivo?
            V61058,
            #[DD0502] avaliação: problema de estar localizado próximo a rio, baía, lago, açude ou represa poluídos?
            V61068,
            #[DD0503] avaliação: problema de estar localizado em área sujeita a inundação?
            V61069,
            #[DD0504] avaliação: problema de estar localizado em encosta ou área sujeita a deslizamento?
            V610610,
            #[DD0505] avaliação: problema de violência ou vandalismo na sua área de residência?
            V610611,
            # Expansion factor
            PESO_FINAL
          ) |>
          dplyr::rename(COD_INFORMANTE_RESP_COND_VIDA = COD_INFORMANTE)
        # Morador ------------------------------------------------------------------
      } else if (pof_tables_names == "Morador") {
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE,
            # #determinants + relationship degree reference
            V0306,
            #determinants + age per chefe de familia: [DC0301]
            #determinants + age por familia: [DF0101], [DF0102], [DF0103], [DF0104], [DF0105]
            V0403,
            #determinants + sex per chefe de familia: [DC0101]
            V0404,
            #determinants + race per chefe de familia: [DC0201]
            V0405,
            #determinants + plano saúde por familia: [DS0401]
            V0406,
            #determinants + education level: [DC0401]
            NIVEL_INSTRUCAO,
            #determinants + renda: [DS0101]
            # Income por familia (não precisa de ajuste pois o valor é repetido dentro de cada UC):
            # Valor em reais (R$), c/ os centavos, do rendimento bruto total mensal da UC.
            # O rendimento é obtido através do somatório dos rendimentos brutos monetários mensais
            # de todos os moradores da Unidade de Consumo, obtidos através do trabalho, transferências e
            # outras rendas, + a parcela relativa aos rendimentos não monetários mensais do domicílio,
            # acrescido da variação patrimonial, que compreende vendas de imóveis, recebimentos de heranças
            # e o saldo positivo da movimentação financeira.
            RENDA_TOTAL,
            # #Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::left_join(
            pof_data |>
              dplyr::mutate(
                # [DF0101]
                V0403_ate12 = ifelse(V0403 <= 12, 1, 0),
                # [DF0102]
                V0403_13a17 = ifelse(((V0403 >= 13) & (V0403 <= 17)), 1, 0),
                # [DF0103]
                V0403_18a49 = ifelse(((V0403 >= 18) & (V0403 <= 49)), 1, 0),
                # [DF0104]
                V0403_50a64 = ifelse(((V0403 >= 50) & (V0403 <= 64)), 1, 0),
                # [DF0105]
                V0403_65mais = ifelse(V0403 >= 65, 1, 0),
                # [DS0401]
                V0406_soma = ifelse(V0406 == 1, 1, 0)
              ) |>
              dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC) |>
              dplyr::summarise(
                # [DF0101]
                V0403_ate12 = sum(V0403_ate12),
                # [DF0102]
                V0403_13a17 = sum(V0403_13a17),
                # [DF0103]
                V0403_18a49 = sum(V0403_18a49),
                # [DF0104]
                V0403_50a64 = sum(V0403_50a64),
                # [DF0105]
                V0403_65mais = sum(V0403_65mais),
                # Saúde
                V0406_soma = sum(V0406_soma)
              ),
            by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC)
          ) %>%
          dplyr::filter(V0306 == 1) %>%
          # Add NUMERO_PESSOAS_DOMICILIO
          dplyr::left_join(
            pof_data |>
              dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC) |>
              dplyr::summarise(NUMERO_PESSOAS_DOMICILIO = n_distinct(COD_INFORMANTE)),
            by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC)
          ) %>%

          dplyr::left_join(
            pof_data %>%
              filter(!(V0306 %in% c(18,19))) |>
              group_by(UF, COD_UPA, NUM_DOM, NUM_UC) |>
              summarise(NUMERO_PESSOAS_FAMILIA = n_distinct(COD_INFORMANTE)),
            by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC)) |>
          #RENDA_TOTAL não precisa de ajuste pois o mesmo valor é repetido para todas as pessoas da mesma UC
          #mas precisamos add o cálculo de RENDA_TOTAL_PER_CAPITA
          dplyr::mutate(RENDA_TOTAL_PER_CAPITA = RENDA_TOTAL/NUMERO_PESSOAS_DOMICILIO)
        ## Morador - Qualidade de vida ---------------------------------------------
      } else if (pof_tables_names == "Morador - Qualidade de Vida") {
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE,
            # [IA0202] #Origem da energia diferente de rede geral em tempo integral?
            # foi temporariamente substituida por POF_DOMICILIO+V0215 que acabou virando IA0203
            V301,
            # Escoadouro do esgoto do domicílio diferente de rede geral, rede pluvial ou fossa ligada à rede?
            #1 – Sim /       0 – Não
            V303,
            # #serviço de energia elétrica diferente de bom ou satisfatório?
            # not used anymore!
            # [IA0201] Carência de fornecimento de energia elétrica considerado de qualidade
            #  IA0201_1 = ifelse(V306 == 1, 1, 0),
            #V306,
            #[IR0301] - Inadimplência de serviços energéticos
            # #últ. de 12 meses, atrasou o pgt de água, luz ou gás?
            V607,
            # Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::select(-COD_INFORMANTE) %>%
          dplyr::distinct(COD_UPA, NUM_DOM, NUM_UC, .keep_all = TRUE)
        ## Despesa Individual ------------------------------------------------------
      } else if(pof_tables_names == "Despesa Individual") {
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, QUADRO, SEQ,
            # #Código do tipo de despesa/aquisição
            V9001,
            # #Valor em reais (R$) da despesa/aquisição na UC
            #V8000,
            #Valor da despesa/aquisição deflacionado.
            V8000_DEFLA,
            #Identifica o número utilizado para anualizar as despesas/aquisições,
            # em função do período de referência definido para coleta. Neste registro,
            # os fatores utilizados são 1, 4, 12 ou 52
            FATOR_ANUALIZACAO,
            # Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::filter(
            V9001 %in% c(
              # DS0401
              2999901, 4299901,
              # IT0101
              2301401, 2301501, 2301502, 2301601, 2301701, 2301801, 2302201,
              # IT0102
              2300101, 2300201, 2300301, 2300401, 2300402, 2300403, 2300404,
              2300405, 2300406, 2300407, 2300408, 2300409, 2300501, 2300502,
              2300601, 2300602, 2300701, 2300801, 2300901, 2300902, 2300903,
              2300904, 2300905, 2300906, 2300907, 2300908, 2300909, 2300910,
              2300911, 2300912, 2301001, 2301101, 2301201, 2301301, 2302301,
              2302302, 2302601, 2302801, 2302901, 2303001, 2303101, 2303102,
              2303201))  |>
          dplyr::select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, QUADRO, SEQ, PESO_FINAL, V8000_DEFLA) |>
          #garantee a unique value per UC – there are variations due to COD_INFORMANTE, QUADRO, SEQ
          dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, QUADRO, SEQ, PESO_FINAL) |>
          #(!) to-do: It has not yet been validated whether this is the correct calculation
          dplyr::summarise(
            V8000_DEFLA = sum(V8000_DEFLA, na.rm = T)
          ) |>
          dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) |>
          #(!) to-do: It has not yet been validated whether this is the correct calculation
          dplyr::summarise(
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = T)
          ) |>
          dplyr::ungroup() |>
          #to identify the origen of the V9001 information: DC = DESPESA_COLETIVA
          dplyr::mutate(V9001 = paste0("DI_V9001_", V9001)) |>
          dplyr::mutate(INDEX = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = c(INDEX, V8000_DEFLA),
                             values_fill = 0,
                             names_glue = "{V9001}_{.value}",
                             names_vary = "slowest")
        ## Despesa Coletiva -----------------------------------------------------
      } else if (pof_tables_names == "Despesa Coletiva") {
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, QUADRO, SEQ,
            # #Código do tipo de despesa/aquisição
            V9001,
            #[IR0101] Qtd consumida em kilowatt (kwh) – apenas para o item Energia Elétrica (V9001=600101)
            V9005,
            # #Valor em reais (R$) da despesa/aquisição pela UC – missing 9999999.99
            #V8000,
            # Valor da despesa/aquisição deflacionado. “V8000” multiplicado pelo “DEFLATOR”.
            #Este quesito deve ser utilizado no cálculo das estimativas pontuais das tabelas de despesas.
            V8000_DEFLA,
            #Identifica o número utilizado para anualizar as despesas/aquisições,
            #em função do período de referência definido para coleta. Neste registro,
            #os fatores utilizados são 1, 4 ou 12
            FATOR_ANUALIZACAO,
            # Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::filter(
            V9001 %in% c(
              #IP0202 IP0201_teste
              #1502201, 1502301, 1505901,
              #IP0302 IP0301_teste
              1501501, 1501502, 1501601,
              # #IP0403 -- indicador excluído em 19/09/24
              #   1503601,
              # #IP0503 -- indicador excluído em 19/09/24
              #   600701, 601001, 601101, 601301, 601401, 601601, 601701,
              # #IP0504 (some codes are repeated) -- indicador excluído em 19/09/24
              #   600501, 600801, 601001, 601201, 601301, 601501, 601601, 601701,
              #IR0201, IR0202, IR0203, IR0204, IR0205 (some codes are repeated)
              600101, #also: IR0101 - ENERGIA ELETRICA (KWH) - EAIR0201
              600301, #vulnerability: IR0201
              700101, 700102, #vulnerability: EAIR0601
              700301,
              700401, #vulnerability: EAIR0701
              700501,  #vulnerability: EAIR0801
              700601, #vulnerability: EAIR0901
              700701, #vulnerability: EAIR1001
              700801,  #vulnerability: EAIR1101
              700901,
              701001, 701002, #vulnerability: EAIR0601
              701101
            )
          ) %>%
          dplyr::select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL,
                        V9005, V8000_DEFLA) |>
          #garantee a unique value per UC – there are variations due to SEQ
          dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) |>
          #(!) to-do: It has not yet been validated whether this is the correct calculation
          dplyr::summarise(
            V9005 = mean(V9005, na.rm = T),
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = T)
          ) |>
          dplyr::ungroup() |>
          #to identify the origen of the V9001 information: DC = DESPESA_COLETIVA
          dplyr::mutate(V9001 = paste0("DC_V9001_", V9001)) |>
          dplyr::mutate(INDEX = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = c(INDEX, V9005, V8000_DEFLA),
                             values_fill = 0,
                             names_glue = "{V9001}_{.value}",
                             names_vary = "slowest") |>
          #among the information ending in "_V9005", keep only DC_V9001_600101_V9005 (EAIR0201)
          dplyr::select(-matches("_V9005$"), matches("_600101_"))
        ## Inventário -----------------------------------------------------------
      } else if (pof_tables_names == "Inventário"){
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, QUADRO, SEQ,
            #Código do tipo de bem durável
            # filters will be applied in sequence
            V9001,
            # #Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::filter(
            V9001 %in% c(
              #IP0101
              '1400201', '1400301', '1400401',
              #IP0201 / IP0202
              '1400101', #-- foi mantido do indicador IP0201 e criado uma versão sem ele: IP0202,
              '1400901', '1401001',
              #IP0401
              '1402201',
              #IP0402
              '1402101',
              #IP0501
              '1401301', '1401401', #IP0501+ teste + add radio #'1401601'
              #IP0502
              '1401901', '1402001',
              #IP0601
              '1401201',
              #IT0201
              '1403001', '1403101'
              # #IP0301 -- foi excluido  em 19/09/24,
              # '1400501'
            )
          ) %>%
          dplyr::select(-SEQ, -QUADRO) |>
          dplyr::select(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL ) |>
          dplyr::mutate(V9001 = paste0("INV_V9001_", V9001)) |>
          dplyr::mutate(n = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = n,
                             values_fill = 0)
        ## Rendimento do Trabalho ------------------------------------------------
      } else if (pof_tables_names == "Rendimento do Trabalho"){
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, QUADRO, SEQ,
            #Tipo de trabalho do informante: 1 – Trabalho Principal / 2 – Outro Trabalho
            SUB_QUADRO,
            # Código do tipo de rendimento
            V9001,
            # [DC0501] Nesse trabalho, _____ era: 1 – Trabalhador Doméstico, 2 – Militar, etc
            V5302,
            # Valor em reais (R$) do rendimento bruto – não determinados:9999999
            #V8500,
            # Identifica o número utilizado para anualizar os rendimentos e deduções,
            # em função do período de referência definido para coleta.
            # Neste registro, o fator utilizado é sempre igual a 1
            FATOR_ANUALIZACAO,
            # #Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::filter(SUB_QUADRO == 1) |>
          dplyr::group_by(UF, COD_UPA, NUM_DOM, NUM_UC,
                          #COD_INFORMANTE,
                          PESO_FINAL) |>
          #não sei qual seria a melhor regra para selecionar aqui
          dplyr::summarise(V5302 = first(V5302))
        ## Domicílio ------------------------------------------------------------
      } else if (pof_tables_names == "Domicílio"){
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM,
            # [DD0201] # Situação do Domicílio: 1 – Urbano / 2 – Rural
            TIPO_SITUACAO_REG,
            # [DD0101] # Este domicílio é do tipo:
            V0201,
            # [DD0402] Presença de telhado, paredes e piso do domicílios apropriados
            V0202,
            V0203,
            V0204,
            #A água canalizada deste domicílio é aquecida por...
            # [DD0401], [IA0105] =ok, e [IP0301] que estava indicado mas não foi utilizado
            V0209, #adicionado para "explicar" os NAs das V02101,V02102 e V02103
            V02101,
            V02102,
            V02103, #tb usado para [DD0701]
            #[DD0406] De que forma é feito o escoadouro deste(s) banheiro(s), sanitário(s) ou buraco(s) para dejeções?
            V02111,
            # # [DD0403] Com ou sem algum problemas além dos problemas estruturais (muitos NA)
            V02113,
            #[DD0406] De que forma é feito o escoadouro deste(s) banheiro(s), sanitário(s) ou buraco(s) para dejeções?
            V0212,
            #[DD0404] Problemas de destino de lixo
            V0213,

            # [IA0101] #A energia elétrica utilizada neste domicílio é...
            V02141, #tb usado para [DD0702]
            V02142, #tb usado para [DD0702]
            # [IA0203] # Carência de acesso à eletricidade proveniente da rede geral em tempo integral
            # foi usada temporariamente para [IA0202], mas será um novo indicador IA0203
            V0215,
            # [IA0103]-[IA0104] #Na preparação dos alimentos é utilizado...
            V02161,
            V02162, #Lenha ou carvão
            V02163, #energia elétrica
            V02164, #Outro combustível (óleo, querosene, etc.)
            # [DD0301] #Domicilio próprio/aligado/cedido
            V0217,
            # [DD0501] #Existe pavimentação na rua onde se localiza este domicílio?
            V0220,
            #[DS0301] Dificuldade alimentar/financeira  (não incluindo energia)/para conseguir a lenha ou carvão
            V6199,
            # #Expansion factor
            PESO_FINAL
          )
        ## Outros Rendimentos ----------------------------------------------------
      } else if (pof_tables_names == "Outros Rendimentos"){
        pof_data <- pof_data %>%
          dplyr::select(
            # #DataKeys
            UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, QUADRO, SEQ,
            # [DS0201] Políticas públicas/Programas sociais
            # Código do tipo de rendimento
            V9001,
            # Identifica o número utilizado para anualizar os rendimentos e deduções,
            # em função do período de referência definido para coleta.
            # Neste registro, o fator utilizado é sempre igual a 1
            FATOR_ANUALIZACAO,
            # #Expansion factor
            PESO_FINAL
          ) %>%
          dplyr::filter(V9001 %in% c('5400101', '5400102', '5400103', '5400104',
                                     '5400105', '5400106', '5400107', '5400108',
                                     '5400201', '5400301', '5400302', '5400303',
                                     '5400304', '5400305', '5400306', '5400307',
                                     '5400308', '5400309', '5400310', '5400311',
                                     '5400312', '5400313', '5400314', '5400315',
                                     '5400316', '5400317', '5402101', '5402201',
                                     '5402301')) |>
          #garantee a unique value per UC – there are variations due to COD_INFORMANTE/SEQ
          dplyr::distinct(UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) |>
          dplyr::mutate(V9001 = paste0("OR_V9001_", V9001)) |>
          dplyr::mutate(n = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = n,
                             values_fill = 0)

      }


    } #End of 2018
    ## If POF year is 2009 --------------------------------------------------------
    if(pof_year == 2009){
      ### Condições de vida ---------------------------------------------------------
      if (pof_tables_names == "Condições de vida ") {
        pof_data <- pof_data %>%
          dplyr::select(
            TIPO_SITUACAO_REG,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            V61071,
            V61075,
            V61073,
            V61072,
            V610710,
            V61074,
            V610711,
            V61076,
            V61081,
            V61083,
            V61084,
            V61085,
            V61086,
            V6109,
            V61089,
            V61088,
            PESO_FINAL,
            RENDA_TOTAL,
            V6104, # correspondencia proxima a V6199 mas não tem as mesmas dimensões
            V61075 # correspondencia proxima a V0215 mas não tem as mesmas dimensões
          ) %>%
          dplyr::rename(COD_INFORMANTE_RESP_COND_VIDA = COD_INFORMANTE) %>%
          dplyr::mutate(UF = stringr::str_sub(COD_UPA, 1, 2) %>% as.numeric(),
                        across(c(COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL), as.numeric))
        ## Morador ------------------------------------------------------------------
      } else if (pof_tables_names == "Morador") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            TIPO_SITUACAO_REG,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            COND_UNIDADE_CONSUMO,
            # Cond familia adicionado pq cond_unidade_consumo valor NA
            COND_FAMILIA,
            IDADE_ANOS,
            V0405,
            V0429,
            PESO_FINAL,
            RENDA_TOTAL,
            RENDA_PERCAPITA,
            NIVEL_INSTRUCAO_MORADOR,
            V0438 # corresponde a V0406 da tabela de morador
          ) %>%
          dplyr::mutate(UF = stringr::str_sub(COD_UPA, 1, 2) %>% as.numeric()) %>%
          #add NUMERO_PESSOAS_DOMICILIO
          #dplyr::filter(COND_UNIDADE_CONSUMO ==1) %>%
          dplyr::left_join(pof_data |>
                      dplyr::mutate(
                        # [DF0101]
                        V0403_ate12 = ifelse(IDADE_ANOS <= 12, 1, 0),
                        # [DF0102]
                        V0403_13a17 = ifelse(((IDADE_ANOS >= 13) & (IDADE_ANOS <= 17)), 1, 0),
                        # [DF0103]
                        V0403_18a49 = ifelse(((IDADE_ANOS >= 18) & (IDADE_ANOS <= 49)), 1, 0),
                        # [DF0104]
                        V0403_50a64 = ifelse(((IDADE_ANOS >= 50) & (IDADE_ANOS)), 1, 0),
                        # [DF0105]
                        V0403_65mais = ifelse(IDADE_ANOS >= 65, 1, 0),
                        # [DS0401]
                        V0438_soma = ifelse(V0438 == 1, 1, 0)) |>
                      dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC) |>
                      dplyr::summarise(
                        # [DF0101]
                        V0403_ate12 = sum(V0403_ate12),
                        # [DF0102]
                        V0403_13a17 = sum(V0403_13a17),
                        # [DF0103]
                        V0403_18a49 = sum(V0403_18a49),
                        # [DF0104]
                        V0403_50a64 = sum(V0403_50a64),
                        # [DF0105]
                        V0403_65mais = sum(V0403_65mais),
                        # Saíde
                        V0438_soma = sum(V0438_soma)
                      ),
                    by = join_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC)) %>%
          dplyr::filter(COND_UNIDADE_CONSUMO ==1) %>%

          #[DF0301] =  NUMERO_PESSOAS_DOMICILIO
          dplyr::left_join(pof_data |>
                      dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC) |>
                      dplyr::summarise(NUMERO_PESSOAS_DOMICILIO = n_distinct(COD_INFORMANTE)),
                    by = join_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC)) |>
          #add QTD_PESSOAS_PER_FAMILIA (without Domestic employee and relatives)
          #diferença de apenas 55 linhas
          dplyr::left_join(pof_data |>
                      #dplyr::filter(!(COND_UNIDADE_CONSUMO %in% c(18,19))) |>
                      dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC) |>
                      dplyr::summarise(NUMERO_PESSOAS_FAMILIA = n_distinct(COD_INFORMANTE)),
                    by = join_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC))

        ### Morador - Qualidade de vida ---------------------------------------------
      }else if (pof_tables_names == "Morador - Qualidade de Vida ") {
        pof_data <- pof_data %>%
          dplyr::select(
            TIPO_SITUACAO_REG,
            UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            V301,
            V607,
            PESO_FINAL
          ) %>%
          dplyr::select(-COD_INFORMANTE,-TIPO_SITUACAO_REG) %>%
          tidyr::drop_na(NUM_UC) %>%
          dplyr::distinct(COD_UPA, NUM_DOM, NUM_UC, .keep_all = TRUE)

        ### Despesa Individual ------------------------------------------------------
      } else if (pof_tables_names == "Despesa Individual") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            TIPO_SITUACAO_REG,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            NUM_QUADRO,
            SEQ_LINHA,
            FATOR_ANUALIZACAO,
            V9001,
            V8000,
            V8000_DEFLA,
            PESO_FINAL,
            RENDA_TOTAL
          ) %>%
          dplyr::filter(V9001 %in% c(
            # DS0401
            2999901, 4299901,
            # IT0101, IT0103
            2300701, 2302801, 2302802, 2300601, 2301301, 2301801, 2301701,
            # IT0102, IT0104
            2300101, 2300201, 2300301, 2300302, 2300401, 2300501, 2300502, 2300503,
            2300504, 2300505, 2300506, 2300507, 2300508, 2300509, 2300510, 2301101,
            2301401, 2301601, 2302001, 2302101, 2302201, 2302302, 2302303, 2302304,
            2302305, 2302306, 2302307, 2302309, 2302601, 2302602, 2302301
          )) %>%
          dplyr::select(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, NUM_QUADRO, SEQ_LINHA, PESO_FINAL, V8000_DEFLA, RENDA_TOTAL) |>
          #garantee a unique value per UC – there are variations due to COD_INFORMANTE, QUADRO, SEQ
          dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, NUM_QUADRO, SEQ_LINHA, PESO_FINAL) |>
          #(!) to-do: It has not yet been validated whether this is the correct calculation
          dplyr::summarise(
            V8000_DEFLA = sum(V8000_DEFLA, na.rm = T),
            RENDA_TOTAL = mean(RENDA_TOTAL)
          ) |>
          dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) |>
          #(!) to-do: It has not yet been validated whether this is the correct calculation
          dplyr::summarise(
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = T),
            RENDA_TOTAL = mean(RENDA_TOTAL)
          ) |>
          dplyr::ungroup() |>
          #to identify the origen of the V9001 information: DC = DESPESA_COLETIVA
          dplyr::mutate(V9001 = paste0("DI_V9001_", V9001)) |>
          dplyr::mutate(INDEX = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = c(INDEX, V8000_DEFLA),
                             values_fill = 0,
                             names_glue = "{V9001}_{.value}",
                             names_vary = "slowest") %>%
          dplyr::mutate(
            # [IT0101] - cálculos prévios: soma de transporte privado
            DI_somaDesp_transpPrivado = DI_V9001_2300701_V8000_DEFLA +
              DI_V9001_2302801_V8000_DEFLA + DI_V9001_2302802_V8000_DEFLA +
              DI_V9001_2300601_V8000_DEFLA + DI_V9001_2301301_V8000_DEFLA +
              DI_V9001_2301801_V8000_DEFLA + DI_V9001_2301701_V8000_DEFLA,
            # [IT0102] - cálculos prévios: soma de transporte público
            DI_somaDesp_transpPublico =  DI_V9001_2300101_V8000_DEFLA +
              DI_V9001_2300201_V8000_DEFLA + DI_V9001_2300301_V8000_DEFLA +
              DI_V9001_2300302_V8000_DEFLA + DI_V9001_2300401_V8000_DEFLA +
              DI_V9001_2300501_V8000_DEFLA + DI_V9001_2300502_V8000_DEFLA +
              DI_V9001_2300504_V8000_DEFLA + DI_V9001_2300505_V8000_DEFLA +
              DI_V9001_2300506_V8000_DEFLA + DI_V9001_2300508_V8000_DEFLA +
              DI_V9001_2300509_V8000_DEFLA + DI_V9001_2301101_V8000_DEFLA +
              DI_V9001_2301601_V8000_DEFLA +
              DI_V9001_2302001_V8000_DEFLA + DI_V9001_2302101_V8000_DEFLA +
              DI_V9001_2302201_V8000_DEFLA + DI_V9001_2302302_V8000_DEFLA +
              DI_V9001_2302303_V8000_DEFLA + DI_V9001_2302304_V8000_DEFLA +
              DI_V9001_2302305_V8000_DEFLA + DI_V9001_2302306_V8000_DEFLA +
              DI_V9001_2302307_V8000_DEFLA + DI_V9001_2302309_V8000_DEFLA +
              DI_V9001_2302601_V8000_DEFLA + DI_V9001_2302602_V8000_DEFLA +
              DI_V9001_2999901_V8000_DEFLA + DI_V9001_4299901_V8000_DEFLA
            # Nao são formados por não existir casos
            #DI_V9001_2300507_V8000_DEFLA - CATAMARA
            #DI_V9001_2300503_V8000_DEFLA - BOTE
            #DI_V9001_2300510_V8000_DEFLA - BALSA
            #DI_V9001_2301401_V8000_DEFLA - BONDE, BONDINHO, ETC.
            #DI_V9001_2302301_V8000_DEFLA - Transporte Alternativo
          )
        # Finalizando cálculos previos para IT01
        pof_data <- pof_data %>%
          #[IT0101] - cálculos prévios: mediana por UF
          dplyr::left_join(pof_data |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::filter(DI_somaDesp_transpPrivado > 0) |>
                      dplyr::summarise(DI_somaDesp_transpPrivado_mdUF = median(DI_somaDesp_transpPrivado)),
                    by = join_by(COD_UF)) |>
          #[IT0101] + cálculos prévios: mediana BR)
          dplyr::bind_cols( pof_data |>
                       dplyr::filter(DI_somaDesp_transpPrivado > 0) |>
                       dplyr::summarise(DI_somaDesp_transpPrivado_mdBR = median(DI_somaDesp_transpPrivado))) |>
          #[IT0102] - cálculos prévios: mediana por UF
          dplyr::left_join(pof_data |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::filter(DI_somaDesp_transpPublico > 0) |>
                      dplyr::summarise(DI_somaDesp_transpPublico_mdUF = median(DI_somaDesp_transpPublico)),
                    by = join_by(COD_UF)) |>
          #[IT0102] + cálculos prévios: mediana BR)
          dplyr::bind_cols( pof_data |>
                       dplyr::filter(DI_somaDesp_transpPublico > 0) |>
                       dplyr::summarise(DI_somaDesp_transpPublico_mdBR = median(DI_somaDesp_transpPublico))) |>
          #[IT0103]- cálculos prévios : mediana por estado da proporção por renda
          dplyr::left_join(pof_data |>
                      dplyr::filter(DI_somaDesp_transpPrivado > 0) |>
                      dplyr::mutate(prop = DI_somaDesp_transpPrivado/RENDA_TOTAL) |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::summarise(DI_somaDesp_transpPrivado_byRenda_mdUF = median(prop, na.rm = T)),
                    by = join_by(COD_UF)) |>
          #[IT0104]- cálculos prévios : mediana por estado da proporção por renda
          dplyr::left_join(pof_data |>
                      dplyr::filter(DI_somaDesp_transpPublico > 0) |>
                      dplyr::mutate(prop = DI_somaDesp_transpPublico/RENDA_TOTAL) |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::summarise(DI_somaDesp_transpPublico_byRenda_mdUF = median(prop, na.rm = T)),
                    by = join_by(COD_UF))
        ### Desesa de 90 dias --------------------------------------------------------
      } else if (pof_tables_names == "Despesas de 90 dias") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            NUM_QUADRO,
            SEQ_LINHA,
            FATOR_ANUALIZACAO, # 4 ou 12
            V9001,
            V9005,
            V8000_DEFLA,
            RENDA_TOTAL,
            PESO_FINAL
          ) %>%
          dplyr::filter(V9001 %in% c(
            #IR0101
            600201,
            #IR0201-IR0205
            600301, 700101, 700103, 700501, 700701, 700801, 701001,
            701101, 700901, 700301, 700403, 700401, 700601,
            #IP0503
            601101, 601301, 600801,
            #IP0504
            600501, 600502, 600503, 600504, 600505, 600506, 600901, 601001, 600801,
            601401
          )) %>%
          dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) %>%
          dplyr::summarise(
            V9005 = mean(V9005, na.rm = TRUE),
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = TRUE),
            RENDA_TOTAL = mean(RENDA_TOTAL)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(V9001 = paste0("DC_V9001_", V9001)) %>%
          dplyr::mutate(INDEX = 1) %>%
          tidyr::pivot_wider(
            names_from = V9001,
            values_from = c(INDEX, V9005, V8000_DEFLA),
            values_fill = 0,
            names_glue = "{V9001}_{.value}",
            names_vary = "slowest"
          ) %>%
          dplyr::select(-matches("_V9005$"), matches("_600201_"))

        # Cálculos prévios
        pof_data <- pof_data %>%
          dplyr::left_join(
            pof_data |>
              dplyr::group_by(COD_UF) |>
              dplyr::summarise(DC_V9001_600201_V9005_mdUF = median(DC_V9001_600201_V9005)),
            by = join_by(COD_UF)) |>
          # [IR0101] + cálculos prévios: mediana BR)
          dplyr::bind_cols(
            pof_data |>
              dplyr::summarise(DC_V9001_600201_V9005_mdBR = median(DC_V9001_600201_V9005))
          ) |>
          # [IR0201], [IR0202], [IR0203], [IR0204] - cálculos prévios:  soma de produtos
          dplyr::mutate(
            DC_V9001_somaDesp =
              DC_V9001_600201_V8000_DEFLA + DC_V9001_600301_V8000_DEFLA +
              DC_V9001_700101_V8000_DEFLA + DC_V9001_700103_V8000_DEFLA +
              DC_V9001_700501_V8000_DEFLA + DC_V9001_700701_V8000_DEFLA +
              DC_V9001_700801_V8000_DEFLA + DC_V9001_701001_V8000_DEFLA +
              DC_V9001_701101_V8000_DEFLA + DC_V9001_700901_V8000_DEFLA +
              DC_V9001_700301_V8000_DEFLA + DC_V9001_700403_V8000_DEFLA +
              DC_V9001_700401_V8000_DEFLA + DC_V9001_700601_V8000_DEFLA)

        ## Calculos finais
        pof_data <- pof_data %>%
          #IR0201, IR0202, IR0203, IR0204 - cálculos prévios: mediana por estado
          dplyr::left_join(pof_data |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::summarise(DC_V9001_somaDesp_mdUF = median(DC_V9001_somaDesp)),
                    by = join_by(COD_UF)) |>
          # [IR0101] + cálculos prévios: mediana BR)
          dplyr::bind_cols(
            pof_data |>
              dplyr::summarise(DC_V9001_somaDesp_mdBR = median(DC_V9001_somaDesp))) |>
          #[IR0205]- cálculos prévios : mediana por estado da proporção por renda
          dplyr::left_join(pof_data |>
                      dplyr::mutate(prop = DC_V9001_somaDesp/RENDA_TOTAL) |>
                      dplyr::group_by(COD_UF) |>
                      dplyr::summarise(DC_V9001_somaDesp_byRenda_mdUF = median(prop, na.rm = T)),
                    by = join_by(COD_UF))
        ### Inventário --------------------------------------------------------------
      } else if (pof_tables_names == "Inventário") {
        pof_data <- purrr::map(
          pof_data %>%
            dplyr::select(
              COD_UF,
              TIPO_SITUACAO_REG,
              COD_UPA,
              NUM_DOM,
              NUM_UC,
              NUM_QUADRO,
              SEQ_LINHA,
              V9001,
              V9005,
              PESO_FINAL,
              RENDA_TOTAL
            ) %>%
            dplyr::filter(V9001 %in% c(
              '1400101', '1400201', '1400301', '1400401', '1400501', '1402601',
              '1401001', '1401201', '1401301', '1401401', '1401701', '1402101',
              '1402301', '1402401', '1401801'
            )) %>%
            dplyr::count(V9001) %>%
            dplyr::pull(V9001),
          ~pof_data %>%
            dplyr::filter(V9001 == .x) %>%
            dplyr::select(-SEQ_LINHA, -NUM_QUADRO) %>%
            dplyr::mutate(V9001 = paste0("INV_V9001_", V9001),
                          UF = COD_UF
            ) %>%
            dplyr::mutate(temp = 1) %>%
            tidyr::pivot_wider(names_from = V9001, values_from = temp, values_fill = 0) %>%
            dplyr::select(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL, last_col())

        ) %>%
          reduce(dplyr::left_join, by = c("UF", "COD_UPA", "NUM_DOM", "NUM_UC", "PESO_FINAL"))


        ### Rendimentos do trabalho -------------------------------------------------
      } else if (pof_tables_names == "Rendimentos do trabalho") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            TIPO_SITUACAO_REG,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            NUM_QUADRO,
            COD_TIPO_OCUP,
            SEQ_LINHA,
            V9001,
            V5303,
            V8500,
            PESO_FINAL,
            RENDA_TOTAL
          ) %>%
          dplyr::filter(COD_TIPO_OCUP == 1) %>%
          dplyr::mutate(UF = stringr::str_sub(COD_UPA, 1, 2) %>% as.numeric()) %>%
          dplyr::select(UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, V5303, PESO_FINAL)

        ### Domicílio ---------------------------------------------------------------
      } else if (pof_tables_names == "Domicílio") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            TIPO_SITUACAO_REG,
            COD_UPA,
            NUM_DOM,
            V0202,

            V0203,
            V0204,
            V0205,
            V02171,
            V02172,

            V02173,
            V0210,
            V0211,
            V0213,
            V02151,
            V02152,
            V02162,
            V02163,
            V02164,
            V02173,
            V02181,

            V02182,
            V02183,
            V02184,
            V02185,
            V0219,

            V0209,
            V0208,
            V02151, # Carência de acesso à eletricidade.
            V0223, # Pavimentacao na rua
            PESO_FINAL
          ) %>%
          dplyr::mutate(UF = stringr::str_sub(COD_UPA, 1, 2) %>% as.numeric())
        ### Outros Rendimentos ------------------------------------------------------
      } else if (pof_tables_names == "Outros Rendimentos") {
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            COD_INFORMANTE,
            NUM_QUADRO,
            SEQ_LINHA,
            V9001,
            PESO_FINAL
          ) %>%
          dplyr::distinct(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) |>
          dplyr::mutate(V9001 = paste0("OR_V9001_", V9001)) |>
          dplyr::mutate(n = 1) |>
          tidyr::pivot_wider(names_from = V9001,
                             values_from = n,
                             values_fill = 0)
        ### Outras Despesas ---------------------------------------------------------
      } else if (pof_tables_names == "Outras Despesas"){
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            NUM_QUADRO,
            SEQ_LINHA,
            FATOR_ANUALIZACAO,
            V9001,
            V8000_DEFLA,
            RENDA_TOTAL,
            PESO_FINAL
          ) %>%
          dplyr::filter(V9001 == "1504001") %>%
          dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) %>%
          dplyr::summarise(
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = TRUE)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(V9001 = paste0("DC_V9001_", V9001)) %>%
          dplyr::mutate(INDEX = 1) %>%
          tidyr::pivot_wider(
            names_from = V9001,
            values_from = c(INDEX, V8000_DEFLA),
            values_fill = 0,
            names_glue = "{V9001}_{.value}",
            names_vary = "slowest"
          )
        ### Despesas de 12 meses ----------------------------------------------------
      } else if (pof_tables_names == "Despesas de 12 meses"){
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            NUM_QUADRO,
            SEQ_LINHA,
            V9001,
            V9002,
            V9010,
            V9011,
            FATOR_ANUALIZACAO,
            V8000,
            V8000_DEFLA,
            RENDA_TOTAL,
            PESO_FINAL
          ) %>%
          dplyr::filter(V9001 %in% c(1000101, 1000201, 1000301, 1000401, 1000501, 1000502,
                                     1000601, 1000603, 1000701, 1000702, 1000801, 1000901, 1001001,
                                     1001002, 1001003, 1001101, 1001102, 1001201, 1001301, 1001401)
          ) %>%
          dplyr::group_by(COD_UF, COD_UPA, NUM_DOM, NUM_UC, V9001, PESO_FINAL) %>%
          dplyr::summarise(
            V8000_DEFLA = mean(V8000_DEFLA, na.rm = TRUE)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(V9001 = paste0("DC_V9001_", V9001)) %>%
          dplyr::mutate(INDEX = 1) %>%
          tidyr::pivot_wider(
            names_from = V9001,
            values_from = c(INDEX, V8000_DEFLA),
            values_fill = 0,
            names_glue = "{V9001}_{.value}",
            names_vary = "slowest"
          )
        ###  Aluguel Estimado ----------------------------------------------------
      } else if (pof_tables_names == "Aluguel Estimado"){
        pof_data <- pof_data %>%
          dplyr::select(
            COD_UF,
            COD_UPA,
            NUM_DOM,
            NUM_UC,
            V8000_DEFLA,
            PESO_FINAL
          )

      }
      pof_data
    }
    pof_data
  }
  ## Applying Data Transformation Function for each table of POF ---------------
  data <- purrr::map2(
    pof_data,
    names(pof_data),
    ~pof_transformation(.x, .y, pof_year)
  )

  # Creating DataFrame POF by UC ------------------------------------------------
  ## For POF 2018
  if(pof_year == 2018){
    data_transformed <- data$Morador %>%
      # Add table Condições de Vida
      dplyr::left_join(data$`Condições de Vida`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      # Add table Morador - Qualidade de Vida
      dplyr::left_join(data$`Morador - Qualidade de Vida`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      # Add table Despesa individual
      dplyr::left_join(data$`Despesa Individual`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(
        dplyr::across(contains("DI_V9001_"), ~tidyr::replace_na(.x,0)),
        dplyr::across(contains("DI_soma"), ~tidyr::replace_na(.x,0))
      ) %>%
      # Add table Despesa Coletiva
      dplyr::left_join(data$`Despesa Coletiva`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(
        dplyr::across(contains("DC_V9001"), ~tidyr::replace_na(.x,0))
      ) %>%
      # Add table Inventário
      dplyr::left_join(data$Inventário, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(
        dplyr::across(contains("INV_V9001_"), ~tidyr::replace_na(.x,0))
      ) %>%
      # Add table Rendimentos do Trabalho
      dplyr::left_join(data$`Rendimento do Trabalho`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)
      ) %>%
      # Add table Outros Rendimentos
      dplyr::left_join(data$`Outros Rendimentos`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(
        dplyr::across(contains("OR_V9001"), ~tidyr::replace_na(.x,0))
      ) %>%
      # Add Table Domicílio
      dplyr::left_join(data$Domicílio, by = join_by(UF, COD_UPA, NUM_DOM, PESO_FINAL)) %>%
      dplyr::mutate(dplyr::across(c(V0215, V02101, V02102, V02103), ~tidyr::replace_na(.x, 0)))

  }

  ## For POF 2009
  if(pof_year == 2009){

    data_transformed <- data$Morador %>%
      # Add table Condições de Vida
      dplyr::left_join(data$`Condições de vida `, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL),
                       suffix = c("_morador", "_condicoes_vida")) %>%
      # Add table Morador - Qualidade de Vida
      dplyr::left_join(data$`Morador - Qualidade de Vida `, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      # Add table Despesa individual
      dplyr::left_join(data$`Despesa Individual`, by = join_by(UF == COD_UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(dplyr::across(contains("DI_V9001_"), ~tidyr::replace_na(.x,0)),
                    dplyr::across(contains("DI_soma"), ~tidyr::replace_na(.x,0)),
                    RENDA_TOTAL = RENDA_TOTAL %>% tidyr::replace_na(0)) %>%
      # Add table Despesa de 90 dias
      dplyr::left_join(data$`Despesas de 90 dias`,  join_by(UF == COD_UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL, RENDA_TOTAL)) %>%
      dplyr::mutate(dplyr::across(contains("V9005_"), ~tidyr::replace_na(.x, 0)),
                    dplyr::across(contains("V8000_"), ~tidyr::replace_na(.x, 0)),
                    dplyr::across(contains("DC_V9001"), ~tidyr::replace_na(.x, 0))) %>%
      # Add table Inventário
      dplyr::left_join(data$Inventário, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(dplyr::across(contains("INV_V9001_"), ~tidyr::replace_na(.x, 0))) %>%
      # Add table Rendimentos do Trabalho
      dplyr::left_join(data$`Rendimentos do trabalho`, by = join_by(UF, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, PESO_FINAL)
      ) %>%
      # Add table Domicílio
      dplyr::left_join(data$Domicílio, by = join_by(UF, COD_UPA, NUM_DOM, PESO_FINAL),
                       suffix = c("", "_morador")) %>%
      # Add table Outros Rendimentos
      dplyr::left_join(data$`Outros Rendimentos`, by = join_by(UF == COD_UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(dplyr::across(contains("OR_V9001"), ~tidyr::replace_na(.x,0))) %>%
      # Add table Outras Despesas
      dplyr::left_join(data$`Outras Despesas`, join_by(UF == COD_UF, COD_UPA, NUM_DOM, NUM_UC, PESO_FINAL)) %>%
      dplyr::mutate(dplyr::across(contains("DC_V9001"), ~tidyr::replace_na(.x,0))) %>%
      dplyr::select(-COD_UF) %>%
      dplyr::relocate(UF)
  }

  data_transformed
}

