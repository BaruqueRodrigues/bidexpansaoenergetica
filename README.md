
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bidexpansaoenergetica

bidexpansaoenergetica é um pacote em R destinadas a facilitar o
download, preparação e análise de dados provenientes de pesquisas do
IBGE, incluindo a Pesquisa de Orçamentos Familiares (POF), a Pesquisa
Nacional por Amostra de Domicílios Contínua (PNADc) e os dados agregados
preliminares do Censo Demográfico 2022.

## Instalação

Você pode instalar a versão de desenvolvimento do bidexpansaoenergetica
diretamente do GitHub com:

``` r
# install.packages("devtools")
devtools::install_github("BaruqueRodrigues/bidexpansaoenergetica")
```

## Funções Principais

### POF

`baixa_pof()`: Baixa os dados da POF para os anos de 2018 e 2009.

``` r
baixa_pof(destination_dir = 'data-raw/POF/')
```

`constroi_pof()`: Constrói um dataset consolidado com os dados da POF
para o ano especifico.

``` r
constroi_pof(diretorio = 'data-raw/POF/2018/')
```

### PNADc

`baixa_visita_5_pnadc()`: Baixa os dados da 5ª visita da PNADc para um
ano específico.

``` r
baixa_visita_5_pnadc(ano = 2016,
                     destination_dir = "data-raw/pnadc/2016/")
```

`baixa_visita_1_pnadc()`: Baixa os dados da 1ª visita da PNADc para um
ano específico.

``` r
baixa_visita_1_pnadc(ano = 2016,
                     destination_dir = "data-raw/pnadc/2016/")
```

`constroi_pnad()`: Combina os dados da 1ª e 5ª visita da PNADc,
preparando-os para análise.

``` r
constroi_pnad(ano = 2016,
                     destination_dir = "data-raw/pnadc/2016/")
```

### Censo 2022

`baixa_censo()`: Baixa e prepara os dados agregados preliminares do
Censo Demográfico 2022 do IBGE. Uso Aqui está um exemplo básico de como
usar baixa_censo():

``` r
 library(bidexpansaoenergetica) 
dados_censo <- baixa_censo(destination_dir = 'data-raw/censo-ibge-2022/') 

dplyr::glimpse(dados_censo) 
```

Para mais exemplos e detalhes sobre cada função, consulte a documentação
interna do pacote.

Contribuições Contribuições são bem-vindas! Por favor, leia
CONTRIBUTING.md para mais detalhes e envie pull requests para nós.

Licença Este pacote é distribuído sob a licença MIT. Veja o arquivo
LICENSE para mais detalhes.

Autores Seu Nome (<a href="mailto:e-mail@exemplo.com"
class="email">quemuel.baruque@ufpe.br</a>)
