O código implementa testes automatizados para a função 
constroi_pnad_tabelas, que processa microdados da Pesquisa 
Nacional por Amostra de Domicílios Contínua (PNADC) de um 
determinado ano. Os testes usam o pacote testthat e cobrem 
diferentes aspectos do funcionamento da função, como 
integridade, dependências e integração de dados.

1. Processamento Correto dos Dados para 2016:

O teste verifica se a função processa corretamente os dados de 2016, a partir dos arquivos das visitas 1 e 5.
Garante que o resultado é um objeto do tipo tbl_df (tibble).
Certifica-se de que o dataset final contém todas as variáveis esperadas listadas em bidexpansaoenergetica::variaveis_pnadc.

2. Erros para Arquivos Ausentes:

Simula um cenário em que os arquivos das visitas 1 e 5 não estão disponíveis no diretório.
Valida que a função lança um erro apropriado indicando a ausência dos arquivos necessários.

3. Integração de Dados das Visitas 1 e 5:

Verifica se os microdados das duas visitas são corretamente combinados em um único dataset.
Garante que:
A coluna de identificação da visita ("visita") está presente no dataset final.
Os valores na coluna "visita" incluem apenas 1 e 5.
Certifica-se de que o dataset final contém dados e todas as variáveis esperadas.