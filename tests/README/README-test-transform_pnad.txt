Este código realiza uma série de testes unitários sobre 
a função transform_pnad, que transforma dados da PNAD 
(Pesquisa Nacional por Amostra de Domicílios) para análise, 
garantindo que os dados estejam no formato esperado e lidem 
corretamente com valores ausentes. A função é testada em 
diferentes cenários para verificar sua robustez.

1. Validação da classe e colunas:
A função retorna um data frame com as colunas esperadas.

2. Tratamento de valores ausentes:
Variáveis com valores ausentes, como VD5010 e VD4008, são substituídas com valores apropriados (ex.: 0 ou "Aposentado ou Desempregado").

3. Cálculos e transformações:
Verifica se os índices de idade (V2009_index_*) são calculados corretamente e se as variáveis são transformadas conforme esperado.

4. Filtragem de dados:
Garantia de que apenas os responsáveis pelo domicílio são mantidos no conjunto de dados (filtrando quem tem a variável V2005 como "Pessoa responsável pelo domicílio").

5. Manipulação de diferentes anos:
A função lida com dados de diferentes anos, ajustando as colunas conforme necessário (por exemplo, removendo a variável S01016A para o ano de 2016).

6. Cálculos de contagem de pessoas:
A variável NUMERO_PESSOAS_DOMICILIO é calculada corretamente.

7. Manutenção da integridade dos dados:
Certifica que os dados de entrada, como o código da UPA e o número do domicílio, são mantidos nas variáveis transformadas.

8. Casos de borda e dados mínimos:
Testa o comportamento da função com dados mínimos (apenas uma linha) e com valores ausentes em variáveis como S01018 e S01019.