Este código implementa e testa a função creates_longer_dataset, 
que processa arquivos .rds contendo dados de indicadores para gerar 
conjuntos de dados no formato "long" e exportá-los em formatos 
.rds e .csv. A função é configurada para permitir o uso de uma 
função personalizada (create_indicator_data_function) para 
criar e manipular os dados, além de lidar com um conjunto de 
indicadores organizados em uma lista nomeada.

1. Criação de diretórios: Certifica-se de que os diretórios de exportação são criados corretamente.

2. Processamento de indicadores: Garante que os arquivos .rds sejam processados e os resultados exportados.

3. Casos de indicadores ausentes: Emite avisos quando indicadores não são definidos para alguma base de dados.

4. Validação de nomes de arquivos: Confirma que os arquivos de saída possuem nomes esperados, incluindo detalhes sobre o número de indicadores e determinantes.

5. Concatenação de dados: Verifica se todos os dados processados são combinados em um único arquivo .rds.

6. Erros em entradas inválidas:

7. Lista de indicadores vazia.

8. lista_indicadores sem nomes apropriados.