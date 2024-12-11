O código consiste em uma bateria de testes em R 
utilizando o pacote testthat para validar a 
funcionalidade da função baixa_visita_1_pnadc, 
que é responsável por baixar arquivos 
relacionados à Pesquisa Nacional por Amostra de 
Domicílios Contínua (PNADC) para um determinado ano.

Resumo dos testes:

1. Criação de diretórios:

Garante que a função cria corretamente o diretório de destino especificado.
Se o diretório já existir, ele é removido antes do teste para garantir um ambiente limpo.

2. Construção de URLs:

Valida que as URLs geradas para os arquivos de dicionário, entrada (input) e microdados correspondem aos padrões esperados para o ano de 2018.
Faz isso verificando se strings específicas estão presentes nas URLs geradas.

3. Manipulação de entradas inválidas:

Testa o comportamento da função ao receber:
Um ano não suportado (por exemplo, 2023).
Um diretório inválido (como uma string vazia).
Em ambos os casos, espera-se que a função retorne um erro apropriado com mensagens descritivas.

4. Download de arquivos:

Garante que a função baixa corretamente os arquivos esperados para o ano de 2018.
Os arquivos baixados são comparados com uma lista de nomes esperados.