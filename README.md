# PFL - Trabalho Prático 1
O trabalho prático tem como objetivo implementar as seguintes funcções:
- ***normalizePolynomials*** - Função que normaliza polinómios
    - Casos de teste *:
        - testNormalize1
        - testNormalize2
        - testNormalize3
        - testNormalize4
- ***addPolynomials*** - Função que soma polinómios
    - Casos de teste:
        - testAdd1
        - testAdd2
        - testAdd3
        - testAdd4
- ***multiplyPolynomials*** - Função que multiplica polinómios
    - Casos de teste:
        - testMultiply1
        - testMultiply2
        - testMultiply3
        - testMultiply4
- ***derivePolynomial*** - Função que deriva polinómios
    - Casos de teste:
        - testDerive1
        - testDerive2
        - testDerive3
        - testDerive4
        
 * *Os casos de teste estão presentes no ficheiro `Polynomials.hs`. Em cada um é utilizado a respetiva função em causa, pelo que no fim é comparado o *output* das mesmas com o caso esperado. 
 
 * Para dar load às funções, deve correr, no ghci, o comando `:l Polynomials.hs` 

## Justificação da apresentação interna
A representação interna dos polinómios é feita através de uma lista de momómios. Cada monónimo (*typeclass*) é representado por 2 elementos, sendo o primeiro o coeficiente e o segundo uma lista de variáveis. Cada variável (*typeclass*) vem acompanhada com o respetivo grau. Esta divisão mostrou ser mais apropriada para elaborar as respetivas funções dos polinómios.
<br> <br>

## Estratégia geral das funções Aux.hs
As funções presentes neste ficheiro tem como objetivos:
- Fazer ***parsing*** de uma *string* para um polinómio
- Transformar o polinómio em uma *string* legível

Também estão presentes funções auxiliares para a normalização de um polinómio

<br>

## Estratégia das funções Polynomials.hs
### ***normalizePolynomial***
A função soma os coeficientes dos monómios com as mesmas variáveis e graus, remove os monómios com coeficiente 0, ordena as variáveis de cada monómio de forma decrescente de grau, ordena os monómios de forma decrescente de variável de maior grau e imprime o polinómio como *string*. Caso a *string* esteja vazia, imprime o valor `0`.

### ***addPolynomials***
A função recebe duas *strings*, concatena-as e normaliza o polinómio, através das funções utilizadas em `normalizePolynomial`. Caso ambas as *strings* estejam vazias, retorna um erro, caso contrário, imprime a *string* do polinómio resultante da soma.

### ***multiplyPolynomials***
A função multiplica dois polinómios com a ajuda das funções auxiliares `distributiveLaw`, que aplica a propriedade distributiva entre monómios, e `multMonos`, que multiplica os coeficientes e junta as variáveis de 2 monómios. Imprime a *string* do polinómio resultante da multiplicação.

### ***derivePolynomial***
Comecamos por definir uma função auxiliar `filterWithVar` que recebe o polinómio inicial e a variável para derivar e retorna um polinómio que apenas contém monómios com a variável a derivar. Depois definimos a função `calculateDerivation` que recebe o polinómio e a variável a derivar e retorna o polinómio derivado, usando uma outra função auxiliar para decrementar o grau da variável derivada. No fim, imprime a *string* do polinómio resultante da derivação.

<br><br>
Realizado pelo grupo G02_09:
- Francisco Pimentel Serra  - up202007723
- João Paulo Moreira Araújo - up202004293
