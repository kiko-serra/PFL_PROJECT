# PFL - Trabalho Prático 1
O trabalho prático tem como objetivo implementar as seguintes funcções:
- **normalizePolynomials** - Função que normaliza polinómios
    - Casos de teste:
- **addPolynomials** - Função que soma polinómios
- **multiplyPolynomials** - Função que multiplica polinómios
- **derivePolynomial** - Função que deriva polinómios


## Funções Polynomials.hs
### normalizePolynomials
Esta função soma os coeficientes dos monómios com as mesmas variaveis e graus, e remove os monómios com coeficiente 0 e retorna o polinómio como string.

### addPolynomials
Esta função soma dois polinómios com a ajuda fa cunção auxiliar 'concatenateStrings'. No caso dos polinómios serem nulos retorna um erro, se não retorna o polinómio resultante da soma como string.

### multiplyPolynomials
Esta função multiplica dois polinómios com a ajuda das funções auxiliares 'distributiveLaw' e 'multMonos'. Retorna o polinómio resultante da multiplicação como string.

### derivePolynomial
Comecamos por definir uma função auxiliar 'filterWithVar' que recebe o polinómio inicial e a variavel para derivar e retorna um polinómio que apenas contém monómios com a variavel a derivar. Depois definimos a função 'calculateDerivation' que recebe o polinómio e a variavel a derivar e retorna o polinómio derivado, usando uma outra função auxiliar para decrementar o grau da variavel derivada.

## Funções Aux.hs
### input
Esta função recebe uma string e retorna um polinómio. A string é uma lista de monómios. Cada monómio tem um coeficiente e uma lista de variaveis. A função 'input' divide a string em monómios e depois divide cada monómio em coeficiente e variaveis. 

### output
Esta função recebe um polinómio e retorna uma string normalizada.

### breakString
Esta função adiciona um espaco no fim da string para que a função 'words' consiga separar os monómios.

### addSpace
Esta função adiciona espacos ao longo da string para ser mais facil separar os monómios.
### auxString2Ponomial


### string2Monomial

### getCoefficient

### getVariables

### parseForVariable

### getVariable

### splitVariable 

### addSpace2Variable

### coefficientFreeString

### removeCoeff0
Esta função recebe um polinómio e retorna um polinómio sem monómios com coeficiente 0.

### clearPolynomial

### filterExponents

### removeExponents
Esta função recebe um polinómio e retorna um polinómio sem monómios com expoente 0.

### sumDegFromSameExp

### clearVar

### filterVar

### sumDeg

### auxNormalizePoly

### clearUsedMonomials

### equalVariables

### filteredMonomials

### sumCoefficients

### polynomial2String

### monomial2String

### variables2StringV2

### variables2String

### cleanUpPolynomial



Realizado pelo grupo G02_09:
- Francisco Pimentel Serra
- João Araújo