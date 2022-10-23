# PFL - Trabalho Prático 1
O trabalho prático tem como objetivo implementar as seguintes funcções:
- **normalizePolynomials** - Função que normaliza polinómios
    - Casos de teste:
- **addPolynomials** - Função que soma polinómios
- **multiplyPolynomials** - Função que multiplica polinómios
- **derivePolynomial** - Função que deriva polinómios

## Justificação da epresentaçã interna
A representação interna dos polinómios é feita através de uma lista de momómio. Cada monónimo é representado por  2 elementos, o primeiro elemento é o coeficiente e o segundo elemento é uma lista de variaveis com cada variável e o respetivo grau. Escolhemos fazer esta divisão porque achamos que é mais fácil trabalhar com os polinómios desta forma.

## Estratégia das funções Polynomials.hs
### perfectPolynomialperfectPolynomial
Esta função soma os coeficientes dos monómios com as mesmas variaveis e graus, e remove os monómios com coeficiente 0 e retorna o polinómio como string.

### addPolynomials
Esta função recebe duas strings, concatena-as e normaliza esse novo polinómio. No caso dos polinómios serem nulos retorna um erro, se não retorna o polinómio resultante da soma como string.

### multiplyPolynomials
Esta função multiplica dois polinómios com a ajuda das funções auxiliares 'distributiveLaw' e 'multMonos'. Retorna o polinómio resultante da multiplicação como string.

### derivePolynomial
Comecamos por definir uma função auxiliar 'filterWithVar' que recebe o polinómio inicial e a variável para derivar e retorna um polinómio que apenas contém monómios com a variável a derivar. Depois definimos a função 'calculateDerivation' que recebe o polinómio e a variavel a derivar e retorna o polinómio derivado, usando uma outra função auxiliar para decrementar o grau da variável derivada. No fim retornamos o polinómio derivado como string.




Realizado pelo grupo G02_09:
- Francisco Pimentel Serra
- João Araújo