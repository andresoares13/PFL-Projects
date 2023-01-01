# Turma 10 UglyDuck_6

## Elementos
- André Tomás da Cunha Soares - up202004161
- Anibal Pereira Neves Ferreira - up202005429

## Contribuição

- André: 50%
- Aníbal: 50%

## Instalação e Execução
### Windows
- Executar `SICStus Prolog`
- `File` -> `Consult...` -> Selecionar ficheiro `uglyduck.pl`
- `Settings` -> `Font` -> Selecionar a fonte `Consolas`
- Na consola do SicStus: `play.`
### Linux
- No terminal: `/usr/local/bin/sicstus` (assumindo ser este o lugar de instalação, que é por default)
- No terminal: `consult(uglyduck).`
- Na consola do SicStus: `play.`
## Ugly Duck - Descrição do Jogo

O objetivo do jogo é levar um dos nossos patos até à linha final para se transformar num cisne e depois trazer o cisne de volta à linha inicial.

Ao longo do caminho vamos encontrar peças do outro jogador, as quais apenas podemos capturar na diagonal, ficando na casa onde a peça estava. Os patos apenas podem andar para a frente e os cisnes para trás, não podendo ir para uma casa onde esteja uma peça do mesmo jogador.

O jogo acaba ou quando o cisne chega à casa de partida ou quando um jogador captura todas as peças do outro jogador, sendo que nunca é possível empatar.



## Lógica do Jogo
### Representação Interna do Estado de Jogo
#### Tabuleiro
O tabuleiro é representado a partir de uma lista com sublistas, sendo cada sublista uma linha do tabuleiro. Existem 5 tipos diferentes de elementos possiveis em cada linha, sendo todos átomos.
([n] representa o número da peça)
1. D[n] - pato do jogador 1
2. d[n] - pato do jogador 2
3. S[n] - cisne do jogador 1
4. s[n] - cisne do jogador 2
5. '  ' - célula vazia (a formatação na permite perceber mas são 2 espaços)


```prolog
Possiveis estados de jogo:

- Inicial:
[ ['D1','D2','D3','D4','D5'],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['d1','d2','d3','d4','d5'] ]

- Intermedio:
[ ['D1','s2','  ','D4','D5'],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['d1','  ','S3','d4','d5'] ]

- Final:
[ ['D1','  ','S3','D4','D5'],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','  ','  ','  ','  '],
  ['  ','s2','  ','  ','  '],
  ['d1','  ','  ','d4','d5'] ]

```

#### Jogador Atual
O jogador atual tem dois estados possíveis que são ints: 1 e 2, que permitem saber que player está a jogar, de quem é suposto ser a peça, etc
Para mudar o player simplesmente usamos o predicado changePlayer(+Player,-NewPlayer)
```prolog
changePlayer(1, 2).
changePlayer(2, 1).
```


### Visualização do Estado do Jogo

Após iniciar o jogo com o predicado `play.` o jogador tem um menu com as opções principais do jogo:

![](https://i.imgur.com/iblWMPf.png)


Para realizar a escolha de uma opção o jogador apenas escreve o número relativo à opção que quer e prime `Enter`. 


Após a opção 1 ainda é possível escolher o modo de jogo:
```
1 - Player vs Player
2 - Player vs Computer
3 - Computer vs Player
4 - Computer vs Computer
```

Nota: Todos estes modos são failure-driven loops, isto porque se tivéssemos usado recursão seria extremamente negativo em termos de memória. 

No caso do modo de jogo Computer vs Computer, usa-se o predicado sleep(2) para ter tempo de se ver o computador a jogar, senão as jogadas seriam instantâneas.

O predicado sleep é da library system. Outras libraries usadas foram lists e random. 

Se o utilizador quiser mudar o tamanho do tabuleiro, no menu inicial apenas tem de carregar em 2 (Change Settings) e escolher a opção de mudar as dimensões da board onde introduz um comprimento e altura. Nas settings ainda é possivel alterar o nivel de dificuldade do computador (easy ou hard).

Após selecionar um modo de jogo o jogo inicia: 

![](https://i.imgur.com/aaB9hMg.png)

As dimensões válidas são entre 3 e 22 inclusivamente sendo que a board não tem de ser quadrada, por exemplo, board 19 por 3:

![](https://i.imgur.com/mNLND2R.png)

No caso de correr o jogo em linux, cada peça terá uma cor, em função do jogador, e a cor dos cisnes fica em bold:

![](https://i.imgur.com/ZDWa6iE.png)

No caso do predicado initial_state(+Size,-GameState) é passado um tuplo Size com Length e Height e é returnado um GameState, que é um tuplo que contém neste caso, apenas a Board. Mais tarde, outros tuplos GameState irão conter mais dados como Player, Move, mas para este predicado não é necessário porque o jogo começa sempre no Player e Move 1.

### Execução de jogadas

Em cada jogada é pedida ao utilizador que escolha uma peça onde apenas precisa de escrever o número da peça e carregar no enter. Após escolher, o input passa por um controlo para garantir que a peça existe. De seguida, após ter escolhido a peça, o utilizador é questionado que move quer fazer. No nosso caso, os moves são q,w,e ou a,s,d. O utilizador apenas tem de escrever w (por exemplo) e clicar no enter. Mais uma vez o input é controlado para garantir a sua validade. Tanto na escolha da peça como da jogada, é apresentado ao user as peças que pode escolher e depois os moves que a peça que escolheu pode fazer:

![](https://i.imgur.com/3T3Vbej.png)

Neste caso a peça 1 do jogador 1 (que começa num canto), não pode ir para cima na diagonal esquerda (jogada q) pois isso ficaria fora da board, então essa opção não é apresentada nos moves possíveis.

O predicado move/3, recebe um tuplo GameState e um move. Neste caso o GameState irá conter a Board, Piece a mover, Player que faz a jogada, Comprimento da Board e número do Move do jogo. É retornado NewGameState que irá conter a nova Board com o move já efetuado. Neste predicado é feito o controlo do input do move que apenas é possivel se a jogada estiver na lista de jogadas válidas. 

### Lista de Jogadas Válidas

Para obter a lista de jogadas válidas temos um predicado valid_moves/3 que recebe um GameState, tuplo que contém a Board, o Comprimento e a Piece sobre a qual queremos saber as jogadas que pode fazer. Para isto, usamos um findall que aplica condições a todos os moves possiveis e vê quais passam as condições todas, guardando-os numa lista. Este predicado é dos mais importantes sendo essencial nas jogadas do computador que precisa após escolher uma peça de saber que moves pode fazer.


### Final do Jogo

Para saber quando o jogo acaba usamos o predicado game_over/2 que recebe um GameState e retorna um Winner, player que ganhou no caso de ter ganho. Para fazer a verificação, basta ver se há algum cisne do último jogador a jogar que esteja na casa inicial desse mesmo jogador ou se após o jogador fazer a jogada, se o outro jogador ficou sem peças. Nesse caso é apresentada uma mensagem a dizer que player ganhou o jogo (player 1 ou 2) e volta para o menu inicial. Este predicado é usado após cada jogada de um jogador. No tuplo GameState passamos a Board atual e o Player que fez a jogada e que queremos testar se acabou de ganhar o jogo.


### Avaliação do Tabuleiro

A avaliação do tabuleiro no nosso caso é apenas relevante quando está a ser usado o AI de nível hard. Este AI é greedy e precisa de saber o valor dos moves para poder ver qual o mais vantajoso (mais informações sobre os 2 niveis de AI no próximo ponto).
Esta avaliação é feita com o predicado value/3 que recebe um GameState, tuplo com apenas a Board neste caso, um Player que está a fazer a jogada e retorna o Value. Para avaliar o valor duma board no nosso caso é importante considerar vários pontos. O primeiro é que um cisne tem de representar mais valor do que um pato pois está mais proximo de ganhar. Depois é mais valioso ter mais patos e cisnes do que o adversário pois é mais facil impedir peças de se mexer sem serem capturadas como fica mais próximo da condição de vitória por captura de todas as peças inimigas. Depois é preciso ter em conta que uma board onde um jogador tenha ganho, é de um valor imesurável (no caso 1000 points para ser o mais valioso de todos). Por fim é preciso considerar um board onde as peças de um jogador estejam vulneráveis a serem capturadas. Para os pontos consideramos o seguinte:

1. 20 Pontos por cada pato a mais que o adversário
2. 50 Pontos por cada cisne a mais que o adversário
3. 1000 Pontos no caso de estar num winning state
4. -5 Pontos no caso de ter peças suscetiveis a serem capturadas

### Jogada do Computador

Para o Computador temos 2 dificuldades: Easy e Hard. Para o computador escolher um move temos um predicado choose_move/4 que recebe um GameState, tuplo com Board, comprimento da Board, e Piece.

No caso do nivel 1, o AI irá tanto escolher uma peça de forma aleatória como um move para essa peça de forma aleatória (para randomness total). Isto origina uma questão, pois no nosso jogo é possivel uma peça não ter jogadas possiveis, por exemplo, se tiver à sua frente e nas suas diagonais peças do mesmo jogador. Para resolver esta questão é usado um repeat, até encontrar uma peça que tenha uma jogada possivel (tudo de forma aleatoria), e depois se isso se verificar tem um cut no fim para poder acabar. Este é o único cut no jogo todo, mas consideramos ser necessário aqui.

No caso do nivel 2, o AI é mais complexo. Ele é greedy então a solução arranjada é simular o todos os moves de todas as peças e ver o melhor valor possivel e executar essa jogada. Garantidamente vamos ter o melhor valor possivel usando o predicado anteriormente mencionado,value/3, porém há mais casos a considerar. Muitas vezes, este AI terá de saber o que fazer se todas as jogadas traduzirem um valor igual. Sendo um AI greedy, a solução passa por, não tendo cisnes, escolher a primeira que encontrar. No entanto, se existir um cisne, ele irá mover essa peça (atenção: isto é apenas se todos os moves tiverem o mesmo valor), pois ele quer acabar o jogo no menor número de jogadas possiveis e ao focar-se no movimento de um cisne isso irá acontecer. Nota que graças ao desconto de -5, o AI terá cuidado em não se mover para casas onde possa ser capturado no turno seguinte.




## Conclusões

Este trabalho foi muito interessante de desenvolver pois permitiu-nos fazer um jogo de uma forma completamente diferente do tudo o que fizémos até agora. No inicio foi complicado perceber a parte de que qualquer componente do jogo não passa de um conjunto de condições, mas posteriormente, começámos a ter mais facilidade e conseguimos fazer o trabalho na totalidade. 

### Known issues

Por vezes em windows (muito raramente), certos unicode characters usados para representar tanto a board, como as setas que permitem ao user melhor perceber os moves ficam desconfigurados.

### Possiveis Melhorias

A melhoria que o nosso jogo podia ter era o nível Hard do AI. Consideramos que ver uma jogada a frente às vezes não é o suficiente. Nós ficamos satisfeitos com o AI (sem conhecimento de como funciona é dificil de vencer), mas se considerasse mais fatores e mais moves à frente ficaria ainda mais desafiante.

## Bibliografia
- [Documentação SICStus 4.7.1](https://sicstus.sics.se/sicstus/docs/4.7.1/pdf/sicstus.pdf)
- [Universidade de Lisboa - Ugly Duck](http://www.di.fc.ul.pt/~jpn/gv/uglyduck.htm)
- [SWI-Prolog](https://www.swi-prolog.org/)


