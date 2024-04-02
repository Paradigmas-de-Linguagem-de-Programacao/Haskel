<h1 align="center">Fliperama em Haskell</h1>
<br>
<br>

<p align="center">
    <img 
        src="./assets/arcade.gif" 
        alt="arcade"
        width="250"
    />
</p>

<p align="center" style="font-weight: bold;">Projeto PLP - UFCG</p>

<p align="center"><span style="font-size: larger;">
    Bem-vindo ao Fliperama em Haskell! Este fliperama baseado em terminal <br>
    oferece uma coleção de atividades divertidas para desfrutar diretamente <br>
    da sua linha de comando. Com uma variedade de jogos e recursos interativos, <br>
    com certeza irá mantê-lo entretido por horas.</span>
</p>

<br>
<h2 align="center">Gerenciamento de Usuários</h2>
<p align="center"><img src="./assets/users.gif" width="250px"/></p>
<p align="center">
    Gerencie sua experiência no fliperama com facilidade! Crie seu próprio perfil<br>
    de usuário, acompanhe suas pontuações altas e compita com amigos pelo topo do ranking
</p>

<br>

<h1 align="center">Tetris</h1>
<p align="center">
    <img 
        src="./assets/tetris.gif"
        width="200px"/>
</p>
<p align="center">  
    Experimente o clássico jogo de quebra-cabeça! <br>
    Teste suas habilidades ao posicionar estrategicamente os blocos para limpar linhas e acumular<br>
    pontos. Com múltiplos níveis de dificuldade, é divertido para jogadores de todas as idades.
</p>

<br>

<h1 align="center">FMCC</h1>
<p align="center">
    <img 
        src="./assets/sword.gif"
        width="200px" />
</p>
<p align="center">  
    Embarque em uma aventura épica no modo de jogo de FMCC! <br>
    Explore descubra tesouros escondidos enquanto viaja por um mundo de FMCCII. 
</p>

<br>

<h1 align="center">Como Jogar: </h1>

1. **Gerenciamento de Usuários**: Comece criando seu próprio perfil de usuário usando as opções fornecidas. Uma vez registrado, você pode fazer login para acessar seu perfil e acompanhar seu progresso.
Uma vez logado:
2. **Tetris**: Entre no modo de jogo Tetris para começar a jogar. Use as setas do teclado para mover e girar os blocos que caem e tente completar linhas para marcar pontos. Limpe o maior número de linhas possível antes que os blocos alcancem o topo!

3. **Jogo de FMCC**: Selecione o modo de jogo FMCC para embarcar em uma aventura emocionante. Suas escolhas irão moldar o desfecho da história!

<h1 align="center">Dependências para instalar</h1>

## Bibliotecas de C

1. OpenGlRaw

2. Glut

* Para Sistemas com Kernel semelhante ao do Ubuntu, insira o comando: sudo apt-get install freeglut3-dev

* Para sistemas sem essa semelhança, vale pesquisar os módulos de Gui proveniente de C.

## Bibliotecas em Haskell

1. Ansi-terminal

2. Directory 

3. Gloss 

4. Mtl

5. Process

6. Split

<h1 align="center"> Divisão de Branchs </h1>

## Main

* A branch main tem o objetivo de sintetizar uma versão buildada do projeto, ela roda a partir do comando stack ghci, vale lembrar que caso você queira rodar essa branch, você não precisará de nenhuma dependência de haskell. ENTRETANTO, **É FUNDAMENTAL AS DEPENDÊNCIAS DE C**

* Tetris -> Funciona
* Sistema de Fliperama -> Funciona
* FMCC -> Funciona

## Master

* A branch master tem o objetivo de sintetizar uma versão compilável do projeto, ela roda a partir do ghc Main dentro do dirétorio "Console". Todas as dependências precisam estar instaladas localmente em seu dispositivo. Após compilar o Main, vale executa-lo com ./Main (se tiver em console), ou ./Console/Main se estiver no repositório.

* **O OBJETIVO DESTA BRANCH, É GARANTIR O FUNCIONAMENTO DO TETRIS E O FLIPERAMA, O FMCC APENAS FUNCIONARÁ NA BRANCH MAIN, JUSTAMENTE POR ESTAR FUNCIONANDO EM UM AMBIENTE BUILDAVEL, FAVOR VERIFICAR SEU STACK E COMO RODA NO GHCI, QUAISQUER DÚVIDAS, CONTATE VICTOR VILI XAVIER LUNA (victor.vili.xavier.luna@ccc.ufcg.edu.br)**

* Tetris -> Funciona
* Sistema de Fliperama -> Funciona
* FMCC -> Não Funciona (Dependências locais, caso você use uma versão do GHC > 9.6.4, irá rodar perfeitamente, apenas renomear as funções com as ')