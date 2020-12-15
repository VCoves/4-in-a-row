# Connect 4

This is a Connect 4 game, where you place the pieces from above, and drop them on one column, the row they stay on depends on the number of pieces below.
As usual, there are 2 players: X y O. 
There are 4 strategies to choose from.

## Getting started
To start a game run *joc.exe*, which you compile doing `ghc joc.hs`.

For each player the program prompts for the strategy number:
* 0 = Random Bot
* 1 = Greedy Bot
* 2 = Smart Bot
* 3 = Human (you call the shots)

The **Human** strategy asks for the desired column to place the piece. The others don't require any input, so you can make them play while you sit back, relax and enjoy while eating some popcorn. 

### Prerequisites

You will need the `random` package.
To install:

In Mac:

```bash
> brew install cabal-install
> cabal update
> cabal install --lib random
```

In Ubuntu:

```bash
> sudo apt install cabal-install
> cabal update
> cabal install random
```

In Windows:

```bash
> cabal update
> cabal install random
```

### Future improvements:
* Smart AI uses minimax with a depth of 4 and the evaluation of a non finished game takes into account the maxStreak of both players. This can be improved by comparing the sum of all streaks for each player. 
* Once improved the quality of the evaluation, the next objective is the time complexity. It can be reduced using a technique called alpha beta pruning, which in short stops us from evaluating moves that are worse than previously examined moves.This way we can increase the depth and still have calculate move in milliseconds. 
* Check if changing the implementation to Data.List yields a good enough runtime without too much of an increase in memory usage.

## Built With

* [Haskell](https://www.haskell.org/)

## Authors

* **Vicente Coves Beneyto** - [VCoves](https://github.com/VCoves)

## License

This project is licensed under the MIT License