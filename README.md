# Logics in Programming - Trees and Tents Puzzle

We are given a board with trees placed at certain positions, as well as integers representing the number of tents that should be placed in each row and column. The goal of the **Trees and Tents puzzle** is to place tents on the board, in the positions imediately adjacent to each tree - right, left, above or below - while respecting the constraints regarding the number of tents per row/column. However, a tent cannot be placed in the extended neighborhood of another tent, that is, immediately to the right, left, above, below or diagonally.

The objective of this project was to develop a **Prolog program** capable of solving any Trees and Tents puzzle. Through this project, I gained a deep understanding of the **Logic Programming Paradigm**.

# Run and Test

This repository includes:
<br>
<br>
✅ A Prolog implementation of the Trees and Tents solver.
<br>
✅ A problem description (in Portuguese).
<br>
✅ A script containing example puzzles and their expected solutions.

To compare the program's output with the expected results and identify any differences, run the following commands:

```
swipl Project.pl
puzzle(6-13, P), resolve(P), sol(6-13, S), P == S.
puzzle(6-14, P), resolve(P), sol(6-14, S), P == S.
puzzle(8-1, P), resolve(P), sol(8-1, S), P == S.
```

If you don't have SWI-Prolog installed, you can download it [here](https://www.swi-prolog.org/Download.html).
