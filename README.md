# lfp-tictactoe

For London (Canada) Functional Programmers: my notes on Chris Penner's recent blog post [Type Tac Toe: Advanced Type Safety](http://chrispenner.ca/posts/type-tac-toe).

Direct from Chris Penner's post: 
* BasicLib.hs: no type safety
* Alternating.hs: types enforce alternating turns
* PreviousMoves.hs: types enforce alternating turns and taken board positions 

Additional stuff by me:
* TypeLevel.hs: lift entire game board to the type level and show how to display it
* Lib.hs: Consolidate "play" functions into single function that can statically enforce
  alternating turns and prevent replays. Entire board is represented at the type level.

To use this repo, first install the [Haskell Tool Stack](https://www.haskellstack.org).

