# reversi

*This repository contains a fork of [Oliver Zhang](https://github.com/oliverzh2000)'s [C# and WPF-based Reversi](https://github.com/oliverzh2000/reversi).
The fork will be used as a test bed environment for the undergraduate course in
AI Programming at Jönköping University, in the fall semester of 2021. I'd like 
to express my gratitude to Mr Zhang for making his source code available under 
the GPL license, allowing such reuse. Any further comments in this file are 
the original contributions of Mr Zhang.*

*Jönköping, 2021-08-24
<br />Karl Hammar*

My goal for this app was to make something simple and beautiful, 
yet featuring a highly competent AI capable of defeating most human players. 

This app is written in C# and XAML for Windows Presentation Foundation (.NET).

The AI and Game logic for this app are contained in `/src/Game.cs`.

The AI of this app uses the minimax algorithm with alpha beta pruning to predict
the best possible AI move. Early in the game, the AI will strive for mobility
and corners, while near the endgame it will strive solely for score. On "legendary"
mode, the AI searches to a depth of 4 moves, and will generate the full game tree
whenever there is less than 10 moves until the end.
