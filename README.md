# Overview
### Solitaire Solver
A solitaire solver that simulates eight-off solitaire games. It uses the iterative deepening depth-first search technique to find the most optimal next move to play. If at the max depth level no move can be found that would increase the number of cards in the foundations, the game terminates. The limit is set to **Max Depth: 5** and can be increased.  
  
**Note!** As the max depth constant increases, in the worst case, the runtime increases exponentially.

# Getting Started
1. Make sure you have a Haskell compiler and the System.Random library installed.

2. Clone the repository  
`git clone https://github.com/Jed-g/solitaire-solver.git`
3. To run, use the function **analyseEO** to simulate a number of games using the aforementioned search technique i.e..  
`analyseEO <Seed for pseudo-random initial game state generation> <Number of games to play>`
    >Returns number of games won and average number of cards in foundations at the point of game termination.
