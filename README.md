# American Roulette Lab
In this app we have coded an American Roulette casino game. You can run the game by clicking on this link [Roulette Lab website](https://ricardobarbosa.shinyapps.io/Roulette-Lab/). 
You can also find the github of the project [here](https://github.com/SimoesBarbosaRicardo/Roulette-Lab).


## How to run the app
Open with Rstudio the "app.r" file, and then click on "Run app" to launch the app.

<iframe width="640" height="360" src="https://www.youtube.com/embed/f_IEJwUHJbs" title="Video presentation" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Package Requirements
To run our app, you need to install some packages. Run the following command in the R console :
`install.packages(c("ggplot2", "shinyjs", "shiny", "DT", "stringr", "dplyr", "data.tabe", "tidyverse", "DescTools", "microbenchmark", "devtools", "shinycssloaders"))`

## App functionalities 
First, when the app starts, you're greeted with a red curtain. Click anywhere to open it.
Then the app is divided in 3 sections. 
* **About us** : This section presents the app, our goals with its development and the inherent risk associated with the roulette game.
* **Roulette** : This is where you can actually play the game. You can input your starting balance, choose your bet amount and choose your chip color. Then you click on the table to place your bets. Once you've placed all your bets, you can spin the wheel and receive your payout.
* **Statistics** : Here we present you with a choice of multiple strategies. You can play with different variables and strategies to generate some plots and read about the strategies.

## References
Initial app : `https://github.com/ignatkulinka/americanRoulette`
We used the app from `ignatkulinka` on Github. We mainly used the definition of the bets and payouts, the roulette table graph and the functions that determine the result of our bets.
For a more comprehensive readout about the strategies you can go [here](https://en.wikipedia.org/wiki/Roulette#Betting_strategies_and_tactics).





