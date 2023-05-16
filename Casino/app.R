
# Notre app -----

# I. bettingTable Setup ---------------------------------------------------
# Setup of the bettingTable, dataframe that keeps track of slots on the table
require(ggplot2)

slotNum <- c(0, "00" , c(1:36))

# On vectorise les couleurs : 3 pour vert, 2 pour noir, 1 pour rouge
color <- c(rep(3, 2),
           rep(c(1, 2), 5),
           rep(c(2, 1), 4),
           rep(c(1, 2), 5),
           rep(c(2, 1), 4))

# on vectorise les nombres pairs : 1 pour pair et le reste 0
even <- c(rep(0, 2), rep(c(0,1), 18))
# on vectorise les nombres impairs : 1 pour impair et le reste 0
odd <- c(rep(0, 2), rep(c(1,0), 18))
# 0 et 00 sont ni impairs ni pairs. donc 0.

# pareil on vectorise low et high
low <- c(rep(0,2), rep(1, 18), rep(0, 18))
high <- c(rep(0,2), rep(0, 18), rep(1, 18))


snakeBet <- c(rep(0,2),
              rep(c(1,0,0,0), 2),
              1, 0, 0,
              rep(c(1,0), 3),
              0,
              rep(c(1,0,0,0), 2),
              1, 0, 0,
              rep(c(1,0),3),
              0)

dozen <- c(rep(0,2),
           rep(1,12),
           rep(2,12),
           rep(3,12))

column <- c(rep(0,2),
            rep(c(1,2,3),12))

bettingTable <- data.frame(slotNum, color, even,
                           odd, low, high,
                           snakeBet, dozen, column)



# II. Roulette Table Graphing Coordinates ---------------------------------

# A. Coordinates for the regular slots section of the roulette table
df <- expand.grid(x = seq(0,4,2), y = seq(0,22,2))
df$z <- c(rep(c(1, 2), 3), 2, 2, 1, 1,
          rep(c(2, 1), 4),
          rep(c(1, 2), 3), 2, 2, 1, 1,
          rep(c(2, 1), 4))

# B. Pentagons for the 0 and 00 slots on the table
zeroPentagon <- data.frame(z = rep(3,5),
                           x = c(-1, -1, 0.5, 2, 2),
                           y = c(23, 24, 25, 24, 23))

doubleZeroPentagon <- data.frame(z = rep(3,5),
                                 x = c(2, 2, 3.5, 5, 5),
                                 y = c(23, 24, 25, 24, 23))

# C. Outside bets bounding boxes
thirdTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                               y = c(-1, 7, 7, -1),
                               z = rep(3, 4))

secondTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                                y = c(7, 15, 15, 7),
                                z = rep(3, 4))

firstTwelveSlots <- data.frame(x = c(-1, -1, -3, -3),
                               y = c(15, 23, 23, 15),
                               z = rep(3, 4))

ninteenThirtysixSlots <- data.frame(x = c(-3, -3, -5, -5),
                                    y = c(-1, 3, 3, -1),
                                    z = rep(3, 4))

oddSlots <- data.frame(x = c(-3, -3, -5, -5),
                       y = c(3, 7, 7, 3),
                       z = rep(3, 4))

blackSlots <- data.frame(x = c(-3, -3, -5, -5),
                         y = c(7, 11, 11, 7),
                         z = rep(2, 4))

redSlots <- data.frame(x = c(-3, -3, -5, -5),
                       y = c(11, 15, 15, 11),
                       z = rep(1, 4))

evenSlots <- data.frame(x = c(-3, -3, -5, -5),
                        y = c(15, 19, 19, 15),
                        z = rep(3, 4))

oneToEighteenSlots <- data.frame(x = c(-3, -3, -5, -5),
                                 y = c(19, 23, 23, 19),
                                 z = rep(3, 4))

twoToOne1 <- data.frame(x = c(-1, -1, 1, 1),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))

twoToOne2 <- data.frame(x = c(1, 1, 3, 3),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))

twoToOne3 <- data.frame(x = c(3, 3, 5, 5),
                        y = c(-1, -3, -3, -1),
                        z = rep(3,4))


# III. Coordinates for Labels ---------------------------------------------

annotationCoords <- data.frame(x = rep(c(0:2), 12),
                               y = rep(c(0:11), 1, each = 3))

zerosAnnotationCoords <- data.frame(x = c(.5, 3.5),
                                    y = c(23.8, 23.8))

columnAnnotationCoords <- data.frame(x = seq(0,4,2),
                                     y = rep(-2,3))

# Labels for the slots
annotationLabels <- c(34, 35, 36,
                      31, 32, 33,
                      28, 29, 30,
                      25, 26, 27,
                      22, 23, 24,
                      19, 20, 21,
                      16, 17, 18,
                      13, 14, 15,
                      10, 11, 12,
                      7, 8, 9,
                      4, 5, 6,
                      1, 2, 3)


# IV. Definition of Bets --------------------------------------------------
# Below are the definitions of all the possible bets. The parameters defined:
#  x,y: coordinates for the table/ggplot2
#  b1 - b6: the numbers included in the bet
#  payOut: the payout for winning the bet
#  type: name of the bet
# A. Inside Bets
#  1. Straight up / Single Bet (0 and 00 included): Bet on any single number
slotsToBets <- data.frame(x = c(rep(c(0, 2, 4), 12), 0.5, 3.5),
                          y = c(rep(seq(0, 22, 2), 1, each = 3), 23.9, 23.9),
                          b1 = c(34, 35, 36,
                                 31, 32, 33,
                                 28, 29, 30,
                                 25, 26, 27,
                                 22, 23, 24,
                                 19, 20, 21,
                                 16, 17, 18,
                                 13, 14, 15,
                                 10, 11, 12,
                                 7, 8, 9,
                                 4, 5, 6,
                                 1, 2, 3,
                                 0, "00"),
                          b2 = rep(NA, 38),
                          b3 = rep(NA, 38),
                          b4 = rep(NA, 38),
                          b5 = rep(NA, 38),
                          b6 = rep(NA, 38),
                          payOut = rep(35, 38),
                          type = "Single",
                          stringsAsFactors = FALSE)

#  2. Split Bet: Bet on any two adjoining numbers vertical/horizontal
splitBets <- data.frame(x = c(rep(1, 12),
                              rep(3, 12),
                              rep(c(0, 2, 4), each = 11)),
                        y = c(rep(seq(0, 22, 2), 2),
                              rep(seq(1,22,2), 3)),
                        b1 = c(seq(34, 1, -3),
                               seq(35, 2, -3),
                               seq(31, 1, -3),
                               seq(32, 2, -3),
                               seq(33, 3, -3)),
                        b2 = c(seq(35, 2, -3),
                               seq(36, 3, -3),
                               seq(34, 4, -3),
                               seq(35, 5, -3),
                               seq(36, 6, -3)),
                        b3 = rep(NA, 57),
                        b4 = rep(NA, 57),
                        b5 = rep(NA, 57),
                        b6 = rep(NA, 57),
                        payOut = rep(17, 57),
                        type = "Split",
                        stringsAsFactors = FALSE)

# 3. Square Bets: Bet on any 4 adjoining numbers in a block
quadBets <- data.frame(x = c(rep(1, 11),
                             rep(3, 11)),
                       y = c(seq(1, 22, 2),
                             seq(1, 22, 2)),
                       b1 = c(seq(31, 1, -3),
                              seq(32, 2, -3)),
                       b2 = c(seq(32, 2, -3),
                              seq(33, 3, -3)),
                       b3 = c(seq(34, 4, -3),
                              seq(35, 5, -3)),
                       b4 = c(seq(35, 5, -3),
                              seq(36, 6, -3)),
                       b5 = rep(NA, 22),
                       b6 = rep(NA, 22),
                       payOut = rep(5, 22),
                       type = "Square Bet",
                       stringsAsFactors = FALSE)

# 4. Street Bets: Bet on any three horizontal numbers in a row
streetBets <- data.frame(x = c(rep(-1, 12),
                               rep(5, 12)),
                         y = c(seq(0, 22, 2),
                               seq(0, 22, 2)),
                         b1 = rep(seq(34, 1, -3), 2),
                         b2 = rep(seq(35, 2, -3), 2),
                         b3 = rep(seq(36, 3, -3), 2),
                         b4 = rep(NA, 24),
                         b5 = rep(NA, 24),
                         b6 = rep(NA, 24),
                         payOut = rep(5, 24),
                         type = "Street Bet",
                         stringsAsFactors = FALSE)

# 5. Line Bets: Bet on any six numbers from two adjacent horizontal row
lineBets <- data.frame(x = c(rep(-1, 11),
                             rep(5, 11)),
                       y = c(seq(1, 21, 2),
                             seq(1, 21, 2)),
                       b1 = rep(seq(31, 1, -3), 2),
                       b2 = rep(seq(32, 2, -3), 2),
                       b3 = rep(seq(33, 3, -3), 2),
                       b4 = rep(seq(34, 4, -3), 2),
                       b5 = rep(seq(35, 5, -3), 2),
                       b6 = rep(seq(36, 6, -3), 2),
                       payOut = rep(5, 22),
                       type = "Line Bet",
                       stringsAsFactors = FALSE)


# B. Outside Bets
#  1. Column Bets: Bet on all numbers in columns 1 - 3
columnBets <- data.frame(x = seq(0,4,2),
                         y = rep(-2,3),
                         b1 = c("1st Column",
                                "2nd Column",
                                "3rd Column"),
                         b2 = rep(NA, 3),
                         b3 = rep(NA, 3),
                         b4 = rep(NA, 3),
                         b5 = rep(NA, 3),
                         b6 = rep(NA, 3),
                         payOut = rep(2,3),
                         type = "Column Bet",
                         stringsAsFactors = FALSE)


#  2. Dozen Bets: 1st through 3rd dozen bets
dozenBets <- data.frame(x = c(rep(-2,15)),
                        y = c(seq(0, 6, 1.5),
                              seq(8, 14, 1.5),
                              seq(16, 22, 1.5)),
                        b1 = c(rep("3rd Dozen", 5),
                               rep("2nd Dozen", 5),
                               rep("1st Dozen", 5)),
                        b2 = rep(NA, 15),
                        b3 = rep(NA, 15),
                        b4 = rep(NA, 15),
                        b5 = rep(NA, 15),
                        b6 = rep(NA, 15),
                        payOut = rep(2,15),
                        type = "Dozen Bet",
                        stringsAsFactors = FALSE)

#  3. Outside Bets: Low, High, Even, Odd, Red, Black
outsideBets <- data.frame(x = rep(-4, 12),
                          y = c(0.3, 1.7,
                                4.2, 5.7,
                                8.3, 9.8,
                                12.2, 13.7,
                                16.3, 17.8,
                                20.2, 21.7),
                          b1 = c(rep("High", 2),
                                 rep("Odd", 2),
                                 rep("Black", 2),
                                 rep("Red", 2),
                                 rep("Even", 2),
                                 rep("Low", 2)),
                          b2 = rep(NA, 12),
                          b3 = rep(NA, 12),
                          b4 = rep(NA, 12),
                          b5 = rep(NA, 12),
                          b6 = rep(NA, 12),
                          payOut = rep(1, 12),
                          type = c(rep("High", 2),
                                   rep("Odd", 2),
                                   rep("Black", 2),
                                   rep("Red", 2),
                                   rep("Even", 2),
                                   rep("Low", 2)),
                          stringsAsFactors = FALSE)

# G. Trio Bets: Bets on any three slot combinations including 0 or 00
trioBets <- data.frame(x = c(1, 3),
                       y = c(23),
                       b1 = c(0, "00"),
                       b2 = c(1, 2),
                       b3 = c(2, 3),
                       b4 = rep(NA, 2),
                       b5 = rep(NA, 2),
                       b6 = rep(NA, 2),
                       payOut = rep(11, 2),
                       type = "Trio Bet",
                       stringsAsFactors = FALSE)

# H. Top Line: Bet on 0-00-1-2-3
topLineBets <- data.frame(x = c(-1, 5),
                          y = c(23),
                          b1 = rep(0, 2),
                          b2 = rep("00", 2),
                          b3 = rep(1, 2),
                          b4 = rep(2, 2),
                          b5 = rep(3, 2),
                          b6 = rep(NA, 2),
                          payOut = rep(6, 2),
                          type = "Top Line Bet",
                          stringsAsFactors = FALSE)

# # G. Basket: bet on 0-00-2
# basketBets <- data.frame(x = c(),
#                          y = c(),
#                          b1 = c(0),
#                          b2 = c("00"),
#                          b3 = c(2),
#                          b4 = c(NA),
#                          b5 = c(NA),
#                          b6 = c(NA),
#                          payOut = rep(6, 2),
#                          type = "Top Line Bet",
#                          stringsAsFactors = FALSE)

# Combine all bet information in one dataframe
clickable <- rbind(columnBets, slotsToBets, splitBets,
                   dozenBets, outsideBets, quadBets,
                   lineBets, streetBets, trioBets,
                   topLineBets)

# Grab nonrepeated entries in columns 3:10
# to use with random bet (i.e. pick a bet from here)
uniqueBets <- clickable[!duplicated(clickable[3:10]),]


# V. Colors and Helpers for ggplot2 ---------------------------------------

# A. Chip text color based on chip color
contrast_color <- function(color, verbose = FALSE){

  # 1. Expect a color within colors()
  # if(!(tolower(color) %in% colors()[grepl("^[^0-9]*$", colors())])) {
  #   if(verbose){
  #     print("Input color not valid! Using white by default")
  #   }
  #   return("white")
  # }

  # 2. Calculate color luminance
  wt <- c(0.2126, 0.7152, 0.0722)
  luminance <- colSums(col2rgb(color) * wt) / 255

  # 3. Determine font color best suited for the chip color
  return(ifelse(luminance > 0.5, "black", "white"))
}

# B. Colors for ggplot2 roulette table
cols <- c("1" = "red", "2" = "black", "3" = "darkgreen", "4" = "white")
colsTwo <- c("1" = "white", "2" = "white", "3" = "white", "4" = rgb(0,0,0,0))

# C. Theme elements for ggplot2 roulette table
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.margin = unit(c(1,1,1,1), "cm"),
  legend.position = "none"
)


# VI. Roulette Function & Helpers -----------------------------------------
# A. Function responsible for picking a random number out of the slots a
#     and returning the winning slot as well its characteristics

roulette <- function(verbose = FALSE) {
  possibleSlots <- c(0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15, 3,
                     24, 36, 13, 1, "00", 27, 10, 25, 29, 12, 8, 19, 31, 18,
                     6, 21, 33, 16, 4, 23, 35, 14, 2)

  slotLanded <- sample(possibleSlots, 1)
  if (verbose)
    cat("Landed on:", slotLanded, "\n")

  tableIndex <- which(bettingTable$slotNum == slotLanded)
  #ce which permet d'assigner à tableIndex l'élément ligne de bettingTable dont le chiffre de soltNum est le meme
  #que slotLanded
  #ie. définit quelle ligne est gagnante


  # Ce return nous renvoie une liste contenant le numéro gagnant ainsi que sont
  # appartenance aux différents types de bets
  # e.g. le 28 est noir, pair etc.
  return(list(slotLanded = slotLanded,
              color = bettingTable$color[tableIndex],
              even = bettingTable$even[tableIndex],
              odd = bettingTable$odd[tableIndex],
              low = bettingTable$low[tableIndex],
              high = bettingTable$high[tableIndex],
              snakeBet = bettingTable$snakeBet[tableIndex],
              dozen = bettingTable$dozen[tableIndex],
              column = bettingTable$column[tableIndex]))

  # roulette retourne une liste contenant le numéro gagnant ainsi que ses charactéristiques
  # en 1 et 0.
}

# The idea of a martingale strategy is to choose an initial bet. (Normally red or black
# or odd or even) and then when you lose, you double your bet, until you win
# When you win you go back to your initial bet.

# Where N is the number of simulations, start_amount is our balance starting amount
# bet_amount is the amount we bet, roulette is the function we've defined above and
# tot_spin is the amount of spins per simulation (in order to have a data frame that has
# the same number of columns)
# By default we're going to bet on even (It's easier that way and does not change in essence
# the strategy. We could do it by betting on numbers, but we would have probably to simulate
# more tries)

martingale_strategy = function(N = 1000, start_amount,bet_amount, roulette, tot_spin = 50) {
  df_amount = data.frame(row.names = 1:N)
  initial_bet_amount = bet_amount
  # first we have to simulate for the strategy for "1 person"
  # then we repeat for N iterations
  # we first fill the dataframe for simulation 1
 for(i in 1:N){
   num_bet = 1
   amount = start_amount
   df_amount[i,1] = start_amount
   bet_amount = initial_bet_amount
  while(amount > 0 && num_bet < tot_spin) {
    bet_result = roulette()$even
    if(bet_result==1) {
      amount = amount + bet_amount
      # we fill the dataframe
      df_amount[i,num_bet+1] = amount
      bet_amount = initial_bet_amount
    } else {
      amount = amount - bet_amount
      # we fill the dataframe
      df_amount[i,num_bet+1] = amount
      bet_amount = bet_amount * 2
    }


    num_bet = num_bet + 1

  }
 }
  browser()

  return(df_amount)

}


win_rate <- function(df_amount) {
  win_rate <- numeric(nrow(df_amount))  # Initialize win rate vector

  for (j in 1:length(df_amount)) {
    number_win <- 0
    number_losses <- 0

    for (i in 2:ncol(df_amount)) {
      if (is.na(df_amount[j, i]) == TRUE) {
        break
      } else {
        if (df_amount[j, i - 1] > df_amount[j, i]) {
          number_losses <- number_losses + 1  # Lost
        } else {
          number_win <- number_win + 1  # Won
        }
      }

      win_rate[j] <- number_win/(number_win + number_losses)  # Compute win rate at each step
    }
  }


  win_rate = as.data.frame(win_rate)
  return(win_rate)
}




library(shiny)
require(DT)
require(shiny)
require(ggplot2)
require(stringr)
require(dplyr)
require(data.table)
library(tidyverse)



# VII. UI-----------------------------------------



# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("American Roulette"),
  fluidRow(
    # Sidebar with a slider and selection inputs
    # column to select the width
    column(5,
           tabsetPanel(
             tabPanel("Betting",
                      numericInput("startbalance", label = h3("Money Balance"), value = 1),
                      actionButton("add", "add"),

                      hr(),
                      #fluidRow(column(3, verbatimTextOutput("money"))),

                      br(),
                      ### Manual Betting
                      h4("Manual Betting"),
                      strong("Bet Amount:"),
                      br(),
                      # \10\25\50\100\250\ bet buttons
                      actionButton("bet1", "$10"),
                      actionButton("bet2", "$25"),
                      actionButton("bet3", "$50"),
                      actionButton("bet4", "$100"),
                      actionButton("bet5", "$250"),
                      br(),
                      br(),
                      h4("Chip Color"),
                      selectizeInput("chipColor", "Choose chip color:",
                                     choices = tolower(colors()[grepl("^[^0-9]*$", colors())]),
                                     selected = "navy"),
                      hr(),

                      actionButton("spin", "Spin Roulette"),
                      actionButton("reset", "Reset Bets"),

                      textOutput("roulette"),

                      # we show our balance of money
                      textOutput("generalbalance")




    ),
    tabPanel("Statistics",
             # Statistics inputs
             numericInput("num_sims", "Number of simulations:", 10, min = 1),
             numericInput("start_bet", "Balance:", 100, min = 1),
             numericInput("bet_amount", "bet amount:", 10, min = 10),
             numericInput("tot_spin", "Number of spins per simulation:", 50, min = 10),
             actionButton("run_simulation", "Run simulation")
    )
           )
    ),
    column(7,
           column(12,
                  br(),
                  h4("Win Rate Percentage"),
                  plotOutput("win_rate_plot", height = "200px")
           ),
           column(12,
                  br(),
                  plotOutput("martingale_plot", height = "400px")
           ),


      # We create other panels to the main one in order to show different things.
      tabsetPanel(
        tabPanel("Roulette Table", plotOutput("rTable", click = "plot_click", width = "20%")),
        )

    ))

    )


# VIII. Server-----------------------------------------

server <- function(input, output,session) {

  # I. Reactive Data Frames -------------------------------------------------
  # Store user names and colors
  shared_vals <- reactiveValues(chip_color = NULL,
                                all_colors = tolower(colors()[grepl("^[^0-9]*$", colors())]),
                                taken_colors = NULL)


  # [0,] to give only the titles of the columns to the data.frame
  selectedPoints <- reactiveValues(data = cbind(clickable[0, ],
                                                betAmount = double()))

  roulette <- reactiveValues(winningSlot = NULL, history = NULL)

  resultsTable <- reactiveValues(data = cbind(clickable[0, ],
                                              betAmount = double()))

  completeList <- reactiveValues(data = cbind(slots = numeric(),
                                              #clickable[0, 9:ncol(clickable)],
                                              betAmount = double(),
                                              outcome = double(),
                                              winningSlot = double()))

  outcomesList <- reactiveValues(data = cbind(balance = 0,
                                              betNum = 0))

  # we define updatedbalance as a reactive value. This is because this value
  # will change depending on if have won or lost the bet...
  updatedbalance <- reactiveValues(balance = 0)



  session_vals <- reactiveValues(user_name = "", user_color = NULL, all_user_names = "", user_score = 0)

  # the dataframe that we are going to use for the plots.
  df_amount = reactive({
    martingale_strategy(input$num_sims, input$start_bet, input$bet_amount, roulette)
  })


  init_user_color <- FALSE

  session$onSessionEnded(function() {
    isolate({
      # User color operations
      shared_vals$all_colors <- c(shared_vals$all_colors, session_vals$user_color)
      shared_vals$taken_colors <- shared_vals$taken_colors[shared_vals$taken_colors != session_vals$user_color]

    })
  })

  # Observer to handle changes to the chip color
  observe({
    input$chipColor

    # G. For uninitialized session - set random color
    if (!init_user_color){
      # 1. Set a random color at first
      session_vals$user_color <- sample(shared_vals$all_colors, 1)
      # 2. Initialize the session
      init_user_color <<- TRUE

    } else {
      isolate({
        # H. Check that the color input is valid and allowed
        #print(input$chipColor)
        if (input$chipColor == session_vals$user_color || input$chipColor == "" || !(input$chipColor %in% shared_vals$all_colors)){
          print("Please choose a unique and allowed color")
          return()
        }
        # I. Put the old color to free colors and take it out of the taken colors
        shared_vals$all_colors <- c(shared_vals$all_colors, session_vals$user_color)
        shared_vals$taken_colors <- shared_vals$taken_colors[shared_vals$taken_colors != session_vals$user_color]

        # J. Update user color with the new choice
        session_vals$user_color <- input$chipColor
      })
    }
    # K. Add the new color to the global list for taken colors and out of avialable colors
    isolate(shared_vals$taken_colors <- c(shared_vals$taken_colors, session_vals$user_color))
    isolate(shared_vals$all_colors <- shared_vals$all_colors[shared_vals$all_colors != session_vals$user_color])

  })

  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul(lapply(shared_vals$users, function(user){
      return(tags$li(user))
    })))
  })



  # II. Bet Amount Selection -------------------------------------------------
  bet <- reactiveValues(amount = 10)

  # Choose the bet amount
  observeEvent(input$bet1, {
    print("Bet changed to $10")
    bet$amount <- 10
  })

  observeEvent(input$bet2, {
    print("Bet changed to $25")
    bet$amount <- 25
  })

  observeEvent(input$bet3, {
    print("Bet changed to $50")
    bet$amount <- 50
  })

  observeEvent(input$bet4, {
    print("Bet changed to $100")
    bet$amount <- 100
  })

  observeEvent(input$bet5, {
    print("Bet changed to $250")
    bet$amount <- 250
  })


  # III. Plots ---------------------------------------------------------------
  # Roulette table
  output$rTable <- renderPlot({
    rouletteTable <- ggplot() +
      geom_tile(data = df, aes(x, y, fill = factor(z), color = factor(z)), size = 1.5) +
      geom_polygon(data = twoToOne1, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = twoToOne2, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = twoToOne3, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = oneToEighteenSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = redSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = evenSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = oddSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = blackSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = thirdTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = secondTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = firstTwelveSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = ninteenThirtysixSlots, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 2) +
      geom_polygon(data = zeroPentagon, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      geom_polygon(data = doubleZeroPentagon, aes(x = x, y = y, fill = factor(z), color = factor(z)), size = 1.5) +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = colsTwo) +
      # 1-3: white circles; 4: transparent
      #geom_circle(aes(x0=c(df$x,0.5, 3.5, columnBets$x, splitBets$x, dozenBets$x,
      # outsideBets$x, quadBets$x, lineBets$x, streetBets$x, trioBets$x, topLineBets$x), y0=c(df$y, 23.9, 23.9, columnBets$y,
      # splitBets$y, dozenBets$y, outsideBets$y, quadBets$y, lineBets$y, streetBets$y, trioBets$y, topLineBets$y), r=.7, color =
      # factor('4'))) +
      annotate("text", x = df$x, y = df$y, label = annotationLabels, color = "white") +
      annotate("text", x = zerosAnnotationCoords$x, y = zerosAnnotationCoords$y, label = c("0", "00"),
               color = "white") +
      annotate("text", x = columnAnnotationCoords$x, y = columnAnnotationCoords$y, label = "2:1",
               size = 3.5, color = "white") +
      annotate("text", x = rep(-2, 3), y = c(3, 11, 19), label = c("3rd 12", "2nd 12", "1st 12"),
               color = "white", angle = -90, size = 5) +
      annotate("text", x = rep(-4, 6), y = c(1, 5, 9, 13, 17, 21),
               label = c("19to36", "Odd", "Black", "Red", "Even", "1to18"), color = "white", angle = -90, size = 4) +
      # show all clickable points
      # geom_point(data = clickable, aes(x=x, y=y)) +
      # Bets are drawn on the table here
      geom_point(data = NULL, aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = "dimgray", size = 12) +
      geom_point(data = NULL, aes(x = selectedPoints$data$x, y = selectedPoints$data$y),
                 colour = selectedPoints$data$user_color, size = 9) +
      # annotate("text", x = selectedPoints$data$x, y = selectedPoints$data$y, label = selectedPoints$data$betAmount,
      #          color = contrast_color(selectedPoints$data$user_color), size = 4) +
      coord_equal() +
      theme_bw() +
      ditch_the_axes

    #print(selectedPoints$data)
    #print(nrow(selectedPoints$data))

    if (nrow(selectedPoints$data) > 0) {
      rouletteTable <- rouletteTable +
        annotate("text", x = selectedPoints$data$x, y = selectedPoints$data$y, label = selectedPoints$data$betAmount,
                 color = contrast_color(selectedPoints$data$user_color), size = 4)

      rouletteTable
    } else {
      rouletteTable

    }
  }, width = 700, height = 700)


  # IV. Event Observers -----------------------------------------------------

  # this is an observer for when we click on the button "add"
  # this will update the balance with what we input numerically in the shiny app.
  # e.g. we input 1000, our balance is 1000.
  # then whenever we bet and loss (or hopefully) win, it will subtract (or add)
  # money to that amount.
  observeEvent(input$add ,{
    startingbalance <- input$startbalance
    updatedbalance$balance = updatedbalance$balance + startingbalance
    #paste("your balance is now ", updatedbalance$balance)
  })



  # click function
  selected_number <- reactiveValues()


  observeEvent(input$plot_click,{
    currentBet <- isolate(bet$amount)

    #this also works
    click <- nearPoints(clickable, input$plot_click, threshold = 20, maxpoints = 1)


    if(nrow(click) != 0 ){
    newBet <- cbind(click, betAmount = currentBet,user_color = session_vals$user_color)

    ## When we use selectedPoints, this is some sort of dataframe we defined
    ##at the beginning of the program. It contains all the info of clickable
    ## and the bet amount.
    ## If we want to see it outside a reactive environment we do :
    #isolate(selectedPoints$data)
    #isolate(newBet$data)

    # add new bet
    selectedPoints$data <- rbind(selectedPoints$data, newBet)

    paste("the number you clicked on is ", click$b1)

    selectedPoints$data <- as.data.frame(selectedPoints$data)

    # combine same bets. Easier to calculate the payouts.
    selectedPoints$data <- selectedPoints$data %>%
      group_by_at(setdiff(names(selectedPoints$data), c("betAmount", "x", "y", "user_color"))) %>%
      summarize(betAmount = sum(betAmount), x = tail(x, 1), y = tail(y, 1), user_color = last(user_color)) %>%
      as.data.table()

    }


  })


  ## D. Spin the wheel------
  observeEvent(input$spin, {
    # save the bets for results tables
    resultsTable$data <- selectedPoints$data

    # clear the betting table
    selectedPoints$data <- cbind(clickable[0, ], betAmount = double())

    # spin the roulette
    # roulette retourne une liste avec le numéro gagnant ainsi que ses charactéristiques
    # sous forme de 1 et 0.
    roulette$winningSlot <- roulette(verbose = TRUE)

    roulette$winningSlot$slotLanded

    # Save spin results
    roulette$history <- c(rep("",10),roulette$history,roulette$winningSlot$slotLanded)


    if (nrow(resultsTable$data) > 0) {
      tableOverall <- data.frame(slots = apply(resultsTable$data, 1, combineSlots),
                                 betAmount = resultsTable$data$betAmount,
                                 outcome = apply(resultsTable$data, 1, checkWin))

      #print("results table")
      #print(resultsTable$data) it prints what is betted on
      print("table_overall")
      print(tableOverall)


      # Grab the bets and sum the bet outcomes
      bet_results <- if(nrow(tableOverall) > 0){
        data.frame(outcome = apply(tableOverall, 1, computeTotal))
      }else{
        data.frame(outcome = NULL,
                   stringsAsFactors = FALSE)
      }


      print("bet_results")
      print(bet_results[[1]])

      #this is the same thing as total above.
    #   manualTotals <- ifelse(nrow(tableOverall[tableOverall$manualBet == TRUE, ]) > 0,
    #                          data.frame(outcome = apply(tableOverall[tableOverall$manualBet == TRUE, ], 1, computeTotal),
    #                                     stringsAsFactors = FALSE),
    #                          data.frame(outcome = NULL,
    #                                     stringsAsFactors = FALSE))
    #
    # }

    }

    # we print the sum of our bets.
    net_gain_loss <- sum(bet_results)
    print(net_gain_loss)


    # we add those gains to our balance
    # we have to use updatedbalance$balance in order for currentBalance to be numeric
    currentBalance <- updatedbalance$balance
    # the reactive value updatedbalance is then calculated this way
    updatedbalance$balance = currentBalance + as.numeric(net_gain_loss)



})

  # Reset the bet
  observeEvent(input$reset, {
    selectedPoints$data <- cbind(clickable[0, ], betAmount = double())
  })

  # V. Helper Functions -----

  checkWin <- function(row) {
    # Cette fonction retourne un string nous disant si on a gagné ou pas et combien

    # check for inside bets first row[10] -> row$type
    # 1 2 3  4  5  6  7  8  9      10    11        12
    # x y b1 b2 b3 b4 b5 b6 payOut type betAmount manualBet

    # On check si on est dans inside ou outside bets. Donc le type de bet
    if (row["type"] %in% c("Single", "Split", "Square Bet", "Line Bet", "Street Bet", "Trio Bet", "Top Line Bet")) {
      #On calcule le payout
      payout <- as.numeric(row["betAmount"]) * as.numeric(row["payOut"])
      # WinnerFlag is TRUE if one of the b1 to b6 is equal to the winning number.
      # na.rm is used to get rid of the NAs
      # e.g. When we bet on a single number. Only b1 is filled, b2 to b6 are not
      winnerFlag <- any(c(row[paste0("b", 1:6)]) == roulette$winningSlot$slotLanded, na.rm = TRUE)
      # this outputs us the amount we won or lost
      if (winnerFlag) {
        return(paste("won: $", payout, sep = ""))
      }
      else {
        return(paste("lost: $", row["betAmount"], sep = ""))
      }

      # checks if we're in outside bets
    }
    else if (row["type"] %in% c("Column Bet", "Dozen Bet", "High", "Low", "Even", "Odd", "Red", "Black")) {
      # we calculate the payouts
      payout <- as.numeric(row["betAmount"]) * as.numeric(row["payOut"])
      if (row["type"] == "Column Bet") {
        if (substr(row["b1"], 1, 1) == roulette$winningSlot$column) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Dozen Bet") {
        # print("check")
        # print(roulette$winningSlot$dozen)
        if (substr(row["b1"], 1, 1) == roulette$winningSlot$dozen) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "High") {
        if (roulette$winningSlot$high) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Low") {
        if (roulette$winningSlot$low) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Even") { # si la personne a bet sur Even
        if (roulette$winningSlot$even) { # Et si le résultat gagnant est even,
          # alors roulette$winningSlot$even est TRUE (ou 1), alors c'est gagné.
          return(paste("won: $", payout, sep = ""))
        }
        else { # sinon c'est perdu
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Odd") {
        if (roulette$winningSlot$odd) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Red") {
        if (roulette$winningSlot$color == 1) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else if (row["type"] == "Black") {
        if (roulette$winningSlot$color == 2) {
          return(paste("won: $", payout, sep = ""))
        }
        else {
          return(paste("lost: $", row["betAmount"], sep = ""))
        }
      }
      else {
        return("Unknown outside bet")
      }
    }
    else {
      return("Unclassified bet")
    }
  }

  computeTotal <- function(row) {
    # Cette fonction extrait les montants gagnés ou perdus.

    # Cette fonction est utilisée avec le data frame tableOverall.
    # Dans outcome : il y a soit "lost $ Bet$amount", soit "won $ payOut"
    # ce if, check si la première lettre est un "l", (ou un "w").
    if (substr(row["outcome"], 1, 1) == "l") {
      return(-1 * as.numeric(row["betAmount"])) # Si perdu, on perd le betAmount
    } else {
      return(as.numeric(str_extract(row["outcome"], "(?<=won: \\$)\\d*")))
      # Si gagné, on extrait ce qui est gagné du string et on le transforme en nombre.
    }
  }

  combineSlots <- function(row) {
    # Take the betting columns out
    row <- row[paste0("b", 1:6)]

    # concatenates all the numbers of the slots we betted on as a string.
    # e.g. if we split bet on 16 and 17 it gives "16, 17"
    # Remark : If we bet on Red, it outputs "Red", same for Dozen Bet it gives
    # "2nd Dozen".
    betList <- str_trim(str_replace_all(paste0(ifelse(is.na(row), "", row), collapse = ", "), "[\\, ]{3,}", ""))

    return(betList)
  }





  # VI. Outputs for UI ------------------------------------------------------

  # output$general_balance <- renderText({
  #     paste("The general balance is :", general_balance)
  # })

  output$roulette <- renderText({
    if (!is.null(roulette$winningSlot)) {
      paste("The winning slot is:", roulette$winningSlot$slotLanded)
      paste("The history of winning slots is : ", paste(roulette$history[length(roulette$history) - 10:length(roulette$history) -1]))
    } else {
      return(invisible(NULL))
    }
  })

  # this is in order for the UI to display or updated balance.
  output$generalbalance <- renderText({
    # if(!is.null(roulette$winningSlot)){
      paste("My balance is now " , updatedbalance$balance)
    # }
    # else if(add_button == TRUE){
    #   paste("My balance is now", updatedbalance$balance)
    # }
    # else{
    #   return(invisible(NULL))
    # }

  })

      observeEvent(input$run_simulation, {
        # Simulation Martingale
        df <- martingale_strategy(input$num_sims, input$start_bet, input$bet_amount, roulette, input$tot_spin)
        output$martingale_plot <- renderPlot({

          # Add a sequence column to represent the rows
          df$row <- seq_len(nrow(df))

          # Create an empty ggplot object
          p = ggplot()+
            labs(x = "Spins", y = "Balance")+
            geom_hline(yintercept = 0, linetype = "dotted", color = "black")+
            theme_minimal ()

        for(i in 1:nrow(df)){
          # Filter the dataframe for the current line
          df_line <- filter(df, row == i)
          df_line_pivot = pivot_longer(df_line,cols =-row, names_to = "Column", values_to = "Balance")
          # Add the line to the plot
          p <- p + geom_line(data = df_line_pivot, aes(x = 1:nrow(df_line_pivot), y = Balance), color = i,show.legend = TRUE)

        }
        print(p)
      })

        # now we plot the plot for the winrate
        df_win_rate <- win_rate(df_amount())
        output$win_rate_plot <- renderPlot({
          ggplot(data = df_win_rate, aes(x = 1:length(win_rate), y = win_rate, fill = win_rate > 0.5))+
            geom_col()+
            scale_fill_manual(values = c("red", "blue"), guide = guide_legend(title = "Winrate above 50%"))+
            labs(x = "ID of the simulation", y = "Winrate Percentage")+
            geom_hline(yintercept = 0.5, linetype = "dotted", color = "black")+
            theme_minimal ()
        })

    })



}




# IX. Run the app-----------------------------------------


# Run the application
shinyApp(ui = ui, server = server)

