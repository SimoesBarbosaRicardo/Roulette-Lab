#Function for delay between frames found on stack overflow
sys_sleep <- function(val, unit = c("s", "ms", "us", "ns")) {
  start_time <- microbenchmark::get_nanotime()
  stopifnot(is.numeric(val))
  unit <- match.arg(unit, c("s", "ms","ns"))
  val_ns <- switch (unit,
                    "s" = val * 10**9,
                    "ms" = val * 10**7,
                    "ns" = val * 10**6,

  )
  repeat {
    current_time <- microbenchmark::get_nanotime()
    diff_time <- current_time - start_time
    if (diff_time > val_ns) break
  }
}

#To convert winning number into the right frame
transform_numb = function(winning_number){
  wheel_numbers <- c(17, 32, 20, 7, 11, 30, 26, 9, 28, 0, 2, 14, 35, 23, 4, 16, 33, 21, 6, 18, 31, 19, 8, 12, 29, 25, 10, 27, "00", 1, 13, 36, 24, 3, 15, 33, 22, 5)
  wheel_numbers_char <- as.character(wheel_numbers)


  index <- which(wheel_numbers_char == as.character(winning_number))[1]
  return(index)
}




#Plot of the wheel
draw_wheel <- function() {
  par(plt = c(0, 1, 0, 1), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-2, 2), asp=1)

  # Define colors for the wheel
  wheel_colors <- c("black", "red", "green")
  # Define the numbers on the wheel, including 0, 00, and 1-36
  wheel_numbers <- c(17, 32, 20, 7, 11, 30, 26, 9, 28, 0, 2, 14, 35, 23, 4, 16, 33, 21, 6, 18, 31, 19, 8, 12, 29, 25, 10, 27, "00", 1, 13, 36, 24, 3, 15, 33, 22, 5)
  n=38

  points(0, 0, pch = 1, col = "#996600", cex = 60.7,lwd= 35)
  points(0, 0, pch = 8, col = "#330000", cex = 46,lwd= 2)

  # Loop through each sector on the wheel
  for (i in 1:n) {
    # Calculate the starting angle of the current sector
    theta1 <- 2*pi * (i - 1) / n
    # Calculate the ending angle of the current sector
    theta2 <- 2*pi * i / n
    # Calculate the x and y coordinates of the starting point of the current sector
    x1 <- 1.5 * cos(theta1)
    y1 <- 1.5 * sin(theta1)
    # Calculate the x and y coordinates of the ending point of the current sector
    x2 <- 1.5 * cos(theta2)
    y2 <- 1.5 * sin(theta2)

    # Define the x and y coordinates of the sector's vertices
    sector_x <- c(0, x1, x2)
    sector_y <- c(0, y1, y2)


    if (i == 10) {
      sector_color <- "green"  # set the color of the number 0 to white
    } else if (i==29){
      sector_color <- "green"
    }
    else {
      sector_color <- wheel_colors[(i-1) %% 2 + 1]
    }

    # Draw the sector using the calculated vertices and the appropriate color
    polygon(sector_x, sector_y, col = sector_color, border = "white")

    # Calculate the midpoint angle of the current sector for placing the number
    mid_theta <- (theta1 + theta2) / 2
    # Calculate the x and y coordinates of the midpoint of the current sector
    mid_x <- 1.3 * cos(mid_theta)
    mid_y <- 1.3 * sin(mid_theta)

    # Place the number at the midpoint of the current sector
    text(mid_x, mid_y, wheel_numbers[i], col = "white", cex = 1.5)
  }

  # Add a white circle in the middle of the plot with a radius of 0.4
  symbols(0, 0, circles = 0.9, inches = FALSE, add = TRUE, fg = "#996633", bg = "#996633")

  points(0, 0, pch = 8, col = "#330000", cex = 22,lwd= 2)
  points(0, 0, pch = 1, col = "#F4edde", cex = 45,lwd= 4)

  points(0, 0, pch = 1, col = "#F4edde", cex = 58,lwd= 6)
  points(0, 0, pch = 1, col = "#F4edde", cex = 33,lwd= 14)

  points(0, 0, pch = 3, col = "#F4edde", cex = 10,lwd= 4)#cross
  points(0, 0, pch = 16, col = "#F4edde", cex = 5,lwd= 4)#Mid_ball

  points(0, 0.4, pch = 16, col = "#F4edde", cex = 2,lwd= 2)#Sides balls
  points(-0.4, 0, pch = 16, col = "#F4edde", cex = 2,lwd= 2)
  points(0, -0.4, pch = 16, col = "#F4edde", cex = 2,lwd= 2)
  points(0.4, 0, pch = 16, col = "#F4edde", cex = 2,lwd= 2)

  #Losanges
  points(1.465, 0.6337427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(-1.465, 0.6337427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(1.465, -0.6337427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(-1.465, -0.6337427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)

  points(0.6525, 1.4537427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(-0.6525, 1.4537427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(0.6525, -1.4537427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)
  points(-0.6525, -1.4537427, pch = 18, col = "#F4edde", cex = 2,lwd= 2)

}






spin_animation = function(Winning_number=10){
  #As frames start with 17 (first frame), we need to transform the winning number to frames
  frame_nb= transform_numb(Winning_number)
  #Point coordinates
  point_x=matrix()
  point_y=matrix()

  #Base distance from origin
  distance = 1.6
  final_speed= 500
  nb_loops = 1

  total_frames = 38*nb_loops + frame_nb
  delay = total_frames/30 #Divided by 30 otherwise delay is to0 big


  for (i in 1:total_frames) {

    n <- 38
    theta1 <- 2*pi * (i - 1) / n
    # Calculate the ending angle of the current sector
    theta2 <- 2*pi * i / n
    # Calculate the midpoint angle of the current sector for placing the number
    mid_theta <- round(((theta1 + theta2) / 2),3)

    # Calculate the x and y coordinates of the midpoint of the current sector
    point_x[i] <- distance * cos(mid_theta)
    point_y[i] <- distance * sin(mid_theta)
    #To reduce the distance from the center after each iteration
    distance = distance - (1.6-1)/total_frames
  }





  for (i in 1:total_frames) {
    draw_wheel()

    points(point_x[i] , point_y[i], pch = 19, col = "#999999",cex=2)
    points(point_x[i] , point_y[i], pch = 19, col = "#cccccc",cex=1.8)
    points(point_x[i] , point_y[i], pch = 19, col = "#ffffff",cex=1.3)



    system.time(sys_sleep(delay, "ns"))
    delay = delay + ((final_speed/total_frames)) #700 ms looks like a nice final speed
    print(delay)


  }
}



spin_animation(Winning_number = 17)






