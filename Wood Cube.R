##########################################################
# Wood Cube
# Lars Mehwald
##########################################################

##########################################################
# create objects ("loading" wood bricks)
##########################################################

# array is indexed starting with 1
e <- list()

e1 <- array(0,dim=c(6,6,6))
e1[3:4,1:6,1] <- 1
e1[3:4,c(1,6),2] <- 1

e2 <- array(0,dim=c(6,6,6))
e2[3:4,1:6,1] <- 1
e2[3:4,c(1,2,6),2] <- 1

e3 <- array(0,dim=c(6,6,6))
e3[3:4,c(1,2,5,6),1] <- 1
e3[3,c(4,5),1] <- 1
e3[3:4,c(1,6),2] <- 1
e3[3,4,2] <- 1

e4 <- array(0,dim=c(6,6,6))
e4[3:4,1:6,1] <- 1
e4[3,4,1] <- 0
e4[3:4,c(1,6),2] <- 1

e5 <- array(0,dim=c(6,6,6))
e5[3:4,1:6,1] <- 1
e5[3:4,c(1,6),2] <- 1
e5[4,c(3,4),2] <- 1

e6 <- array(0,dim=c(6,6,6))
e6[3:4,1:6,1] <- 1
e6[3,c(3,4),1] <- 0
e6[3:4,c(1,6),2] <- 1
e6[4,c(2,5),2] <- 1

e[[1]] <- e1
e[[2]] <- e2
e[[3]] <- e3
e[[4]] <- e4
e[[5]] <- e5
e[[6]] <- e6

##########################################################
# create functions to manipulate objects/ bricks
##########################################################

rotate_along_z_r <- function(x) {
  # going throuh each horizontal layer separately
  for (i in 1:length(x[1,1,])) {
    x[,,i] <- t(apply(x[,,i], 2, rev))
  }
  return(x)
}

rotate_along_y_r <- function(x) {
  # going throuh each layer of the y axis separately
  for (i in 1:length(x[1,,1])) {
    x[,i,] <- t(apply(x[,i,], 2, rev))
  }
  return(x)
}

rotate_along_x_r <- function(x) {
  # going throuh each layer of the y axis separately
  for (i in 1:length(x[,1,1])) {
    x[i,,] <- t(apply(x[i,,], 2, rev))
  }
  return(x)
}

lift_1 <- function(x) {
  
  # create a new object to store the new object
  tt <- array(0,dim=c(6,6,6))
  
  tt[,,2] <- x[,,1]
  tt[,,3] <- x[,,2]
  
  return(tt)
}

##########################################################
# Create aggregate functions that move a brick to a defined position
##########################################################

to1 <- function(x) lift_1(x)
# to1b <- function(x) rotate_along_z_r(rotate_along_z_r(to1(x)))
to2 <- function(x) rotate_along_x_r(rotate_along_x_r(lift_1(x)))
to2b <- function(x) rotate_along_z_r(rotate_along_z_r(to2(x)))

to3 <- function(x) rotate_along_x_r(rotate_along_z_r(lift_1(x)))
to3b <- function(x) rotate_along_y_r(rotate_along_y_r(to3(x)))
to4 <- function(x) rotate_along_x_r(rotate_along_x_r(rotate_along_x_r(rotate_along_z_r(lift_1(x))))) 
to4b <- function(x) rotate_along_y_r(rotate_along_y_r(to4(x)))

to5 <- function(x) rotate_along_y_r(rotate_along_z_r(lift_1(x)))
to5b <- function(x) rotate_along_x_r(rotate_along_x_r(to5(x)))
to6 <- function(x) rotate_along_y_r(rotate_along_y_r(rotate_along_y_r(rotate_along_z_r(lift_1(x)))))
to6b <- function(x) rotate_along_x_r(rotate_along_x_r(to6(x)))

##########################################################
# create a df that controlls the rotations 
# (the values of the df are used as inputs to the rotation)
##########################################################

df <- data.frame()
i = 1
for (i2 in 1:2) {
  for (i3 in 1:2) {
    for (i4 in 1:2) {
      for (i5 in 1:2) {
        for (i6 in 1:2) {
          df[i, 1] <- i2
          df[i, 2] <- i3
          df[i, 3] <- i4
          df[i, 4] <- i5
          df[i, 5] <- i6
          i <- i + 1
        }
      }
    }
  }
}
rm(i)

##########################################################
# Loop to test all possibilities, incl a function to evaluate success
# possible combinations are printed to the console
##########################################################

for (l2 in 2:6) {
  for (l3 in 2:6) {
    for (l4 in 2:6) {
      for (l5 in 2:6) {
        for (l6 in 2:6) {
          for (i in 1:nrow(df)) {
            if (length(unique(c(l2, l3, l4, l5, l6))) == 5) {
              catch <- array(0, dim = c(6, 6, 6))
              catch <- catch + to1(e1)
              if (df[i, 1] == 1) {
                catch <- catch + to2(e[[l2]])
              } else {
                catch <- catch + to2b(e[[l2]])
              }
              if (df[i, 2] == 1) {
                catch <- catch + to3(e[[l3]])
              } else {
                catch <- catch + to3b(e[[l3]])
              }
              if (df[i, 3] == 1) {
                catch <- catch + to4(e[[l4]])
              } else {
                catch <- catch + to4b(e[[l4]])
              }
              if (df[i, 4] == 1) {
                catch <- catch + to5(e[[l5]])
              } else {
                catch <- catch + to5b(e[[l5]])
              }
              if (df[i, 5] == 1) {
                catch <- catch + to6(e[[l6]])
              } else {
                catch <- catch + to6b(e[[l6]])
              }
              catch <- catch[2:5, 2:5, 2:5]
              
              if (!is.element(TRUE, catch > 1)) {
                print(
                  paste0(
                    "element ",
                    l2,
                    " on position 2, and ",
                    "element ",
                    l3,
                    " on position 3, and ",
                    "element ",
                    l4,
                    " on position 4, and ",
                    "element ",
                    l5,
                    " on position 5, and ",
                    "element ",
                    l6,
                    " on position 6"
                  )
                )
              }
            }
          }
        }
      }
    }
  }
}
