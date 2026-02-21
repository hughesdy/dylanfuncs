#' Estimate the amount of time left in a loop
#'
#' @description
#'  This function is embedded in a loop and will print updates on the estimated time remaining in the loop to the R console.
#' @usage how_much_longer(toti)
#' @param toti This represents the length of the vector the loop iterates over. So if the loop will iterate over a vector of length 10 (i.e., 10 iterations), toti would equal 10.
#' @details
#' This function can be embedded in any loop to estimate how much time is left before the loop finishes. See examples for how to use within a loop. It estimates the remaining time by creating and continually updating a vector containing the amount of time each loop takes and then taking the mean of the vector and multiplying by the number of iterations remaining. Probs one of the most powerful funcs I've written hehe
#' @author Original code by Dylan E. Hughes
#' @examples
#' dvs = sample(LETTERS, 10)
#'
#' timer <- how_much_longer(toti = length(dvs))
#'
#' for (i in seq_along(dvs)) {
#'   dv = dvs[i]

#'   time = system.time({
#'     final_df[,i] = some_function(dv = dv)
#'   })

#'   timer(time, i)
#' }
#'

how_much_longer <- function(toti) {

  times <- numeric(0)

  function(process_time, i) {
    elapsed <- as.numeric(process_time[3])
    times[i] <<- elapsed

    avg_time <- mean(times[1:i])
    time_left <- (avg_time * (toti - i)) / 60 # estimate minutes left

    mins_left <- floor(time_left) # pull minutes
    secs_left <- round((time_left %% 1) * 60) # pull seconds

    cat(sprintf("\rEstimated time remaining: %g mins %g secs     ", mins_left, secs_left))
    flush.console()
  }
}
