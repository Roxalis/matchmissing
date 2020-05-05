library(tidyverse)
library(lubridate)

######## Functions

compare_two <- function(querry, search) {
  result <- list()
  
  for (n in seq_along(querry)) {
    tmp <-
      ifelse(!is.na(search) &
               !is.na(querry[n]),
             ifelse(search == querry[n], 1, 0),
             0)
    result[[n]] <- tmp
  }
  
  out <- do.call("rbind", result)
  
  return(out)
  
}

compare_two_bolean <- function(querry, search) {
  result <- list()
  
  for (n in seq_along(querry)) {
    tmp <-
      ifelse(!is.na(search) & !is.na(querry[n]),
             ifelse((search & querry[n]) == TRUE, 1, 0),
             0)
    result[[n]] <- tmp
  }
  
  out <- do.call("rbind", result)
  
  return(out)
  
}

compare_three <- function(ins_avg, ins1, ins2) {
  result <- list()
  
  for (n in seq_along(ins_avg)) {
    tmp <-
      ifelse(
        !is.na(ins1) &
          !is.na(ins2) &
          !is.na(ins_avg[n]),
        ifelse(ins1 - 10 < ins_avg[n] &
                 ins_avg[n] < ins2 + 10, 1, 0),
        0
      )
    result[[n]] <- tmp
  }
  
  out <- do.call("rbind", result)
  
  return(out)
  
}

compare_dates <- function(edod.from, edod.to, dod, m.d) {
  result <- list()
  
  for (n in seq_along(dod)) {
    tmp <-
      ifelse(
        !is.na(m.d) &
          !is.na(edod.from[n]) &
          !is.na(edod.to[n]) &
          !is.na(dod[n]),
        ifelse((edod.from[n] - years(5) < m.d &
                  m.d < edod.from[n]) |
                 (edod.from[n] < m.d &
                    m.d < edod.to[n]) |
                 (dod[n] - years(5) < m.d & m.d < dod[n]),
               1,
               0
        ),
        0
      )
    result[[n]] <- tmp
  }
  
  out <- do.call("rbind", result)
  
  return(out)
  
}

get_instances <- function(df) {
  m <- 1
  tmp <- vector()
  
  for (i in 2:(ncol(df))) {
    for (n in 1:nrow(df)) {
      if (!is.na(df[n, i])) {
        tmp[m] <- paste(df[n, 1], colnames(df[n, i]), sep = ",")
        
        
      } else{
        tmp[m] <- NA
        
      }
      
      m <- m + 1
    }
    
    
    
  }
  
  return(tmp)
  
}

###########################################

df_d <- read_csv("data/missing.csv")
df_u <- read_csv("data/unidentified.csv")

# Compare eye color
m_1 <- compare_two(df_d$color.eyes, df_u$color.eyes)

# Compare hair color
m_2 <- compare_two(df_d$color.hair,df_u$color.hair)

# Compare race
m_3 <- compare_two(df_d$race,df_u$race)

# Compare gender
m_4 <- compare_two(df_d$gender,df_u$gender)

# Compare state
#m_5 <- compare_two(df_d$state.lastseen,df_u$state.found)

# Compare tattoos
m_6 <- compare_two_bolean(df_d$tattoo,df_u$tattoo)

# Compare scars
m_7 <- compare_two_bolean(df_d$scar,df_u$scar)

# Compare age
age_avg <- (df_d$age.from + df_d$age.to) / 2
m_8 <- compare_three(age_avg, df_u$age.from, df_u$age.to)

# Compare weight
weight_avg <- (df_d$weight.from + df_d$weight.to) / 2
m_9 <- compare_three(weight_avg, df_u$weight.from, df_u$weight.to)

# Compare height
height_avg <- (df_d$height.from + df_d$height.to) / 2
m_10 <- compare_three(height_avg, df_u$height.from, df_u$height.to)

# Compare dates

m_11 <- compare_dates(df_u$edod.date.from, df_u$edod.date.to, df_u$dod, df_d$missing.date)

# Matrix addition

m <- m_1 + m_2 + m_3 + m_4 + m_6 + m_7 + m_8 + m_9 + m_10 + t(m_11) # + m_5

# First check how many instances (fast)

m[is.na(m)] <- 0
length(m[m == 9])

# Now get those instances (slow! 1 hour for complete set)

m_t <- as_tibble(m)
m_t[m_t < 9] <- NA
m_t <- add_column(m_t, id = df_d$id,.before = "V1")
colnames(m_t) <- c("id", df_u$id)
  
tmp <- get_instances(m_t)

r <- tmp[!is.na(tmp)]
write_lines(r, "data/result_9.txt")

############################









