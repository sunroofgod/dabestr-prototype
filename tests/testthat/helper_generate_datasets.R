generate_non_proportional_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed)
  
  c1 <- rnorm(N, mean = 3, sd = 0.4)
  c2 <- rnorm(N, mean = 3.5, sd = 0.75)
  c3 <- rnorm(N, mean = 3.25, sd = 0.4)
  
  t1 <- rnorm(N, mean = 3.5, sd = 0.5)
  t2 <- rnorm(N, mean = 2.5, sd = 0.6)
  t3 <- rnorm(N, mean = 3, sd = 0.75)
  
  gender <- c(rep('Male', N/2), rep('Female', N/2))
  id <- 1: N
  
  wide_data <- tibble::tibble(
    Control1 = c1, Control2 = c2, Control3 = c3,
    Test1 = g1, Test2 = g2, Test3 = g3, 
    Gender = gender, ID = id)
  
  sample_tidy_dabestr_dataset <- wide_data %>%
    tidyr::gather(key = Group, value = Measurement, -ID, -Gender)
  
  return(sample_tidy_dabestr_dataset)
}

generate_proportional_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed)
  
  my.data.control_proportional <- data.frame(
    Group = sample(c("Control1"), N, TRUE)
  )
  my.data.control_proportional$Success <- numeric(nrow(my.data.control_proportional))
  my.data.control_proportional$Success[sample(nrow(my.data.control_proportional), 8)] <- 1
  
  my.data.test_proportion1 <- data.frame(
    Group = sample(c("Test1"), N, TRUE)
  )
  my.data.test_proportion1$Success <- numeric(nrow(my.data.test_proportion1))
  my.data.test_proportion1$Success[sample(nrow(my.data.test_proportion1), 22)] <- 1
  
  my.data.test_proportion2 <- data.frame(
    Group = sample(c("Test2"), N, TRUE)
  )
  my.data.test_proportion2$Success <- numeric(nrow(my.data.test_proportion2))
  my.data.test_proportion2$Success[sample(nrow(my.data.test_proportion2), 15)] <- 1
  
  my.data.test_proportion3 <- data.frame(
    Group = sample(c("Test3"), N, TRUE)
  )
  my.data.test_proportion3$Success <- numeric(nrow(my.data.test_proportion3))
  my.data.test_proportion3$Success[sample(nrow(my.data.test_proportion3), 18)] <- 1
  
  my.data.test_proportion4 <- data.frame(
    Group = sample(c("Test4"), N, TRUE)
  )
  my.data.test_proportion4$Success <- numeric(nrow(my.data.test_proportion4))
  my.data.test_proportion4$Success[sample(nrow(my.data.test_proportion4), 20)] <- 1
  
  my.data.test_proportion5 <- data.frame(
    Group = sample(c("Control2"), N, TRUE)
  )
  my.data.test_proportion5$Success <- numeric(nrow(my.data.test_proportion5))
  my.data.test_proportion5$Success[sample(nrow(my.data.test_proportion5), 4)] <- 1
  
  my.data.proportional <- rbind(my.data.control_proportional, my.data.test_proportion1, my.data.test_proportion2, my.data.test_proportion3, my.data.test_proportion4, my.data.test_proportion5)
  id <- data.frame(ID = rep(c(1:N), 6))
  
  my.data.proportional <- cbind(my.data.proportional, id)
}

generate_deltadelta_dataset <- function(N = 40, seed = 12345) {
  set.seed(seed)

  placebo <- rnorm(N, mean = 3, sd = 0.4)
  drug <- rnorm(N, mean = 3.5, sd = 0.75)
  genotype <- c(rep('M', N/2), rep('W', N/2))
  id <- 1: N
  Rep <- rep(c("Rep1", "Rep2"), N/2)
  
  wide_data <- tibble::tibble(
    Placebo = placebo,
    Drug = drug,
    Genotype = genotype, 
    ID = id,
    Rep = Rep)
  
  sample_tidy_dabestr_dataset <- wide_data %>%
    tidyr::gather(key = Treatment, value = Measurement, -ID, -Genotype, -Rep)
  
  return(sample_tidy_dabestr_dataset)
}
