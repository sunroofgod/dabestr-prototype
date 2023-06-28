sigmoid <- function(x_from, y_from, y_to, smooth = 5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) + x_from,
                    y = y * (y_to - y_from) + y_from)
}




#create dataframes for rect and flows

library(dplyr)
group_counts <- df %>%
  group_by(Group) %>%
  count()

means_s_f <- dabest_obj_proportional.cohens_h[["proportional_data"]]$proportion_success

N = 40
gap = 0.1

group_sizes <-dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(Group) %>%
  summarize(group_size = n())

value_end1 <- c(means_s_f[2]- gap/2)

success_continued <- dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(ID) %>%
  summarise(success_change = any(Success == 1 & Group == "Control1") & any(Success == 1 & Group == "Test1")) %>%
  filter(success_change) %>%
  summarise(C1T1 = n()/N) %>%
  mutate(next1 = value_end1[1]) 
value_end2 <- c(means_s_f[1] - gap/2 )
failure_continued <- dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(ID) %>%
  summarise(success_change = any(Success == 0 & Group == "Control1") & any(Success == 0 & Group == "Test1")) %>%
  filter(success_change) %>%
  summarise(C1T1F = n()/N) %>%
  mutate(nextnode = c(value_end2[1])) 
ss_sig$y[2]
ss_sig <- data.frame(y = unlist(c(success_continued$C1T1, success_continued$next1)))
ss_sig1 <- data.frame(y = unlist(c(failure_continued$C1T1F, failure_continued$nextnode)))

sig1 <- sigmoid(1, ss_sig$y[1], ss_sig$y[2])
start1 <- data.frame(x = c(sig1$x[1]), y = c(sig1$y[1]))
gap1  <- data.frame(x = c(2, 2), y = c(1-means_s_f[2], means_s_f[2] + gap/2))
sig2 <- sigmoid(1, means_s_f[1] - gap/2, 1 - ss_sig1$y[1])
end1 <- data.frame(x=c(1), y = c(ss_sig[[1]]))
data_for_flow1 <- rbind(end1, sig2, sig1, start)


ggplot(data_for_flow1, aes(x = x, y = y))+
         geom_sankeyflow()



# Print the result
print(ndf)
sig1 <- sigmoid()


#find ss and ff, then it should solve for all flows



