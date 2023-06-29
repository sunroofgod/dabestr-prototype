sigmoid <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_from)
}

flipped_sig <- function(x_from, factor, y_from, y_to, smooth = 5.5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- -exp(-x) / (exp(-x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * factor + x_from,
                    y = y * (y_to - y_from) + y_to)
}

factor <- 1-0.2

#create dataframes for rect and flows

library(dplyr)
group_counts <- dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(Group) %>%
  count()


means_c_t <- dabest_obj_proportional.cohens_h[["proportional_data"]]$proportion_success

N = group_counts[1,2][[1]]
N
gap = 0.05

#group_sizes <-dabest_obj_proportional.cohens_h[["raw_data"]] %>%
#  group_by(Group) %>%
#  summarize(group_size = n())


success_success <- dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(ID) %>%
  summarise(success_change = any(Success == 1 & Group == "Control1") & any(Success == 1 & Group == "Test1")) %>%
  filter(success_change) %>%
  summarise(C1T1 = n()/N)

failure_failire <- dabest_obj_proportional.cohens_h[["raw_data"]] %>%
  group_by(ID) %>%
  summarise(success_change = any(Success == 0 & Group == "Control1") & any(Success == 0 & Group == "Test1")) %>%
  filter(success_change) %>%
  summarise(C1T1F = n()/N)
ss <- success_success$C1T1[1]
value_start1 <- success_success$C1T1[1] - gap/8
value_start2 <- means_c_t[1]- gap/2 - gap/8
value_end1 <- means_c_t[2] + gap/2 +gap/8
value_end2 <- 1- failure_failire$C1T1F[1] + gap/8

#Pss --> mean-t + gap/2
#mean-c - gap/2 --> 1-ff

flow_start1 <- 1- failure_failire$C1T1F[1]
flow_end1 <- means_c_t[2] - gap/2
flow_start2 <- means_c_t[1] + gap/2
flow_end2 <- success_success$C1T1[1]

# from ss to mean plus gap
sig1 <- sigmoid(1 + 0.1, 0.8, value_start1, value_end1)
sig2 <- sigmoid(1 + 0.1, 0.8, value_start2, value_end2)
sig1 <- arrange(sig1, desc(x))
sig3 <- flipped_sig(1 + 0.1, 0.8, flow_start1, flow_end1)
sig4 <- flipped_sig(1 + 0.1 , 0.8, flow_start2, flow_end2)
sig4 <- arrange(sig4, desc(x))
data_for_flow1 <- rbind(sig2, sig1)
data_for_flow2 <- rbind(sig3, sig4)
data_for_rects <- data.frame(x = c( 1, 2, 2, 1, 1, 2, 2, 1, 1), y = c( 1, 1, flow_start1, flow_start1, value_start1, value_start1, 0, 0, value_start1))
data_for_rect_top <- data.frame(x = c( 1, 2, 2, 1), y = c( 1, 1, flow_start1, flow_start1))
data_for_rect_bot <- data.frame(x = c( 1, 2, 2, 1), y = c(ss, ss, 0, 0))
data_for_bars <- dabest_obj_proportional.cohens_h[["proportional_data"]]
idx_axis <- seq(1, length(unlist(dabest_obj_proportional.cohens_h[["idx"]])), 1)

library(ggplot2)
ggplot()+
  geom_sankeyflow(data = data_for_flow1, aes(x = x, y = y, fillcol = "#db6159")) +
  geom_sankeyflow(data = data_for_flow2, aes(x = x, y = y, fillcol = "#818181")) +
  geom_sankeyflow(data = data_for_rect_top, aes(x = x, y = y, fillcol = "#818181")) +
  geom_sankeyflow(data = data_for_rect_bot, aes(x=x, y = y, fillcol = "#db6159")) +
  geom_sankeybar(data = data_for_bars, 
                 aes(x = idx_axis,
                     ysuccess = y_success, 
                     yfailure = y_failure, 
                     proportionsuccess = proportion_success, width = 0.1)) +
  scale_x_continuous(limits = c(0,3),
                     breaks = c(1,2),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1),
                     expand = c(0,0))
dabest_obj_proportional.cohens_h[["proportional_data"]]



#find ss and ff, then it should solve for all flows



