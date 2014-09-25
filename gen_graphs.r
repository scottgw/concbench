#!/usr/bin/Rscript

library(ggplot2)

results = read.csv('perf_results.csv')

tasks = c('condition', 'mutex', 'noshare', 'prodcons', 'share')
#langs = c('java', 'scala', 'stm', 'scoop', 'scoop_poc')
langs = c('java', 'scala', 'stm', 'scoop', 'qs')

for (task in tasks) {
  for (lang in langs) {
    for (thread in unique(results$Thread)) {
      sub = results[results$Task == task &
        results$Language == lang &
        results$Thread == thread,]
      sub$Time <- mean(sub$Time)
      results[results$Task == task &
              results$Language == lang &
              results$Thread == thread,] <- sub
    }
  }
}

# results = unique(results)
# print (results)
# 
# 
# # ES / PoC comparison, relies on 'Time' being replaced with speedup
# # in the previous loop.
# results32 = results[results$Threads == 32,]
# results32$VsScoop <- NA
# 
# for (task in tasks) {
#   task_data = results32[results32$Task == task,]
# 
#   scoop = task_data [task_data$Language == 'scoop' &
#     task_data$Threads == 32,c('Time')]
#   task_data$VsScoop <- scoop / task_data$Time
#   results32[results32$Task == task,] <- task_data
# }
# 
# p <- ggplot(results32[results32$Language == 'scoop_poc',],
#             aes(x=Task, y=VsScoop, fill=Task))
# p <- p + geom_bar() + guides (fill=FALSE)
# ggsave (paste (task, "-bar.pdf", sep=""))
# 
# 
# # remove scoop to show others better
# results = results[results$Language != 'scoop_poc',]
# #results = results[results$Task != 'noshare',]

## time faceted graph
#p <- ggplot(results, aes(x=Threads, y=Time, group=Language))
#p <- p + geom_line (aes(color = Language))
#p + facet_wrap (~ Task, scales = "free_y")
#ggsave("time-facet.pdf", width=10, height=7)

## individual time graphs
#for (task in tasks) {
#  task_data = results[results$Task == task,]
#  p <- ggplot(task_data, aes(x=Threads, y=Time, group=Language))
#  p <- p + geom_line (aes(color = Language))
##  print (task)
##  data_32 = task_data[task_data$Thread == 32,]
##  scoop_poc_data = data_32[data_32$Language == 'scoop_poc',]
##  qs_data = data_32[data_32$Language == 'qs',]
##  print (scoop_poc_data / qs_data)
#  ggsave (paste (task, "-time.pdf", sep=""))
#}

# individual time graphs
for (task in tasks) {
  task_data = results[results$Task == task,]
  p <- ggplot(task_data, aes(x=Language, y=Time))
  p <- p + geom_bar (aes(color = Language))
#  print (task)
#  data_32 = task_data[task_data$Thread == 32,]
#  scoop_poc_data = data_32[data_32$Language == 'scoop_poc',]
#  qs_data = data_32[data_32$Language == 'qs',]
#  print (scoop_poc_data / qs_data)
  ggsave (paste (task, "-time.pdf", sep=""))
}


# 
# 
# speedup_results = results
# speedup_results$Speedup <- NA
# 
# for (task in tasks) {
#   task_data = speedup_results[speedup_results$Task == task,]
#   for (lang in langs) {
#     thread1 = task_data [task_data$Language == lang & task_data$Threads == 1,c('Time')]
#     task_data [task_data$Language == lang, c('Speedup')] <- thread1 /
#       task_data [task_data$Language == lang, c('Time')]
#   }
#   speedup_results[results$Task == task,] <- task_data
# 
#   p <- ggplot(task_data, aes(x=Threads, y=Speedup, group=Language))
#   p <- p + geom_line (aes(color = Language))
#   ggsave (paste (task, "-speedup.pdf", sep=""))
# }
# 
# p <- ggplot(speedup_results, aes(x=Threads, y=Speedup, group=Language))
# p <- p + geom_line (aes(color=Language))
# p + facet_wrap(~ Task, scales = "free_y")
# ggsave("speedup-facet.pdf")
# # ggsave(paste("graph", task, ".pdf", sep=""))
# 
