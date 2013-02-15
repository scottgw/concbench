#!/usr/bin/Rscript

library(ggplot2)

results = read.csv('perf_results.csv')

tasks = c('condition', 'mutex', 'noshare', 'prodcons', 'share')
langs = c('java', 'scala', 'stm', 'scoop', 'scoop_poc')

# remove scoop to show others better
results = results[results$Language != 'scoop',]
results = results[results$Task != 'noshare',]


p <- ggplot(results, aes(x=Threads, y=Time, group=Language))
p <- p + geom_line (aes(color = Language))
p + facet_wrap (~ Task)
ggsave("time_graph.pdf")

for (task in tasks) {
  task_data = results[results$Task == task,]
  for (lang in langs) {
    thread1 = task_data [task_data$Language == lang & task_data$Threads == 1,c('Time')]
    task_data [task_data$Language == lang, c('Time')] <- thread1 /
      task_data [task_data$Language == lang, c('Time')]
  }
  results[results$Task == task,] <- task_data

}


p <- ggplot(results, aes(x=Threads, y=Time, group=Language))
p <- p + geom_line (aes(color = Language))
p + facet_wrap (~ Task)
ggsave("speedup_graph.pdf")


# ggsave(paste("graph", task, ".pdf", sep=""))