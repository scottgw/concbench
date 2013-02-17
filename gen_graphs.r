#!/usr/bin/Rscript

library(ggplot2)

results = read.csv('perf_results.csv')

tasks = c('condition', 'mutex', 'noshare', 'prodcons', 'share')
langs = c('java', 'scala', 'stm', 'scoop', 'scoop_poc')

# remove scoop to show others better
# results = results[results$Language != 'scoop',]
# results = results[results$Task != 'noshare',]


# ES / PoC comparison, relies on 'Time' being replaced with speedup
# in the previous loop.
results32 = results[results$Threads == 32,]
results32$VsScoop <- NA

for (task in tasks) {
  task_data = results32[results32$Task == task,]

  scoop = task_data [task_data$Language == 'scoop' &
    task_data$Threads == 32,c('Time')]
  task_data$VsScoop <- scoop / task_data$Time
  results32[results32$Task == task,] <- task_data
}

print (results32)
p <- ggplot(results32[results32$Language == 'scoop_poc',],
            aes(x=Task, y=VsScoop, fill=Task))
p <- p + geom_bar()
ggsave (paste (task, "-bar.pdf", sep=""))


## p <- ggplot(results, aes(x=Threads, y=Time, group=Language))
## p <- p + geom_line (aes(color = Language))
## p + facet_wrap (~ Task)
## ggsave("time_graph.pdf")


## for (task in tasks) {
##   task_data = results[results$Task == task,]
##   p <- ggplot(task_data, aes(x=Threads, y=Time, group=Language))
##   p <- p + geom_line (aes(color = Language))
##   ggsave (paste (task, "-time.pdf", sep=""))
## }


for (task in tasks) {
  task_data = results[results$Task == task,]
  for (lang in langs) {
    thread1 = task_data [task_data$Language == lang & task_data$Threads == 1,c('Time')]
    task_data [task_data$Language == lang, c('Time')] <- thread1 /
      task_data [task_data$Language == lang, c('Time')]
  }
  results[results$Task == task,] <- task_data

  ## p <- ggplot(task_data, aes(x=Threads, y=Time, group=Language))
  ## p <- p + geom_line (aes(color = Language))
  ## ggsave (paste (task, "-speedup.pdf", sep=""))
}


## p + facet_wrap (~ Task)
## ggsave("speedup_graph.pdf")
# ggsave(paste("graph", task, ".pdf", sep=""))

