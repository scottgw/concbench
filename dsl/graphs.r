#!/usr/bin/Rscript

library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

results = read.csv(args[1])

ops = unique(results$operation)
types = unique(results$type)

for (op in ops) {
  opResults = results[results$operation == op,]

  p <- ggplot(opResults, aes(x=test, y=avg, fill=type))
  p <- p + ggtitle(op)
  p <- p + geom_bar(position="dodge", stat="identity")
  p <- p + geom_errorbar (aes(ymin=lower, ymax=upper),
                          position=position_dodge(.9))
  p <- p + theme(axis.text.x = element_text(angle=90, hjust=1))
  ggsave (paste (op, "-bar.pdf", sep=""))
}
