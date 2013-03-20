#!/usr/bin/Rscript

library(ggplot2)
library(plyr)

args <- commandArgs(trailingOnly = TRUE)

results <- read.csv(args[1])
results$time <- results$time * 1000

## ops = unique(results$operation)
## types = unique(results$type)

## for (op in ops) {
##   opResults = results[results$operation == op,]

##   p <- ggplot(opResults, aes(x=test, y=avg, fill=type))
##   p <- p + ggtitle(op)
##   p <- p + geom_bar(position="dodge", stat="identity")
##   p <- p + geom_errorbar (aes(ymin=lower, ymax=upper),
##                           position=position_dodge(.9))
##   p <- p + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
##   ggsave (paste (op, "-bar.pdf", sep=""))
## }

resultsCI <- ddply (results,
                    .(type, operation, test),
                    summarise,
                    N    = length(time),
                    median = median(time),
                    q1   = quantile (time, 0.25),
                    q3   = quantile (time, 0.75),
                    mean = mean(time),
                    sd   = sd(time),
                    se   = sd(time) / sqrt(length(time)))

ciMult <- qt(0.995, resultsCI$N-1)

resultsCI$lower <- resultsCI$mean - resultsCI$se*ciMult
resultsCI$upper <- resultsCI$mean + resultsCI$se*ciMult

## p <- ggplot(resultsCI, aes(x=test, y=median, fill=type))
p <- ggplot(resultsCI, aes(x=test, y=mean, fill=type))
## p <- p + geom_point(aes(y=mean), position=position_dodge(width=0.9))
## p <- p + geom_boxplot(aes(fill = factor(type)))
p <- p + geom_bar(position="dodge", stat="identity")

p <- p + geom_errorbar (aes(ymin=lower, ymax=upper),
                        position=position_dodge(.9))

## p <- p + geom_errorbar (aes(ymin=q1, ymax=q3),
##                         position=position_dodge(.9))


p <- p + theme(axis.text.x = element_text(size=4, colour="black", angle=90, hjust=1, vjust=0.5))
p <- p + facet_wrap(~ operation, scales="free_y")
ggsave (paste ("group.pdf", sep=""))
