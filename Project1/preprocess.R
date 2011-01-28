bb = read.csv('bb-2009-all-correct.csv')
library(ggplot2)
dim(bb)
summary(bb)

dup.players = with(bb, ilkid %in% ilkid[duplicated(ilkid)])
bb[dup.players, c(1:3, 5, 25, 26)]
print(qplot(minutes, salary, data = bb, color = factor(dup.players), size=as.integer(dup.players)) + scale_colour_hue('duplicate')+ scale_size(to=c(1,2),legend = FALSE))

bb = bb[!dup.players, -c(4, 6, 7)]

## now work on bb
