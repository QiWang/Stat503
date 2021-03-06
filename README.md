Steps to work with GIT:

1. clone the repository first: git@github.com:QiWang/Stat503.git This only needs to be done once; you will not need it later.
2. make your modifications; commit when you are done, then push to the server.
3. next time before you work, sync with the server first (pull)

Some conventions:

1. name your files properly, i.e. use meaningful names (Qi.R is good; hw1.r is bad it is inconvenient for others to know who made this hw1.r); by the way, please do include an extension ".R" for your R scripts
2. make the working directory of R to be the same as your R script (e.g. read.csv('basketball.csv') is reproducible for others, whereas read.csv('/home/yihui/work/basketball.csv') is not)
3. do not change the original data file -- use R code to deal with it in real-time; this is to make sure we are all working on the same dataset
4. consider brevity and meaningfulness when naming variables, e.g. fgp is good, fgpercent is too long and it is inappropriate to use single letters for "field goal" but a complete word for "percentage"

On the writing:

1. use the dictionary if you are not sure how to spell a word; make sure there are no typos
2. write in a defensive manner: imagine you are trying to convince your reader that your findings are meaningful and important -- he/she might be very skeptical about your writing
