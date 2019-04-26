library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
library(data.table)

# Association rule mining

groceries_raw <- read.csv("groceries.txt", header=FALSE)
setDT(groceries_raw, keep.rownames = "id")[]
names(groceries_raw) <- c("id","item1","item2","item3","item4")
groceries <- gather(groceries_raw,order,item,item1:item4,factor_key = TRUE)

str(groceries)
summary(groceries)

# Barplot of top grocery lists
# Cool use of magrittr pipes (%>%) in plotting/summary workflow
groceries_raw$item1 %>%
  summary(maxsum=Inf) %>%
  sort(decreasing=TRUE) %>%
  head(20) %>%
  barplot(las=2, cex.names=0.6)

# First create a list of baskets: vectors of items by first item

# apriori algorithm expects a list of baskets in a special format
# In this case, one "basket" of songs per user
# First split data into a list of artists for each user
basket = split(x=groceries$item, f=groceries$id)

# the first users's playlist, the second user's etc
# note the [[ ]] indexing, this is how you extract
# numbered elements of a list in R
basket[[1]]
basket[[2]]

## Remove duplicates ("de-dupe")
# lapply says "apply a function to every element in a list"
basket = lapply(basket, unique)

## Cast this variable as a special arules "transactions" class.
basket.trans = as(basket, "transactions")
summary(basket.trans)

# Now run the 'apriori' algorithm
# Look at rules with support > .001 & confidence >.01 & length (# items) <= 4
basket.rules = apriori(basket.trans, parameter=list(support=.001, confidence=.01, maxlen=4))

# Look at the output... so many rules!
#inspect(basket.rules)

## Choose a subset
#inspect(subset(basket.rules, lift > 5))
#inspect(subset(basket.rules, confidence > 0.6))
#inspect(subset(basket.rules, lift > 10 & confidence > 0.5))

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(basket.rules)

# can swap the axes and color scales
plot(basket.rules, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(basket.rules, method='two-key plot')

# can now look at subsets driven by the plot
#inspect(subset(basket.rules, support > 0.035))
#inspect(subset(basket.rules, confidence > 0.6))


# graph-based visualization
sub2 = subset(basket.rules, subset=confidence > 0.1 & support > 0.01)
#summary(sub2)
plot(basket.rules, method='graph')
#?plot.rules

plot(head(sub2, 100, by='lift'), method='graph')

# export a graph
saveAsGraph(sub2, file = "basket_rules.graphml")
