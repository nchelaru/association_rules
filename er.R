library(arules)
 
data(Groceries)

# explore pre-mined rules
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

ruleExplorer(rules)

# mine and explore rules on the fly
ruleExplorer(Groceries)
