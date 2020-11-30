homophily_reciprocity_model <- ergm(fm_net ~ edges +  nodematch('valence') + nodefactor('valence'))
summary(homophily_reciprocity_model)

fm_net <- asNetwork(fm_netI)

model2 <- ergm(fm_net ~ edges +  nodecov('degree') + nodefactor('valence'))
summary(model2)


model3 <- ergm(fm_net ~ edges +  nodecov('degree') + nodefactor('valence')+ nodefactor('group'))
summary(model3)

model.fit <- gof(model2,
                 GOF = ~distance + espartners +
                   triadcensus,
                 burnin=1e+5, interval = 1e+5)

model.fit


model.fit3 <- gof(model3,
                 GOF = ~distance + espartners +
                   triadcensus,
                 burnin=1e+5, interval = 1e+5)

model.fit3
