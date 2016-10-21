# loan default

# This competition asks you to determine whether a loan will default, as well as the loss incurred if it does default.
# we seek to anticipate and incorporate both the default and the severity of the losses that result. 

#  we are building a bridge between traditional banking, where we are looking at reducing the consumption of 
# economic capital, to an asset-management perspective, where we optimize on the risk to the financial investor.
train1 <- read.csv('data/loan_default/train_v2.csv')
test1 <- read.csv('data/loan_default/test_v2.csv')