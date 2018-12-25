data <- matrix(sample(c(0,1),40,replace = TRUE), nrow = 10, ncol = 4)
dataset <- BNDataset(data = data,
                     discreteness = rep('d',ncol(data)),
                     variables = letters[1:ncol(data)],
                     node.sizes = c(2,2,2,2))
net <- learn.network(dataset)