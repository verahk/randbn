# initialize bn-object with a single node 
bn <- rand_bn(2, 0, type = "cat", nlev = c(2, 3))
ls.str(bn)

# generate a new node 
name <- "X3"
parents <- "X1"
children <- "X2"
prob <- rand_cpt(dims = c(4, 2), alpha = 1, scope = c(name, parents))
dimnames(prob)   # CPT must have dimnames
node <- new_node(name, parents, children, prob)
class(node)
node

# add node to bn 
bn2 <- add_node_to_bn(bn, node)
ls.str(bn2)
bnlearn::amat(bn2)

# the new DAG structure is checked with pcalg::isValidGraph
node <- new_node(name, parents, children = parents, prob)
try(add_node_to_bn(bn, node)) # returns NULL + message from pcalg::isValidGraph

# add new parent to a node 
child_node <- bn$X2
parent_node <- new_node(name, parents, children, prob)
new_child_node <- add_parent_to_child(child_node, parent_node)

ls.str(child_node)
ls.str(new_child_node)  # same local distribution for the first parent configurations
