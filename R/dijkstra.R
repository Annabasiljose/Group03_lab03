dijkstra <- function(graph, init_node){
  nodes <- union(unique(graph$v1),unique(graph$v2))
  distance_mat <- matrix(NA, nrow = length(nodes), ncol = 2)
  colnames(distance_mat) <- c("node", "distance")
  distance_mat[, "node"] <- nodes
  distance_mat[distance_mat[,"node"] == init_node,"distance"] = 0
  visited <- c(init_node)
  distance_mat <- get_distance(graph, init_node,
                               distance_mat, visited,0)

  for (i in 1:nrow(distance_mat)) {
    distance_mat <- distance_mat[order(distance_mat[,"distance"]),]
    if (!distance_mat[i,"node"] %in% visited) {
      distance_mat <- get_distance(graph,distance_mat[i,"node"],
                                   distance_mat, visited,
                                   distance_mat[i,"distance"])
      visited <- c(visited, distance_mat[i,"node"])
    }
  }
  distance_mat <- distance_mat[order(distance_mat[,"node"]),]
  return(distance_mat[,"distance"])
}

get_distance <- function(graph, node, df, visited_vector, node_dist){
  for (i in 1:nrow(graph)) {
    v1 <- graph[i,"v1"]
    v2 <- graph[i,"v2"]
    weight <- graph[i,"w"]
    neigh_dist <- df[df[,"node"] == graph[i,"v2"],"distance"]

    if(v1 == node && (!v2 %in% visited_vector)
       && (is.na(neigh_dist) ||
           ((node_dist + weight) < neigh_dist))){
      df[df[,"node"] == graph[i,"v2"],"distance"] =
        (node_dist + weight)
    }
  }
  return(df)
}
