old <- data$`Oldest Known Age (Ma)`
#old <- old[-97]
old_0 <- old>0
which(old_0)
old_0_data <- new_data2[which(old_0),]

old_249 <- old>249
which(old_249)
old_249_data <- new_data2[which(old_249),]

old_499 <- old>499
which(old_499)
old_499_data <- new_data2[which(old_499),]

old_1999 <- old>1999
which(old_1999)
old_1999_data <- new_data2[which(old_1999),]

old_2499 <- old>2499
which(old_2499)
old_2499_data <- new_data2[which(old_2499),]

old_2999 <- old>2999
which(old_2999)
old_2999_data <- new_data2[which(old_2999),]

old_3999 <- old>3999
which(old_3999)
old_3999_data <- new_data2[which(old_3999),]

old_4499 <- old>4499
which(old_4499)
old_4499_data <- new_data2[which(old_4499),]

rows_0 <- old_249_data %*% t(old_249_data)
diag(rows_0) <- 0

ma_0 <- graph_from_adjacency_matrix(rows_0, mode = "undirected", weighted = TRUE, diag = FALSE)
#ma_0 <- delete_vertices(ma_0, igraph::degree(ma_0) == 0)
type_0 <- data$`(Biotic, Abiotic, Both)`[which(old_0)]
names(type_0) <- data$`Mineral Name`[which(old_0)]

V(ma_0)$type <- type_0[V(ma_0)$name]

# 3. 색상 매핑 정의
type_colors <- c(
  "Biotic" = "green",
  "Abiotic" = "blue",
  "Both" = "yellow"
)

#V(ma_0)$size <- sqrt(degree(ma_0))
E(ma_0)$width <- log1p(E(ma_0)$weight)
set.seed(42)

layout <- layout_with_fr(ma_0, niter = 1000, area = vcount(ma_0)^2)

V(ma_0)$color <- type_colors[V(ma_0)$type]
#png("mineral_type1.png", width = 1600, height = 1200, res = 200)
plot(ma_0,
     layout = layout,
     vertex.label = NA,
     vertex.color = V(ma_0)$color,
     vertex.size = V(ma_0)$size,
     edge.width = E(ma_0)$width,
     edge.color = E(ma_0)$color,
     main = "Mineral Network by Type"
)

legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Type")
#dev.off()

