#stage <- stage*9
stage<-round(stage)
table(stage)
stage_0 <- new_data2[which(stage <1),]
stage_1 <- new_data2[which(stage <2),]
stage_2 <- new_data2[which(stage <3),]
stage_3 <- new_data2[which(stage <4),]
stage_4 <- new_data2[which(stage <5),]
stage_5 <- new_data2[which(stage <6),]
stage_6 <- new_data2[which(stage <7),]
stage_7 <- new_data2[which(stage <8),]
stage_8 <- new_data2[which(stage <9),]
stage_9 <- new_data2[which(stage <10),]

rows_sta <- stage_9 %*% t(stage_9)
diag(rows_sta) <- 0

stage0 <- graph_from_adjacency_matrix(rows_sta, mode = "undirected", weighted = TRUE, diag = FALSE)
stage0 <- delete_vertices(stage0, igraph::degree(stage0) == 0)
type0 <- data$`(Biotic, Abiotic, Both)`[which(stage <10)]
names(type0) <- data$`Mineral Name`[which(stage <10)]

V(stage0)$type <- type0[V(stage0)$name]

# 3. 색상 매핑 정의
type_colors <- c(
  "Biotic" = "green",
  "Abiotic" = "blue",
  "Both" = "yellow"
)

#V(ma_0)$size <- sqrt(degree(ma_0))
E(stage0)$width <- log1p(E(stage0)$weight)
set.seed(42)
E(stage0)$color <- "gray80"
layout_stage0  <- layout_with_fr(stage0, niter = 1000, area = vcount(stage0)^2)

V(stage0)$color <- type_colors[V(stage0)$type]
#png("mineral_type1.png", width = 1600, height = 1200, res = 200)
plot(stage0,
     layout = layout_stage0 ,
     vertex.label = NA,
     vertex.color = V(stage0)$color,
     vertex.size = V(stage0)$size,
     edge.width = E(stage0)$width,
     edge.color = E(stage0)$color,
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
