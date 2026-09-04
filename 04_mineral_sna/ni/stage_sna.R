#stage <- stage*9
#stage<-round(stage)
library(scales)
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
type0 <- data$`Origin (7 Category)`[which(stage <10)]

names(type0) <- data$`Mineral Name`[which(stage <10)]

V(stage0)$type <- type0[V(stage0)$name]

type_colors <- c(
  "Biological" = "red",
  "Extraterrestrial" = "orange",
  "Hydrothermal" = "darkgreen",
  "Igneous" = "green",
  "Metamorphic" = "blue",
  "Sedimentary" = "skyblue",
  "Supergene" = "purple"
)
#V(ma_0)$size <- sqrt(degree(ma_0))
E(stage0)$width <- log1p(E(stage0)$weight)
set.seed(42)
E(stage0)$color <- adjustcolor("gray40", alpha.f = 0.3)
layout_stage0  <- layout_with_fr(stage0, niter = 1000, area = vcount(stage0)^2)
#imp_num <- which(V(stage0)$name%in%importana_names)
#imp_name <- V(stage0)$name[imp_num]
V(stage0)$color <- type_colors[V(stage0)$type]

layout_rescaled_stage <- apply(layout_stage0, 2, function(x) {
  (x - min(x)) / (max(x) - min(x)) * 2 - 1
})
#V(stage0)$size <- sqrt(igraph::degree(stage0))/2
#names <- rownames(new_data)
#names <- names[-97]
V(g1)$size[which(names%in%V(stage0)$name)]
#V(stage0)$size <- sqrt(igraph::degree(stage0))/2
#V(stage0)$size <- rescale(V(stage0)$size, to = c(min(V(g1)$size), max(V(g1)$size)))
V(stage0)$size <- V(g1)$size[which(names%in%V(stage0)$name)]
#imp_xxyy<- imp_xy <- layout_rescaled_stage[imp_num,]
#png("mineral_stage0_label.png", width = 1600, height = 1200, res = 200)
plot(stage0,
     layout = layout_rescaled_stage ,
     rescale=F,
     vertex.label = NA,
     vertex.color = V(stage0)$color,
     vertex.size = V(stage0)$size,
     edge.width = E(stage0)$width,
     edge.color = E(stage0)$color
)
#dev.off()
#imp_xxyy[1,] <- c(0.8876509, 0.7247483)
#imp_xxyy[2,] <- c(-0.6091929, -0.1262374)
#imp_xxyy[3,] <- c(0.3134826, 0.9374947)
#imp_xxyy[4,] <- c(0.0612504, -0.3389839)
#imp_xxyy[5,] <- c(0.482724, -0.1262374)
#imp_xxyy[6,] <- c(0.7629139, -0.01986421)
#imp_xxyy[7,] <- c(0.3887029, -0.3389839)
#segments(imp_xy[,1], imp_xy[,2], imp_xxyy[,1], imp_xxyy[,2], col = "gray30")
#text(imp_xxyy,imp_name,cex=0.6)

legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Type")


