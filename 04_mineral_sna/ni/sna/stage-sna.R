# 1. 타입 정보 가져오기
rows <- new_data2%*%t(new_data2)
sta <- graph_from_adjacency_matrix(rows, mode = "undirected", weighted = TRUE, diag = FALSE)
#stage <- stage*9
stage<-round(stage)
table(stage)
mineral_names <- V(sta)$name
matched_age <- stage # name 매핑으로 안전하게 추출
color_palette <- c(
  "0" = "#d73027",
  "1" = "#fc8d59",
  "2" = "#fee090",
  "3a" = "#e0f3f8",
  "3b" = "#abd9e9",
  "4a" = "#74add1",
  "4b" = "#4575b4",
  "5" = "#91cf60",
  "7" = "#1a9850",
  "10" = "#006837"
)

color_palette <- setNames(
  colorRampPalette(brewer.pal(9, "Blues"))(10),
  c("0","1","2","3a","3b","4a","4b","5","7","10")
)

matched_age_cut <- cut(matched_age,
                       breaks = c(0,1,2,3,4,5,6,7,8,9, 10),
                       labels = names(color_palette)[1:10],
                       right = FALSE  # 0 <= x < 1, 1 <= x < 2 ... 식
)

V(sta)$size <- sqrt(igraph::degree(sta))/2
V(sta)$color <- color_palette[matched_age_cut]
sta <- delete_vertices(sta, igraph::degree(sta) == 0)
set.seed(42)
layout <- layout_with_fr(sta, niter = 1000, area = vcount(sta)^2)
E(sta)$color <- adjustcolor("gray60", alpha.f = 0.3)


#png("mineral_stage_seq_all.png", width = 1600, height = 1200, res = 200)

plot(sta,
     layout = layout,
     vertex.label = NA,
     vertex.color = V(sta)$color,
     vertex.size = V(sta)$size,
     edge.width = E(sta)$width,
     edge.color = E(sta)$color,
     main = "Mineral Network by stage"
)

legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "stage")
#dev.off()
