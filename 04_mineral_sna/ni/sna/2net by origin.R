library(igraph)

# 1. incidence matrix 준비
# new_data2: row = mineral, col = element (0/1)
# data: 원본 데이터프레임

# 2. 이분 그래프 생성
bip_net <- graph_from_incidence_matrix(new_data2, mode = "all")

# 3. 노드 타입 설정 (FALSE: 광물, TRUE: 원소)
V(bip_net)$type <- bipartite_mapping(bip_net)$type

# 4. 노드 이름 설정
V(bip_net)$name <- c(rownames(new_data2), colnames(new_data2))


deg <- igraph::degree(bip_net)
V(bip_net)$size <- ifelse(
  V(bip_net)$type,
  sqrt(deg),     # 원소
  deg*1.5       # 광물
)
# 6. 연대 정보 기반 색상 지정
#data <- data[-97,]

# 광물 노드 이름 추출
mineral_nodes <- V(bip_net)$type == FALSE
mineral_names <- V(bip_net)$name[mineral_nodes]

type <- data$`Origin (7 Category)`
names(type) <- data$`Mineral Name`
# 연대 구간 및 색상 팔레트 정의
color_palette <- c(
  "Biological" = "red",
  "Extraterrestrial" = "orange",
  "Hydrothermal" = "darkgreen",
  "Igneous" = "green",
  "Metamorphic" = "blue",
  "Sedimentary" = "skyblue",
  "Supergene" = "purple"
)

# 7. 광물 노드에 type 색상 매핑
V(bip_net)$color <- ifelse(
  V(bip_net)$type,  # 원소면
  "black",          # 원소: 회색
  color_palette[type[V(bip_net)$name]]  # 광물: type 색상
)

# 8. 레이아웃 설정 (bipartite layout)
set.seed(42)

#png("origin_Ni.png", width = 1600, height = 1200, res = 200)
layout_bip <- layout_with_fr(bip_net)
#layout_bip <- layout_as_bipartite(bip_net)
#layout_bip <- layout_bip[, c(2, 1)]

plot(bip_net,
     layout = layout_bip,
     vertex.label = NA,
     vertex.size = V(bip_net)$size,
     vertex.color = V(bip_net)$color,
     edge.color = adjustcolor("gray20", alpha.f = 0.2))
legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Origin")
#dev.off()
plot(bip_net,
     layout = layout_bip,
     vertex.label = F,  # 라벨 생략 (복잡할 수 있어서)
     vertex.color = V(bip_net)$color,
     vertex.size = c(rep(3,190),rep(0,40)),
     edge.color = adjustcolor("blue", alpha.f = 0.15),
     edge.width = 0.5
)

# 10. 범례 추가
#dev.off()

layout_bip <- matrix(NA, nrow = vcount(bip_net), ncol = 2)

# 노드 분류
mineral_nodes <- V(bip_net)$type == FALSE
element_nodes <- V(bip_net)$type == TRUE

# 원소 노드 좌표 (왼쪽 정렬, 위에서 아래로)
layout_bip[element_nodes, 1] <- 0
layout_bip[element_nodes, 2] <- seq(from = 1, to = 0, length.out = sum(element_nodes))

# 광물 노드 좌표 (오른쪽 정렬, 위에서 아래로)
layout_bip[mineral_nodes, 1] <- 1
layout_bip[mineral_nodes, 2] <- seq(from = 1, to = 0, length.out = sum(mineral_nodes))
#png("noNi.png", width = 3000, height = 2000, res = 200)
plot(bip_net,
     layout = layout_bip,
     vertex.label = label_vec,
     vertex.label.cex = 0.8,# 원소에만 이름, 광물은 ""
     vertex.color = V(bip_net)$color,
     vertex.size = c(rep(3, sum(mineral_nodes)), rep(0, sum(element_nodes))),  
     edge.color = adjustcolor("blue", alpha.f = 0.2),
     edge.width = 0.8
)
#dev.off()
label_vec <- ifelse(V(bip_net)$type, V(bip_net)$name, "")

