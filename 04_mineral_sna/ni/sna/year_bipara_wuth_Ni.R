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
stage

names(stage) <- data$`Mineral Name`

# 광물 노드 이름 추출
mineral_nodes <- V(bip_net)$type == FALSE
mineral_names <- V(bip_net)$name[mineral_nodes]
pal <- colorRampPalette(c("green", "lightgreen"))
pal(3)[1]
# 연대 구간 및 색상 팔레트 정의
color_palette <- c(
  "0"  = "lightgreen",
  "1"  = "springgreen1",
  "2"  = "green",
  "3a" = "#9ecae1",
  "3b" = "#6baed6",
  "4a" = "#4292c6",
  "4b" = "#2171b5",
  "5"  = "#08519c",
  "7"  = "#08306b",
  "10" = "#041f4a"
)

library(RColorBrewer)

# Blues 팔레트를 바탕으로 10단계 색상 생성
#color_palette <- c("lightgreen","green","darkgreen","#EFF3FF","#6baed6","#4292C6","#2171b5","#08519c","#08306b","#041f4a")

# 스테이지 이름을 수동으로 할당
#names(color_palette) <- c("0", "1", "2", "3a", "3b", "4a", "4b", "5", "7", "10")

# (2) 이름 맞추기: 이름이 정확히 일치하도록 검사
matched_age <- stage[mineral_names]  # name 매핑으로 안전하게 추출
matched_age_cut <- cut(matched_age,
                       breaks = c(0,1,2,3,4,5,6,7,8,9, 10),
                       labels = names(color_palette)[1:10],
                       right = FALSE  # 0 <= x < 1, 1 <= x < 2 ... 식
)

# (3) NA 처리
matched_age_cut <- as.character(matched_age_cut)
matched_age_cut[is.na(matched_age_cut)] <- "Unknown"
matched_age_cut <- factor(matched_age_cut, levels = names(color_palette))

# (4) 색상 매핑
mineral_colors <- color_palette[matched_age_cut]

# (5) 전체 노드 색상 지정
V(bip_net)$color <- ifelse(
  V(bip_net)$type,
  "red",                # 원소는 회색
  mineral_colors           # 광물은 연대 색상
)

# 7. 레이아웃 고정
set.seed(42)
layout <- layout_with_fr(bip_net)
layout_bip <- layout_with_fr(bip_net)
# 8. 네트워크 시각화
#png("stage_Ni.png", width = 1600, height = 1200, res = 200)
plot(bip_net,
     layout = layout,
     vertex.label = NA,
     vertex.size = V(bip_net)$size,
     vertex.color = V(bip_net)$color,
     edge.color = adjustcolor("gray20", alpha.f = 0.2))

# 9. 범례 추가
legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       title = "stage",
       bty = "n")
#dev.off()
