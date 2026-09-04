rm(list=ls())

#install.packages("sna")
library(sna)
library(readxl)
#install.packages("tidyverse")
library(tidyverse)
library(scales)
data <- read_xlsx("C:\\Users\\admin\\Desktop\\fe_sna\\Fe Minerals_database_20250922.xlsx")
data <- data[,1:7]
importana_num <- c(26,309,475,523,567,741,968,1056,1146)
imp_name <- data[importana_num,]$`Mineral Name`

element_list <- strsplit(data$`Chemistry Elements`, " ")
all_elements <- sort(unique(unlist(element_list)))
element_matrix <- sapply(all_elements, function(elem) {
  sapply(element_list, function(elist) ifelse(elem %in% elist, 1, 0))
})
new_data <- bind_cols(data["Mineral Name"], as_tibble(element_matrix))
new_data <- as.data.frame(new_data)
dim(new_data)

#검증

count <- rep(0,length(element_list))
for(i in 1:length(element_list)){
  count[i] <- length(element_list[[i]])
}
count
valid <- rep(rep(0,dim(new_data)[1]))
for(i in 1:dim(new_data)[1]){
  valid[i] <- sum(new_data[i,-1])
}
sum(count != valid)

#구성원소에 따른 2상 네트워크 대충 형태만 보기
rownames(new_data) <- new_data[,1]
new_data <- new_data[,-1]
new_data <- as.matrix(new_data)
#n <- nrow(aa)
#m <- ncol(aa)
#vertex.col <- c(rep("blue",n),rep("red",m))
#vertex.cex <- c(rep(1,n),rep(2,m))
#aa <- aa[-572,]
#gplot(aa,displaylabels = F, boxed.labels = F,
#      vertex.col = vertex.col, vertex.cex = vertex.cex,
#      label.col = vertex.col, label.cex = 1, usearrows = F,
#      edge.col = "gray50", edge.lwd = 1.5)

#구성원소에 따른 거리 계산
n <- nrow(new_data)
dist_mat1 <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    x <- new_data[i, ]
    y <- new_data[j, ]
    union_count <- sum((x + y) > 0)  # x 또는 y가 1인 항목 수
    diff_count <- sum(x != y)        # 서로 다른 항목 수
    dist_mat1[i, j] <- if (union_count == 0) 0 else diff_count / union_count
  }
}
vals <- data$`Fe Valance`        # 값: 0, 2, 2.5, 3 중 하나
eps  <- 1e-8

# 모든 쌍의 절댓값 차이
D <- abs(outer(vals, vals, "-"))
table(D)
# 규칙 적용
dist_mat2 <- ifelse(D < eps, 0,
                    ifelse(abs(D - 0.5) < eps, 0.5, 1))
table(dist_mat2)
new_data2<- new_data[,-19]
which(rowSums(new_data2)==0)
#Iron 광물 : 구성원소가 Fe만 있음
stage <- data$Stage
table(stage)

dist_mat3 <- matrix(0, n, n)

as.factor(stage)
#10개 범주
stage[stage=="Stage 0"]=0
stage[stage=="Stage 1"]=1/10
stage[stage=="Stage 2"]=2/10
stage[stage=="Stage 3a"]=3/10
stage[stage=="Stage 3b"]=4/10
stage[stage=="Stage 4a"]=5/10
stage[stage=="Stage 4b"]=6/10
stage[stage=="Stage 5"]=7/10
stage[stage=="Stage 7"]=8/10
stage[stage=="Stage 10a"]=9/10
stage[stage=="Stage 10b"]=1
stage <- as.numeric(stage)
table(stage)
for(i in 1:length(stage)){
  for(j in 1: length(stage)){
    dist_mat3[i,j] <- abs(stage[i]-stage[j])
  }
}

origin <- data$Origin
dist_mat4 <- matrix(0, n, n)
for(i in 1:length(origin)){
  for(j in 1: length(origin)){
    dist_mat4[i,j] <- origin[i] != origin[j]
  }
}
MODE <- data$`Paragenetic Modes`
table(MODE)
dist_mat5 <- matrix(0, n, n)
for(i in 1:length(MODE)){
  for(j in 1: length(MODE)){
    dist_mat5[i,j] <- MODE[i] != MODE[j]
  }
}

sum_dist <- (dist_mat1+dist_mat2+dist_mat3+dist_mat4+dist_mat5)/5
mds_result <- cmdscale(sum_dist, k = 2)
plot(mds_result, pch=19, col="blue")
type <- data$Origin
type_colors <- c(
  "Anthropogenic" = "brown",
  "Biological" = "magenta",
  "Extraterrestrial" = "orange",
  "Hydrothermal" = "darkgreen",
  "Igneous" = "green",
  "Metamorphic" = "blue",
  "Sedimentary" = "skyblue",
  "Supergene" = "purple"
)
names(type) <- data$`Mineral Name`      # 매칭 위해 이름 부여
point_colors <- type_colors[type]
#png("mds_label.png", width = 1600, height = 1200, res = 200)

#text(label, labels = nnn, cex = 1, col = "black")
#segments(imp_xy[,1], imp_xy[,2], label[,1], label[,2], col = "gray30")

# MDS 시각화
plot(mds_result,
     col = point_colors,
     pch = 19,
     xlab = "Dimension 1",
     ylab = "Dimension 2")
imp_xy <- mds_result[importana_num,]
points(imp_xy, col="red")
label<- imp_xy
label[1,] <- c(0.4,-0.15)
label[2,] <- c(0.371, 0.05)
label[3,] <- c(-0.1,-0.05)
label[4,] <- c(0,-0.25)
label[5,] <- c(0.3,0.27)
label[6,] <- c(0.15,-0.03)
label[7,] <- c(0.3,0.32)
label[8,] <- c(0.18,0.1)
label[9,] <- c(0.38,0.28)
#text(label, labels = nnn, cex = 1, col = "black")
#segments(imp_xy[,1], imp_xy[,2], label[,1], label[,2], col = "gray30")

legend("bottomleft",                     # 위치: "topright", "bottomleft" 등
       legend = names(type_colors), # 범례 이름
       col = type_colors,           # 각 그룹의 색상
       pch = 19,                      # 점 모양
       pt.cex = 1.2,                  # 점 크기
       cex = 0.6,                     # 글자 크기
       bty = "n",                     # 박스 없애기
       title = "Mineral Origin")      # 범례 제목
#dev.off()
rownames(sum_dist) <- colnames(sum_dist) <- rownames(new_data)
dist_obj <- as.dist(sum_dist)

hc <- hclust(dist_obj, method = "ward.D2")
plot(hc, labels = rownames(new_data), main = "Hierarchical Clustering", cex = 0.1)
library(ape)

phy <- as.phylo(hc)
tip_cols <- type_colors[type]
table(tip_cols)
#png("dend4.png", width = 1600, height = 1200, res = 200)
plot(as.phylo(hc), type = "fan", tip.color = tip_cols, cex = 0.3)
plot(as.phylo(hc), type = "cladogram", tip.color = tip_cols, cex = 0.3)
plot(as.phylo(hc), type = "unrooted", tip.color = tip_cols, cex = 0.3)
#dev.off()
library(dendextend)
dend <- as.dendrogram(hc)
# type 벡터: 각 광물의 type (Biological 등)
type_named <- type
names(type_named) <- data$`Mineral Name`

# 덴드로그램 순서에 맞게 정렬
type_in_order <- type_named[labels(dend)]

# 색상 매핑
tip_cols <- type_colors[type_in_order]
labels_cex(dend) <- 0.5

labels_colors(dend) <- tip_cols
plot(dend, cex = 0.3)


new_data2

library(igraph)

elem <- new_data2%*%t(new_data2)
diag(elem) <- 0
dim(elem)
g <- graph_from_adjacency_matrix(elem, mode = "undirected", weighted = TRUE, diag = FALSE)

g1 <- delete_vertices(g, igraph::degree(g) == 0)
V(g1)$size <- sqrt(igraph::degree(g1))
summary(V(g1)$size)
E(g1)$width <- E(g1)$weight
E(g1)$color <- "gray60"
V(g1)$size <- rescale(V(g1)$size, to = c(2,5))  # 0~1 사이
# 4. 레이아웃
set.seed(42)
layout <- layout_with_fr(g1, niter=5000)



#layout <- layout_with_drl(g1)  # DRL은 대규모 네트워크에 빠름

# 1. 타입 정보 가져오기

type <- data$Origin
names(type) <- data$`Mineral Name`
# 2. igraph 노드 순서에 맞춰 매핑
V(g1)$type <- type[V(g1)$name]
table(data$`Origin`)
# 3. 색상 매핑 정의
type_colors <- c(
  "Anthropogenic" = "brown",
  "Biological" = "magenta",
  "Extraterrestrial" = "orange",
  "Hydrothermal" = "darkgreen",
  "Igneous" = "green",
  "Metamorphic" = "blue",
  "Sedimentary" = "skyblue",
  "Supergene" = "purple"
)

V(g1)$color <- type_colors[V(g1)$type]
#png("fe_size5.png", width = 1600, height = 1200, res = 200)
plot(g1,
     layout = layout,
     vertex.label = NA,
     vertex.color = V(g1)$color,
     vertex.size = V(g1)$size,
)

legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Origin")
#dev.off()

plot(g1,
     layout = layout,
     vertex.label = NA,
     vertex.color = V(g1)$color,
     vertex.size = V(g1)$size,
     edge.color = E(g1)$color
)

bip_net <- graph_from_biadjacency_matrix(new_data2, mode = "all")
V(bip_net)$type <- bipartite_mapping(bip_net)$type
# 4. 노드 이름 설정
V(bip_net)$name <- c(rownames(new_data2), colnames(new_data2))
deg <- igraph::degree(bip_net)
summary(deg)
V(bip_net)$size <- ifelse(
  V(bip_net)$type,
  rescale(sqrt(deg)/1.5, to = c(1,10)),     # 원소
  deg/2       # 광물
)
summary(V(bip_net)$size)
rescale(sqrt(deg)/1.5, to = c(2,8))
sta <- stage*10
table(sta)
names(sta) <- data$`Mineral Name`
mineral_nodes <- V(bip_net)$type == FALSE
mineral_names <- V(bip_net)$name[mineral_nodes]
pal <- colorRampPalette(c("green", "lightgreen"))
pal(3)[1]
table(data$Stage)
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
  "10a" = "#041f4a",
  "10b" = "#000000"
)
matched_age <- sta[mineral_names]  # name 매핑으로 안전하게 추출
matched_age_cut <- cut(matched_age,
                       breaks = c(0,1,2,3,4,5,6,7,8,9, 10,11),
                       labels = names(color_palette)[1:11],
                       right = F  # 0 <= x < 1, 1 <= x < 2 ... 식
)
table(matched_age_cut)
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
bip_net <- delete_vertices(bip_net, igraph::degree(g) == 0)
# 7. 레이아웃 고정
set.seed(42)
layout <- layout_with_fr(bip_net, niter = 4000)
# 8. 네트워크 시각화
#png("stage_fe_2.png", width = 3200, height = 2400, res = 200)
plot(bip_net,
     layout = layout,
     vertex.label = NA,
     vertex.size = V(bip_net)$size,
     vertex.color = V(bip_net)$color,
     edge.color = adjustcolor("gray40", alpha.f = 0.2))

# 9. 범례 추가
legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       title = "stage",
       bty = "n")
#dev.off()
#png("origin_fe_label.png", width = 3200, height = 2400, res = 200)
V(bip_net)$color <- c(V(g1)$color,rep("red",62))
plot(bip_net,
     layout = layout,
     vertex.label = NA,
     vertex.size = V(bip_net)$size,
     vertex.color = V(bip_net)$color,
     edge.color = adjustcolor("gray20", alpha.f = 0.2))

# 9. 범례 추가
legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       title = "Origin",
       bty = "n")
#dev.off()

norm_layout <- norm_coords(layout, -1, 1, -1, 1)
xx <- which(V(bip_net)$name%in%imp_name)
imp_xy <- norm_layout[xx, ]
points(imp_xy)
label <- imp_xy
label[1,] <- c(0.9,0.6)
label[2,] <- c(0.9,0.1)
label[3,] <- c(-0.5,0.8)
label[4,] <- c(0.25,0.95)
label[5,] <- c(0.3,-0.8)
label[6,] <- c(0.8,-0.7)
label[7,] <- c(-0.9,-0.2)
label[8,] <- c(0.8,0.8)
label[9,] <- c(-0.8,-0.8)
#text(label, labels = imp_name, cex = 1.5, col = "black")
#segments(imp_xy[,1], imp_xy[,2], label[,1], label[,2], col = "gray30")


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
label_vec <- ifelse(V(bip_net)$type, V(bip_net)$name, "")
#png("bipara_nofe.png", width = 3000, height = 2000, res = 200)
plot(bip_net,
     layout = layout_bip,
     vertex.label = label_vec,
     vertex.label.cex = 0.8,# 원소에만 이름, 광물은 ""
     vertex.color = V(bip_net)$color,
     vertex.size = c(rep(3, sum(mineral_nodes)), rep(0, sum(element_nodes))),  
     edge.color = adjustcolor("blue", alpha.f = 0.1),
     edge.width = 0.8
)
#dev.off()




stage_0 <- new_data2[which(sta <1),]
stage_1 <- new_data2[which(sta <2),]
stage_2 <- new_data2[which(sta <3),]
stage_3 <- new_data2[which(sta <4),]
stage_4 <- new_data2[which(sta <5),]
stage_5 <- new_data2[which(sta <6),]
stage_6 <- new_data2[which(sta <7),]
stage_7 <- new_data2[which(sta <8),]
stage_8 <- new_data2[which(sta <9),]
stage_9 <- new_data2[which(sta <10),]
stage_10 <- new_data2[which(sta <11),]

names <- rownames(new_data)
names <- names[-572]

rows_sta <- stage_5 %*% t(stage_5)
diag(rows_sta) <- 0

stage0 <- graph_from_adjacency_matrix(rows_sta, mode = "undirected", weighted = TRUE, diag = FALSE)
stage0 <- delete_vertices(stage0, igraph::degree(stage0) == 0)
type0 <- data$Origin[which(sta <6)]

names(type0) <- data$`Mineral Name`[which(sta <6)]

V(stage0)$type <- type0[V(stage0)$name]
type_colors <- c(
  "Anthropogenic" = "brown",
  "Biological" = "magenta",
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
layout_stage0  <- layout_with_fr(stage0, niter = 1000)
V(stage0)$color <- type_colors[V(stage0)$type]

V(stage0)$size <- V(g1)$size[which(names%in%V(stage0)$name)]
#png("mineral_stage0116_label.png", width = 3200, height = 2400, res = 200)
plot(stage0,
     layout = layout_stage0, 
     vertex.label = NA,
     vertex.color = V(stage0)$color,
     vertex.size = V(stage0)$size,
     edge.width = E(stage0)$width,
     edge.color = E(stage0)$color
)
#dev.off()
#norm_layout <- norm_coords(layout_stage0, -1, 1, -1, 1)
#xx <- which(V(stage0)$name%in%imp_name)
#nnn <- V(stage0)$name[xx]
#imp_xy <- norm_layout[xx, ]
#points(imp_xy, col = "red", cex=1.0)
#label <- imp_xy
#label[1,] <- c(0.2,0.88)
#label[2,] <- c(-0.4,-0.8)
#label[3,] <- c(-0.7,-0.7)
#label[4,] <- c(-0.75,0.5)
#label[5,] <- c(-0.92,-0.6)
#label[6,] <- c(-0.95,0.4)
#label[7,] <- c(0.8,0.4)
#label[8,] <- c(-0.65,0.8)
#label[9,] <- c(0.7,-0.9)
#text(label, labels = nnn, cex = 1.5, col = "black")
#segments(imp_xy[,1], imp_xy[,2], label[,1], label[,2], col = "gray30")

#dev.off()

legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Type")


rows_sta <- stage_6 %*% t(stage_6)
diag(rows_sta) <- 0

stage0 <- graph_from_adjacency_matrix(rows_sta, mode = "undirected", weighted = TRUE, diag = FALSE)
stage0 <- delete_vertices(stage0, igraph::degree(stage0) == 0)
type0 <- as.character(data$`Fe Valance`[which(sta < 7)])

names(type0) <- data$`Mineral Name`[which(sta <7)]
table(data$`Fe Valance`)
V(stage0)$type <- type0[V(stage0)$name]
type_colors <- c(
  "0" = "gray",
  "2" = "blue",
  "2.5" = "green",
  "3" = "red"
)

#V(ma_0)$size <- sqrt(degree(ma_0))
E(stage0)$width <- log1p(E(stage0)$weight)
set.seed(42)
E(stage0)$color <- "gray40"
layout_stage0  <- layout_with_fr(stage0, niter = 1000)
V(stage0)$color <- type_colors[V(stage0)$type]

V(stage0)$size <- V(g1)$size[which(names%in%V(stage0)$name)]
#png("mineral_stage0_label.png", width = 3200, height = 2400, res = 200)
plot(stage0,
     layout = layout_stage0, 
     vertex.label = NA,
     vertex.color = V(stage0)$color,
     vertex.size = V(stage0)$size,
     edge.width = E(stage0)$width,
     edge.color = E(stage0)$color
)
#dev.off()
