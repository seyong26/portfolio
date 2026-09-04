stata <- matched_age_cut
names(stata) <- data$`Mineral Name`      # 매칭 위해 이름 부여

# 고유 그룹별 색상 팔레트 지정
group_colors <- c(
  "0"  = "#ffffd9",
  "1"  = "#edf8b1",
  "2"  = "#c7e9b4",
  "3a" = "#7fcdbb",
  "3b" = "#41b6c4",
  "4a" = "#1d91c0",
  "4b" = "#225ea8",
  "5"  = "#253494",
  "7"  = "#081d58",
  "10" = "#4b0055"  # 마지막 단계는 별도 강조
)

# 각 점에 대응하는 색상 추출
point_colors <- color_palette[type]

# MDS 시각화
plot(mds_result,
     col = point_colors,
     pch = 19,
     xlab = "Dimension 1",
     ylab = "Dimension 2",
     main = "MDS by stage")

rownames(dist) <- colnames(dist) <- rownames(new_data)
dist_obj <- as.dist(dist)

hc <- hclust(dist_obj, method = "ward.D2")

plot(hc, labels = rownames(new_data), main = "Hierarchical Clustering", cex = 0.5)
library(ape)
type_vec <- data$`Origin (7 Category)`
names(type_vec) <- data$`Mineral Name`
phy <- as.phylo(hc)
tip_types <- type_vec[phy$tip.label]

tip_cols <- point_colors

plot(as.phylo(hc), type = "fan", tip.color = tip_cols, cex = 0.5, main = "Phylogenetic Tree Colored by stage")
plot(as.phylo(hc), type = "cladogram", tip.color = tip_cols, cex = 0.5, main = "Phylogenetic Tree Colored by stage")
plot(as.phylo(hc), type = "unrooted", tip.color = tip_cols, cex = 0.5, main = "Phylogenetic Tree Colored by stage")
