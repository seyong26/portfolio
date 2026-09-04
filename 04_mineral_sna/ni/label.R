#png("imsi.png", width = 3000, height = 2000, res = 300)  # 300dpi 고해상도

plot(g1,
     layout = layout,
     rescale=F,xlim = range(layout[,1]),  # x축 범위 수동 설정
     ylim = range(layout[,2]),
     vertex.label = NA,
     vertex.color = V(g1)$color,
     vertex.size = V(g1)$size,
     edge.width = E(g1)$width,
     edge.color = E(g1)$color
)
importana_names <- rownames(new_data)[importana_num]
#text(layout[importana_num,],importana_names)
#dev.off()
#points(-1,1)
data_no <- data[-97,]
importana_num_rael <- which(data_no$`Mineral Name`%in%importana_names)
imp_xy <- ldata_noimp_xy <- layout[importana_num_rael,]
imp_xy_add <- imp_xy
imp_xy_add[1,] <- c(2, 1)
imp_xy_add[2,] <- c(-4, -3)
imp_xy_add[3,] <- c(-0.30151575, 2)
imp_xy_add[4,] <- c(-1.3125720, -4)
imp_xy_add[5,] <- c(0.37687728, -3)
imp_xy_add[6,] <- c(1.5, -2.5)
imp_xy_add[7,] <- c(0, -4)
text(imp_xy_add,importana_names, cex=0.6)
segments(
  x0 = imp_xy[,1], y0 = imp_xy[,2],   # 시작점 (원래 점 위치)
  x1 = imp_xy_add[,1], y1 = imp_xy_add[,2],  # 끝점 (라벨 위치)
  col = "black", lty = 1
)
#dev.off()
# layout rescale
layout_rescaled <- apply(layout, 2, function(x) {
  (x - min(x)) / (max(x) - min(x)) * 2 - 1
})
a <- max(layout[,1])
b <- min(layout[,1])
c <- max(layout[,2])
d <- min(layout[,2])
(0-b)/(a-b) *2 -1
(-4-d)/(c-d) *2 -1
# 레이블도 같은 방식으로 이동 좌표 보정
imp_xy <- layout_rescaled[importana_num_rael,]
imp_xy_add <- imp_xy
imp_xy_add[1,] <- c(0.8876509, 0.7247483)
imp_xy_add[2,] <- c(-0.6091929, -0.1262374)
imp_xy_add[3,] <- c(0.3134826, 0.9374947)
imp_xy_add[4,] <- c(0.0612504, -0.3389839)
imp_xy_add[5,] <- c(0.482724, -0.1262374)
imp_xy_add[6,] <- c(0.7629139, -0.01986421)
imp_xy_add[7,] <- c(0.3887029, -0.3389839)

#png("label.png", width = 1600, height = 1200, res = 200)  # 300dpi 고해상도
plot(g1,
     layout = layout_rescaled,
     rescale = T,
     vertex.label = NA,
     vertex.color = V(g1)$color,
     vertex.size = V(g1)$size,
     edge.width = E(g1)$width,
     edge.color = E(g1)$color)
# 텍스트 + 연결선
text(imp_xy_add, labels = importana_names, cex = 0.6, col = "black")
segments(imp_xy[,1], imp_xy[,2], imp_xy_add[,1], imp_xy_add[,2], col = "gray30")
legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Origin")
#dev.off()

layout_rescaled_bip <- apply(layout_bip, 2, function(x) {
  (x - min(x)) / (max(x) - min(x)) * 2 - 1
})
#png("label2.png", width = 1600, height = 1200, res = 200)  
plot(bip_net,
     layout = layout_rescaled_bip,
     rescale=T,
     vertex.label = NA,  # 라벨 생략 (복잡할 수 있어서)
     vertex.color = V(bip_net)$color,
     vertex.size = V(bip_net)$size,
     edge.color = adjustcolor("gray40", alpha.f = 0.4),
     edge.width = 0.5,
)

# 10. 범례 추가
legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "stage")
#text(layout_rescaled_bip[importana_num,], importana_names,cex=0.6)
nnn <- imp<- layout_rescaled_bip[importana_num,]
nnn[1,] <- c(0.3,0.8)
nnn[2,] <- c(-1.1,-0.65)
nnn[3,] <- c(0.7,0.3)
nnn[4,] <- c(-1.3,-0.2)
nnn[5,] <- c(-0.9,0.3)
nnn[6,] <- c(-1.1,0.6)
nnn[7,] <- c(-1.03,0)

text(nnn, importana_names,cex=0.6)
segments(imp[,1], imp[,2], nnn[,1], nnn[,2], col = "gray30")
#dev.off()

layout_rescaled_bip <- apply(layout_bip, 2, function(x) {
  (x - min(x)) / (max(x) - min(x)) * 2 - 1
})
#png("label3.png", width = 1600, height = 1200, res = 200)  
plot(bip_net,
     layout = layout_rescaled_bip,
     rescale=T,
     vertex.label = NA,  # 라벨 생략 (복잡할 수 있어서)
     vertex.color = V(bip_net)$color,
     vertex.size = V(bip_net)$size,
     edge.color = adjustcolor("gray40", alpha.f = 0.4),
     edge.width = 0.5
)
V(bip_net)$name[importana_num]
# 10. 범례 추가
legend("topright",
       legend = names(color_palette),
       col = color_palette,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Origin (Type)")
#text(layout_rescaled_bip[importana_num,], importana_names,cex=0.6)
nnn <- imp<- layout_rescaled_bip[importana_num,]
nnn[1,] <- c(-0.9,0.8)
nnn[2,] <- c(0.8,-0.6)
nnn[3,] <- c(-0.25,0.75)
nnn[4,] <- c(-0.3,-0.8)
nnn[5,] <- c(-0.7,-0.3)
nnn[6,] <- c(-0.8,-0.6)
nnn[7,] <- c(-0.15,-0.9)

text(nnn, importana_names,cex=0.6)
segments(imp[,1], imp[,2], nnn[,1], nnn[,2], col = "gray30")
#dev.off()

plot(mds_result,
     col = point_colors,
     pch = 19,
     xlab = "Dimension 1",
     ylab = "Dimension 2",
     main = "MDS by Type")
text(mds_result[importana_num,],importana_names, cex=0.6)