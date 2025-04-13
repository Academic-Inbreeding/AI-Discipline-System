setwd("C:\\Users\\80577\\Desktop\\1.10AI学科体系\\图")
library(ggplot2)
library(dplyr)
library(extrafont)
library(gridExtra)
library(patchwork)
library(viridis)
library(ggalluvial)
library(graphlayouts) # 高级布局算法
library(concaveman)   # 社群轮廓绘制
library(ggforce)
library(ggrepel)
library(igraph)
library(showtext)
library(tidygraph)
library(ggraph)
loadfonts('win')
df1a<-read.csv('df1a.csv')
df1b<-read.csv('df1b.csv')
df1c<-read.csv('df1c.csv')
df1a_sorted <- df1a %>% 
  arrange(desc(AI_RS))  # 按 AI_RS 从高到低排序
common_categories <- unique(df1a_sorted$cip2_name)

# 生成固定配色（按 df1a 的顺序）
color_palette <- viridis::plasma(length(common_categories))
names(color_palette) <- common_categories  # 命名颜色向量

# 确保 df1b 的 cip2_name 为因子，且水平顺序与 df1a 一致
df1b$cip2_name <- factor(df1b$cip2_name, levels = common_categories)
# 3. 绘制水平柱状图
fig1a <- ggplot(df1a_sorted, 
              aes(x = reorder(cip2_name, AI_RS), 
                  y = AI_RS, 
                  fill = cip2_name)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = color_palette) +  # 使用手动定义的配色
  labs(x = "Discipline Category", y = "AI Relevance Score (AI_RS)", tag = "A") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.tag = element_text(size = 14, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )


fig1b <- ggplot(df1b, 
              aes(x = year, 
                  y = AI_RS, 
                  color = cip2_name,  # 颜色映射
                  group = cip2_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = color_palette) +  # 使用相同的配色
  labs(x = "Year", y = "AI Relevance Score (AI_RS)", tag = "B") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.tag = element_text(size = 14, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "none"
  )

fig1c <- ggplot(df1c, 
               aes(x = year, 
                   y = AI_RS, 
                   color = cip2_name,  # 颜色按学科大类区分
                   group = cip2_name)) +
  geom_line(linewidth = 1) +          # 折线
  geom_point(size = 2) +              # 数据点
  scale_color_manual(values = color_palette) +  # 固定配色
  facet_wrap(~ cip2_name, 
             ncol = 3, 
             scales = "free") +       # 关键修改：坐标轴独立
  labs(
    x = "Year", 
    y = "AI Relevance Score (AI_RS)",
    tag = "C"  # 标签 "C"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),  # 统一字体
    plot.tag = element_text(size = 14, face = "bold"),  # 标签加粗
    axis.title = element_text(face = "bold"),         # 坐标轴标题加粗
    axis.text = element_text(size = 10),              # 坐标轴标签大小
    strip.text = element_text(face = "bold", size = 6),  # 分面标题加粗
    legend.title = element_blank(),                   # 隐藏图例标题
    legend.text = element_text(size = 8),             # 图例文本大小
    legend.position = "bottom"                        # 图例放在底部
  )
fig1d<-grid.arrange(fig1a,fig1b)
fig1<-grid.arrange(fig1d,fig1c,ncol=2)
ggsave(
          filename = "fig1.pdf",
          plot = fig1,
          device = cairo_pdf,
          width = 16,          
          height = 12,
          dpi = 600)
ggsave("fig1.tiff", fig1, width = 16, height = 12, dpi = 600, compression = "lzw")

####
df2a<-read.csv('df2a.csv')
fig2a <- ggplot(df2a, aes(x = x, y = y, fill = Complementary.score)) +
  geom_tile(color = "white", linewidth = 0.3, na.rm = TRUE) +
  scale_fill_gradientn(
    colours = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),  # 红渐变（9阶）
    name = "Complementarity\nScore",
    limits = c(0, 1),
    na.value = "transparent",
    breaks = seq(0, 1, by = 0.2),
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(0.5, "cm"),
      barheight = unit(4, "cm"),
      frame.colour = "black")  # 图例边框
  ) +
  labs(
    x = NULL,
    y = NULL,
    tag = 'A'
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right",
    aspect.ratio = 1,
    legend.title = element_text(size = 10, face = "bold"),
    legend.background = element_rect(fill = "white", color = NA)  # 图例白底
  ) +
  coord_fixed()

fig2b <- ggplot(df2b, 
       aes(axis1 = x, axis2 = y, y = common_count)) +
  geom_alluvium(aes(fill = x), alpha = 0.7) +
  geom_stratum(width = 0.2, fill = "gray90") +
  geom_text(
    stat = "stratum", 
    aes(label = ifelse(
           nchar(as.character(after_stat(stratum))) > 12,  # 超过12字符换行
           gsub("(.{12,}?)\\s", "\\1\n", after_stat(stratum)),  # 按空格智能换行
           as.character(after_stat(stratum))
         )),
    size = 3,
    family = "Times New Roman",
    lineheight = 0.8  # 调整行间距
  ) +
  scale_fill_manual(
    values = color_palette,
    drop = FALSE
  ) +
  labs(y = "Combination count", tag = 'B') +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )


df2c<-read.csv('df2c.csv')
fig2c<-ggplot(df2c, aes(x = x, y = y)) +
  geom_point(aes(size = common_count, color = common), alpha = 0.8) +
  scale_size_continuous(
    range = c(2, 10), 
    name = "Co-occurrence\nCount",
    breaks = c(500, 2000, 5000, 10000)) +
  scale_color_gradient2(
    low = "#FDE725", mid = "#21918C", high = "#440154", 
    midpoint = 0.5,
    name = "Complementarity\nScore") +
  labs(x = NULL, y = NULL,tag='C') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90")
  ) +theme(
    text = element_text(family = "Times New Roman"),
    plot.tag = element_text(size = 14, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9))

df2d<-read.csv('df2d.csv')
edges <- df2d %>%
  select(x, y, weight = common_count) %>%
  mutate(weight = log(weight + 1))  # 对数变换平衡权重

# 2. 构建网络图
graph <- graph_from_data_frame(edges, directed = FALSE)

# 3. Louvain社区检测
set.seed(100)  # 保证可重复性
louvain_clusters <- cluster_louvain(graph)
V(graph)$community <- louvain_clusters$membership

# 4. 计算布局（力导向算法）
layout <- layout_with_fr(graph, weights = E(graph)$weight)

# 5. 设置字体（Science标准）
font_add("Times New Roman", "times.ttf")
showtext_auto()

# 6. 绘制学科簇图
fig2d<-ggraph(graph, layout = layout) +
  # 边（按权重透明度渐变）
  geom_edge_link(
    aes(alpha = weight),
    width = 0.3,
    color = "gray70",
    show.legend = FALSE
  ) +
  # 节点（按社区着色）
  geom_node_point(
    aes(fill = as.factor(community), size = centrality_degree()),
    shape = 21,
    color = "white",
    stroke = 0.3
  ) +
  # 节点标签（学科名称）
  geom_node_text(
    aes(label = name, filter = centrality_degree() > median(centrality_degree())),
    family = "Times New Roman",
    size = 3,
    repel = TRUE,
    max.overlaps = 20
  ) +
  # 配色与比例尺
  scale_fill_viridis_d(
    option = "plasma",
    name = "Community",
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  scale_size_continuous(
    range = c(2, 8),
    name = "Degree Centrality"
  ) +
  # 图题与样式
  labs(
    
    tag = "D"
  ) +
  theme_graph(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.tag = element_text(size = 14, face = "bold"),
    legend.position = "none",
    legend.box = "horizontal"
  )

ggsave(
          filename = "fig2.pdf",
          plot = fig2,
          device = cairo_pdf,
          width = 18,          
          height = 13,
          dpi = 600)
ggsave("fig2.tiff", fig2, width = 18, height = 13, dpi = 600, compression = "lzw")

df3a<-read.csv('df3a.csv')
cor_test <- cor.test(
  df3a$lightcast_ai_score, 
  df3a$revelio_ai_score,
  method = "pearson"
)
cor_label <- sprintf("r = %.2f\np = %.2e", 
                    cor_test$estimate, 
                    cor_test$p.value)

# 3. 绘制点图与拟合线
fig3a<-ggplot(df3a, aes(x = lightcast_ai_score, 
                y = revelio_ai_score)) +
  geom_point(
    alpha = 0.6,
    size = 3,
    shape = 16
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x, 
    se = TRUE,
    linewidth = 1.2,
    fill = "gray80"     # 置信区间填充
  ) +
  # 添加相关系数标注
  annotate(
    "text",
    x = max(df3a$lightcast_ai_score) * 0.9,
    y = max(df3a$revelio_ai_score) * 0.1,
    label = cor_label,
    family = "Times New Roman",
    size = 5,
    fontface = "bold",
    color = "black",
    hjust = 1
  ) +
  # 坐标轴与图题
  labs(
    x = "Lightcast AI RS",
    y = "Revelio AI RS",
    tag = "A"
  ) +
  # 科学图表主题
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.tag = element_text(size = 14, face = "bold")
  ) +
  # 保证正方形比例
  coord_fixed(ratio = 1)

df3b<-read.csv('df3b.csv')
set.seed(123)

# 创建节点数据
nodes <- data.frame(
  name = df3b$cip6_name,
  weight = df3b$weight,
  x = runif(nrow(df3b)),  # 随机x坐标(0-1)
  y = runif(nrow(df3b)),  # 随机y坐标(0-1)
  stringsAsFactors = FALSE
)

# 创建边数据
edges <- data.frame()
for(i in 1:nrow(df3b)){
  row <- df3b[i, ]
  cip <- row$cip6_name
  
  # 向上的关联(Revelio→Lightcast)
  if(!is.na(row$f_revelio2lightcast)){
    edges <- rbind(edges, data.frame(
      from = cip,
      f_value = row$f_revelio2lightcast,
      weight = row$weight,
      direction = "up",
      stringsAsFactors = FALSE
    ))
  }
  
  # 向下的关联(Lightcast→Revelio)
  if(!is.na(row$f_lightcast2revelio)){
    edges <- rbind(edges, data.frame(
      from = cip,
      f_value = row$f_lightcast2revelio,
      weight = row$weight,
      direction = "down",
      stringsAsFactors = FALSE
    ))
  }
}

label_threshold <- quantile(nodes$weight, 0.7, na.rm = TRUE)

# 绘制图形
fig3b<-ggplot() +
  # 绘制下平面(Revelio) - 绿色
  geom_rect(aes(xmin = -0.1, xmax = 1.1, ymin = -0.1, ymax = 0.1),
            fill = "#66BB6A", alpha = 0.2) +
  geom_text(aes(x = 0.5, y = 0), label = "REVELIO", 
            size = 8, fontface = "bold", color = "darkgreen") +
  
  # 绘制上平面(Lightcast) - 蓝色
  geom_rect(aes(xmin = -0.1, xmax = 1.1, ymin = 0.9, ymax = 1.1),
            fill = "#42A5F5", alpha = 0.2) +
  geom_text(aes(x = 0.5, y = 1), label = "LIGHTCAST", 
            size = 8, fontface = "bold", color = "darkblue") +
  
  # 绘制学科节点 (统一黑色)
  geom_point(data = nodes, 
             aes(x = x, y = y), 
             size = 3, color = "black", alpha = 0.8) +
  
  # 绘制向上的箭头(Revelio→Lightcast) - 蓝色
  geom_segment(data = edges[edges$direction == "up", ],
               aes(x = nodes$x[match(from, nodes$name)],
                   xend = nodes$x[match(from, nodes$name)],
                   y = nodes$y[match(from, nodes$name)],
                   yend = 0.9,
                   linewidth = 0.3 + 1.7 * weight/max(weight)), # 线宽范围0.3-2
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "#1565C0") + # 深蓝色箭头
  
  # 绘制向下的箭头(Lightcast→Revelio) - 绿色
  geom_segment(data = edges[edges$direction == "down", ],
               aes(x = nodes$x[match(from, nodes$name)],
                   xend = nodes$x[match(from, nodes$name)],
                   y = nodes$y[match(from, nodes$name)],
                   yend = 0.1,
                   linewidth = 0.3 + 1.7 * weight/max(weight)), # 线宽范围0.3-2
               arrow = arrow(type = "closed", length = unit(2, "mm")),
               color = "#2E7D32") + # 深绿色箭头
  
  # 添加学科标签(只显示权重较大的)
  geom_text(data = nodes[nodes$weight > label_threshold, ],
            aes(x = x, y = y, label = name, 
                size = 3 + 5 * weight/max(weight)), # 字体大小范围3-8
            color = "black",
            check_overlap = TRUE, 
            vjust = ifelse(nodes[nodes$weight > label_threshold, "y"] > 0.5, -1.2, 1.2)) +
  
  # 调整比例
  scale_linewidth_continuous(range = c(0.3, 2)) +
  scale_size_identity() + # 直接使用计算的大小值
  
  # 设置坐标范围并添加边界保护
  coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) +
  
  # 主题设置
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )+labs(tag='B')

df3c<-read.csv('df3c.csv')
df3c <- df3c %>%
  mutate(Start = as.numeric(Start),
         End = as.numeric(End))

# 创建年份序列
years <- min(df3c$Start):max(df3c$End)

# 创建完整的热图数据
heatmap_data <- expand.grid(
  cip6_name = unique(df3c$cip6_name),
  Year = years
) %>%
  left_join(df3c, by = "cip6_name") %>%
  mutate(
    Active = ifelse(Year >= Start & Year <= End, 1, 0),
    Color = ifelse(Active == 1, "#1565C0", "white")
  )

# 绘制热图
ggplot(heatmap_data, aes(x = Year, y = reorder(cip6_name, Weight))) +
  geom_tile(aes(fill = Color), color = "gray80", linewidth = 0.2) +
  scale_fill_identity() +  # 直接使用颜色值
  scale_x_continuous(breaks = seq(min(years), max(years), by = 1)) +
  labs(
    x = "Year",
    y = "Discipline",
    title = "Disciplinary Activity Timeline"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

df3c <- df3c %>%
  mutate(Start = as.numeric(Start),
         End = as.numeric(End))

# 创建完整的热图数据
heatmap_data <- expand.grid(
  cip6_name = unique(df3c$cip6_name),
  Year = min(df3c$Start):max(df3c$End)
) %>%
  left_join(df3c, by = "cip6_name") %>%
  mutate(
    # 根据type设置颜色
    Color = case_when(
      Year >= Start & Year <= End & type == "Lightcast" ~ "#1565C0",
      Year >= Start & Year <= End & type == "Revelio" ~ "#66BB6A",
      TRUE ~ "white"  # 非活跃期为白色
    )
  )

# 设置Times New Roman字体
windowsFonts(Times = windowsFont("Times New Roman"))

# 绘制热图
fig3c<-ggplot(heatmap_data, aes(x = Year, y = reorder(cip6_name, Weight))) +
  geom_tile(aes(fill = Color), color = "gray80", linewidth = 0.2) +
  scale_fill_identity() +  # 直接使用定义的颜色值
  scale_x_continuous(breaks = seq(min(df3c$Start), max(df3c$End), by = 1)) +
  labs(x = NULL, y = NULL) +  # 移除坐标轴标签
  theme_minimal(base_family = "Times") +  # 设置基础字体
  theme(
    text = element_text(size = 8),  # 统一字号为12
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_blank(),  # 移除标题
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # 调整边距
  )+labs(tag='C')

df3d<-read.csv('df3d.csv')
graph <- tbl_graph(
  nodes = nodes,
  edges = edges
)

# 4. 视觉参数
class_colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")
label_padding <- 1.15  # 标签外扩系数

# 5. 绘制优化后的弦图
fig3d<-ggraph(graph, layout = "linear", circular = TRUE) +
  # ===== 立体连接带 =====
  geom_edge_arc(
    aes(color = factor(modularity_class)),
    strength = 0.15,
    alpha = 0.7,
    width = 1.2,  # 固定线宽
    lineend = "round"
  ) +
  
  # ===== 3D节点效果 =====
  # 底层阴影
  geom_node_point(
    aes(size = centrality * 4),
    color = "gray30",
    alpha = 0.2
  ) +
  # 主体节点
  geom_node_point(
    aes(size = centrality * 5, color = factor(modularity_class)),
    alpha = 0.9
  ) +
  # 顶部高光
  geom_node_point(
    aes(size = centrality * 2),
    color = "white",
    alpha = 0.4
  ) +
  
  # ===== 显示所有标签 =====
  geom_node_text(
    aes(label = cip6_name,
        angle = -((-node_angle(x, y) + 90) %% 180 + 90),
        x = x * label_padding,  # 外扩标签位置
        y = y * label_padding),
    size = 3.5,
    family = "Times New Roman",
    fontface = "bold",
    hjust = "outward",
    vjust = 0.5,
    check_overlap = FALSE  # 强制显示所有标签
  ) +
  
  # ===== 视觉映射 =====
  scale_edge_color_manual(
    values = class_colors,
    guide = guide_legend(title = "Module", override.aes = list(width = 3))
  ) +
  scale_color_manual(
    values = class_colors,
    guide = "none"
  ) +
  scale_size_identity() +
  
  # ===== 专业主题 =====
  coord_fixed(clip = "off") +  # 允许标签超出绘图区
  theme_graph(base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    legend.position = "none",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(2, 2, 2, 2, "cm")  # 增加边距容纳标签
  )+labs(tag='D')
fig3<-grid.arrange(fig3a,fig3b,fig3c,fig3d)
ggsave(
          filename = "fig2.pdf",
          plot = fig2,
          device = cairo_pdf,
          width = 18,          
          height = 16,
          dpi = 600)
ggsave("fig3.tiff", fig3, width = 18, height = 16, dpi = 600, compression = "lzw")


#####################################################################################
figs1.1<-read.csv('dfs1.1.csv')
# 加载必要的包
library(ggplot2)
library(extrafont) # 用于使用Times New Roman字体


dfs1.1<-read.csv('dfs1.1.csv')
figs1.1<-ggplot(dfs1.1, aes(x = AI_RS, y = reorder(cip2_name, AI_RS))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(x = "AI Relative Score (AI_RS)", 
       y = "Disciplinary Categories",
       tag = "A") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"), # 全部文本加粗
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 6, face = "bold"),
    panel.grid.major.x = element_line(color = "gray90"), # 只保留水平网格线
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm") # 调整边距
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = round(AI_RS, 2)), 
            hjust = -0.2, 
            size = 2, 
            family = "Times New Roman",
            fontface = "bold") # 在柱条末端添加数值标签

dfs1.2<-read.csv('dfs1.2.csv')
figs1.2<-ggplot(dfs1.2, aes(x = AI_RS)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "steelblue4", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS)", 
       y = "Density",
       tag = "B") +  # 移除了title
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    plot.title = element_blank(),  # 确保没有标题
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  # 添加AI_RS=1的参考线和标注
  geom_vline(aes(xintercept = 1), 
             color = "red3", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = 1, y = Inf, 
           label = "AI_RS = 1", 
           vjust = 2, hjust = -0.1, 
           color = "red3", family = "Times New Roman", fontface = "bold") +
  # 可选：添加大于1的区域阴影
  geom_area(data = subset(data.frame(x = density(dfs1.2$AI_RS)$x, 
                                   y = density(dfs1.2$AI_RS)$y), 
                    x > 1),
            aes(x = x, y = y), 
            fill = "red3", alpha = 0.2)

dfs1.2_above1 <- subset(dfs1.2, AI_RS > 1)
dfs1.2_below1 <- subset(dfs1.2, AI_RS < 1)

# 图C: AI_RS > 1的密度图
figs1.3<- ggplot(dfs1.2_above1, aes(x = AI_RS)) +
  geom_density(fill = "red3", alpha = 0.5, color = "red4", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS > 1)", 
       y = "Density",
       tag = "C") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(limits = c(1, max(dfs1.2$AI_RS))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# 图D: AI_RS < 1的密度图
figs1.4 <- ggplot(dfs1.2_below1, aes(x = AI_RS)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "steelblue4", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS < 1)", 
       y = "Density",
       tag = "D") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(limits = c(min(dfs1.2$AI_RS), 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
figs1<-grid.arrange(figs1.1,figs1.2,figs1.3,figs1.4)
ggsave(filename = "figs1.pdf",plot = figs1, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs1.tiff", figs1, width = 12, height = 8, dpi = 600, compression = "lzw")

dfs2<-read.csv('dfs2.csv')
dfs2$CIP6_code <- factor(dfs2$CIP6_code, levels = unique(dfs2$CIP6_code))
figs2<-ggplot(dfs2, aes(x = CIP6_code, y = AI_RS)) +
  geom_bar(stat = "identity", 
           aes(fill = CIP2_name),
           width = 0.7) +
  facet_wrap(~ CIP2_name, scales = "free_x", ncol = 3) + # 3列布局
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(x = "Disciplinary Sub-category (CIP6 Code)", 
       y = "AI Relative Score (AI_RS)",
       ) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # 缩小x轴标签字号
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    strip.background = element_rect(fill = "gray90"),
    panel.spacing = unit(1, "lines"), # 增加分面间距
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_hline(yintercept = 1, color = "red3", linetype = "dashed")
ggsave(filename = "figs2.pdf",plot = figs2, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs2.tiff", figs2, width = 12, height = 8, dpi = 600, compression = "lzw")

dfs3<-read.csv('dfs3.csv')
new_color_palette <- c(
  "Architecture and Related Services" = "#264653",
  "Business, Management, Marketing, and Related Support Services" = "#2a9d8f",
  "Communication, Journalism, and Related Programs" = "#e9c46a",
  "Culinary, Entertainment, and Personal Services" = "#f4a261",
  "Engineering/Engineering-related Technologies/Technicians" = "#e76f51",
  "Health Professions and Related Programs" = "#9b5de5",
  "Legal Professions and Studies" = "#f15bb5",
  "Philosophy and Religious Studies" = "#fee440",
  "Psychology" = "#00bbf9"
)

# 确保数据准备正确
dfs3$CIP6_code <- factor(dfs3$CIP6_code) # 确保是因子变量

# 创建分面柱状图
figs3<-ggplot(dfs3, aes(x = CIP6_code, y = AI_RS)) +
  geom_bar(aes(fill = CIP2_name), 
           stat = "identity",
           width = 0.7,
           show.legend = FALSE) +
  facet_wrap(~ CIP2_name, 
             scales = "free_x",
             ncol = 3) + # 3列布局
  # 应用全新配色方案
  scale_fill_manual(values = new_color_palette) +
  # 添加参考线和标签
  geom_hline(yintercept = 1, 
             color = "red3", 
             linetype = "dashed", 
             linewidth = 0.8) +
  # 设置标签和主题
  labs(x = "Disciplinary Sub-category (CIP6 Code)",
       y = "AI Relative Score (AI_RS)") + # 下一个顺序标签
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray30"),
    panel.spacing = unit(1.2, "lines"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
ggsave(filename = "figs3.pdf",plot = figs3, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs3.tiff", figs3, width = 12, height = 8, dpi = 600, compression = "lzw")
#################################################################
dfs4.1<-read.csv('dfs4.1.csv')
dfs4.2<-read.csv('dfs4.2.csv')
dfs4.1<-subset(dfs4.1,dfs4.1$discipline.complementarity.score>=0.1)
figs4.1<-ggplot(dfs4.1, aes(x = discipline.complementarity.score)) +
  geom_density(
    fill = "#3B9AB2",
    alpha = 0.5,
    color = "#3B9AB2",
    linewidth = 0.8
  ) +
  labs(
    x = "Discipline Complementarity Score(CIP2_codes)", 
    y = "Density",
    tag = "A"
  ) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_vline(
    aes(xintercept = median(discipline.complementarity.score)),
    color = "#E41A1C",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = median(dfs4.1$discipline.complementarity.score),
    y = Inf,
    label = paste("Median =", round(median(dfs4.1$discipline.complementarity.score), 2)),
    vjust = 2,
    hjust = -0.1,
    color = "#E41A1C",
    family = "Times New Roman",
    fontface = "bold"
  )
figs4.2<-ggplot(dfs4.2, aes(x = discipline.complementarity.score)) +
  geom_density(
    fill = "#3B9AB2",
    alpha = 0.5,
    color = "#3B9AB2",
    linewidth = 0.8
  ) +
  labs(
    x = "Discipline Complementarity Score(CIP6_codes)", 
    y = "Density",
    tag = "B"
  ) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_vline(
    aes(xintercept = median(dfs4.2$discipline.complementarity.score)),
    color = "#E41A1C",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
 annotate(
    "text",
    x = median(dfs4.2$discipline.complementarity.score),
    y = Inf,
    label = paste("Median =", round(median(dfs4.2$discipline.complementarity.score), 2)),
    vjust = 2,
    hjust = -0.1,
    color = "#E41A1C",
    family = "Times New Roman",
    fontface = "bold"
  )
figs4<-grid.arrange(figs4.1,figs4.2,ncol=2)
ggsave(filename = "figs4.pdf",plot = figs4, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs4.tiff", figs4, width = 12, height = 8, dpi = 600, compression = "lzw")
########################
dfs5<-read.csv('dfs5.csv')
edges <- dfs5 %>%
  select(x, y, weight = common_count) %>%
  mutate(weight = log(weight + 1))  # 对数变换平衡权重

# 2. 构建网络图
graph <- graph_from_data_frame(edges, directed = FALSE)

# 3. Louvain社区检测
set.seed(100)  # 保证可重复性
louvain_clusters <- cluster_louvain(graph)
V(graph)$community <- louvain_clusters$membership

# 4. 计算布局（力导向算法）
layout <- layout_with_fr(graph, weights = E(graph)$weight)

# 5. 设置字体（Science标准）
font_add("Times New Roman", "times.ttf")
showtext_auto()

# 6. 绘制学科簇图
figs5<-ggraph(graph, layout = layout) +
  # 边（按权重透明度渐变）
  geom_edge_link(
    aes(alpha = weight),
    width = 0.3,
    color = "gray70",
    show.legend = FALSE
  ) +
  # 节点（按社区着色）
  geom_node_point(
    aes(fill = as.factor(community), size = centrality_degree()),
    shape = 21,
    color = "white",
    stroke = 0.3
  ) +
  # 节点标签（学科名称）
  geom_node_text(
    aes(label = name, filter = centrality_degree() > median(centrality_degree())),
    family = "Times New Roman",
    size = 3,
    repel = TRUE,
    max.overlaps = 20
  ) +
  # 配色与比例尺
  scale_fill_viridis_d(
    option = "plasma",
    name = "Community",
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  scale_size_continuous(
    range = c(2, 8),
    name = "Degree Centrality"
  ) +
  theme_graph(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.tag = element_text(size = 14, face = "bold"),
    legend.position = "none",
    legend.box = "horizontal"
  )
ggsave(filename = "figs5.pdf",plot = figs5, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs5.tiff", figs5, width = 12, height = 8, dpi = 600, compression = "lzw")

dfs6<-read.csv('dfs6.csv')
edges <- dfs6 %>%
  select(x, y, weight = common_count) %>%
  mutate(weight = log(weight + 1))  # 对数变换平衡权重

# 2. 构建网络图
graph <- graph_from_data_frame(edges, directed = FALSE)

# 3. Louvain社区检测
set.seed(100)  # 保证可重复性
louvain_clusters <- cluster_louvain(graph)
V(graph)$community <- louvain_clusters$membership

# 4. 计算布局（力导向算法）
layout <- layout_with_fr(graph, weights = E(graph)$weight)

# 5. 设置字体（Science标准）
font_add("Times New Roman", "times.ttf")
showtext_auto()

# 6. 绘制学科簇图
figs6<-ggraph(graph, layout = layout) +
  # 边（按权重透明度渐变）
  geom_edge_link(
    aes(alpha = weight),
    width = 0.3,
    color = "gray70",
    show.legend = FALSE
  ) +
  # 节点（按社区着色）
  geom_node_point(
    aes(fill = as.factor(community), size = centrality_degree()),
    shape = 21,
    color = "white",
    stroke = 0.3
  ) +
  # 节点标签（学科名称）
  geom_node_text(
    aes(label = name, filter = centrality_degree() > median(centrality_degree())),
    family = "Times New Roman",
    size = 3,
    repel = TRUE,
    max.overlaps = 20
  ) +
  # 配色与比例尺
  scale_fill_viridis_d(
    option = "plasma",
    name = "Community",
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  scale_size_continuous(
    range = c(2, 8),
    name = "Degree Centrality"
  ) +
  theme_graph(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.tag = element_text(size = 14, face = "bold"),
    legend.position = "none",
    legend.box = "horizontal"
  )
ggsave(filename = "figs6.pdf",plot = figs6, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs6.tiff", figs6, width = 12, height = 8, dpi = 600, compression = "lzw")
#####################################################
# 读取数据
dfs7.1 <- read.csv('dfs7.1.csv')

# 创建水平柱状图
figs7.1 <- ggplot(dfs7.1, aes(x = AI_RS, y = reorder(cip2_name, AI_RS))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(x = "AI Relative Score (AI_RS)", 
       y = "Disciplinary Categories",
       tag = "A") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 6, face = "bold"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = round(AI_RS, 2)), 
            hjust = -0.2, 
            size = 2, 
            family = "Times New Roman",
            fontface = "bold")


dfs7.2 <- read.csv('dfs7.2.csv')
# 主密度图
figs7.2 <- ggplot(dfs7.2, aes(x = AI_RS)) +
  geom_density(fill = "#A8D8B9", alpha = 0.5, color = "#5C9E7B", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS)", 
       y = "Density",
       tag = "B") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    plot.title = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  # 添加参考线和标注
  geom_vline(aes(xintercept = 1), 
             color = "#E41A1C", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = 1, y = Inf, 
           label = "AI_RS = 1", 
           vjust = 2, hjust = -0.1, 
           color = "#E41A1C", family = "Times New Roman", fontface = "bold") +
  # 添加大于1的区域阴影
  geom_area(data = subset(data.frame(x = density(dfs7.2$AI_RS)$x, 
                                   y = density(dfs7.2$AI_RS)$y), 
                    x > 1),
            aes(x = x, y = y), 
            fill = "#E41A1C", alpha = 0.2)
# 创建子数据集
dfs7.2_above1 <- subset(dfs7.2, AI_RS > 1)
dfs7.2_below1 <- subset(dfs7.2, AI_RS < 1)

# 图C: AI_RS > 1的密度图
figs7.3 <- ggplot(dfs7.2_above1, aes(x = AI_RS)) +
  geom_density(fill = "#E41A1C", alpha = 0.5, color = "#B2182B", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS > 1)", 
       y = "Density",
       tag = "C") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(limits = c(1, max(dfs7.2$AI_RS))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# 图D: AI_RS < 1的密度图
figs7.4 <- ggplot(dfs7.2_below1, aes(x = AI_RS)) +
  geom_density(fill = "#A8D8B9", alpha = 0.5, color = "#5C9E7B", linewidth = 0.8) +
  labs(x = "AI Relative Score (AI_RS < 1)", 
       y = "Density",
       tag = "D") +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_x_continuous(limits = c(min(dfs7.2$AI_RS), 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
figs7<-grid.arrange(figs7.1,figs7.2,figs7.3,figs7.4)
ggsave(filename = "figs7.pdf",plot = figs7, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs7.tiff", figs7, width = 12, height = 8, dpi = 600, compression = "lzw")

color_palette <- c(
    "Mathematics and Statistics" = "#0D0887FF",
    "Multi/Interdisciplinary Studies" = "#4C02A1FF",
    "Foreign Languages, Literatures, and Linguistics" = "#7E03A8FF",
    "Computer and Information Sciences and Support Services" = "#A92395FF",
    "Engineering" = "#E56B5DFF", 
    "Physical Sciences" = "#F89441FF",
    "Engineering/Engineering-related Technologies/Technicians" = "#E56B5DFF"  # 与Engineering同色
)
figs8<-ggplot(dfs8, aes(x = CIP6_code, y = AI_RS)) +
  geom_bar(stat = "identity", 
           aes(fill = CIP2_name),
           width = 0.7) +
  facet_wrap(~ CIP2_name, scales = "free_x", ncol = 3) + # 3列布局
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(x = "Disciplinary Sub-category (CIP6 Code)", 
       y = "AI Relative Score (AI_RS)",
       ) +
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # 缩小x轴标签字号
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    strip.background = element_rect(fill = "gray90"),
    panel.spacing = unit(1, "lines"), # 增加分面间距
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_hline(yintercept = 1, color = "red3", linetype = "dashed")
ggsave(filename = "figs8.pdf",plot = figs8, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs8.tiff", figs8, width = 12, height = 8, dpi = 600, compression = "lzw")


dfs9<-read.csv('dfs9.csv')
dfs9$CIP6_code <- factor(dfs9$CIP6_code) # 确保是因子变量
new_color_palette <- c(
  "Biological and Biomedical Sciences" = "#F0F921FF",  # 亮黄色
  "Business, Management, Marketing, and Related Support Services" = "#2A9D8FFF",  # 新增蓝绿色
  "Computer and Information Sciences and Support Services" = "#A92395FF",  # 紫红色
  "Health Professions and Related Programs" = "#E76F51FF",  # 新增珊瑚色
  "Library Science" = "#FDC328FF",  # 金黄色
  "Medical Residency/Fellowship Programs" = "#264653FF",  # 新增深蓝绿色
  "Philosophy and Religious Studies" = "#7E03A8FF",  # 紫色
  "Psychology" = "#CC4678FF",  # 玫红色
  "Public Administration and Social Service Professions" = "#4C02A1FF",  # 深紫色
  "Social Sciences" = "#0D0887FF",  # 深蓝色
  "Visual and Performing Arts" = "#1F9E89FF"  # 新增蓝绿色
)
# 创建分面柱状图
figs9<-ggplot(dfs9, aes(x = CIP6_code, y = AI_RS)) +
  geom_bar(aes(fill = CIP2_name), 
           stat = "identity",
           width = 0.7,
           show.legend = FALSE) +
  facet_wrap(~ CIP2_name, 
             scales = "free_x",
             ncol = 3) + # 3列布局
  # 应用全新配色方案
  scale_fill_manual(values = new_color_palette) +
  # 添加参考线和标签
  geom_hline(yintercept = 1, 
             color = "red3", 
             linetype = "dashed", 
             linewidth = 0.8) +
  # 设置标签和主题
  labs(x = "Disciplinary Sub-category (CIP6 Code)",
       y = "AI Relative Score (AI_RS)") + # 下一个顺序标签
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray30"),
    panel.spacing = unit(1.2, "lines"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
ggsave(filename = "figs9.pdf",plot = figs9, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs9.tiff", figs9, width = 12, height = 8, dpi = 600, compression = "lzw")

dfs10.1<-read.csv('dfs10.1.csv')
dfs10.1 <- dfs10.1 %>%
  mutate(lightcast_neg = -lightcast)

# 颜色方案
color_palette <- c("#3574b2", "#5C9E7B")  # 蓝色, 绿色

# 创建对称条形图
figs10.1 <- ggplot() +
  # lightcast数据（负半轴）
  geom_bar(data = dfs10.1,
           aes(x = reorder(naics, lightcast), 
               y = lightcast_neg,
               fill = "lightcast"),  # 改为使用名称而非直接颜色值
           stat = "identity",
           width = 0.6) +
  # revelio数据（正半轴）
  geom_bar(data = dfs10.1,
           aes(x = reorder(naics, lightcast), 
               y = revelio,
               fill = "revelio"),  # 改为使用名称而非直接颜色值
           stat = "identity",
           width = 0.6) +
  # 坐标轴和标签
  labs(x = "Industry (NAICS Code)",
       y = "Proportion of High AI-RS Disciplines",
       tag = "A",
       fill = "Database") +  # 添加图例标题
  # 对称y轴
  scale_y_continuous(labels = function(x) scales::percent(abs(x)),
                     breaks = seq(-1, 1, by = 0.2),
                     limits = c(-max(dfs10.1$lightcast, dfs10.1$revelio)*1.1, 
                               max(dfs10.1$lightcast, dfs10.1$revelio)*1.1)) +
  # 颜色映射和图例
  scale_fill_manual(
    values = c("lightcast" = "#3574b2", "revelio" = "#5C9E7B"),  # 指定颜色映射
    labels = c("lightcast", "revelio")  # 图例标签
  ) +
  
  # 添加垂直参考线
  geom_vline(xintercept = seq(0.5, length(unique(dfs10.1$naics))+0.5, by = 1),
             color = "gray90", linewidth = 0.3) +
  
  # 添加水平零线
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  
  # 主题设置
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "right",  # 图例位置在右侧
    legend.title = element_text(size = 10, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 9, face = "bold")  # 图例文本样式
  )


dfs10.2<-read.csv('dfs10.2.csv')
dfs10.2 <- dfs10.2 %>%
  mutate(lightcast_neg = -lightcast)

# 颜色方案
color_palette <- c("#3574b2", "#5C9E7B")  # 蓝色, 绿色

# 创建对称条形图
figs10.2 <-  ggplot() +
  # lightcast数据（负半轴）
  geom_bar(data = dfs10.2,
           aes(x = reorder(onet, lightcast), 
               y = lightcast_neg,
               fill = "lightcast"),  # 改为使用名称而非直接颜色值
           stat = "identity",
           width = 0.6) +
  # revelio数据（正半轴）
  geom_bar(data = dfs10.2,
           aes(x = reorder(onet, lightcast), 
               y = revelio,
               fill = "revelio"),  # 改为使用名称而非直接颜色值
           stat = "identity",
           width = 0.6) +
  # 坐标轴和标签
  labs(x = "Occupation (SOC Code)",
       y = "Proportion of High AI-RS Disciplines",
       tag = "B",
       fill = "Database") +  # 添加图例标题
  # 对称y轴
  scale_y_continuous(labels = function(x) scales::percent(abs(x)),
                     breaks = seq(-1, 1, by = 0.2),
                     limits = c(-max(dfs10.1$lightcast, dfs10.1$revelio)*1.1, 
                               max(dfs10.1$lightcast, dfs10.1$revelio)*1.1)) +
  # 颜色映射和图例
  scale_fill_manual(
    values = c("lightcast" = "#3574b2", "revelio" = "#5C9E7B"),  # 指定颜色映射
    labels = c("lightcast", "revelio")  # 图例标签
  ) +
  
  # 添加垂直参考线
  geom_vline(xintercept = seq(0.5, length(unique(dfs10.1$naics))+0.5, by = 1),
             color = "gray90", linewidth = 0.3) +
  
  # 添加水平零线
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  
  # 主题设置
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "right",  # 图例位置在右侧
    legend.title = element_text(size = 10, face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 9, face = "bold")  # 图例文本样式
  )
figs10<-grid.arrange(figs10.1,figs10.2,ncol=2)
ggsave(filename = "figs10.pdf",plot = figs10, device = cairo_pdf,width = 12,height = 8, dpi = 600)
ggsave("figs10.tiff", figs10, width = 12, height = 8, dpi = 600, compression = "lzw")