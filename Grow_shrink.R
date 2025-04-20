library(dplyr)
library(gRbase)
library(RGraphics) 
library(gRain)
library(bnlearn)
library(igraph)
library(tidyr)
library(graph)
library(bnstruct)
library(deal)
# 1. یادگیری ساختار شبکه از داده آموزشی
fit_gs = gs(train_data)

# 📊 نمایش گراف DAG به‌دست‌آمده
  viewer(fit_gs,
         bayesianNetwork.width = "100%",
         bayesianNetwork.height = "80vh",
         bayesianNetwork.layout = "layout_with_sugiyama",
         bayesianNetwork.title="Discrete Bayesian Network - ",
         bayesianNetwork.subtitle = "Monitoring of emergency care patients",
         bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama"
  )

# ------------------------------------------
# 2. استخراج Markov Blanket برای متغیر مورد نظر
mb_nodes = mb(fit_gs, node = "readmitted")

# 3. اضافه کردن خود متغیر به لیست نودهای زیرشبکه
mb_subgraph_nodes = c("readmitted", mb_nodes)

# ------------------------------------------
# 4. استخراج ماتریس مجاورت از DAG اصلی
amat_full = amat(fit_gs)

# فیلتر کردن ماتریس مجاورت برای نودهای Markov Blanket
amat_mb = amat_full[mb_subgraph_nodes, mb_subgraph_nodes]

# ------------------------------------------
# 5. ساخت شیء bn با استفاده از زیرماتریس
class(amat_mb) = "matrix"
mb_bn = empty.graph(nodes = mb_subgraph_nodes)
amat(mb_bn) = amat_mb

# تبدیل گراف جزئی به DAG کامل (درصورت نیاز)
mb_bn_dag = cextend(mb_bn)

# ------------------------------------------
# ✅ مرحله 6: Moralization
# ابتدا یک گراف بی‌جهت از مدل Markov Blanket بساز
mb_bn_ug = moral(mb_bn_dag)  # moral از bnlearn ساختار moral شده رو می‌ده

# تبدیل به adjacency matrix برای رسم یا ادامه
amat_moral = amat(mb_bn_ug)

# رسم گراف moralized
library(igraph)
g_moral = graph.adjacency(amat_moral, mode = "undirected")

plot(g_moral,
     vertex.label.cex = 0.8,
     vertex.size = 25,
     edge.color = "gray30",
     vertex.color = "lightblue",
     main = "Moralized Graph")


# رسم گراف moralized
g_moral = graph.adjacency(amat_moral, mode = "undirected")
plot(g_moral,
     vertex.label.cex = 0.8,
     vertex.size = 25,
     edge.color = "gray30",
     vertex.color = "lightblue",
     main = "Moralized Graph")

# ------------------------------------------
#step 7 :allready is trangulated
"""
g_moral_undirected = ug(amat_moral)

triangulated_graph = triangulate(g_moral_undirected)
"""
# ✅ مرحله 8: ساخت junction tree (clique tree)
junction_tree = rip(g_moral)

# ------------------------------------------
# 9. فیت کردن پارامترها با داده‌های همین زیرشبکه
train_data_mb = train_data[, mb_subgraph_nodes]
fit_mb_bn = bn.fit(mb_bn_dag, train_data_mb)

# ------------------------------------------
# 10. تبدیل به object gRain برای inference
grain_mb = as.grain(fit_mb_bn)

# propagate برای استنتاج
grain_mb_propagated = propagate(grain_mb)

# ------------------------------------------
# 11. نمایش خلاصه‌ای از شبکه
summary(grain_mb_propagated)
# فقط متغیرهای Markov Blanket رو از داده‌ی تست بردار
test_data_mb = test_data[, mb_subgraph_nodes]

# پیش‌بینی مقدار readmitted برای هر رکورد
preds = predict(grain_mb_propagated, response = "readmitted", newdata = test_data_mb)

real = test_data_mb$readmitted 

comparison = data.frame(real, preds)

library(caret)
confusionMatrix(as.factor(comparison$readmitted), as.factor(comparison$real))
