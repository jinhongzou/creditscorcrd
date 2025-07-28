 

# creditscorcrd

## 项目简介

**项目名称：** ScorCrd  
**项目简介：** ScorCrd 是一个专为零售银行部设计的评分卡开发工具，提供从数据加载到模型评估的完整流程支持。主要功能包括数据加载、样本分析、WOE 转换、特征表现分析、模型评估等。  
**适用场景：** 零售信贷评分卡建模、风控建模、特征工程、模型评估等。  
**项目链接：** [GitHub项目链接](https://github.com/jinhognzou/ScorCrd)  


###评分卡建模的步骤介绍

### 步骤 1: 数据准备
- 使用 `数据加载` 功能上传你的数据集，并根据需要调整抽样比例。

### 步骤 2: 样本分析
- 分析数据集以了解其结构、分布以及缺失值情况，确保数据质量适合建模。

### 步骤 3: 变量筛选与处理
- 利用 `特征表现` 分析功能确定哪些变量对于预测目标最为重要。
- 对选定的变量进行 WOE 转换，以便于后续的评分卡构建。

### 步骤 4: 模型构建
- 根据业务需求选择合适的模型算法（例如逻辑回归），并使用经过 WOE 转换的数据进行模型训练。

### 步骤 5: 模型评估
- 使用 `模型评估` 功能评估模型性能，重点关注模型在训练集和测试集上的表现，如 PSI、ROC 曲线等指标。

### 步骤 6: 评分卡部署
- 将最终的评分卡模型部署至生产环境，用于实时或批量评分。


## 技术栈

- R语言
- Shiny
- Shinydashboard
- ggplot2
- scorecard 包

---

## 安装和运行

### 安装依赖库

```r
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("scorecard")
```

### 运行项目

```r
library(shiny)
runApp("path/to/ScorCrd")
```

---

## 功能介绍

### 1. 数据加载
- 支持通过 `fileInput` 加载本地 CSV 文件。
- 提供抽样比例设置，便于快速测试和调试。

### 2. 样本分析
- **样本概况**：展示数据集的基本统计信息。
- **源样本数据**：查看原始数据样本。
- **训练样本清单**：展示训练集样本。
- **测试样本清单**：展示测试集样本。

### 3. WOE 转换
- 使用 `mWoebin` 函数对变量进行 WOE（Weight of Evidence）分箱转换。
- 输出 WOE 转换后的数据框，可用于后续建模。

### 4. 特征表现分析
- **特征重要性**：展示 IV 值（Information Value）和随机森林方法计算的特征重要性。
- **特征表现图**：可视化特征分布、WOE 趋势图等。

### 5. 模型评估
- **模型稳定性**：通过 PSI（Population Stability Index）评估模型稳定性。
- **ROC 曲线**：展示训练样本和测试样本的 ROC 曲线及 AUC 值。

---

## 使用示例

### 数据加载

```r
g_dt_file_tmp <- browse_dt(isolate(g_var$browse_file), mStr2c(isolate(g_var$charcolname)))
```

### 样本分析

```r
mod_dat <- mSample(g_dt$g_dt_file, isolate(g_var$sample_rate))
```

### WOE 转换

```r
bins <- mWoebin(dt = dt, y = y, print_step = 0)
```

---

## 贡献指南

我们欢迎社区贡献！请遵循以下流程：

### 代码规范
- 使用标准 R 语言编码风格。
- 保持函数命名清晰、注释完整。

### 提交流程
1. `git add .` 添加修改文件
2. `git commit -m "Your commit message"` 提交更改
3. `git push` 推送至远程仓库

### Pull Request
- Fork 本项目，创建自己的分支。
- 提交 PR 时，请简要说明修改内容和目的。

---

## 问题反馈

如果您在使用过程中遇到任何问题，欢迎在 [GitHub Issues](https://github.com/jiinhongzou/creditscorcrd/issues) 中提交反馈或建议。

---

## 许可证

本项目采用 MIT 许可证。详情请参阅 [LICENSE](LICENSE) 文件。

---

## 作者

- [jinhongzou](https://github.com/jinhongzou)

---

## 致谢

感谢以下开源项目的支持：
- [shiny](https://shiny.rstudio.com/)
- [scorecard](https://cran.r-project.org/web/packages/scorecard/index.html)
