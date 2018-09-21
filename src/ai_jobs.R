library(rvest)
library(ggplot2)
library(ggsci)
library(scales)
library(ggpubr)
source('./src/lago.R')

jobs <- lago('rengongzhineng', 1:30, sleep.time = 3)

jobs <- readRDS('./data/ai_jobs.rds')

jobs <- within(jobs, {
  Job <- as.character(Job)
  Location <- as.character(Location)
  Company <- as.character(Company)
  Industry <- as.character(Industry)
  Salary <- as.character(Salary)
  Salary_Lower <- unlist(strsplit(Salary, split = '-'))[seq(1, nrow(jobs)*2, by = 2)]
  Salary_Upper <- unlist(strsplit(Salary, split = '-'))[seq(2, nrow(jobs)*2, by = 2)]
  Salary_Lower <- as.numeric(gsub('[kK]', '', Salary_Lower))
  Salary_Upper <- as.numeric(gsub('[kK]', '', Salary_Upper))
  Experience <- as.character(Experience)
  Experience <- gsub('经验', '', Experience)
  Experience <- factor(Experience, 
                       levels = c('不限', '应届毕业生', '1年以下', '1-3年', '3-5年', '5-10年', '10年以上'))
  Financing <- factor(Financing,
                      levels = c('不需要融资', '未融资', '天使轮', 'A轮', 'B轮', 'C轮', 'D轮及以上', '上市公司'))
  Scale <- factor(Scale,
                  levels = c('少于15人', '15-50人', '50-150人', '150-500人', '500-2000人', '2000人以上'))
  Degree <- factor(Degree,
                   levels = c('不限', '大专', '本科', '硕士', '博士'))
  City <- NULL
  for(i in 1:nrow(jobs)){
    if(grepl('·', Location[i])){
      City_1 <- unlist(strsplit(Location[i], split = '·'))[1]
      }else{
        City_1 <- Location[i]
        }
    City <- c(City, City_1)
    rm(City_1)
  }
  Dist <- unlist(strsplit(Location, split = '·'))[seq(2, nrow(jobs)*2, by = 2)]
})

jobs <- jobs[jobs$Degree == '本科',]

## barplot -----------------------------------------------------------


bar('Scale','招本科生的公司人员规模', 3)
ylab = '岗位数量（百分比）'
b1 <- bar('Scale','招本科生的公司人员规模', 3, ylab) +
  theme(axis.text.x = element_text(angle = 45))
b2 <- bar('Financing','招本科生的公司融资情况', 3, ylab) +
  theme(axis.text.x = element_text(angle = 45))
ggarrange(b1, b2, ncol = 2)

a <- colorRampPalette(pal_npg()(10))(21)
b <- pal_npg()(4)
# belt ---------------------------------------------------------------


s1 <- belt('Experience', title = '平均薪资与经验', color = b[1], ylab)
s2 <- belt('Degree', title = '平均薪资与学历', color = b[2], ylab)
s3 <- belt('Scale', title = '平均薪资与公司规模', color = a[10], ylab)
s4 <- belt('Financing', title = '平均薪资与公司融资情况', color = a[14], ylab)
ggarrange(s1, s2, s3, s4, ncol = 2, nrow = 2)

# bubble -------------------------------------------------------------
set.seed(1000)
Industry <- jobs$Industry
Industry <- unlist(strsplit(Industry, split = ','))
Industry <- unlist(strsplit(Industry, split = '、'))
Industry <- unlist(strsplit(Industry, split = ' '))

Indstry_tab <- data.frame(Count = table(Industry))
Indstry_tab$X <- runif(nrow(Indstry_tab),0,30)
Indstry_tab$Y <- runif(nrow(Indstry_tab),0,30)

ggplot(Indstry_tab, aes(x = X, y = Y, color = Count.Industry)) +
  geom_point(aes(size = Count.Freq *5), alpha = 0.5) +
  scale_size(range = c(5,40)) +
  scale_color_manual(values = a) +
  geom_text(aes(label = Count.Industry), color = 'black') +
  guides(color = F, size = F)

ggplot(Indstry_tab, aes(x = Count.Industry, y = Count.Freq)) +
  geom_bar(aes(fill = Count.Industry), width = 0.7, stat = 'identity') +
  geom_text(aes(label = paste0(Count.Freq, ' (', percent(Count.Freq/nrow(jobs)), ')'),
                y = Count.Freq), nudge_y = 30) +
  labs(x = NULL, y = '标签数量（占岗位百分比）', title = '行业标签') +
  scale_fill_manual(values = a) +
  coord_flip() +
  ylim(c(0, 350)) +
  theme(legend.position = 'none')
  