library(rvest)
library(ggplot2)
library(ggsci)
library(scales)
library(ggpubr)

lago <- function(category, page, sleep.time) {
  url_init <- 'https://www.lagou.com/zhaopin/'
  url_init <- paste0(url_init, category, '/')
  pb <- txtProgressBar(min = 0, max = length(page), style = 3)
  jobs <- NULL
  for (i in page) {
    url <- paste0(url_init, i, '/')
    # Job---------------------------
    job <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        job <- read_html(url) %>% html_nodes('h3') %>% html_text()
        if(length(job) > 0){
          break
        }else{
          message('zzZZZ... in job')
          Sys.sleep(20)
        }
      }
    }
    # Location-------------------------------------
    loc <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        loc <- read_html(url) %>% html_nodes('.add em') %>% html_text()
        if(length(loc) > 0){
          break
        }else{
          message('zzZZZ... in loc')
          Sys.sleep(20)
        }
      }
    }
    # Salary-------------------------------
    salary <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        salary <- read_html(url) %>% html_nodes('.money') %>% html_text()
        if(length(salary) > 0){
          break
        }else{
          message('zzZZZ... in salary')
          Sys.sleep(20)
        }
      }
    }
    # Experience and Degree---------------------------
    labs <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        labs <- read_html(url) %>% html_nodes('.li_b_l') %>% html_text(trim = T)
        if(length(labs) > 0){
          break
        }else{
          message('zzZZZ... in experience')
          Sys.sleep(20)
        }
      }
    }
    req <- unlist(strsplit(labs[seq(1, 30, by = 2)], split = '\n'))
    req <- req[seq(2, 30, by = 2)]
    req <- gsub(' *', '', req)
    exp <- unlist(strsplit(req, '/'))[seq(1, 30, by = 2)]
    degree <- unlist(strsplit(req, '/'))[seq(2, 30, by = 2)]
    # Company--------------------------------------------
    company <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        company <- read_html(url) %>% html_nodes('.company_name a') %>% html_text(trim = T)
        if(length(company) > 0){
          break
        }else{
          message('zzZZZ... in company')
          Sys.sleep(20)
        }
      }
    }
    # Industry, Financing and Scale---------------------
    compack <- NULL
    tm <- Sys.time() + sleep.time
    while(TRUE){
      Sys.sleep(sleep.time)
      if(Sys.time() >= tm){
        compack <- read_html(url) %>% html_nodes('.industry') %>% html_text(trim = T)
        if(length(compack) > 0){
          break
        }else{
          message('zzZZZ... in industry')
          Sys.sleep(20)
        }
      }
    }
    compack <- unlist(strsplit(compack, split = ' / '))
    industry <- compack[seq(1, length(compack), by = length(compack)/15)]
    financing <- compack[seq(2, length(compack), by = length(compack)/15)]
    scale <- compack[seq(3, length(compack), by = length(compack)/15)]
    # Join------------------------------------------
    jobs_1 <- data.frame(
      'Job' = job,
      'Location' = loc,
      'Salary' = salary,
      'Experience' = exp,
      'Degree' = degree,
      'Company' = company,
      'Industry' = industry,
      'Financing' = financing,
      'Scale' = scale
    )
    jobs <- rbind(jobs, jobs_1)
    setTxtProgressBar(pb, value = i)
  }
  return(jobs)
}

bar <- function(x, title, devi, ylab = ylab){
  ggplot(jobs, aes(x = jobs[,x])) +
    geom_bar(aes(fill = jobs[,x]), width = 0.7, stat = 'count') +
    geom_text(aes(label = paste0(..count.., ' (', percent(..count../nrow(jobs)), ')'),
                  y = ..count..), stat = 'count', nudge_y = devi) +
    labs(x = NULL, y = ylab, title = title) +
    scale_fill_npg() +
    theme(legend.position = 'none')
  
}

belt <- function(x, title, color, ylab){
  mean_upper <- data.frame(tapply(jobs$Salary_Upper, jobs[,x], mean))
  mean_lower <- data.frame(tapply(jobs$Salary_Lower, jobs[,x], mean))
  Salary <- merge(mean_lower, mean_upper, by = 'row.names')
  colnames(Salary) <- c(x, 'Lower', 'Upper')
  Salary[,x] <- factor(Salary[,x], levels = levels(jobs[,x]))
  ggplot(Salary, aes(x = as.numeric(factor(Salary[,x],
                                           levels = levels(jobs[,x]))))) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = color) +
    labs(x = NULL, y = ylab, title = title) +
    scale_x_continuous(breaks = c(1:length(levels(jobs[,x])))
                       ,labels = levels(jobs[,x])) +
    geom_label(label = round(Salary$Upper,2), y = Salary$Upper, alpha = 0.6) +
    geom_label(label = round(Salary$Lower,2), y = Salary$Lower, alpha = 0.6) +
    ylim(c(0, 100))
}