
#  title: "PSet 4"
# author: "Xinrui Bai, Siyuan Sang,  Zewen Shi"

  
  ```{r setup, include=FALSE}
# this prevents package loading message from appearing in the rendered version of your problem set
knitr::opts_chunk$set(warning = FALSE, message = FALSE,
                      echo = TRUE)
```

Note: Grading is based both on your graphs and verbal explanations. Follow all best practices *as discussed in class*, including choosing appropriate parameters for all graphs. *Do not expect the assignment questions to spell out precisely how the graphs should be drawn. Sometimes guidance will be provided, but the absense of guidance does not mean that all choices are ok.*
  
  IMPORTANT: THIS TEMPLATE DOES NOT INCLUDE THE SAMPLE GRAPHS THAT APPEAR IN THE .HTML VERSION OF THE ASSIGNMENT SO BE SURE TO VIEW THAT FILE AS WELL.

### 1. `mycars` missing patterns

Create a missing values plot for the `mycars` dataset created below (slightly different from the one in the lecture slides). Your plot should be in the style of `extracat::visna()` (no longer available on CRAN) using **ggplot2** to create the main plot and two side plots and **patchwork** to put them together. It should show accurately: missing patterns,  counts for missing by column and missing by pattern, and make it clear which row respresents complete cases. Bars in the side plots should be sorted and correspond to the rows and columns of the main plot. An example is provided though the aesthetics of your plot do not have to conform precisely to the example. Some code is provided to get you started with finding missing patterns. (Keep in mind that in the next question you will be turning this code into a function so to save yourself time later on write as generically as possible.)

```{r}

.libPaths("C:/Rpackage")
library(tidyverse)
library(patchwork)

# Add NAs to mtcars dataset
set.seed(5702)
mycars <- mtcars
mycars[1:25, "gear"] <- NA
mycars[10:20, 3:5] <- NA
for (i in 1:10) mycars[sample(32,1), sample(11,1)] <- NA
```

**Hints:**
  
  * You can use this code to find and count the missing patterns:
  ```{r}
missing_patterns <- data.frame(is.na(mycars)) %>%
  group_by_all() %>%
  count(name = "count", sort = TRUE) %>%
  ungroup()
```

* To highlight the complete cases row you need a different fill scale than the one used to show missing vs. non-missing in the main plot (purple and grey in the example). This is a little tricky since you're only allowed one fill scale in **ggplot2**. You can either use `alpha` to control the highlight with `scale_alpha_manual(values = ...)` or use the **ggnewscale** package which provides for multiple fill scales in the same graph.

```{r}
#count the missing patterns
missing_patterns <- data.frame(is.na(mycars)) %>%
  group_by_all() %>%
  count(name = "count", sort = TRUE) %>%
  ungroup()
```

```{r}
#divide the different cases
missing<-data.frame(ifelse(missing_patterns[,1:11]==TRUE,'1','0'))
missing[3,]<-'2'
missing[,12]<-missing_patterns[,12]
```

```{r}
tidypattern <- missing %>% 
    rownames_to_column("pattern") %>% 
    gather(key, value, -c(pattern,count)) 
```


```{r}
p1<-ggplot(tidypattern,aes(x=factor(key),y=fct_rev(factor(pattern)),fill=value)) +
  geom_tile(color="grey50") +
  scale_fill_manual(values = c("white", "purple","grey")) +
  ylab("missing pattern") +
  theme(legend.position="none")
```

```{r}
t1 <- data.frame(count = colSums(is.na(mycars))) %>% 
    rownames_to_column("columns") %>% 
    arrange(desc(count))
  lab1 <- "num rows missing"
  ylim1 <- max(t1$count)
```

```{r}
p2 <-ggplot(t1,aes(x=factor(columns),y=count)) + 
    geom_bar(stat="identity",fill="lightblue") + 
    labs(y=lab1,x="", title = "Missing value patterns") +
    ylim(0,ylim1) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())
```

```{r}
 test <- missing_patterns %>% 
    mutate(index = row_number()) %>%
    gather(key, value, -c(index,count)) %>%
    group_by(index) %>%
    mutate(label = ifelse(sum(value)==0, "complete", "pass")) %>%
    ungroup() %>%
    mutate(label = ifelse(label=="complete", "complete",ifelse(value==TRUE, "true", "false") ))
```

```{r}
t2 <- tidypattern %>%
    group_by(pattern) %>%
    summarise(count=mean(count))
  lab2 <- "row count"
  xlim2 <- max(t2$count)
```

```{r}
p3<-ggplot(t2, aes(x=count, y=fct_rev(factor(pattern)))) +
    geom_bar(stat="identity",fill="lightblue") +
    labs(x=lab2,y="") +
    xlim(0,xlim2) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()) +
    theme(legend.position = "none")
```



```{r}




patch = (p1 | p3) +  plot_layout(widths = c(4, 1))
patch2 = (p2 |  plot_spacer()) + plot_layout(widths = c(3, 1))
patch2 / patch +  plot_layout(widths = c(4, 1), heights = c(1, 4))
```


### 2. Missing value plot function

a) Create a function for creating missing plots based on your code from question 1. It should provide an option to show either missing counts or missing percent. The percent option for `mycars` is shown below.



```{r}

#to generate a function for the missing plots 
plot_missing_graphs <- function(df,percent=TRUE) {
  
  #missing graphs and make assumptions
  nrow = nrow(df)
  ncol = ncol(df)

#generate missing patterns based on the provisions of the questions 
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()

#group the data based on the requirements of the questions
  missing_datagroup <- missing_patterns %>% 
    mutate(index = row_number()) %>%
    gather(key, value, -c(index,count)) %>%
    group_by(index) %>%
    #generate missing patterns based on the provisions of the questions
    mutate(label = ifelse(sum(value)==0,
                          "complete", "pass")) %>%
    ungroup() %>%
    mutate(label = ifelse(label=="complete", 
                          "complete",ifelse(value==TRUE, "true", "false") ))

#make loops tto create more comprehensieve quotes and data
  if(!percent){
  missing_datagroup1 <- data.frame(count = colSums(is.na(df))) %>% 
    rownames_to_column("columns") %>% 
    arrange(desc(count))
#make loops tto create more comprehensieve quotes and data
  level <- missing_datagroup1$columns
  lab1 <- "num rows missing"
  ylim1 <- max(missing_datagroup1$count)

#make loops tto create more comprehensieve quotes and data
  missing_datagroup2 <- missing_datagroup %>%
    group_by(index) %>%
    summarise(count=mean(count),
              complete = ifelse(sum(value)==0,"yes","no"))
  lab2 <- "row count"
  xlim2 <- max(missing_datagroup2$count)
  }
  
  else{
  missing_datagroup1 <- data.frame(count = colSums(is.na(df))/nrow*100) %>% 
    rownames_to_column("columns") %>% 
    arrange(desc(count))
  level <- missing_datagroup1$columns
  lab1 <- "% rows missing"
  ylim1 <- 100

#make another looo make things better
  missing_datagroup2 <- missing_datagroup %>%
    group_by(index) %>%
    summarise(count=mean(count)/nrow*100, 
              complete = ifelse(sum(value)==0,"yes","no"))
  lab2 <- "% rows"
  xlim2 <-100
  }

  #start to create the graphs on the top  
  upper_graph <- ggplot(missing_datagroup1,aes(x=factor(columns,
                                  levels = level),y=count)) + 
    geom_bar(stat="identity",
             fill="lightblue") + 
    labs(y=lab1,x="", title = "Missing value patterns") +
    ylim(0,ylim1) +
    theme_bw() +
    theme(panel.grid.major.x = 
            element_blank(),
      panel.grid.minor.x = element_blank())
  
  
  index <- missing_datagroup2[missing_datagroup2$complete=="yes",]$index
  cols <- c("false" = "gray83", "true" = "rosybrown2",
            "complete" = "purple")
  #generate the first graph based on the requirement
  middle_graph <- ggplot(missing_datagroup,aes(x=factor(key,levels = level),
                        y=fct_rev(factor(index)),fill=label)) +
    geom_tile(color="white") + 
    scale_fill_manual(values = cols) +
    labs(x="variables",y="missing patterns") +
    theme(legend.position = "none")
  
  npattern <- nrow(missing_patterns)
  if(length(index)>=1){
    for(i in index){
      p1 <- p1 + geom_text(x=ncol/2+0.5, y=npattern+1-i, 
                           label="complete cases",size=5)
    }
  }
#start to create the plots on the right-hand side
  right_hand_plot <- ggplot(missing_datagroup2, aes(x=count, y=fct_rev(factor(index)),
                          fill=complete)) + 
    scale_fill_manual(values=c("lightblue", "lightblue")) +
    geom_bar(stat="identity") +
    labs(x=lab2,y="") +
    xlim(0,xlim2) +
    #to create better graphs
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()) +
    theme(legend.position = "none")

  
  #we want to generate the graph at the end
  combination= (middle_graph | right_hand_plot) +  
    plot_layout(widths = 
                  c(4, 1))
combination2 = (upper_graph |  plot_spacer()) + 
  plot_layout(widths = c(3, 1))
combination2 / combination +  plot_layout(widths = c(4, 1), heights = c(1, 4))
}




```


You either put the function code in a separate `.R` file or include it in the `.Rmd` file.

b) Show the output for both options (counts / percent) for the `economics` dataset in the **ggplot2** package. (This is a test to see if your function works if there are no missing values.)


```{r}



economic_data<- economics
plot_missing_graphs(economic_data,percent=TRUE)


plot_missing_graphs(economic_data,percent=FALSE)

```


c) Show the output for both options (counts / percent) for the `HollywoodMovies2011` dataset in the **Lock5withR** package. You can shorten the column names so they don't overlap in the plot.



```{r}

hollywood_data<- Lock5withR::HollywoodMovies2011
#name all the data
names <- c("movie","studio",
           "tomat","score",
           "story","genre",
           "tweek","bweek", 
           "dgro","fgro",
           "wgro","budget",
           "pro","open")
colnames(hollywood_data) <- names




plot_missing_graphs(hollywood_data,percent=TRUE)


plot_missing_graphs(hollywood_data,percent=FALSE)
```



### 3. Setup your GitHub final project repo

a) Set up your final project repository following the [EDAVproject template](https://github.com/jtr13/EDAVtemplate). You can either choose one team member's GitHub account, or create an organization to house the final project. *Be sure to follow all of the steps in the README so your bookdown book renders with your information, not the placeholders in the template.* Edit the link below to point to your rendered book:

https://aladhe.github.io/EDAV-final-project

b) Make sure that all team members have write access to the repository and have practiced making contributions. Edit the link below to point to your contributors page, showing that all team members have made contributions to the repo (Note that we do not have the ability to see who has write access, only who has contributed):

https://github.com/aladhe/EDAV-final-project/graphs/contributors

c) Discuss a plan for dividing up the work for the final project and briefly summarize what each person will do.

Our group dicide to work together on the main process, both of us will focus on a part of the index we would like to analyze. 

### 4. Missing values chapter

Write a first draft of the missing values chapter of your final project. You do not have to include all of the data you use in the final project. Choose one file and analyze it using techniques discussed in class for missing values. Include a plot using your function from Q2 as well as verbal interpretation of the plot. Edit this link to point to your chapter:

https://aladhe.github.io/EDAV-final-project/missing-values.html

**If your data for the final project has no missing values, you may use one of the following datasets instead for this question. You can include your answer here since it doesn't belong in your final project.**
  
  **fivethirtyeight** package: `avengers`, `bachelorette`, `dem_candidates`, `steak_survey`, `trumpworld_polls`

**openintro** package: `birds`, `ucla_textbooks_f18` 


```{r}

Project_data = read.csv("stock_data.csv", header = T)

plot_missing_graphs(Project_data,percent=TRUE)


plot_missing_graphs(Project_data,percent=FALSE)


```

This is the plot of missing value of main columns about stock data, which includes Eurekahedge CTA Index, SG CTA Index, S&P 500, US Aggregate Bond Index, and risk free rate. From the plot, we can see that four of them have evenly amount of missing values. The four missing patterns contain about 75% of the total number of rows.


```{r}


Project_data_return = read.csv("Data_with_return.csv", header = T)
plot_missing_graphs(Project_data_return,percent=TRUE)


plot_missing_graphs(Project_data_return,percent=FALSE)


```

This is the plot of missing value of monthly returns of the main stock data, which includes Eurekahedge CTA Index monthly return, SG CTA Index monthly return, S&P 500 monthly return, US Aggregate Bond Index monthly returns, etc. From the plot, we can see that six of them have evenly amount of missing values. The six missing patterns contain about 75% of the total number of rows.

