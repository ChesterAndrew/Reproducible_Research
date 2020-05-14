---
title: "Reproducible_Research"
author: "Chester Sellers"
date: "5/14/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Course Project 1
================

First we load our data

```{r}
activity <- read.csv("C:/Users/casel/Desktop/Coursera/Reproducible_Research/repdata_data_activity/activity.csv")

```
View our data
```{r}
View(activity)
```
