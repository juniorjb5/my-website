---
authors:
- admin
categories:
- R
- Shiny
- Finance
- Optimization
- NASDAQ 100
- Index Fund
date: "2021-05-25T00:00:00Z"
draft: false
featured: false
image:
  caption: 'Visit the app: [**Index Fund NASDAQ100**](https://juniorjb5.shinyapps.io/IndexFund_NASDAQ100/
)'
  focal_point: ""
  placement: 2
  preview_only: false
lastmod: "2021-05-25T00:00:00Z"
projects: []
subtitle: "In this post, I present a dashboard that optimizes through an Index Fund the NASDAQ💯."
summary: "In this post, I present a dashboard that optimizes through an Index Fund the NASDAQ💯."
tags:
- Academic
- NASDAQ 100
- Shiny
- Optimization
- Index Fund
title: Index Fund to NASDAQ100.
---

## Index Fund

An index fund is a portfolio designed to track the movements of a general market population or a market index.




**Variables**

$$\rho_{ij}: \text{Correlation between the stock i and the stock j}$$

$$x_{ij}: 
\begin{cases}
1 &  \text{If j is the stock most correlated stock in the Index Fund} \\\\
 & \text{for the stock i of NASDAQ100} \quad i,j: 1, ...,n  \\\\
0 & \text{Other case}
\end{cases}$$


$$y_{j}:
\begin{cases}
1 &  \text{If the stock j is in the Index Fund} \quad j = 1, ..., n \\\\
0 & \text{Other case}
\end{cases}$$




**Objective function**

$$
Z = \quad \text{Max} \quad  \sum_{i=1}^n \sum_{j=1}^n \rho_{ij} x_{ij}
$$





**Subject to**

$$\sum_{j=1}^n y_j = q$$

$$\sum_{j=1}^n x_{ij} = 1 \quad _{i=1, ..., n}$$

$$x_{ij} \leq y_j \quad _{i,j = 1, ..., n}$$

$$x_{ij}, y_j = 0 \quad o \quad 1 \quad _{i,j = 1, ..., n}$$



Money management strategies are primarily classified as "active" or "passive."

- Active portfolio management seeks to achieve superior performance through the use of technical and fundamental analysis, as well as forecasting techniques.

- Passive portfolio management avoids any forecasting technique and relies rather on diversification to achieve the desired performance.


**Objective**

It is desirable to have an index fund with **q** stocks, where **q** is substantially less than the size of the target population **(n)**.


**Problem** 

Create a model that groups assets into similar asset groups and selects a representative asset from each group to be included in the Index Fund's portfolio.


**Approach**

- Create an Index Fund with **q** stocks to track the NASDAQ100 index.

- Each share *i* has a share *j* that represents it in the Index Fund

- Each share *i* can be represented by share *j*, only if *j* is in the Index Fund

The objective of the model is to maximize the similarity between the n stocks and their representatives in the Index Fund.




## Visit

- 👉 [**Index Fund - NASDAQ100 App**](https://juniorjb5.shinyapps.io/IndexFund_NASDAQ100/
)

