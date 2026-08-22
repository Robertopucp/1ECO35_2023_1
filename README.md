# Python and R 

## Lectures Overview

This repository contains the materials for a 12-lecture Python and R programming course I taught at the undergraduate level. The course was designed to give economics students the practical programming and data science skills used in applied economic research: data wrangling, applied econometrics, machine learning, web scraping, and geospatial analysis in both Python and R.

Each `Lecture_N` folder contains the lecture materials in both languages: a Jupyter notebook for Python and an `.R` script for R, so students could compare the same workflow across both tools.

## Skills and Tools Covered

- **Programming fundamentals:** data types, data structures, control flow, functions, functional programming (`lambda`, `map`, `apply`), and error handling.
- **Data wrangling:** `pandas` DataFrames, `merge`, `groupby`, `concat`, reshaping data (`pivot`, `melt`, wide-to-long), and data cleaning (missing values, type conversion, filtering).
- **Data visualization:** `matplotlib`, `seaborn`, and `ggplot2`.
- **Text processing:** regular expressions and PDF data extraction (`PyPDF2`, `camelot`).
- **Applied econometrics:** linear regression, coefficient plots, instrumental variables, and panel data models with fixed effects, with results exported to LaTeX tables.
- **Machine learning:** regularized regression (ridge, lasso), pruned decision trees, random forests, and boosted trees.
- **Automation and web scraping:** `requests`, `BeautifulSoup`, and `Selenium` for static and dynamic (JavaScript-rendered) websites, applied to real sources such as job postings and government/economic data portals.
- **Geospatial analysis:** `geopandas`, shapefiles, spatial joins, geocoding, and map visualization.

---

### Lecture 1 — Data Types and Data Structures
Introduction to core Python building blocks: variable types (`string`, `integer`, `boolean`, `float`), and fundamental data structures including `tuple`, `set`, `dictionary`, `array`, and matrices. Indexing and slicing are also covered.

---

### Lecture 2 — Control Flow and Functions
Conditional logic (`if`, nested `if`), iteration (`while`, `for` loop), error handling (`try/except`), and user-defined functions.

---

### Lecture 3 — Functional Programming
Lambda functions, `map()`, `apply()` on DataFrames and matrices, and flexible function arguments (`*args`, `**kwargs`).

---

### Lecture 4 — DataFrames and ETL
Introduction to `pandas` DataFrames. Covers data loading, transformation, `groupby` aggregations, and the Extract-Transform-Load (ETL) pipeline fundamentals.

---

### Lecture 5 — ETL Part II: Data Cleaning
Advanced data cleaning techniques including handling missing values, data type conversion, filtering, and combining datasets with `merge` and `concat`.

---

### Lecture 6 — Data Visualization
Data visualization in Python and R using `matplotlib`, `seaborn`, and `ggplot2`. Covers histograms, scatter plots, bar charts, and line graphs.

---

### Lecture 7 — Regular Expressions and Reshaping Data
Text processing using `regex` in Python: pattern matching, string extraction, and text cleaning applied to real datasets. Also covers reshaping datasets between wide and long formats (`pivot`, `melt`).

---

### Lecture 8 — Extracting Data from PDFs
Extracting structured data (text and tables) from PDF documents using `PyPDF2` and `camelot`, applied to a real policy evaluation report.

---

### Lecture 9 — Regression, Machine Learning, and Panel Data Econometrics
Applied econometrics and machine learning in Python and R. Covers linear regression, coefficient plots, and exporting results to LaTeX tables, as well as instrumental variables and panel data models (fixed effects). Also introduces predictive modeling: regularized linear regression (ridge and lasso), decision trees with pruning, random forests, and boosted trees.

---

### Lecture 10 — Web Scraping
Automated data collection from websites using `requests`, `BeautifulSoup`, and `Selenium`. Covers both static pages and dynamic (JavaScript-rendered) content, applied to real-world sources such as job postings (Bumeran) and government/economic data portals (MEF, PCM).

---

### Lecture 11 and 12 — Geospatial Data
Geospatial data handling in Python. Topics include reading shapefiles with `geopandas`, spatial joins, geocoding, and map visualization.
