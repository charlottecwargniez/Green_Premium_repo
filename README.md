---
title: "README"
output: html_document
---

This repository indicates the various figures necessary for the paper. 



## 📦 Project Environment (via `renv`)

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies.  
Using `renv` ensures that all collaborators work with the same package versions, improving reproducibility and avoiding "works on my machine" problems.

### 🔧 Getting Started

To set up the environment on your machine:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/your-username/your-project.git
   cd your-project

2. **Install `renv`** (if you haven't already):
   ```R
   install.packages("renv")
   ```
3. **Restore the environment**:
   ```R
   library(renv)
   renv::restore()
   ```