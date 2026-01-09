# formr: R Client for formr.org

[![CRAN status](https://www.r-pkg.org/badges/version/formr)](https://CRAN.R-project.org/package=formr)
[![Codecov test coverage](https://codecov.io/gh/rubenarslan/formr/graph/badge.svg)](https://app.codecov.io/gh/rubenarslan/formr)

The **formr** R package is the official companion to the [formr survey framework](https://formr.org). 

While the formr-package is designed to provide useful helper functions *inside* formr surveys (for feedback plots and logic), **version 0.12.0** introduces a robust API client. You can now use R to manage your studies locally, sync files, version control your surveys, and fetch results securely.

## Documentation
Full documentation is available at **[rubenarslan.github.io/formr](https://rubenarslan.github.io/formr)**.

## Installation

> **⚠️ Compatibility Note:** Version 0.12.0 introduces breaking changes to support the new formr API (v1). If your formr server instance does not yet support the v1 API, you should install the previous version (0.11.1).

**To install the latest version (v0.12.0+):**
```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("rubenarslan/formr")
```

**To install the legacy version (compatible with older servers):**
```r
remotes::install_github("rubenarslan/formr@873c3ba")
```

## Authentication

To use the API features (fetching results, managing files), you must first authenticate. You can store your credentials securely in your system's keyring so you don't have to type them every time.

1.  Log in to formr.org and go to **Account > API Settings**.
2.  Create a Client ID and Secret (or generate a Personal Access Token).

```r
library(formr)

# Store your credentials once
# This saves them securely in your OS credential store
formr_store_keys(
  host = "https://formr.org", # or the URL of your instance
  client_id = "YOUR_CLIENT_ID", 
  client_secret = "YOUR_CLIENT_SECRET"
)

# Then, in all of your scripts, simply run:
formr_api_authenticate()
```

#### You can use the API within your runs R-Code. 

```r
# Simply run:
formr_api_authenticate()
```

## Fetching & Processing Results

The package provides tools to fetch data from complex runs and automatically join survey results, handle randomization (shuffles), and type items.

```r
# 1. Fetch results from a run
# join = TRUE automatically merges multiple surveys and shuffles by session
results <- formr_results("my-run-name", join = TRUE)

# 2. Get the run structure (items, choices, types)
items <- formr_run_structure("my-run-name")

# 3. Post-process
# This handles type conversion, reverse-scoring, and aggregation for you.
cleaned_data <- formr_post_process_results("my-run-name")

# Your data is now ready for analysis!
head(cleaned_data)
```

## Study Management (Push/Pull)

You can now develop surveys and runs locally (using Excel/JSON/RMarkdown) and sync them to the server. 
This allows you to use Git for version control.

### Backing up a study
Download everything—results, assets, survey items, and run structure—to a local folder.

```r
formr_backup_run("my-run-name", dir = "backup_folder")
```

### Developing locally
You can pull a project structure, make edits to Excel files or CSS locally, and push them back.

```r
# Initialize/Sync local folder with server state
formr_pull_project("my-run-name", dir = "my_project")

# ... Make changes to surveys/my-survey.xlsx or css/custom.css ...

# Push changes back to formr.org
formr_push_project("my-run-name", dir = "my_project")
```

## Workflow 3: Session Management

Control participants programmatically. Useful for automated testing or managing users.

```r
# Create a test session
formr_create_session("my-run-name", testing = TRUE)

# Find a specific session
session_info <- formr_sessions("my-run-name", session_codes = "...")

# Move a user to a specific position in the run
formr_session_action("my-run-name", session_codes = "...", action = "move_to_position", position = 10)
```

## Helper Functions (Inside formr)

The package contains the utility functions used *inside* formr's OpenCPU environment for dynamic feedback and logic:

* **Logic Shorthands:** `time_passed()`, `%contains%`, `if_na()`.
* **Feedback Plots:** `qplot_on_normal()`, `qplot_on_bar()`.
* **Markdown Helpers:** `formr_render_commonmark()`.

```r
# Example: Check if a string contains a word
"apple, banana" %contains% "apple" # TRUE

# Example: Feedback text logic
feedback_chunk(0.5, c("Low", "Average", "High")) # Returns "Average"
```

## License

MIT