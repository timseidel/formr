# Push Project to Server

Uploads local project files (surveys, assets, settings) to the formr
server. Optionally monitors the directory for subsequent changes
(Watcher mode).

## Usage

``` r
formr_push_project(run_name, dir = ".", watch = FALSE, interval = 2)
```

## Arguments

- run_name:

  Name of the run.

- dir:

  Local directory (default ".").

- watch:

  Logical. If TRUE, keeps the process running to monitor and push
  changes (default FALSE).

- interval:

  Seconds between checks in watch mode (default 2).
