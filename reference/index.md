# Package index

## Authentication & Configuration

Setup connection to the formr server

- [`formr_api_authenticate()`](http://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
  : Authenticate with formr
- [`formr_store_keys()`](http://rubenarslan.github.io/formr/reference/formr_store_keys.md)
  : Store API Credentials in Keyring
- [`formr_api_logout()`](http://rubenarslan.github.io/formr/reference/formr_api_logout.md)
  : Revoke Access Token (Logout)
- [`formr_api_session()`](http://rubenarslan.github.io/formr/reference/formr_api_session.md)
  : Get Current API session

## Project Management & Backup

High-level workflow for syncing and backing up entire projects

- [`formr_pull_project()`](http://rubenarslan.github.io/formr/reference/formr_pull_project.md)
  : Pull Project from Server Scaffolds folder structure if missing, then
  overwrites local files with Server state.
- [`formr_push_project()`](http://rubenarslan.github.io/formr/reference/formr_push_project.md)
  : Push Project to Server
- [`formr_backup_run()`](http://rubenarslan.github.io/formr/reference/formr_backup_run.md)
  : Backup a study

## Run Management

Operations for Run structures and settings

- [`formr_runs()`](http://rubenarslan.github.io/formr/reference/formr_runs.md)
  : List all runs
- [`formr_create_run()`](http://rubenarslan.github.io/formr/reference/formr_create_run.md)
  : Create a new run
- [`formr_run_settings()`](http://rubenarslan.github.io/formr/reference/formr_run_settings.md)
  : Get or Update Run Settings
- [`formr_run_structure()`](http://rubenarslan.github.io/formr/reference/formr_run_structure.md)
  : Get or Update Run Structure (Run Units)
- [`print(`*`<formr_run_structure>`*`)`](http://rubenarslan.github.io/formr/reference/print.formr_run_structure.md)
  : Print method for formr run structure
- [`formr_delete_run()`](http://rubenarslan.github.io/formr/reference/formr_delete_run.md)
  : Delete a Run
- [`as.data.frame(`*`<formr_run_structure>`*`)`](http://rubenarslan.github.io/formr/reference/as.data.frame.formr_run_structure.md)
  : Convert formr run structure to data.frame

## Survey Management

Upload, retrieve, and delete surveys

- [`formr_surveys()`](http://rubenarslan.github.io/formr/reference/formr_surveys.md)
  : List Surveys
- [`formr_survey_structure()`](http://rubenarslan.github.io/formr/reference/formr_survey_structure.md)
  : Get Survey Structure (Items)
- [`formr_upload_survey()`](http://rubenarslan.github.io/formr/reference/formr_upload_survey.md)
  : Upload/Update Survey
- [`formr_delete_survey()`](http://rubenarslan.github.io/formr/reference/formr_delete_survey.md)
  : Delete a Survey

## Session Management

Manage participants and test sessions

- [`formr_sessions()`](http://rubenarslan.github.io/formr/reference/formr_sessions.md)
  : List Sessions in a Run
- [`formr_create_session()`](http://rubenarslan.github.io/formr/reference/formr_create_session.md)
  : Create Session(s)
- [`formr_session_action()`](http://rubenarslan.github.io/formr/reference/formr_session_action.md)
  : Perform Action on Session(s)

## File Management

Manage assets and file attachments

- [`formr_files()`](http://rubenarslan.github.io/formr/reference/formr_files.md)
  : List files attached to a run
- [`formr_upload_file()`](http://rubenarslan.github.io/formr/reference/formr_upload_file.md)
  : Upload File(s) to Run
- [`formr_delete_file()`](http://rubenarslan.github.io/formr/reference/formr_delete_file.md)
  : Delete file(s) from a run
- [`formr_delete_all_files()`](http://rubenarslan.github.io/formr/reference/formr_delete_all_files.md)
  : Delete ALL files attached to a run

## Results & Data Processing

Fetch, clean, and aggregate data

- [`formr_results()`](http://rubenarslan.github.io/formr/reference/formr_results.md)
  : Get Run Results
- [`formr_post_process_results()`](http://rubenarslan.github.io/formr/reference/formr_post_process_results.md)
  : Process Results Wrapper
- [`formr_recognise()`](http://rubenarslan.github.io/formr/reference/formr_recognise.md)
  : Recognise data types based on survey metadata
- [`formr_reverse()`](http://rubenarslan.github.io/formr/reference/formr_reverse.md)
  : Reverse Items
- [`formr_aggregate()`](http://rubenarslan.github.io/formr/reference/formr_aggregate.md)
  : Aggregate Scales
- [`formr_label_missings()`](http://rubenarslan.github.io/formr/reference/formr_label_missings.md)
  : Tag Missing Values (Placeholder)

## Item Utilities

Metadata extraction and scale construction

- [`items()`](http://rubenarslan.github.io/formr/reference/items.md) :
  Get item metadata from survey results
- [`item()`](http://rubenarslan.github.io/formr/reference/item.md) : Get
  specific item metadata
- [`aggregate_and_document_scale()`](http://rubenarslan.github.io/formr/reference/aggregate_and_document_scale.md)
  : Aggregate variables and document construction
- [`choice_labels_for_values()`](http://rubenarslan.github.io/formr/reference/choice_labels_for_values.md)
  : Switch choice values with labels
- [`reverse_labelled_values()`](http://rubenarslan.github.io/formr/reference/reverse_labelled_values.md)
  : Reverse labelled values
- [`rescue_attributes()`](http://rubenarslan.github.io/formr/reference/rescue_attributes.md)
  : Rescue lost attributes

## Feedback & Plotting

Visualization and feedback generation

- [`qplot_on_normal()`](http://rubenarslan.github.io/formr/reference/qplot_on_normal.md)
  : Plot a normed value on the standard normal
- [`qplot_on_bar()`](http://rubenarslan.github.io/formr/reference/qplot_on_bar.md)
  : Plot normed values as a barchart
- [`qplot_on_polar()`](http://rubenarslan.github.io/formr/reference/qplot_on_polar.md)
  : Time-polar plot
- [`feedback_chunk()`](http://rubenarslan.github.io/formr/reference/feedback_chunk.md)
  : Text feedback based on groups

## Rendering & Markdown

RMarkdown helpers for formr feedback

- [`formr_render()`](http://rubenarslan.github.io/formr/reference/formr_render.md)
  : render text for formr
- [`formr_render_commonmark()`](http://rubenarslan.github.io/formr/reference/formr_render_commonmark.md)
  : render inline text for formr
- [`formr_inline_render()`](http://rubenarslan.github.io/formr/reference/formr_inline_render.md)
  : render inline text for formr
- [`formr_knit()`](http://rubenarslan.github.io/formr/reference/formr_knit.md)
  : knit rmarkdown to markdown for formr
- [`asis_knit_child()`](http://rubenarslan.github.io/formr/reference/asis_knit_child.md)
  : knit_child as is
- [`knit_prefixed()`](http://rubenarslan.github.io/formr/reference/knit_prefixed.md)
  : knit prefixed
- [`markdown_custom_options()`](http://rubenarslan.github.io/formr/reference/markdown_custom_options.md)
  : custom markdown options for rmarkdown's pandoc
- [`markdown_github()`](http://rubenarslan.github.io/formr/reference/markdown_github.md)
  : github_markdown for rmarkdown
- [`markdown_hard_line_breaks()`](http://rubenarslan.github.io/formr/reference/markdown_hard_line_breaks.md)
  : hard line breaks
- [`render_text()`](http://rubenarslan.github.io/formr/reference/render_text.md)
  : render text
- [`word_document()`](http://rubenarslan.github.io/formr/reference/word_document.md)
  : word_document from rmarkdown, but has an added option not to break
  on error
- [`paste.knit_asis()`](http://rubenarslan.github.io/formr/reference/paste.knit_asis.md)
  : paste.knit_asis
- [`print(`*`<knit_asis>`*`)`](http://rubenarslan.github.io/formr/reference/print.knit_asis.md)
  : Print new lines in knit_asis outputs

## Messaging & Notifications

SMS and Email helpers

- [`text_message_twilio()`](http://rubenarslan.github.io/formr/reference/text_message_twilio.md)
  : Send text message via Twilio
- [`text_message_clickatell()`](http://rubenarslan.github.io/formr/reference/text_message_clickatell.md)
  : Send text message via Clickatell
- [`text_message_massenversand()`](http://rubenarslan.github.io/formr/reference/text_message_massenversand.md)
  : Send text message via Massenversand.de
- [`email_image()`](http://rubenarslan.github.io/formr/reference/email_image.md)
  : Generate email CID

## Time & Logic Utilities

Shorthands for time calculations and common logic

- [`time_passed()`](http://rubenarslan.github.io/formr/reference/time_passed.md)
  : checks how much time has passed relative to the user's last action

- [`next_day()`](http://rubenarslan.github.io/formr/reference/next_day.md)
  : checks whether a new day has broken (date has increased by at least
  one day)

- [`in_time_window()`](http://rubenarslan.github.io/formr/reference/in_time_window.md)
  : checks whether the current time is in a certain time window

- [`random_date_in_range()`](http://rubenarslan.github.io/formr/reference/random_date_in_range.md)
  : Random date in range

- [`first()`](http://rubenarslan.github.io/formr/reference/first.md) :
  Gives the first non-missing element

- [`last()`](http://rubenarslan.github.io/formr/reference/last.md) :
  Gives the last non-missing element

- [`current()`](http://rubenarslan.github.io/formr/reference/current.md)
  : Gives the last element, doesn't omit missings

- [`finished()`](http://rubenarslan.github.io/formr/reference/finished.md)
  : How many surveys were finished?

- [`expired()`](http://rubenarslan.github.io/formr/reference/expired.md)
  : How many surveys were expired?

- [`if_na()`](http://rubenarslan.github.io/formr/reference/if_na.md) :
  Replace NA values with something else

- [`if_na_null()`](http://rubenarslan.github.io/formr/reference/if_na_null.md)
  : This function makes sure you know what to expect when evaluating
  uncertain results in an if-clause. In most cases, you should not use
  this function, because it can lump a lot of very different cases
  together, but it may have some use for fool-proofing certain
  if-clauses on formr.org, where a field in a survey may either not
  exist, be missing or have a value to check.

- [`ifelsena()`](http://rubenarslan.github.io/formr/reference/ifelsena.md)
  :

  Like [`ifelse()`](https://rdrr.io/r/base/ifelse.html), but allows you
  to assign a third value to missings.

- [`` `%contains%` ``](http://rubenarslan.github.io/formr/reference/grapes-contains-grapes.md)
  : check whether a character string contains another

- [`` `%contains_word%` ``](http://rubenarslan.github.io/formr/reference/grapes-contains_word-grapes.md)
  : check whether a character string contains another as a word

- [`` `%begins_with%` ``](http://rubenarslan.github.io/formr/reference/grapes-begins_with-grapes.md)
  : check whether a character string begins with a string

- [`` `%ends_with%` ``](http://rubenarslan.github.io/formr/reference/grapes-ends_with-grapes.md)
  : check whether a character string ends with a string

- [`get_opencpu_rds()`](http://rubenarslan.github.io/formr/reference/get_opencpu_rds.md)
  : Get OpenCPU RDS
