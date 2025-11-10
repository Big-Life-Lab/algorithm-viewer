# Documentation

This folder contains the project documentation built with
[Quarto](https://quarto.org/).

## Folder Structure

- `_quarto.yml` - Quarto project configuration file
- `index.qmd` - Main landing page
- `*.qmd` - Additional documentation pages
- `_site/` - Generated HTML output

## Common Commands

* Build and view the documentation

  ```bash
  quarto preview
  ```
  This starts a local server with live reload. Any changes you make to
  `.qmd` files will be reflected immediately.

* Build the documentation

  ```bash
  quarto render
  ```
  This generates the HTML output in the `_site/` directory.

## Adding New Pages

1. Create a new `.qmd` file in this directory
2. Add it to the sidebar in `_quarto.yml`:
   ```yaml
   website:
     sidebar:
       contents:
         - index.qmd
         - your-new-page.qmd
   ```
3. Write your content using Quarto markdown syntax

