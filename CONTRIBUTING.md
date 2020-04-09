# How to Contribute

Contributions are welcome! Whether you're new to gitlab or a git guru, there are ways you can help! Feel free to make your own branch and try some new things. All of the app is done in R right now, but it has the ability to port functions from Python.

If you find any bugs or problems in the code, please open an [issue](https://github.com/treypujats/COVID19/issues). Feature requests or suggestions for new approaches are also welcome in issues.

For code contributions, please do your best to adhere to the R Tidyverse [style guide](https://style.tidyverse.org/). 

## I'm new to gitlab, how can I help?

New to gitlab and not sure how to help? There are a few good starting points for newcomers:
* Open an [issue](https://github.com/treypujats/COVID19/issues) for a problem you've had
  * For example, if you tried to follow the gettting started section of the documentation but got stuck, you can open an issue describing the problem you had.
* Comment on an issue
  * If you see an issue where you had a similar problem or have advice for the team, you can post a comment
* Update documentation
  * The project documentation is hosted in the Wiki. You can edit the text in these pages without messing around with code or git.
  * If you had a problem and find out how to fix it, addding that information to the Wiki documentation will help other people avoid the same issue.
* Share this repository with other coders in the analytic community
  * Starring the [project](https://github.com/treypujats/COVID19) helps other people find the project and know that it's a useful project to you
  * Sharing the url and access instructions helps to grow out contributor base (note that this is a private project but we're open to analysts across the AFAC and in partner analytic organizations)

## Code

Code contributions are extremely helpful. We want to make it as easy as possible to contribute while also maintaing high quality code. If you want to write new code, please work in an existing branch or make a new branch. Once you've pushed your changes to your branch, submit a merge request to the `master` branch and assign to a reveiwer (or reviewers by pinging multiple people in a comment). Anyone with `developer` or higher permissions can act as a reviewer and accept merge requests. This ensures that everything on `master` has been reviewed at least once. It's much easier for a reviewer to review code that includes decent coverage by unit tests. 

Anyone with `developer` or higher permisisons can push directly to the protected `master` branch. This is fine for README updates or minor housekeeping commits, but please keep meaningful development to branches. 

Please keep all code in the `src` (source) directory, all data in the `data` directory, and all tests in the `tests` directory. The root directory should only include standard files for project admin. 

## Releases and Stability

This project follows [semantic versioning](https://semver.org/) for releases. We have four main levels of stability: 
* **tagged releases >=1.0**: tested code that is ready for CSAF/CSO consumption
* **tagged releases <1.0**: all code has been reviewed but there is a mix of decision-quality and quickturn code -- check with developers of individual files for code quality
* **`master` branch**: code has been reviewed by at least one other person via merge request (good tests make ths easier)
* **other branches**: breaking changes, experimentation, untested code, etc

When you push commmits to any branch, continuous integration (CI) testing will start automatically to let you know if you've broken something. We are not planning on backporting bugfixes to older releases.
