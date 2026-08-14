# Security Policy

## Supported versions

`pagerankr` is experimental and is not yet distributed through CRAN. Security
fixes are made against the latest development version on `main`; please
upgrade to the most recent commit or release before reporting.

| Version                    | Supported          |
| -------------------------- | ------------------ |
| Latest `main` / release    | :white_check_mark: |
| Older development versions | :x:                |

## Reporting a vulnerability

**Please do not report security vulnerabilities through public issues.**

Preferred channel — **email the maintainer at bartek@turczynski.pl**. Please
put `pagerankr security` in the subject line, and include the package version
and enough detail to reproduce.

If you would rather report through the tracker, open a
[confidential issue](https://gitlab.com/bart-turczynski/pagerankr/-/issues/new)
and tick **This issue is confidential**, which restricts it to project members.

(Before the move to GitLab this pointed at GitHub private vulnerability
reporting. GitLab has no equivalent feature — confidential issues are the
closest thing — so email is now the primary channel.)

Do not include secrets, credentials, tokens, or private customer data in
issues, pull requests, logs, or scratch files.

## What to expect

- We aim to acknowledge a report within **7 days**.
- We will investigate, work on a fix, and coordinate disclosure with you.
- We are happy to credit reporters in the release notes unless you prefer to
  remain anonymous.

## Scope

`pagerankr` is an R package for modeling link graphs and PageRank-style scores
from crawl and analytics data. It handles untrusted URL, redirect, link-graph,
and imported crawl-data inputs. The package does not provide authentication or
credential storage; security reports should focus on unsafe parsing, resource
exhaustion, unexpected code execution, or information disclosure caused by
those inputs.
