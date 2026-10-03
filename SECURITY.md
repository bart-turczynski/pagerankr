# Security Policy

## Supported versions

`pagerankr` is distributed through CRAN. Security fixes are made against the
latest released version; please upgrade to the most recent release before
reporting.

| Version              | Supported          |
| -------------------- | ------------------ |
| Latest CRAN release  | :white_check_mark: |
| Older releases       | :x:                |

## Reporting a vulnerability

**Please do not report security vulnerabilities through public issues.**

Preferred channel — **email the maintainer at bartek@turczynski.pl.**

Alternatively, open a **confidential issue** on the GitLab project:

1. Go to [Issues](https://gitlab.com/bart-turczynski/pagerankr/-/work_items) and click
   **New issue**.
2. Tick **This issue is confidential** before submitting.

A confidential issue is visible only to you, its assignees and the project
members whose role lets them see confidential issues.

Email is listed first deliberately: it works whether or not you have a GitLab
account, and it is the channel the maintainer monitors.

Do not include secrets, credentials, tokens, or private customer data in a
report, an issue, a merge request or a log.

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
