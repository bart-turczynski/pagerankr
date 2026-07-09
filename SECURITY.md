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

**Please do not report security vulnerabilities through public GitHub issues.**

Preferred channel — **GitHub private vulnerability reporting**:

1. Go to the repository's **Security** tab.
2. Click **Report a vulnerability**.

This opens a private security advisory visible only to the maintainers.

If you cannot use that channel, email the maintainer at
**bartek@turczynski.pl** instead.

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
