# scrape

An escript to run via cron that runs the webscraper in the `web` app

## Build

Run this command from the project home
    
    $ cd ../dta
    $ rebar3 escriptize
    
Any escript specific rebar3 config needs to live in the project home dir `rebar.config`

If there's a dependency that the escript needs, it should be added in the applications section of `<app>.app.src` file.

## Run

Syntax is:

    $ _build/default/bin/scrape <bike> <url>

Meta AM 29:

    $ _build/default/bin/scrape MetaAm29 https://www.commencalusa.com/meta-am-29-c102x3872635
    
Or Clash for example:

    $ _build/default/bin/scrape Clash https://www.commencalusa.com/clash-c102x3872636

### Run with Cron

Initially source the `env-file` so the required environment variables are present, then run escript with params

    $ * * * * * source $HOME/<env-file> && /Users/aaron/Documents/erlang/dta/_build/default/bin/scrape Clash https://www.commencalusa.com/clash-c102x3872636

#### Cron workflow

Edit crontab

    $ crontab -e

Check crontab

    $ crontab -l

Check cronjob output

    $ tail /var/mail/aaron

## References

### Escript

Build an escript example with rebar3:
- https://www.rebar3.org/discuss/5d29172873e1ed00447cb362

Config:
- https://www.rebar3.org/docs/configuration#escript

Commands:
- https://www.rebar3.org/docs/commands#escriptize

### Cron

Scenic walk through cron
- https://kvz.io/schedule-tasks-on-linux-using-crontab.html

How to run for multiple days
- https://stackoverflow.com/a/1382638/1913888

