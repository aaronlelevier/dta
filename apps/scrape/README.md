# scrape

Here's some additional notes no cron and escript's from what's in the project README.

### Run with Cron

Initially source the `env-file` so the required environment variables are present, then run escript with params

    $ 0 5 * * * source $HOME/<env-file> && /Users/aaron/Documents/erlang/dta/_build/default/bin/scrape

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

Crontab reference:
- https://crontab.guru/

### Cron

Scenic walk through cron
- https://kvz.io/schedule-tasks-on-linux-using-crontab.html

How to run for multiple days
- https://stackoverflow.com/a/1382638/1913888
