# scrape

An escript to run via cron that runs the webscraper in the `web` app

## Build

Run this command from the project home
    
    $ cd ../dta
    $ rebar3 escriptize
    
Any escript specific rebar3 config needs to live in the project home dir `rebar.config`

If there's a dependency that the escript needs, it should be added in the applications section of `<app>.app.src` file.

## Run

    $ _build/default/bin/scrape MetaAm29 https://www.commencalusa.com/meta-am-29-c102x3872635
    
## References
    
Build an escript example with rebar3:
- https://www.rebar3.org/discuss/5d29172873e1ed00447cb362

Config:
- https://www.rebar3.org/docs/configuration#escript

Commands:
- https://www.rebar3.org/docs/commands#escriptize

## Cron workflow

Edit crontab

    $ crontab -e

Check crontab

    $ crontab -l

Example crontab
   
    $ * * * * * /Users/aaron/Documents/erlang/dta/_build/default/bin/scrape MetaAm29 https://www.commencalusa.com/meta-am-29-c102x3872635

Check cronjob output

    $ tail /var/mail/aaron
