# dta

dta stands for "data to api".

## Summary

This project web scrapes mountain bike product info, checks for what changed, and then sends an email. 

The code currently only works for [Chromag Bikes](https://chromagbikes.com/), but more bike sites are planned in the future.

This is an exploratory project, meant for the purpose of learning how to build a stable in Erlang. 

## Details

This project does the following:
- web scrapes using [mochiweb](https://github.com/mochi/mochiweb)
- converts product info from the web scraper to Erlang using [jsx](https://github.com/talentdeficit/jsx)
- product info is then compared to the previous product info to see what changed
- the diff is sent using [gen_smtp](https://github.com/gen-smtp/gen_smtp)

# Usage

The project is ran via an escript.

## Before using

These 3 environment variables must be set and are used for the email functionality:

- DTA_FROM_EMAIL_USERNAME - email address to send from
- DTA_FROM_EMAIL_PW - password for email address t send from
- DTA_TO_EMAIL_USERNAME - email address to send to

## Build

To build project:
```
$ rebar3 release
```

To build escript:
```
$ rebar3 escriptize
```

## Run

To run the escript end-to-end:

```
$ _build/default/bin/scrape
```

## Testing

Run unit tests:
```
$ rebar3 eunit
```

## Example Email

Here's an example email of what to expect. Only bikes that changed quantity from day to day will show.

![Imgur](https://i.imgur.com/iOXMpsg.png)

# License

Apache 2.0
