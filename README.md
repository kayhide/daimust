# daimust

`daimust` is used to update your timesheet on Daim through the command line.

## Install

You can build and install `daimust` with either `stack` or `nix-build`.

## Quickstart

You must have your username and password set in environment variables for
`daimust` to read.  You also need to set the Daim URL:

```console
$ export DAIM_URL="https://pay.kizunajapan.co.jp/itr/n/MainLogin.php"
$ export DAIM_USERNAME="123457890"
$ export DAIM_PASSWORD="foobarbaz"
```

You can use `daimust` to list your time for the current month:

```console
$ daimust list
```

You can also `daimust` to add time entries:

```console
$ daimust put 4 1000 1800
```

This adds a time entry for the 4th of the month from 10:00 am to 6:00 pm (18:00).

You can change the current time frame `daimust` will use.  For instance, to put
the focus on the previous time period, use the next command.  This is useful to
use if you always fill out your time for the previous month (instead of filling
it out everyday).

```console
$ daimust focus prev
```
