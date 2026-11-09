# worktime.el

Tracks working days and breaks and shows the remaining time and the end
of the day in the mode line.

## Installation

Requires Emacs 29.1 or later. To install with `use-package` (Emacs 30
or later):

```elisp
(use-package worktime
  :vc (:url "https://github.com/fmarl/worktime.el")
  :config (worktime-mode))
```

## Usage

| Command              | Effect                          |
|----------------------|---------------------------------|
| `worktime-start`     | Start the working day           |
| `worktime-pause`     | Start a break                   |
| `worktime-resume`    | End the running break           |
| `worktime-add-break` | Record a past break             |
| `worktime-stop`      | End the working day             |

If the previous day is still open, `worktime-start` asks for its end
first. It warns if the rest since the previous day is shorter than
`worktime-min-rest-minutes`.

See `M-x customize-group RET worktime` for all options.

## Breaks

The working time per day is `worktime-hours-per-week` divided by
`worktime-days-per-week`. `worktime-required-break-function` returns
the break required for the time worked so far. The default follows
German law (§ 4 ArbZG): no break up to 6 hours, 30 minutes up to 9
hours and 45 minutes beyond that.

Recorded breaks of at least `worktime-min-break-minutes` count toward
the required break. Whatever is missing is added to the end of the
day. For example, a 24-minute break on an 8-hour day adds 6 minutes.

For other rules, for example in another country, set the variable to
your own function of the minutes worked. A fixed 30-minute break:

```elisp
(setq worktime-required-break-function (lambda (_minutes) 30))
```

## Development

```sh
guix shell -m manifest.scm
eldev compile
eldev test
eldev lint
```

## License

GPL-3.0-or-later
