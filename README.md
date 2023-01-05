# far

`far` is a TUI tool for project-wide find and replace. It's `.gitignore`-aware
and uses PCRE-style regexes.

![image](https://user-images.githubusercontent.com/823979/210457145-ba4bbedd-993f-44cc-b7a2-f8e538cf67ff.png)

## Why?

I use vim as my editor, but it has awful support for this sort of thing. Some plugins get close, but in my experience they still require a ton of keystrokes to set up what should be a fast and easily-checkable process.

## Installation

With [stack](https://docs.haskellstack.org/en/stable/) installed, just do `stack install` in this directory.

## Usage

To start a find-and-replace from scratch in a directory just use `far` in the
root directory (or `far PATH` from elsewhere). To replace a set of paths e.g.
from a previous `ls` or `rg` command you can just pipe:

```bash
rg -l foobar | far
```

## Options

`far [FILES] [-f|--from FROM] [-t|--to TO] [-i|--case-insensitive] [-e|--extended]`

| Short | Long | Description | Default |
| ----- | ---- | ----------- | ------- |
| `-i` | `--case-insensitive` | Case-insensitive search | false |
| `-e` | `--extended` | Use [extended](https://www.pcre.org/original/doc/html/pcrepattern.html#atomicgroup) PCRE regexes | false |
| `-f` | `--from` | Initial from-regex | Empty |
| `-t` | `--to` | Initial to-regex (use `\1`, `\2`, etc. for capture groups) | Empty |

## Commands

| Key | Action |
| --- | ------ |
| `Tab` | Next input |
| `Shift-Tab` | Previous input |
| `Space` | Toggle option |
| `Enter` | Start replacing |
