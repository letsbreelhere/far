# far

`far` is a TUI tool for project-wide find and replace. It's `.gitignore`-aware
and uses PCRE-style regexes.

![image](https://user-images.githubusercontent.com/823979/210457145-ba4bbedd-993f-44cc-b7a2-f8e538cf67ff.png)

## Usage

To start a find-and-replace from scratch in a directory just use `far` in the
root directory (or `far PATH` from elsewhere). To replace a set of paths e.g.
from a previous `ls` or `rg` command you can just pipe:

```bash
rg -l foobar | far
```

## Commands

| Key | Action |
| --- | ------ |
| `Tab` | Next input |
| `Shift-Tab` | Previous input |
| `Space` | Toggle option |
| `Enter` | Start replacing |
