# far

`far` is a TUI tool for project-wide find and replace. It's `.gitignore`-aware
and uses PCRE-style regexes.

![image](https://user-images.githubusercontent.com/823979/210441826-d26c25bd-030c-402c-9528-89fb477d3584.png)

## Usage

To start a find-and-replace from scratch in a directory just use `far` in the
root directory (or `far PATH` from elsewhere). To replace a set of paths e.g.
from a previous `ls` or `rg` command you can just pipe:

```bash
rg -l foobar | far
```

## Navigation

Use `<Tab>` and `<Shift-Tab>` to navigate between the file list and from/to
regexes. Hit `<Enter>` to start replacing.
