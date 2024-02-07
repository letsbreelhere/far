# far

`far` is a TUI tool for project-wide find and replace. It's `.gitignore`-aware
and uses PCRE-style regexes.

![image](https://user-images.githubusercontent.com/823979/210457145-ba4bbedd-993f-44cc-b7a2-f8e538cf67ff.png)

## Why?

I use vim as my editor, but it has awful support for this sort of thing. Some plugins get close, but in my experience they still require a ton of keystrokes to set up what should be a fast and easily-checkable process.

## Installation

With [stack](https://docs.haskellstack.org/en/stable/) installed, just do `stack install` in this directory.

The `far` executable will be installed into `~/.local/bin`.
If it is not in your PATH environment variable yet, you need to append it to PATH.

<details>
<summary>Troubleshooting on macOS with MacPorts</summary>

On macOS systems with MacPorts, if build failed due to linking issues with libiconv,
it is probably because there is [a conflicting version of libiconv installed from MacPorts][57821].
As a workaround, you can try to deactivate libiconv installed from MacPorts,
then run `stack install` again.

[57821]: https://trac.macports.org/ticket/57821

```sh
; stack install
# uninterested output omitted
Building executable 'far' for far-0.1.0.0..

[3 of 3] Linking .stack-work/dist/x86_64-osx/ghc-9.4.7/build/far/far
ld: Undefined symbols:
  _iconv, referenced from:
      _hs_iconv in libHSbase-4.17.2.0.a[6](iconv.o)
  _iconv_close, referenced from:
      _hs_iconv_close in libHSbase-4.17.2.0.a[6](iconv.o)
  _iconv_open, referenced from:
      _hs_iconv_open in libHSbase-4.17.2.0.a[6](iconv.o)

Stack failed to execute the build plan.
; sudo port deactivate libiconv
Note: It is not recommended to uninstall/deactivate a port that has dependents as it breaks the dependents.
The following ports will break:
# port list omitted
Continue? [y/N]: y
Warning: Deactivate forced.  Proceeding despite dependencies.
--->  Deactivating libiconv @1.17_0
--->  Cleaning libiconv
; stack install
# success
; far -h
# display help info
; sudo port activate libiconv
# Reactivate libiconv from MacPorts so other ports can work again. 
```
</details>

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
