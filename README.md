# haskell-markdown-parser

![license](https://img.shields.io/github/license/MurakamiKennzo/haskell-markdown-parser)
![build](https://img.shields.io/badge/build-0.1.0.0--test-green)

## shell

Here is a shell build on Mac OSX, you can use like this:

```shell
$ ./shell/md-parser input-markdown-filepath output-html-filepath
```

of course, you can use stack like this if you system is not a Mac OSX:

```shell
$ stack setuo
$ stack build
$ stack exec haskell-markdown-parser-exe input-markdown-filepath output-html-filepath
```
