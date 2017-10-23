# Keymap generator for agda-mode

Parses Quail files and generates a keymap for [agda-mode on Atom](https://github.com/banacorn/agda-mode)

## Input

The mappings are currently drawn from two sources (in order):

1. [agda-input.el](https://raw.githubusercontent.com/agda/agda/master/src/data/emacs-mode/agda-input.el): The Agda input method

2. [latin-ltx.el](https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/leim/quail/latin-ltx.el): Quail package for TeX-style input

## Output

1. [keymap.ts](https://github.com/banacorn/keymap/blob/master/assets/keymap.ts): The generated keymap trie
