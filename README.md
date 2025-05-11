# bf_interpreter
A very simple BrainFuck-Interpreter written in Zig.

Uses a channel to communicate between two parts of the program:

The file-reader reduces bf-instructions into an instruction-set that combines common patterns. E.g.
`+++` -> `Incr(3)`
`<<<<` -> `MovLeft(4)`
`[+]` -> `Zero`

The instruction-reader reads messages from the channel when seeing parts of the program for the first time but then stores them locally (so that loops for instance don't have to rely on the channel).

This program is definitely not perfect in any sense of the word. It simply served as part of my introduction to the zig programming language, nothing more.
