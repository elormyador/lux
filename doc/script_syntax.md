Script syntax
=============

The Lux script syntax is as follows. The **first non whitespace**
character on each line determines how it will be processed. Lines
beginning with a `#` are comments. It is recommended to use
indentation and comments to make the scripts more readable. The **Lux
mode for [Emacs][]** (`lux/emacs/lux-mode.el`) is quite useful as it
simplifies the indentation and makes scripts more easy to read as it
provides different coloring for different types of language
constructs.

Lines beginning with `"""Char` are **multi line quotes**. The quote
ends with the next line beginning with `"""`. The opening quote and
closing quote must be in the same column of the script. The char right
after the first `"""` determines how the multi line quote will be
interpreted. The char is interpreted as a statement just like any of
the single line statement characters (so it can be e.g. `?`, `!`, `~`,
`#`, `-`, etc).

When multi line quotes are indented the leading whitespaces are
stripped from the quoted lines, up to but not including the column
of the double quote character, or to the first non-whitespace
character, whichever occurs first. In this process, a tab character
is treated as 8 space characters.


Interacting with a shell
------------------------

**#String**  
Inline style comment. The `#` must be the first non-whitespace
character on the line.

**!String**  

A `send` operation. Sends a `String` to the `stdin` of the active
shell. Adds a `LF` at the end of the string. `String` may contain
references to variables using `$Var` or `${Var}`.

**~String**  
Same as `!String`, but it does NOT add a `LF` at the end.

**?Regexp**  
An `expect` operation which waits for a string matching a
[regular expression][] to appear on the shell output (either `stdout`
or `stderr`). If no matching output does appear within the timeout
period, the test case is considered as failed. See the `--timeout`
option. If no `Regexp` is given, the output streams (`stdout`,
`stderr`) are flushed. This means any output that has already been
received is discarded. See also the `--flush_timeout` and
`--poll_timeout` configuration parameters about customizing the
`?` behavior.

**??Template**  
Like `?Regexp`, but more restricted as all regular expression
keywords are ignored. Variables are still substituted.

**???Verbatim**  
Like `??Template`, but more restricted as no variables are substituted.
That is the string is matched as is.

*-*
*-Regexp*
Sets a failure condition regular expression [regular expression][]. If
the given `Regexp` ever matches, the test case is considered to have
failed (no further processing of the script will be performed besides
cleanup). If no `Regexp` is given, the old failure condition is reset
(cleared). It is typically used to match error messages.

In the active shell, the `Regexp` is tried on the output preceding
each successful match of expect expressions. The characters up to, but
not including, the (successful) match are tried against the failure
condition. In non-active shells the `RegExp` is tried when the shell
produces new output.

**+**
**+Regexp**  
Sets a success condition regular expression [regular expression][]. If
the given `Regexp` ever matches, the test case is considered a success
(no further processing of the script will be performed besides
cleanup). If no `Regexp` is given, the old success condition is reset
(cleared). It is typically used to match error messages.

In the active shell, the `Regexp` is tried on the output preceding
each successful match of expect expressions. The characters up to, but
not including, the (successful) match are tried against the success
condition. In non-active shells the `RegExp` is tried when the shell
produces new output.

**\[endshell\]**  
An `expect` operation like `?`, but it waits for the `stdout` stream
of the shell to be closed. This means the shell has terminated.

### Meta statements ###

**\[**  
Indicates the beginning of a meta statement. Meta statements are ended
on the same line with a `]`.

**\[shell Name\]** Switches to the named shell, to make it active. In
case there is no such shell started yet, a new shell named `Name` is
created. By default a `/bin/sh` shell (Bourne shell) is started. See
the `--shell_wrapper`, `--shell_cmd` and `--shell_arg` configuration
parameters. The current working directory of a newly started shell is
the same as the dirname of the script file. The **environment
variable** `LUX_SHELLNAME` is set to `Name`. The shell prompt variable
`PS1` is set to `SH-PROMPT:` and the first printout of the prompt is
automatically matched in a expect manner in order to ensure that the
shell is ready for input. The `Name` may contain variables. Shell
names beginning with `lux` and `cleanup` are reserved for internal
purposes. The **environment variable** `LUX_START_REASON` is
initially set to `normal`. See also `[cleanup]`.

**\[cleanup\]**  
is the cleanup marker. If the script is prematurely aborted due to
failure (or due to a matching success pattern) the remaining
statements in the file are normally skipped. But if the there is a
cleanup marker after the failing line (and this is the only
cleanup marker), the lines after the cleanup marker will also be
run in order to enable a controlled cleanup of leftovers. Such as
killing processes, removing files etc. When the cleanup marker is
evaluated, the running shells will be set into a non accessible mode
(**zombie mode**) and their failure and success patterns will be
reset (cleared). A brand new shell (called something beginning with
`cleanup`) will also be started. If the cleanup code causes a failure
the remaining statements (on that level) will be skipped.

Cleanup code in included files will always be run, even if the failure
occurred in the included file. This means that each file can take care
of its own failures. This does also apply on nested include files. On
the topmost level the automatically started shell will be called
`cleanup`, on the next level it is called `cleanup2`, on next level
`cleanup3` etc.

The **environment* variable** `LUX_START_REASON` is set to `normal`
in most shells, but if the cleanup is run due to premature failure or
premature success it will be set to `fail` or `success` respectively.
This can for example be used if you want to save the contents of
error logs, core dumps etc. in case of failure. Textual logs can
simply be written to `stdout` in order to be easily accessible in
the post mortem analyzis. For the purpose of saving binary files
the **environment* variable** `LUX_EXTRA_LOGS` may be used. It
refers to a log directory name unique for each test case. The
directory is however not automatically created. It must be created
by you in the test script if you want to use it. If you have created
the directory, it will turn up as a link in the annotated event log.

**\[include FileName\]**  
Includes and runs the specified script at this point. The `FileName`
is relative to the currently executing script, unless given as an
absolute path. `.luxinc` is preferred as file extension. If the included
file contains a `[cleanup]` marker, the statements after that will be
evaluated in order to clean up unwanted side effects.

**\[macro MacroName ArgName1 ArgName2 ...\]**  
  ...  
**\[endmacro\]**  
Declare a macro. The body of the macro consists of all lines up to
the next `[endmacro]` line. The scope of the arguments are local
within the macro. The arguments can be accessed via their names as
normal variables, such as `$ArgName1`. `[my Var=Value]` can be used to
assign temporary variables that only are valid within the macro. If a
macro switches to another shell it is good practice to switch back to
the calling shell before the end of the macro. One way of doing this
is to get the name of the active shell from the **environment variable**
`LUX_SHELLNAME` with `[my old=$LUX_SHELLNAME]` and later switch back
to the shell with `[shell $old]`. If the macro file contains a
`[cleanup]` marker, the statements after that will be evaluated in order
to clean up unwanted side effects.

**\[invoke MacroName ArgVal1 ArgVal ...\]**  
Invoke a macro. The arguments are separated with spaces. Arguments
can be quoted with the double quote (`"`) character. Double quotes
and backslashes (`\`) must be escaped with a backslash.

**\[loop Var Item1 Item2 ...\]**  
  ...  
**\[endloop\]**  
Declare a loop. The body of the loop consists of all lines up to the
next `[endloop]` line. The commands within the loop are repeated for
each item. For each iteration the loop variable `Var` is set to the
value of the current `Item`. The scope of the loop variable is the
same as a macro variable (defined with my). The `Item list` may
contain variables and these are expanded before the first
iteration. Items in the expanded list are separated with spaces. For
example `[loop colors blue red green]`.  When iterating over a set of
consecutive integers, such as `[loop iter 4 5 6 7]`, this can be
written as a range expression, like `[loop iter 4..7]`. In the logs
the iteration counter is represented as a negative line number. For
example "8:-2:10" would mean line 10 in the second loop iteration
where the loop starts at line 8.

###Variables###

**\[local Var=Value\]**  
assigns a value to a variable that is local to the current
shell. `Value` may contain references to variables using `$Var`,
`${Var}` or `$N`, where `N` is an integer. `$N` refers to a captured
substring from the most recent `expect` operation. Subsequent `send`
operations may refer to this new variable in the same manner as
environment variables. In order to prevent variable substitutions and
keep a `$Var` string literally it must be escaped as `$$Var`. For
example this is needed when "true" environment variables needs to be
read. In order to read a variable like `$?` it must be written as
`$$?`.

**\[global Var=Value\]**  
assigns a value to a global variable. Works like `[local]`, but the
variable setting is propagated to all shells. Global variables
may be set before any shell has been started.

**\[my Var=Value\]**  
assigns a value to a macro variable. Works like `[global]`, but can
only be set and used in a macro. The variable setting is only valid
within the macro that assigns the variable.

###Builtin variables###

    _BS_      - backspace       (ASCII 8)
    _TAB_     - tab             (ASCII 9)
    _LF_      - line feed       (ASCII (10)
    _CR_      - carriage return (ASCII 13)
    _DEL_     - delete          (ASCII 127)
    _CTRL_A_  - control a       (ASCII 1)
    ...
    _CTRL_Z_  - control z       (ASCII 26)
    N         - where N is an integer refering to a captured substring

###Builtin environment variables###

    LUX_SHELLNAME    - name of active Lux shell
    LUX_START_REASON - reason for starting a shell (normal|fail|success)
    PS1              - shell prompt variable set by Lux

###Miscellaneous statements##d##

**\[doc String\]**  
**\[docN String\]**  
A test case slogan that will be displayed in the summary log. It
is also possible to document parts of a test case by specifying a
documentation level `N`. In that case the doc statement should look
like `[docN String]` where `N` is a integer. `doc2` would mean
that the documentation is on level 2. Doc strings can be extracted
from the scripts with the `--mode=doc` command line option.

**\[timeout Seconds\]**  
sets the timeout for the current shell to the given number of
seconds multiplied with a configurated factor. By default the
multiplier is `1000`. For example, by setting the `--multiplier`
parameter to `2000` all timeouts will be doubled. The resulting
timeout value affects how long time `expect` operations will wait
before reporting failure. If time is not specified `[timeout]`, it
is reset to default the timeout specified with the `--timeout`
configuration parameter. The timeout value `infinity` means infinity.

**\[sleep Seconds\]**  
waits given number of seconds before proceeding in the script. No
`multiplier` factor is applied.

**\[progress String\]**  
Displays `String` on the `stdout` stream together with the rest of the
progress info.

**\[config Var=Value\]**  
assigns a value to a [configuration parameter](#config_params). The
assignment takes place during parsing of the script file. The
configuration parameters in **architecture specific files** can be
overridden by **command line options**. For example `[config
timeout=2000]` can be overridden with `--timeout=4000`.  Explicit
`[config Var=Value]` settings in scripts takes however precedence over
settings in architecture specific files and command line options. See
the section *Configuration parameters* about valid configuration
parameters. Some config parameters can have multiple values, such as
`skip` and `require`. See their respective descriptions. See also the
configuration parameter `--config_dir` about the location of the
architecture specific files.
