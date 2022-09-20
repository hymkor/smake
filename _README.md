SMake (Make by S-expression)
============================

```lisp
<%
(let ((line nil))
    (with-open-input-file (fd "Makefile.lsp")
        (while (setq line (read-line fd nil nil))
            (format (standard-output) "~a~%" line)
        )
    )
)
%>
```

## The functions available in Makefile.lsp

### (make ('(TARGET [SOURCE...]) COMMANDS...)...)

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

### (x COMMANDNAME ARGS...)

Execute the external COMMAND. If it fails, stop.

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (1&gt; FILENAME COMMANDS...)

Open FILENAME for writing and redirect Standard-output to it while COMMANDS are executed.

### (1&gt;&gt; FILENAME COMMANDS...)

Open FILENAME for appending and redirect Standard-output to it while COMMANDS are executed.

### (let),(format) and so on

They are compatible functions with ISLisp.

## The built-in variables

- $@ - the target filename
- $&lt; - the first source filename

## The macros in the STRING

(rule),(x),(touch),(1&gt;) and (1&gt;&gt;) expands these in their parameters.

- "$(x)" to the value of the symbol x
- "$&lt;" is same as "$($&lt;)"
- "$@" is same as "$($@)"
