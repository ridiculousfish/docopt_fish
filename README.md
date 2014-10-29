docopt_fish
===========

Experimental implementation of docopt, targeted for [fish](http://github.com/fish-shell/fish-shell/).

docopt_fish (better name TBD) is a "toolkit" that supports command line argument parsing needs. Given a docopt usage specification, fish_docopt can:

1. Validate arguments (e.g. for syntax highlighting)
2. Suggest arguments (e.g. for tab completion)
3. Expose the commands, options, variables, descriptions, etc. to the client app
4. Parse a set of arguments (argv) against the spec, to extract values

Usage specifications are treated as user-supplied (not developer-supplied) data. This means that rich error messages are generated for invalid usage specs.

docopt_fish is written in C++98. It supports both std::string and std::wstring, and does not throw or catch exceptions.

Philosophical Differences from docopt
=====================

docopt is prescriptive, meaning that it attempts to define an argument-passing style. For example, all long options must use two dashes, and options that accept values may take them with a space or equals separator. "Legacy" styles are not supported.

docopt_fish is descriptive: it attempts to be sufficiently expressive as to describe the usage specifications of arbitrary commands. This means that it has to support all of the argument styles in common use.

Syntactic Differences from docopt
=====================

fish_docopt usage specification syntax has the following differences from docopt:

1. Variable names must be enclosed in brackets: `<foo>`, not `FOO`. Rationale: this avoids ambiguities for the case of abutting option-value names (think `-DMACRO` in gcc), and docopt [plans the same change](https://github.com/docopt/docopt/issues/50).
2. Single-dash long names are supported. Rationale: we need to express the full usage spec of external commands, and single-dash long names are common.
3. docopt has relaxed separator semantics: `--foo <var>` may be written in argv as either `--foo var` or `--foo=var`. fish_docopt has strict semantics: argv must use the separator style specified in the usage spec. Rationale: most commands require one or the other style, and the usage spec must be able to express that.
4. Unambiguous-prefix abbreviation is not supported (e.g. `--ver` for `--version`). Rationale: most external commands do not support this, and docopt is [likely to remove it](https://github.com/docopt/docopt/issues/104#issuecomment-17539841).
5. Positional arguments after options are treated as values. For example: `prog [-m <msg>]` is assumed to be an option that takes a value `<msg>`. docopt instead treats this as a bare option, followed by a positional argument. Rationale: this seems more natural and more likely to be what the user expects.
6. Long usage specs may be broken across lines, by increasing the indent on subsequent lines. For example:

          	   Usage: prog [--some-long-option <some_value>]
          	               [--another-long-option <another_value>]
          		      prog --help

	Tabstops are currently fixed at 4.
	Rationale: Long usage specs are common in man pages.
7. 'Conditions' metadata. A section "Conditions:" can be used to describe what values are allowed for variables. For example, in `cd <dir>`, there would be a condition on `<dir>` that expresses that only directory paths are allowed. docopt_fish does not itself use the conditions, but does parse them and expose them to the app. Rationale: fish needs this.
