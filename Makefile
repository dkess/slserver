PROJECT = slserver
PROJECT_DESCRIPTION = A multiplayer server for SixLetters
PROJECT_VERSION = 0.0.1

# Whitespace to be used when creating files from templates.
SP = 2

DEPS = cowboy
dep_cowboy_commit = e291c3bb940a00cc037a88aee981f8b7df1a7f42

include erlang.mk
