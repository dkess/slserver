PROJECT = slserver
PROJECT_DESCRIPTION = A multiplayer server for SixLetters
PROJECT_VERSION = 0.0.1

# Whitespace to be used when creating files from templates.
SP = 2

DEPS = cowboy
dep_cowboy_commit = 1.0.4

RELX_OPTS = -d true

include erlang.mk
