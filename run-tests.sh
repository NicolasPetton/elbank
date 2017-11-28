#!/bin/sh -e
cask exec ecukes "$@"
cask exec buttercup -l features/support/test-helper.el -L . "$@"
