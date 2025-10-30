#!/bin/bash
# Headless Emacs script to publish Org files

set -e

# Change to repo root
cd "$(dirname "$0")/.."

# Ensure Emacs can find packages
emacs -Q --script org_publish.el --eval "(progn (require 'org) (org-publish-project \"all\" t))"
