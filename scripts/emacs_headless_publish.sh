#!/bin/bash
# Headless Emacs script to publish Org files

set -e

# Change to repo root if needed
cd "$(dirname "$0")/.."

emacs -Q --script org_publish.el --eval "(progn (require 'org) (org-publish-project \"themkat\" t))"
