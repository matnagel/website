#!/bin/sh
set -e
ln -s /stack/.stack-work .
stack test
