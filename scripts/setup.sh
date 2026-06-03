#!/bin/bash

# This script can run in a standard Jepsen environment to install optional dependencies.

# Install UV
curl -LsSf https://astral.sh/uv/install.sh | sh &&
source $HOME/.local/bin/en
