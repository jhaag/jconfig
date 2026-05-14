#!/usr/bin/env bash
# Thin entry shim. See .toolchain/AGENTS.md.

JCONFIG_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export JCONFIG_ROOT

"$JCONFIG_ROOT/.toolchain/bootstrap.sh"
"$JCONFIG_ROOT/.toolchain/sync.sh"
