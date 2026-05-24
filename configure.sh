#!/usr/bin/env bash
# Thin, self-repairing entry shim. See .toolchain/AGENTS.md.

set -euo pipefail

JASPAH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export JASPAH_ROOT

"$JASPAH_ROOT/.toolchain/bootstrap.sh"
"$JASPAH_ROOT/.toolchain/sync.sh"
