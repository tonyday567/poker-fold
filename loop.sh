#!/bin/bash
# Watch candidate.strat and re-run coordinate ascent whenever it changes.
# The optimized result is written to candidate-optimized.strat.

set -e

FILE="candidate.strat"
LAST=""

echo "Watching $FILE for changes. Edit and save the file to trigger an optimization run."
echo "Local optimum is preserved in local-optima.strat."
echo "Press Ctrl-C to stop."
echo

while true; do
  if [ ! -f "$FILE" ]; then
    echo "Waiting for $FILE to appear..."
    sleep 2
    continue
  fi

  MD5=$(md5 -q "$FILE")
  if [ "$MD5" != "$LAST" ]; then
    LAST="$MD5"
    echo "=== $(date) : change detected in $FILE ==="
    cabal run poker-fold:exe:poker-fold-opt3bb -- --candidate "$FILE" --optimize --output candidate-optimized.strat
    echo "=== optimization complete ==="
    echo
  fi
  sleep 2
done
