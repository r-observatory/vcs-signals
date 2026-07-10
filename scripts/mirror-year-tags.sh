#!/usr/bin/env bash
# Mirror each published per-year signal-series shard to a stable per-year
# release tag, so a completed year is archived independently of the rolling
# "current" release (whose assets are re-clobbered every run).
#
# For each year in the manifest it creates the year's prerelease the first time
# that shard appears, and re-uploads the shard only when that year changed this
# run. The rolling "current" release is left untouched and remains --latest.
#
# Usage: scripts/mirror-year-tags.sh [out_dir]
#   env: GH_TOKEN (contents:write), GITHUB_REPOSITORY (owner/repo)
set -euo pipefail

OUT="${1:-out}"
REPO="${GITHUB_REPOSITORY:-r-observatory/vcs-signals}"
MANIFEST="$OUT/manifest.json"

if [ ! -f "$MANIFEST" ]; then
  echo "mirror-year-tags: no manifest at $MANIFEST; nothing to mirror"
  exit 0
fi

mapfile -t years < <(jq -r '.summary.years[]?' "$MANIFEST")
changed="$(jq -r '.changed_shards[]?' "$MANIFEST")"

for y in "${years[@]}"; do
  shard="vcs-signals-$y.db"
  if [ ! -f "$OUT/$shard" ]; then
    echo "  $y: shard not present in $OUT, skipping"
    continue
  fi
  if ! gh release view "$y" --repo "$REPO" >/dev/null 2>&1; then
    echo "  $y: creating per-year archive release"
    gh release create "$y" --repo "$REPO" --prerelease \
      --title "vcs-signals $y" \
      --notes "Per-year archive of the $y signal-series shard (\`$shard\`). The rolling \`current\` release always holds the latest of every year; this tag is a stable snapshot of $y." \
      || true
    gh release upload "$y" "$OUT/$shard" --repo "$REPO" --clobber
  elif grep -qxF "$shard" <<<"$changed"; then
    echo "  $y: shard changed this run, updating archive"
    gh release upload "$y" "$OUT/$shard" --repo "$REPO" --clobber
  else
    echo "  $y: unchanged"
  fi
done
