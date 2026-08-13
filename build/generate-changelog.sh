#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <commit>" >&2
  exit 1
}

[[ $# -eq 1 ]] || usage

commit="$(git rev-parse --verify "$1^{commit}")"

resolve_repo() {
  if [[ -n "${GITHUB_REPOSITORY:-}" ]]; then
    echo "$GITHUB_REPOSITORY"
    return
  fi

  local url
  url="$(git remote get-url origin)"
  url="${url%.git}"
  case "$url" in
  git@github.com:*)
    echo "${url#git@github.com:}"
    ;;
  https://github.com/* | http://github.com/* | ssh://git@github.com/*)
    echo "${url#*github.com/}"
    ;;
  *)
    echo "Unable to determine github repository from remote url: $url" >&2
    exit 1
    ;;
  esac
}

repo="$(resolve_repo)"

# Find the closest tagged ancestor, excluding any tag pointing at the given
# commit itself.
previous_tag="$(git describe --tags --abbrev=0 "$commit^" 2>/dev/null || true)"

if [[ -n "$previous_tag" ]]; then
  range="$previous_tag..$commit"
else
  range="$commit"
fi

resolve_username() {
  local sha="$1"
  local result
  if result="$(gh api "repos/$repo/commits/$sha" --jq '.author.login // empty' 2>/dev/null)"; then
    echo "$result"
  fi
}

echo "## Changelog"
echo

for sha in $(git rev-list --reverse --no-merges "$range"); do
  subject="$(git log -1 --format=%s "$sha")"
  body="$(git log -1 --format=%b "$sha")"
  username="$(resolve_username "$sha")"

  entry="- **$subject**"
  if [[ -n "$username" ]]; then
    entry+=" by @$username"
  fi
  entry+=" in $(git rev-parse --short "$sha")"
  echo "$entry"

  if [[ -n "$body" ]]; then
    while IFS= read -r line; do
      if [[ -n "$line" ]]; then
        echo "  > $line"
      else
        echo "  >"
      fi
    done <<<"$body"
  fi
done

if [[ -n "$previous_tag" ]]; then
  end_ref="$(git describe --tags --exact-match "$commit" 2>/dev/null || git rev-parse --short "$commit")"
  echo
  echo "For the full changelog visit https://github.com/$repo/compare/$previous_tag...$end_ref"
fi
