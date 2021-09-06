# `hours`

Super-simple hour-tracking CLI tool.

Demo:

```
λ ~/me/dev/hours ← nix-build
/nix/store/p4gzr2v1lbr8sbhvvivi50fx318lj9p7-hours
λ ~/me/dev/hours ← alias hours="./result/bin/hours"
λ ~/me/dev/hours ← hours topic new --name project-A
λ ~/me/dev/hours ← hours topic log --topic project-A --amount '3h 15m'
λ ~/me/dev/hours ← hours show

        Topic  │  Logged  │  Unbilled
  ─────────────┼──────────┼────────────
    project-A  │  3h 15m  │    3h 15m

λ ~/me/dev/hours ← hours session start -t project-A --comment 'starting work on project a'
λ ~/me/dev/hours ← # 1h 15m later
λ ~/me/dev/hours ← hours show

         Topic  │  Logged  │  Unbilled
  ──────────────┼──────────┼────────────
    project-A*  │  3h 15m  │    3h 15m

  * Active session on topic project-A (for 1h 15m)

λ ~/me/dev/hours ← hours session stop
λ ~/me/dev/hours ← hours show

        Topic  │  Logged  │  Unbilled
  ─────────────┼──────────┼────────────
    project-A  │  4h 30m  │    4h 30m

λ ~/me/dev/hours ← hours topic flush -t project-A
λ ~/me/dev/hours ← hours show

        Topic  │  Logged  │  Unbilled
  ─────────────┼──────────┼────────────
    project-A  │  4h 30m  │     0h 0m

```
