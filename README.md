# `hours`

Super-simple hour-tracking CLI tool.

Demo:

```
$ nix-build
/nix/store/mqk01igqjqnnffjc2bxzb81fk8iiw65j-hours
$ alias hours="./result/bin/hours"
$ hours new-topic --name project-A

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │           │  0h 0m   │  0h 0m

$ hours log -H3 -M15 --topic project-A --note 'Did 3h15m of billable work on Project A'

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │           │  3h 15m  │  3h 15m

$ hours start-work --topic project-A --note 'Starting work for some task on Project A'

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │  0h 0m    │  3h 15m  │  3h 15m

$ # some time later ...
$ hours status

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │  1h 30m   │  3h 15m  │  3h 15m

$ hours stop-work -t project-A

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │           │  4h 45m  │  4h 45m

$ hours billed -t project-A

    Topic      │  Current  │  Logged  │  Unbilled
  ─────────────┼───────────┼──────────┼────────────
    project-A  │           │  4h 45m  │  0h 0m

```
