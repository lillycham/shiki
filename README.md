# shiki 式

Small probes that report back. shiki runs a list of probes on timers and serves their last results as JSON, for a dashboard such as [Glance](https://github.com/glanceapp/glance).

A request never runs a probe. It reads the results that the timers left in memory, so a slow probe never makes a page wait. Commands come only from the config, never from a request.

## Use

```sh
shiki check config.json   # run every probe once and print the results
shiki serve config.json   # run them on timers and serve JSON
```

Endpoints:

- `GET /probes`: every group and probe, with the worst state on top.
- `GET /probes/GROUP`: one group.
- `GET /health`: `ok`.

A result looks like this:

```json
{"name": "backup", "group": "notes", "state": "ok", "summary": "idle · exit 0 · 3 runs", "detail": null, "checked": "2026-10-14T08:00:00Z"}
```

The states, from best to worst: `ok`, `unknown` (not checked yet, or not sure), `warn`, `fail`. A group has the worst state of its probes.

## Config

```json
{
  "host": "127.0.0.1",
  "port": 8092,
  "probes": [
    {"name": "backup", "group": "notes", "kind": "launchd", "label": "notes-backup"}
  ]
}
```

Every probe has a `name` (unique), a `kind`, and optionally a `group` (default `default`), `every` (seconds between runs, default 60) and `timeout` (seconds, default 10). Durations are in seconds.

| kind | fields | ok when |
|---|---|---|
| `launchd` | `label`, `running` (default false) | the last exit code is 0 (or it never exited), and it runs if `running` |
| `http` | `url`, `expect` (default 200) | the response has the expected status |
| `file-age` | `path`, `warn-after`, `fail-after` | the file, or the newest entry of the directory, is younger than `warn-after` |
| `dir-count` | `path`, `warn-above`, `fail-above` | the directory has at most `warn-above` entries (dot files left out) |
| `git` | `path`, `warn-after`, `upstream` (default true) | the last commit is younger than `warn-after`, and nothing is unpushed |
| `command` | `argv`, `output`, and for `json-array`: `fail-where`, `warn-where`, `count-by` | see below |

A `command` with `"output": "exit"` (the default) is ok when it exits 0, and shows its last line. With `"output": "json-array"`, its output must be a JSON array: an element that has all the fields of `fail-where` fails the probe, and one with all the fields of `warn-where` warns. `count-by` names a field to count the elements by in the summary. For example:

```json
{"name": "check", "kind": "command", "argv": ["/path/to/tool", "check", "--json"],
 "output": "json-array", "warn-where": {"severity": "problem"}, "count-by": "severity"}
```

Give commands full paths: a launchd agent has a short `PATH`.

## Nix

The flake has the package and a home-manager module (`homeManagerModules.default`), which writes the config from `services.shiki.probes` and runs a launchd agent on macOS or a systemd user service on Linux.

## Development

```sh
nix develop
cabal test
cabal2nix . > shiki.nix   # after a change to shiki.cabal
nix flake check
```
