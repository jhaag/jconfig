# packages/

Related `jaspah` workspace members. The workspace root (repo root) is the primary
`jaspah` package; each subdirectory here is an additional, independently
installable package (`pyproject.toml` + `src/<import_name>/`).

## Add a member

```sh
mkdir -p packages/<name>/src/<import_name>
# packages/<name>/pyproject.toml: [project] name=<name>, uv_build backend,
#   [project.scripts] if it ships a CLI.
uv sync --all-packages --inexact      # installs it editable into ~/.venv/dev
```

`members = ["packages/*"]` in the root `pyproject.toml` picks it up automatically.
Another uv workspace can consume the whole repo (`{ path = "…/jaspah" }`) or a single
member (`{ path = "…/jaspah/packages/<name>" }`) as an editable path/git source —
never as a workspace member (uv forbids nested workspaces).
