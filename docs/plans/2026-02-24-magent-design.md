# Magent Design

A magit-style Emacs porcelain for orchestrating AI agent sessions across repositories and worktrees.

## Motivation

The current workflow for managing multiple agent sessions is terminal tabs: one tab per worktree, each running a claude session, round-robining through them to check status, provide input, and review changes. This works but doesn't scale — you lose track of which agents need you, you can't search across sessions, and every interaction requires context-switching to the right tab.

Magent replaces terminal tabs with a single magit-section buffer that shows all active work across all repos, with state shown via faces and actions available via single-key bindings.

## Object Model

### Work

The single core object. One Work corresponds to one worktree with one agent session.

```
Work
  dir:        path        ; worktree path
  repo:       path        ; derived via git rev-parse --show-toplevel
  branch:     string      ; derived from git
  purpose:    string      ; user-provided description of what the agent is doing
  state:      symbol      ; working | needs-input | idle | done
  session-id: string|nil  ; backend session identifier, used for resume
  pr:         string|nil  ; PR URL if one exists
  files:      [path]      ; files the agent is currently touching
  recent:     [string]    ; last few actions/tool calls
```

**State derivation:**
- `working` — agent subprocess is running and emitting events
- `needs-input` — agent process exited normally or is waiting for user input
- `idle` — no active process, but session-id exists (resumable)
- `done` — user-marked complete, or PR merged

### Grouping

Works are grouped by repo in the buffer. Repos are derived from worktree paths, not configured. Per-repo backlog TODO counts are discovered by scanning org files in the repo root.

### Persistence

Works are persisted in `~/.magent/state.el`. On refresh, persisted state is reconciled with live backend state (running processes, git branch info).

## Architecture

```
magent.el          ; entry point, autoloads, defcustom
magent-core.el     ; Work cl-defstruct, state management, persistence
magent-ui.el       ; magit-section rendering, faces, keybindings
magent-backend.el  ; cl-defgeneric protocol, default implementation
```

### Backend Protocol

Five generic functions define the interface between magent and the agent tool:

```elisp
(cl-defgeneric magent-backend-list ()
  "Return list of active sessions as alists with keys:
   session-id, dir, state, files, recent.")

(cl-defgeneric magent-backend-launch (dir prompt)
  "Start a new agent session in DIR with PROMPT.
   Return session-id string.")

(cl-defgeneric magent-backend-resume (session-id)
  "Resume an idle session. Return process or session handle.")

(cl-defgeneric magent-backend-send (session-id input)
  "Send INPUT string to a running session.")

(cl-defgeneric magent-backend-kill (session-id)
  "Kill a running session.")
```

### Default Backend

The default backend runs the agent CLI directly as an Emacs subprocess:

```elisp
(defcustom magent-agent-command "claude"
  "The agent CLI command.")

(defcustom magent-agent-args
  '("--dangerously-skip-permissions"
    "--output-format" "stream-json"
    "--verbose")
  "Default arguments for the agent CLI.")
```

Launches via `start-process`, parses JSONL from stdout via process filter. Extracts tool calls, file paths, and completion status from the event stream.

Alternative backends (claude-squad, ccmanager, workmux) implement the same five functions, translating their native JSON APIs to magent's protocol.

### Refresh Strategy

Interrupt-driven, not polling. Agent subprocesses emit JSONL events via process filters. Each event triggers a Work struct update and selective buffer re-render.

For external backends that manage sessions outside Emacs, the backend must support a watch/subscribe mechanism. Polling at a configurable interval is the degraded fallback.

## Buffer Layout

```
Magent

~/repos/mind-cluster  5 TODOs
  feat/multi-ring-stress   [working]  Bash(deploy.sh)
    src/stress/deploy.sh  src/stress/monitor.sh
  feat/voltage-monitor     [needs input]  waiting 5m
    src/voltage/detect.ts  src/voltage/alert.ts
  fix/pmic-reset           [done]  PR #47 merged

~/repos/app  3 TODOs
  feat/oauth-refresh       [working]  Edit(token.ts)
    src/auth/token.ts  src/auth/refresh.ts
  feat/jwt-validation      [idle]  resumable, 2h ago
```

Built with `magit-section`. Each repo is a top-level section. Each Work is a subsection. Each Work's files are nested subsections. All levels support Tab/S-Tab folding.

## Faces

| Face | State | Appearance |
|------|-------|------------|
| `magent-face-working` | working | default weight, active color |
| `magent-face-needs-input` | needs-input | bold, warning color (yellow/orange) |
| `magent-face-idle` | idle | dim, grey |
| `magent-face-done` | done | green |

## Keybindings

Aligned with magit conventions. Same key means the same concept where possible.

### Navigation
| Key | Action |
|-----|--------|
| `TAB` | Toggle section (fold/unfold) |
| `S-TAB` | Toggle all sections |
| `n` / `p` | Next / previous section |
| `M-n` / `M-p` | Next / previous sibling section |

### Actions on Work at point
| Key | Magit analogue | Magent action |
|-----|---------------|---------------|
| `RET` | visit at point | Open agent output buffer (on Work) or visit file (on file path) |
| `$` | process buffer | Show raw agent output buffer |
| `!` | run shell cmd | Shell command in that worktree |
| `c` | commit | Tell agent to commit |
| `P` | push | Tell agent to open PR |
| `d` | diff | Open magit-diff in that worktree |
| `k` | discard | Kill agent session |
| `r` | rebase | Resume idle session |
| `b` | branch | Browse backlog org files for repo |
| `l` | log | Show agent event log |
| `f` | fetch | Fetch/pull in that worktree |

### Magent-specific
| Key | Action |
|-----|--------|
| `i` | Send input to agent (minibuffer prompt) |
| `N` | New Work (prompts for repo, branch, purpose) |
| `D` | Mark done / archive |
| `g` | Refresh buffer |

## Configuration

```elisp
;; Agent command (don't hardcode claude)
(defcustom magent-agent-command "claude"
  "The agent CLI command to invoke.")

(defcustom magent-agent-args
  '("--dangerously-skip-permissions"
    "--output-format" "stream-json"
    "--verbose")
  "Default arguments passed to the agent CLI.")

;; Backlog discovery
(defcustom magent-backlog-glob "*.org"
  "Glob pattern for finding backlog org files in repo roots.")

;; State file
(defcustom magent-state-file
  (expand-file-name "state.el" (locate-user-emacs-file "magent"))
  "Path to persist Work state.")
```

## Future (v2+)

- **Org-mode launchpad:** `(magent-plan)` opens an org file where C-c C-c on a TODO heading dispatches it as a new Work to the dashboard.
- **Cross-repo views:** Filter/group by tags, state, or custom predicates.
- **Agent event timeline:** Detailed event log per Work with timestamps, diffs, tool call details.
- **Auto-archive:** When PR merges (detected via forge or gh), auto-transition Work to done and clean up worktree.
- **Transient menus:** Replace single-key bindings with magit-style transient popups for multi-option actions (commit with message, PR with reviewers, etc).
