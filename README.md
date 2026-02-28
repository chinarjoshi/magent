# magent

magit for agents. A porcelain over your AI coding sessions.

```
Active (12)
infra
  user/chijoshi/mcu-heartbeat       [working]  implement heartbeat monitoring
  user/chijoshi/infra-docs          [idle]     Remove the human readable version

ai-platform
  user/chijoshi/fix-prod-soc..      [idle]     There are two problems with the mind dashboard

Archive (3)
  firmware - feature/das-fsd
  neuralinux - replace-hugo
```

## what

Auto-discovers your Claude Code sessions from `~/.claude/projects/`. Groups by repo. Color = state. TAB to fold. RET to open the session. `i` to send a prompt. `c` to tell it to commit. That's it.

## install

```elisp
(use-package magent
  :load-path "~/magent"
  :commands (magent)
  :hook (after-init . magent))
```

Requires `magit-section`. Works with Evil.

## keys

```
RET   open agent (via claude-code.el)
i     send input
c     tell agent to commit
P     tell agent to open PR
r     resume (work or repo)
d     diff unstaged
=     diff since agent started
w     new worktree + agent
N     new work (existing dir)
D     archive (offers worktree cleanup)
!     shell command in worktree
b     browse backlog org files
?     help
```

`magent-org-dispatch` from an org TODO creates a worktree and launches an agent with the heading as the prompt.

## how it works

Scans `~/.claude/projects/` for session JSONL files. Reads first 8K + last 32K (not the whole file). Extracts session ID, branch, last prompt, last output. Sends prompts to idle sessions via `claude -p --resume <id>`. Auto-archives branches merged into main.

Three states: **working** (green), **idle** (grey), **done** (purple).

No daemon. No database. Persistence is just overrides (start-commit, done state) on top of discovery.
