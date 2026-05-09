---
name: ui-review
description: >
  Capture a screenshot of the caller's window to assist with UI/UX requests.
  Use proactively when the user asks about colors, themes, layout, spacing,
  fonts, contrast, or visual design — and also after making any UI/UX change
  to visually validate the result looks correct before reporting done.
allowed-tools: Bash(scripts/selfie.sh), Read
---

Take a screenshot of the caller's window, then use it as visual context to answer the user's request.

1. Run the screenshot tool:
```bash
bash scripts/selfie.sh
```

2. Read the resulting image file (the path is printed by the script).

3. Use what you see to give specific, accurate feedback — or to confirm that a change looks correct before reporting done.
