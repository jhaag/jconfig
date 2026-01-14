import os
from ..utils import BasicSegment


class Segment(BasicSegment):
    def add_to_powerline(self):
        # Check if opam is available
        if os.system('which opam >/dev/null 2>&1') != 0:
            return

        # Run opam switch list and capture output
        try:
            import subprocess
            result = subprocess.run(['opam', 'switch', 'list'], capture_output=True, text=True)
            stdout = result.stdout
            stderr = result.stderr
        except:
            return

        # Find active switch line and extract name
        active_line = None
        for line in stdout.splitlines():
            if line.startswith('→'):
                active_line = line
                break

        if not active_line:
            return

        # Parse switch name and description
        parts = active_line.split()
        if len(parts) < 2:
            return

        switch_name = parts[1]

        # Description is everything after the compiler column (if present)
        # Format: → switch compiler description
        description = None
        if len(parts) >= 4:
            description = ' '.join(parts[3:])
        elif len(parts) == 3:
            # No compiler column, description is at index 2
            description = parts[2]

        # Pick the nicest/shortest name
        if description and ' ' not in description:
            # No spaces in description, pick shortest
            env_name = description if len(description) < len(switch_name) else switch_name
        else:
            # Description has spaces or is empty, use switch name
            env_name = switch_name

        # Check whether the switch is active, or a folder-switch
        # is being used
        is_active = (
            (not stderr) or
            ("You should run: eval $(opam env)" not in stderr)
        )

        # Set colors
        bg = self.powerline.theme.VIRTUAL_ENV_BG
        if is_active:
            fg = self.powerline.theme.VIRTUAL_ENV_FG
        else:
            fg = self.powerline.theme.REPO_DIRTY_FG

        # Append switch name with sand dune icon
        self.powerline.append(" 🏜️  " + env_name + " ", fg, bg)
