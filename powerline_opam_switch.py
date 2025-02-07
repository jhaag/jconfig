import os
from ..utils import BasicSegment


class Segment(BasicSegment):
    def add_to_powerline(self):
        # Check if opam is available
        if os.system('which opam >/dev/null 2>&1') != 0:
            return
            
        # Run opam switch and capture output
        try:
            import subprocess
            result = subprocess.run(['opam', 'switch'], capture_output=True, text=True)
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
            
        # Get switch name (first part after arrow)
        env_name = active_line.split()[1]
        
        # Set colors
        bg = self.powerline.theme.VIRTUAL_ENV_BG
        if stderr:
            fg = self.powerline.theme.REPO_DIRTY_FG
        else: 
            fg = self.powerline.theme.VIRTUAL_ENV_FG
        
        # Append switch name
        self.powerline.append(" " + env_name + " ", fg, bg)