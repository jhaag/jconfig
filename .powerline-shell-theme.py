from powerline_shell.themes.default import DefaultColor

"""
absolute colors based on
https://github.com/morhetz/gruvbox/blob/master/colors/gruvbox.vim
"""
# Stolen from b-ryan/powerline_shell/themes/gruvbox.py
dark0 = 235
dark1 = 237
dark2 = 239
dark3 = 241
dark4 = 243

light0 = 229
light1 = 223
light2 = 250
light3 = 248
light4 = 246

dark_gray  = 245
light_gray = 244

neutral_red    = 124
neutral_green  = 106
neutral_yellow = 172
neutral_blue   = 66
neutral_purple = 132
neutral_aqua   = 72
neutral_orange = 166

bright_red    = 167
bright_green  = 142
bright_yellow = 214
bright_blue   = 109
bright_purple = 171
bright_aqua   = 108
bright_orange = 208

faded_red    = 88
faded_green  = 100
faded_yellow = 136
faded_blue   = 24
faded_purple = 96
faded_aqua   = 66
faded_orange = 130

class Color(DefaultColor):
    USERNAME_ROOT_BG = faded_red
    USERNAME_BG = dark0
    USERNAME_FG = bright_purple

    HOSTNAME_BG = dark1
    HOSTNAME_FG = bright_purple

    HOME_SPECIAL_DISPLAY = True
    HOME_BG = dark0
    HOME_FG = bright_orange
    PATH_BG = dark0
    PATH_FG = faded_orange
    CWD_FG = bright_orange

    READONLY_BG = dark0
    READONLY_FG = bright_red

    SSH_BG = dark0
    SSH_FG = faded_purple

    REPO_CLEAN_BG = dark0
    REPO_CLEAN_FG = bright_green
    REPO_DIRTY_BG = dark0
    REPO_DIRTY_FG = bright_red

    JOBS_FG = bright_aqua
    JOBS_BG = dark0

    CMD_PASSED_FG = bright_purple
    CMD_PASSED_BG = dark0
    CMD_FAILED_FG = bright_red
    CMD_FAILED_BG = dark0

    GIT_AHEAD_BG = dark0
    GIT_AHEAD_FG = neutral_green
    GIT_BEHIND_BG = dark0
    GIT_BEHIND_FG = neutral_red
    GIT_STAGED_BG = dark0
    GIT_STAGED_FG = bright_green
    GIT_NOTSTAGED_BG = dark0
    GIT_NOTSTAGED_FG = bright_yellow
    GIT_UNTRACKED_BG = dark0
    GIT_UNTRACKED_FG = bright_orange
    GIT_CONFLICTED_BG = dark0
    GIT_CONFLICTED_FG = bright_red
    GIT_STASH_BG = dark0
    GIT_STASH_FG = bright_aqua

    VIRTUAL_ENV_BG = dark0
    VIRTUAL_ENV_FG = bright_purple

    TIME_FG = bright_purple
    TIME_BG = dark0
