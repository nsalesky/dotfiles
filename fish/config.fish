if status is-interactive
    # Commands to run in interactive sessions can go here
end

# set -gx EDITOR "emacsclient -c"
# set -gx VISUAL "emacsclient -c"
set -gx EDITOR "nvim"
set -gx VISUAL "nvim"

# Use Vi keybindings

# fish_vi_key_bindings
# set -g fish_cursor_default block
# set -g fish_cursor_insert line
# set -g fish_cursor_replace_one underscore
# set -g fish_cursor_visual block

# set -g fish_vi_force_cursor 1

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end

direnv hook fish | source
zoxide init fish | source
starship init fish | source
