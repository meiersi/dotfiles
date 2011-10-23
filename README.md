Central .dotfiles for my tools...yay :-)

To include bash configurations use:

    for m in alias prompt; do
        . "~/shared-dotfiles/bash/$m"
    done

in your `.bashrc`

Big Thx @ [iff](https://github.com/iff/shared-dotfiles "that prompt just rocks...")