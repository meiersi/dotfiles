if status is-interactive
    # Commands to run in interactive sessions can go here

    direnv hook fish | source
    starship init fish | source

    fish_add_path ~/.daml/bin
    fish_add_path ~/.dpm/bin

    abbr --position command --add lg lazygit
    abbr --position command --add gg lazygit
    abbr --position command --add hh jj
    abbr --position command --add hs jj st
    abbr --position command --add hl jj log
    abbr --position command --add hla jj log -r "'::(mutable()|trunk())'"
    abbr --position command --add hls jj log --stat
    abbr --position command --add hf jj git fetch
    abbr --position command --add hp jj git push

    abbr --position command --add js jj st
    abbr --position command --add jl jj log
    abbr --position command --add jls jj log --stat
    abbr --position command --add jla jj log -r "'::(mutable()|trunk())'"
    abbr --position command --add jf jj git fetch
    abbr --position command --add jp jj git push

    # ~/.local/bin/jj util completion fish | source

    # Set it rather once too many than too few times ;-)
    # xset r rate 150 45

end