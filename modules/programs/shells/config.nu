$env.config.buffer_editor = "vi"

# A dense, pretty, human-readable version of ls
def l [path = "."] {
  ls $path --short-names |
    sort-by type name --ignore-case |
    grid --color --icons --separator "  "
}

# Create a new directory and enter it (mkdir + cd)
def --env md [dirname: string] {
    mkdir $dirname; cd $dirname
}


$env.config.hooks = {
    env_change: {
        PWD: [{|before, after|
            l $after | print --no-newline $"\n($in)"
        }]
    }
}
