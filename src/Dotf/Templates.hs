module Dotf.Templates (
  missingRepoMessage,
  scaffoldPluginsYaml,
  scaffoldProfilesYaml,
  migrateSuccessMessage,
) where

import           Data.String.Interpolate (__i)

missingRepoMessage :: String
missingRepoMessage =
  [__i|Error: Missing bare repository for your dotfiles!

  Dotfile directory is set to $HOME/.dotf and is currently
  missing on your system.

  To connect to an existing repository, use:
    dotf init <url> [--profile <name>]

  To create a new dotfile repository, use:
    dotf new|]

scaffoldPluginsYaml :: String
scaffoldPluginsYaml =
  [__i|\# dotf v3 plugin definitions
  \#
  \# plugins:
  \#   shell:
  \#     description: "Zsh config"
  \#     paths:
  \#       - .zshrc
  \#       - .zprofile
  \#   neovim:
  \#     description: "Neovim editor config"
  \#     depends: []
  \#     paths:
  \#       - .config/nvim/
  \#     post-install: ["nvim --headless +PlugInstall +qa"]
  \#
  \# watchlist:
  \#   - .config/

  plugins: {}

  watchlist: []|]

scaffoldProfilesYaml :: String
scaffoldProfilesYaml =
  [__i|\# dotf v3 machine profiles
  \#
  \# profiles:
  \#   work-mac:
  \#     plugins: [shell, git, neovim]
  \#   linux-desktop:
  \#     plugins: [shell, git, neovim, linux-desktop]

  profiles: {}|]

migrateSuccessMessage :: String
migrateSuccessMessage =
  [__i|Migration scaffolding complete!

  Created:
    ~/.config/dotf/plugins.yaml
    ~/.config/dotf/profiles.yaml

  All tracked files are currently unassigned. Use:
    dotf plugin new <name>        - create a plugin
    dotf track <file> --plugin <name> - assign files

  Run 'dotf status' to see unassigned file count.|]
