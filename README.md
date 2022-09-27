* Provides a list of configuration files for emacs, nvim, gtags and kak.
** Emacs
Currently using straight.el to create a clean package install mechanism using use-package.
** Vim
.vimrc file, previously used in neovim
** Neovim
init.lua - now uses Packer and a limited set of plugins. Install Packer first.
** tmux
** kak
kakrc and kak-lsp.toml file provided. Please install kak-lsp first.
** Clojure
A set of user deps.edn definitions to get started. Always look for the newest version and update.
Command: clj -X:deps find-versions :lib "lib-name" should return the latest versions from the repositories.
Don't always need to be running the latest version
