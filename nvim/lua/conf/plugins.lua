

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  -- Allow Packer to manage itself
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  -- All the tpope goodness above in one section.

  use 'neovim/nvim-lspconfig'
 -- use {
 --   'w0rp/ale',
 --   ft = {'sh', 'bash', 'markdown', 'racket', 'vim' },
 --   cmd = 'ALEEnable',
 --   config = 'vim.cmd[[ALEEnable]]'
 -- }

 use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' } --Fails on first install but updates will work
 use 'Olical/conjure' -- Clojure editing support. 
  -- Colorthemes, Airline Stuff etc
  use'altercation/vim-colors-solarized'
  use'cocopon/iceberg.vim'
  use'joshdick/onedark.vim'
  use'junegunn/seoul256.vim'
  use 'vim-airline/vim-airline'
  use {
    'vim-airline/vim-airline-themes',
    config = 'vim.cmd[[silent! AirlineTheme seoul256]]'
  }
  
end)
