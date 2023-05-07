local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  -- Allow Packer to manage itself
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  -- All the tpope goodness above in one section.

  use 'neovim/nvim-lspconfig'

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
  
  if packer_bootstrap then
    require('packer').sync()
  end
end)
