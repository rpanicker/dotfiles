-- nvim config written in lua.
--
-- Load packer.nvim automatically
local set = vim.opt
-- basic hygenie settings
set.number = true
set.relativenumber = true
set.tabstop = 2
set.smarttab = true
set.shiftwidth = 2
set.softtabstop = 0
set.expandtab = true
set.swapfile = false
set.linebreak = true -- dont split words when softwrapping
set.splitbelow = true
set.splitright = true

-- Set mapleader to \ and maplocalleader to ,
vim.g.mapleader = "space"
vim.g.maplocalleader = ","
-- source conf/plugins.lua here
require('conf/keymaps')
require('conf/plugins')
require('conf/lsp')
-- Settings that need to happen post plugin load.
vim.cmd[[colorscheme onedark]]
