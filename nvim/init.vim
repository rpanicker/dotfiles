set nocompatible
colorscheme desert
filetype plugin on
filetype indent on
set path+=**
set number
set relativenumber
" Using vim plug as plugin manager
" Set Leader to ,
let mapleader = ","
set splitbelow
set splitright
nnoremap <C-H> <C-W><C-H>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fireplace'
Plug 'vim-airline/vim-airline'
Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline-themes'
Plug 'jalvesaq/Nvim-R'
Plug 'junegunn/fzf', { 'do': { -> fzf#install()}}
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'autozimu/LanguageClient-neovim', { 
	\ 'branch': 'next',
	\ 'do': './install.sh'
	\}
call plug#end()
let g:airline#extensions#tabline#enabled = 1
let g:fzf_layout = { 'right': '~40%' }
set signcolumn=yes
"Settings for LSP completion.
set hidden "For operations that change multiple buffers like rename.
let g:LanguageClient_serverCommands = {
	\ 'rust': ['~/.cargo/bin/rustup','run','stable','rls'],
	\ 'ruby': ['~/.rbenv/shims/solargraph','stdio'],
	\'python': ['~/.local/bin/pyls'],
	\'haskell': ['hie-wrapper', '--lsp']
	\}
let g:LanguageClient_preferredMarkupKind = ['plaintext','markdown']
let g:LanguageClient_windowLogMessageLevel="Error" 
"Mappings for completion
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent>  <Leader>lk :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent>  <Leader>ld :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent>  <Leader>lr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent>  <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
nnoremap <silent>  <Leader>lb :call LanguageClient#textDocument_references()<CR>
nnoremap <silent>  <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
nnoremap <silent>  <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

set completefunc=LanguageClient#complete
set completeopt=menu,preview
