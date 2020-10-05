filetype plugin on
filetype indent on
set signcolumn=yes
set number
set relativenumber
" Using vim plug as plugin manager
" Set Leader to ,
let mapleader = ","
" nnoremap <M-j> <C-W>j
" nnoremap <M-k> <C-W>k
" nnoremap <M-l> <C-W>l
" nnoremap <M-h> <C-W>h
" Changing the cursor shape in insert mode inside and outside tmux.
if exists('$TMUX')
	let &t_SI = "\<Esc>Ptmux;\<Esc>\e[5 q\<Esc>\\"
	let &t_EI = "\<Esc>Ptmux;\<Esc>\e[2 q\<Esc>\\"
else
	let &t_SI = "e[5 q"
	let &t_EI = "e[2 q"
endif
set splitbelow
set splitright
set path+=**
if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
	set grepformat=%f:%l:%c:%m,%f:%l%m
endif
" set tags+=~/.config/tagdir/pytags

call plug#begin('~/.config/nvim/plugins')
" Plug 'jsfaint/gen_tags.vim'
Plug 'majutsushi/tagbar'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
" Plug 'tpope/vim-fireplace'
Plug 'vim-airline/vim-airline'
Plug 'skywind3000/vim-preview'
Plug 'altercation/vim-colors-solarized'
Plug 'cocopon/iceberg.vim'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/seoul256.vim'
Plug 'vim-airline/vim-airline-themes'
" Plug 'jalvesaq/Nvim-R'
Plug 'junegunn/fzf', { 'do': { -> fzf#install()}}
Plug 'junegunn/fzf.vim'
" Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'autozimu/LanguageClient-neovim', { 
	\ 'branch': 'next',
	\ 'do': './install.sh'
	\}
call plug#end()

colorscheme onedark
" Features of the plugins that I have enabled.
" Airline Mode and Themes
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_theme = 'lucius'
"FZF
" let g:fzf_layout = { 'down': '~80%' }
let g:fzf_preview_window = 'right:70%'
let g:fzf_buffers_jump = 1

"Preview
"Mappings for using vim-preview and quickfix buffer.
autocmd FileType qf nnoremap <silent><buffer> p :PreviewQuickfix<cr>
autocmd FileType qf nnoremap <silent><buffer> P :PreviewClose<cr>
"Preview function signatures
set noshowmode
nnoremap <F4> :PreviewSignature!<cr>
inoremap <F4> <c-\><c-o>:PreviewSignature!<cr>
"Preview Tags
nnoremap <F3> :PreviewTag<cr>
inoremap <F3> <c-\><c-o>:PreviewTag<cr>
" scroll preview windows without moving.
nnoremap <m-u> :PreviewScroll -1<cr>
nnoremap <m-d> :PreviewScroll +1<cr>
inoremap <m-u> <c-\><c-o>:PreviewScroll -1<cr>
inoremap <m-d> <c-\><c-o>:PreviewScroll +1<cr>
"Map find command and vimgrep and grep commands to keys
nnoremap <leader>f :find 
nnoremap <leader>v :vimgrep 
nnoremap <leader>g :grep!

"Tagbar
nnoremap <F2> :TagbarToggle<cr>
inoremap <F2> <c-\><c-n>:TagbarToggle<cr>

"LSP completion.
set hidden "For operations that change multiple buffers like rename.
let g:LanguageClient_hoverPreview="Always"
let g:LanguageClient_useFloatingHover=0
let g:LanguageClient_diagnosticsMaxSeverity="Hint"
let g:LanguageClient_diagnosticsList="Location"
let g:LanguageClient_useVirtualText="No"
let g:LanguageClient_serverCommands = {
	\ 'rust': ['~/.cargo/bin/rustup','run','stable','rls'],
	\ 'ruby': ['~/.rbenv/shims/solargraph','stdio'],
	\'python': ['~/.local/bin/pyls'],
	\'haskell': ['hie-wrapper', '--lsp'],
	\'go': ['gopls']
	\}
let g:LanguageClient_preferredMarkupKind= ['plaintext', 'markdown']
" Mappings for completion
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent>  <Leader>lk :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent>  <Leader>ld :call LanguageClient#textDocument_definition({'gotoCmd':'PreviewFile'})<CR>
nnoremap <silent>  <Leader>lr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent>  <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
nnoremap <silent>  <Leader>ln :call LanguageClient#textDocument_references()<CR>
nnoremap <silent>  <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
nnoremap <silent>  <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

set completefunc=LanguageClient#complete
set completeopt=menu,preview
