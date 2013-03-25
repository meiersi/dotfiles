" Purpose: Set appropriate defaults
" Author:  Simon Meier
" Date:    2005/11/2
" Updated: 2013/02/09

" seting up the mapleader to ';'
let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxy'
let g:EasyMotion_leader_key = '<Leader>'

" use pathogen to manage vim plugins
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" basic options
set autoindent
set mouse=a
set incsearch
set shiftwidth=2
set sts=2
set expandtab
set nowrap
set gdefault
set tw=78                     " default textwidth is a max of 78
set list                      " enable custom list chars
" set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮    " replace tabs, eol
set listchars=tab:▸\ ,extends:❯,precedes:❮    " replace tabs, eol
set showbreak=↪               " show breaks
set colorcolumn=+1
set number
syntax enable
let g:solarized_termcolors=256
let g:solarized_contrast="high"    "default value is normal
" let g:solarized_hitrail=1
set background=light
colorscheme solarized
filetype plugin on

" Temporary files
set directory=~/tmp/vim/
set undofile
set undodir=~/tmp/vim/
set backup
set backupdir=~/tmp/vim/
set backupskip=~/tmp/vim/
set writebackup


" ---------------------------------------------------------------------------
"  configurations for vim or gvim only
if !has("gui_running")
    set background=dark
    colorscheme darkblue
    " let g:AutoClosePreservDotReg=0
end
if has("gui_running")
    set guioptions-=T        " no toolbar
    set guioptions-=m        " no menubar
    set guioptions-=l        " no left scrollbar
    set guioptions-=L        " no left scrollbar
    set guioptions-=r        " no right scrollbar
    set guioptions-=R        " no right scrollbar
    set clipboard=unnamed
    " disalbe stupid bells
    set vb t_vb=
end

" Save on pressing shift
nnoremap ; :

" CtrlP remaps
:nnoremap CTRL-p :CtrlP

" Tabularize remaps
:nnoremap <F2>j :Tabularize /\( \\|^\)->\( \\|$\)/l0<CR>
:nnoremap <F2>k :Tabularize /\( \\|^\)<-\( \\|$\)/l0<CR>
:nnoremap <F2>l :Tabularize /\( \\|^\)=\( \\|$\)/l0<CR>
:nnoremap <F2>o :Tabularize /\( \\|^\)::\( \\|$\)/l0<CR>
                                  
:vnoremap <F2>j :Tabularize /\( \\|^\)->\( \\|$\)/l0<CR>
:vnoremap <F2>k :Tabularize /\( \\|^\)<-\( \\|$\)/l0<CR>
:vnoremap <F2>l :Tabularize /\( \\|^\)=\( \\|$\)/l0<CR>
:vnoremap <F2>o :Tabularize /\( \\|^\)::\( \\|$\)/l0<CR>

" Stylize haskell imports
fun StylizeHaskell()
    let l = line(".")
    let c = col(".")
    % ! stylish-haskell
    call cursor(l, c)
endfun

map <F3> :call StylizeHaskell()<CR>

" filetype for .ML files
au! BufRead,BufNewFile *.ML         setfiletype sml

" mapping for calling make
map <F7> :w<CR>:!pdflatex %<CR>
map <F8> :w<CR>:!make<CR>

" NerdTree
map <F9> :NERDTreeToggle<CR>
" BufExplorer
map <F11> :BufExplorer<CR>

" ensure that buffer position is restored
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Change to the directory the file in your current buffer is in
"autocmd BufEnter * :lcd %:p:h
" From http://www.vim.org/tips/tip.php?tip_id=370
" autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /

" Removing trailing whitespace
""""""""""""""""""""""""""""""

fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    let _s=@/ 
    %s/\s\+$//e
    let @/=_s
    call cursor(l, c)
endfun

autocmd FileType javascript,latex,org,haskell,c,cpp,java,php,ruby,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
