" Purpose: Set appropriate defaults
" Author:  Simon Meier
" Date:    2005/11/2

" use pathogen to manage vim plugins
call pathogen#infect()

" basic options
set autoindent
set mouse=a
set incsearch
set shiftwidth=2
set sts=2
set expandtab
set nowrap
syntax on
filetype plugin on

colorscheme koehler

" Tabularize remaps
:nnoremap <F2>j :Tabularize /\( \\|^\)->\( \\|$\)/l0<CR>
:nnoremap <F2>k :Tabularize /\( \\|^\)<-\( \\|$\)/l0<CR>
:nnoremap <F2>l :Tabularize /\( \\|^\)=\( \\|$\)/l0<CR>
                                  
:vnoremap <F2>j :Tabularize /\( \\|^\)->\( \\|$\)/l0<CR>
:vnoremap <F2>k :Tabularize /\( \\|^\)<-\( \\|$\)/l0<CR>
:vnoremap <F2>l :Tabularize /\( \\|^\)=\( \\|$\)/l0<CR>

" filetype for .ML files
au! BufRead,BufNewFile *.ML         setfiletype sml

" mapping for calling make
map <F7> :w<CR>:!pdflatex %<CR>
map <F8> :w<CR>:!make<CR>
map <F9> :NERDTreeToggle<CR>

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

" Use alt-cursors to flip between windows
map <A-Up> <C-W><Up>
map <A-Down> <C-W><Down>
map <A-Left> <C-W><Left>
map <A-Right> <C-W><Right>
map <A-c> <C-W>c

" Change to the directory the file in your current buffer is in
"autocmd BufEnter * :lcd %:p:h
" From http://www.vim.org/tips/tip.php?tip_id=370
autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /
" Filetype stuff
""""""""""""""""

" Latex
au FileType tex 	setlocal autowrite
au FileType tex		setlocal efm=%E!\ LaTeX\ %trror:\ %m,
	\%E!\ %m,
	\%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
	\%-GOverfull\ %m,
	\%-GUnderfull\ %m,
	\%+W%.%#\ at\ lines\ %l--%*\\d,
	\%WLaTeX\ %.%#Warning:\ %m,
	\%Cl.%l\ %m,
	\%+C\ \ %m.,
	\%+C%.%#-%.%#,
	\%+C%.%#[]%.%#,
	\%+C[]%.%#,
	\%+C%.%#%[{}\\]%.%#,
	\%+C<%.%#>%.%#,
	\%C\ \ %m,
	\%-GSee\ the\ LaTeX%m,
	\%-GType\ \ H\ <return>%m,
	\%-G\ ...%.%#,
	\%-G%.%#\ (C)\ %.%#,
	\%-G(see\ the\ transcript%.%#),
	\%-G\\s%#,
	\%+O(%f)%r,
	\%+P(%f%r,
	\%+P\ %\\=(%f%r,
	\%+P%*[^()](%f%r,
	\%+P[%\\d%[^()]%#(%f%r,
	\%+Q)%r,
	\%+Q%*[^()])%r,
	\%+Q[%\\d%*[^()])%r
