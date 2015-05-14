if filereadable("session.vim")
    source session.vim
endif

""" Autosave session on idle. 
" autocmd! CursorHold * mksession! session.vim

""" File type detect
" augroup filetypedetect
"     autocmd! BufNewFile,BufRead *.yml setf ansible 
" augroup end

""" Custom Ctrlp command to ignore specific type of files
" let g:ctrlp_user_command="ag %s -U --nocolor --nogroup --ignore '.git' --ignore '*.pyc' -g ''"

""" Like Ctrlp command, grep command could be customized as well
" set grepprg="ag --nogroup --nocolor -U --ignore '.git' --ignore '*.pyc"

""" Multiple root directories support
" nmap ;E :CtrlP /nail/src<CR>
" nmap ;K :CtrlP /nail/kaylee_src<CR>
" nmap ;B :CtrlP /nail/bryo_src<CR>
