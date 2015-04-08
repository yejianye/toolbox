if filereadable("session.vim")
    source session.vim
endif
autocmd! CursorHold,CursorHoldI * mksession! session.vim

" File type detect
" augroup filetypedetect
"     autocmd! BufNewFile,BufRead *.yml setf ansible 
" augroup end

" set wildignore+=*.pyc
