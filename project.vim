cd ~/toolbox
if filereadable("session.vim")
    source session.vim
endif
autocmd CursorHold,CursorHoldI * mksession! session.vim
set wildignore+=*.pyc
