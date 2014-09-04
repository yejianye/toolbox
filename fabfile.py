import os
import sys

from fabric.api import env, local, settings, run, sudo, cd, task
from fabric.contrib.files import exists, upload_template, sed

from fablibs.files import ensure_link, ensure_file, ensure_dir, ensure_bin_path
from fablibs.utils import is_macos, is_linux, program_exists
from fablibs.packages import ensure_package, ensure_python_pkg, ensure_nodejs_pkg, ensure_ruby_pkg
from fablibs.vcs import ensure_git_repo

from fabscripts import wordpress

if os.path.exists(os.path.join(os.environ['HOME'], '.ssh/config')):
    env.use_ssh_config = True

if not env.hosts:
    env.hosts = ['localhost']

TOOLBOX_DIR = os.path.abspath(os.path.dirname(__file__))

def toolbox_path(path):
    return os.path.join(TOOLBOX_DIR, path)

@task
def ssh_localhost():
    home_dir = os.getenv('HOME')
    local('mkdir -p ~/.ssh')
    if not os.path.exists(home_dir + '/.ssh/id_rsa'):
        local("ssh-keygen -t rsa -N'' ~/.ssh/id_rsa")
    local('ssh-keygen -y -f ~/.ssh/id_rsa >> ~/.ssh/authorized_keys')

@task
def install_vim73():
    ensure_package('gcc')
    ensure_package('libncurses5-dev')
    if not exists('/tmp/vim73'):
        ensure_package('wget')
        with(cd('/tmp')):
            run('wget -q -O - ftp://ftp.vim.org/pub/vim/unix/vim-7.3.tar.bz2 | tar jx')
    with(cd('/tmp/vim73')):
        run('./configure --enable-pythoninterp --enable-rubyinterp --enable-cscope')
        run('make')
        sudo('make install')

@task
def toolbox(path='~/toolbox'):
    ensure_git_repo(path, 'git://github.com/yejianye/toolbox.git', pushurl='git@github.com:yejianye/toolbox.git')

@task
def vim():
    # get vim version
    ensure_git_repo('~/.vim', 'git://github.com/yejianye/vim-setup.git', pushurl='git@github.com:yejianye/vim-setup.git', submodules=True)
    ensure_link('~/.vimrc_common', '~/.vim/.vimrc_common')
    ensure_file('~/.vimrc', append='source ~/.vimrc_common')
    # With Mac gcc and make should be installed with Xcode
    if not is_macos():
        ensure_package('gcc')
        ensure_package('make')
    with(cd('~/.vim/bundle/vimproc')):
        if is_macos():
            run('make -f make_mac.mak')
        elif is_linux():
            run('make -f make_unix.mak')
    # setup pushurl for submodules
    with(cd('~/.vim/bundle/textobj_function')):    
        run('git config remote.origin.pushurl git@github.com:yejianye/vim-textobj-function.git')
    with(cd('~/.vim/bundle/vim-ref-jquery')):
        run('git config remote.origin.pushurl git@github.com:yejianye/vim-ref-jquery.git')
    with(cd('~/.vim/bundle/syntastic')):
        run('git config remote.origin.pushurl git@github.com:yejianye/syntastic.git')

@task
def zsh():
    if not program_exists('zsh'):
        ensure_package('zsh')
    ensure_git_repo('~/.oh-my-zsh', 'git://github.com/yejianye/oh-my-zsh.git', pushurl='git@github.com:yejianye/oh-my-zsh.git')
    ensure_link('~/.zshrc_common', toolbox_path('dotfiles/zshrc_common'))
    ensure_file('~/.zshrc', append='source ~/.zshrc_common')
    ensure_file('~/.zshenv', append=['setopt ALL_EXPORT'])
    ensure_dir('~/bin')
    ensure_git_repo('~/utils', 'git://github.com/yejianye/util-scripts.git', pushurl='git@github.com:yejianye/util-scripts.git')
    ensure_bin_path(['.', '~/bin', '~/utils', '/usr/local/bin'])

@task
def watchdog():
    if is_linux():
        ensure_python_pkg('pyinotify')
    ensure_python_pkg('git+git://github.com/yejianye/watchdog-tricks.git')

@task
def gitconfig(name='Ryan Ye', email='yejianye@gmail.com'):
    upload_template('gitconfig', '.gitconfig', context={'name': name, 'email' : email})    

@task
def tmux():
    ensure_package('tmux')
    ensure_link('~/.tmux.conf', toolbox_path('dotfiles/tmux.conf'))
    if is_macos():
        if not program_exists('reattach-to-user-namespace'):
            ensure_git_repo('/tmp/reattach-to-user-namespace', 'git://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard.git')
            with cd('/tmp/reattach-to-user-namespace'):
                run('make reattach-to-user-namespace')
                run('cp reattach-to-user-namespace ~/bin')
        ensure_link('~/.tmux.platform.conf', toolbox_path('dotfiles/tmux.mac.conf'))
    else:
        ensure_file('~/.tmux.platform.conf')
    ensure_ruby_pkg('tmuxinator')

@task
def ctags():
    if is_macos():
        ensure_package('ctags', '--HEAD')  # Ensure to get Ctags trunk for Objective-C support
    else:
        ensure_package('ctags')
    ensure_link('~/.ctags', toolbox_path('dotfiles/ctags'))

@task
def terminfo():
    ensure_link('~/.terminfo', toolbox_path('dotfiles/terminfo'))

@task
def python():
    pkgs = [
        'ipython', 
        'ipdb',
        'pylint',
        'fabric',
        'requests'
    ]
    [ensure_python_pkg(pkg) for pkg in pkgs]

@task
def coffeescript():
    ensure_nodejs_pkg('coffee-script')

@task
def less():
    ensure_nodejs_pkg('less')

@task
def migrate_from_oldrepo():
    zsh()
    tmux()
    ctags()
    terminfo()

@task
def all():
    toolbox()
    zsh()
    vim()
    tmux()
    ctags()
    terminfo()
    python()

@task
def test():
    ensure_bin_path(['.', '~/bin', '~/utils', '~/localbin'])
