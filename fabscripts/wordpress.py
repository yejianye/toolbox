from fabric.api import local, settings, run, sudo, cd, task
from fabric.contrib.files import exists, upload_template, sed

from fablibs.utils import is_linux

@task
def nginx():
    if not is_linux():
        print 'This task is only supported on linux' 
    # install LEMP (nginx + mysql + php)
    pkgs = ['mysql-server', 'mysql-client', 'nginx', 'php5', 'php5-fpm', 'php5-mysql', 'wordpress']
    #[ensure_package(pkg) for pkg in pkgs]
    sed('/etc/php5/fpm/php.ini', '^;? *cgi.fix_pathinfo *= *1', 'cgi.fix_pathinfo=0', use_sudo=True)
    upload_template('wp_blog', '/etc/nginx/sites-available/wp_blog', use_sudo=True,
        context={'server_name' : 'ryanye.me, blog.ryanye.me, www.ryanye.me'})    
    with cd('/etc/nginx/sites-enabled/'):
        sudo('ln -sf ../sites-available/wp_blog')
    sudo('service nginx restart')
    sudo('bash /usr/share/doc/wordpress/examples/setup-mysql -n wordpress blog.ryanye.me')
    # fix uploads directory permission issue
    sudo('chown www-data:www-data -R /srv/www/wp-uploads')
    # set default language to Chinese in wp-config
    print "To allow direct plugin/content upload, add the following line to /etc/wordpress/confg-localhost.php"
    print "    define('FS_METHOD', 'direct');"
    print "To change default language to Chinese,"
    print "    define('WPLANG', 'zh_CN');"
