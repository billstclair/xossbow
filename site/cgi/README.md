# Configuring Apache to enable uploads

Any web server that provides access to static files can be used to serve an existing Xossbow site. But if you want to use Xossbow's editing features to change settings, and edit and create template and page files, you'll need some way to upload those files to your backend.

I plan to provide backends for various key/value stores, likely starting with Amazon S3, but for now I'm providing CGI scripts that respond to the HTTP POST method. The scripts in this directory are written in Perl, since that is built in to most Linux server distributions, without installing anything extra.

In order for these script to work, you've got to enable `.htaccess` overrides in your Apache configuration. Most commercial web hosting services do this already, but if you run your own server, you'll need a `directory` directive allowing overrides. This usually goes in the configuration file for your site: `/etc/apache2/sites-enabled/xxx.conf`. Here's the setting for Xossbow.com:

    <Directory /var/www/xossbow.com>
        Options -Indexes
        AllowOverride All
    </Directory>

Next, make sure the settings file, and the `template` and `page` directories are world writeable. In cPanel-managed sites, you may have to do this from the file browser, but if you have command line access, you can do it as follows:

    cd /var/www/xossbow.com
    chmod -R a+w template
    chmod -R a+w page
    chmod -R a+w images
    chmod a+w settings.json

Unless you want anybody to be able to change your files, you'll need to add authentication to the `cgi` directory. If you're running a cPanel site, there will likely be a "Directory Privacy" link you can use to to this.

If you're running your own web server, there's a good tutorial, for Ubuntu 16.04, [at Digital Ocean](https://www.digitalocean.com/community/tutorials/how-to-set-up-password-authentication-with-apache-on-ubuntu-16-04). As shipped, the `cgi/.htaccess` file has a path to the `.htpasswd` file on my web server. You'll have to change it to a path to that file on your server, and then create the files as follows:

    cd /path/to/htpasswd/dir
    htpasswd -c .htpasswd <username>
    
You can test it, by aiming your web browser at, e.g. [xossbow.com/cgi/hello.cgi](https://xossbow.com/cgi/hello.cgi). It should ask for a userid and password, and then display simply "OK". Xossbow does this itself when you log in.

