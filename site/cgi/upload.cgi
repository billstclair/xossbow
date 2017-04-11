#!/usr/bin/perl

######################################################################
##
## upload.cgi
## Perl upload script for Xossbow.com
## Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
## Some rights reserved.
## Distributed under the MIT License
## See LICENSE.txt
##
######################################################################
    

use CGI;
use File::Path qw(make_path);
use File::Basename;

#
# Expects POST parameters of
#
# 'type':
#    'settings', 'template', 'page', or 'image'.
#    This sets the base directory for the 'name' parameter.
# 'name':
#    The path from the base directory to the file name, e.g. 'foo.jpg' for an 'image'
#    Or 'myblog/index.json' for a template.
# 'file':
#    The file content. Usually encoded as multipart/form-data
#
# Will automatically create directories as needed.
#

my $cgi = new CGI;
my $dir = '';
my $type = $cgi->param('type');
my $name = $cgi->param('name');
my $error = '';

if ($type eq 'settings') {
    $dir = '../';
    $name = 'settings.json';
} elsif ($type eq 'template') {
    $dir = '../template';
} elsif ($type eq 'page') {
    $dir = '../page';
} elsif ($type eq 'image') {
    $dir = '../images';
} else {
    $error = "Illegal type: '$type'"
}

my $path = "$dir/$name";
make_path(dirname($path));

if ($error eq '') {
    if (open(LOCAL, ">$path")) {
	my $file_handle = $cgi->upload('file');
	while(<$file_handle>) {
	    print LOCAL $_;
	}
	close($file_handle);
	close(LOCAL);
    } else {
	$error = "Can't open '$path'";
    }
}

print $cgi->header();

if ($error eq '') {
    print 'OK\n';
} else {
    print "$error\n";
}
