#!/bin/bash

cpanm install Data::ULID

#for JSON support, some recommendations from Perl Maven
cpanm install JSON
cpanm install JSON::PP
cpanm install Cpanel::JSON::XS # not actually a part of Cpanel, but named for them because they sponsor it.
cpanm install JSON::MaybeXS

#these modules are sensitive on OSX
cpanm install -n Net::SSLeay
cpanm install -n IO::Socket::SSL
cpanm install -n LWP::Protocol::https

#Mail::Sendmail tests require zen.spamhaus.org to like you.  This is unlikely on a workstation.
cpanm install -n Mail::Sendmail

# This is for testing, it wasn't detected automatically
cpanm install Test::Pod
cpanm install Test::Pod::Coverage
cpanm install Test2::Tools::Process

# for building modules
cpanm install Module::Starter
cpanm install Module::Build
cpanm install Software::License
cpanm install Software::License::CC_BY_4_0
cpanm install Dist::Zilla
cpanm install Dist::Zilla::Plugin::Git
cpanm install Dist::Zilla::PluginBundle::Git
cpanm install Pod::Weaver
cpanm install Pod::Elemental::Transformer
cpanm install Pod::Elemental::Transformer::List

# for the website
cpanm install Plack
cpanm install Mojolicious

# for ikiwiki
cpanm install DateTime::Format::ISO8601
cpanm install FFI::CheckLib
# shellcheck disable=SC2155
export CURDIR=`pwd`
cd packages/ikiwiki || exit
PERL5LIB=`pwd` PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'CPAN::Shell->install("Bundle::IkiWiki")'
PERL5LIB=`pwd` PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'CPAN::Shell->install("Bundle::IkiWiki::Extras")'
cd "${CURDIR}" || exit

