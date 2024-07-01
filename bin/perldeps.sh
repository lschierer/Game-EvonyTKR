#!/bin/bash

cpanm install Data::ULID

#for JSON support, some recommendations from Perl Maven
cpanm install JSON
cpanm install JSON::PP
cpanm install Cpanel::JSON::XS # not actually a part of Cpanel, but named for them because they sponsor it.
cpanm install JSON::MaybeXS

# for building modules
cpanm install Module::Starter
cpanm install Module::Build
cpanm install Software::License
cpanm install Software::License::CC_BY_4_0
