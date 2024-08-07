requires 'perl', '5.40.0';
requires 'Carp', '0';
requires 'Clone', '0';
requires 'Dancer2', '1.1.1';
requires 'Data::Dumper', '0';
requires 'DBM::Deep', '0'
requires 'Devel::Peek', '0';
requires 'File::HomeDir', '0';
requires 'File::Path', '0';
requires 'File::ShareDir', '0';
requires 'File::Slurp', '0';
requires 'File::Spec', '0';
requires 'List::MoreUtils', '0';
requires 'namespace::autoclean', '0';
requires 'overload', '0';
requires 'Pod::Weaver', '0';
requires 'PPI::Document', '0';
requires 'Software::License::CC_BY_4_0', '0';
requires 'Type::Utils', '0'; 
requires 'Types::Common::Numeric', '0';
requires 'Types::Common', '0';
requires 'Types::Standard', '0';
requires 'utf8::all', '0';
requires 'YAML::PP::LibYAML', '0';
requires 'YAML::XS', '0';



recommends "YAML"                    => "0";
recommends "URL::Encode::XS"         => "0";
recommends "CGI::Deurl::XS"          => "0";
recommends "CBOR::XS"                => "0";
recommends "YAML::XS"                => "0";
recommends "Class::XSAccessor"       => "0";
recommends "Crypt::URandom"          => "0";
recommends "HTTP::XSCookies"         => "0";
recommends "HTTP::XSHeaders"         => "0";
recommends "Math::Random::ISAAC::XS" => "0";
recommends "MooX::TypeTiny"          => "0";
recommends "Type::Tiny::XS"          => "0";
recommends "Unicode::UTF8"           => "0";

feature 'accelerate', 'Accelerate Dancer2 app performance with XS modules' => sub {
    requires "URL::Encode::XS"         => "0";
    requires "CGI::Deurl::XS"          => "0";
    requires "YAML::XS"                => "0";
    requires "Class::XSAccessor"       => "0";
    requires "Cpanel::JSON::XS"        => "0";
    requires "Crypt::URandom"          => "0";
    requires "HTTP::XSCookies"         => "0";
    requires "HTTP::XSHeaders"         => "0";
    requires "Math::Random::ISAAC::XS" => "0";
    requires "MooX::TypeTiny"          => "0";
    requires "Type::Tiny::XS"          => "0";
    requires "Unicode::UTF8"           => "0";
};

on "test" => sub {
    requires 'Test::More', '0.96';
    requires 'Test::Most', '0',
    requires 'CPAN::Meta::Check', '0';
};

