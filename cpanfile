requires 'perl', '5.40.0';
requires 'Carp', '0';
requires 'Clone', '0';
requires 'Data::Dumper', '0';
requires 'DBM::Deep', '0';
requires 'Devel::Peek', '0',
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

# requires 'Some::Module', 'VERSION';

on test => sub {
    requires 'Test::More', '0.96';
    requires 'Test::Most', '0',
		requires 'CPAN::Meta::Check', '0';
};



