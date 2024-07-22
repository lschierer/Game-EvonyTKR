requires 'perl', '5.40.0';


# requires 'Some::Module', 'VERSION';

on test => sub {
    requires 'Test::More', '0.96';
    requires 'Test::Most', '0',
};
