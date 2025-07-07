use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Readonly;
require JSON::PP;
require YAML::PP;
require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

class Game::EvonyTKR::Converter::General :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use namespace::autoclean;

  field $name = '';

  field $basic = Game::EvonyTKR::Model::BasicAttributes->new();
  field $book  = '';

  field $specialties;

  field $ascending = 0;
  field $stars     = 'none';

  method getPrimaryFields {
    say "Enter the General's Name:";
    my $n = <STDIN>;
    chomp $n;
    # todo: make sure this is a UTF8 string;
    $name = $n;
    my $v;
    foreach my $attribute ('Leadership', 'Attack', 'Defense', 'Politics') {
      say "$attribute:";
      $v = <STDIN>;
      chomp $v;
      my $b = $v;
      say "$attribute increment:";
      $v = <STDIN>;
      chomp $v;
      my $i  = $v;
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        base           => $b,
        increment      => $i,
        attribute_name => lc($attribute),
      );
      $basic->setAttribute(lc($attribute), $ba);
    }
    say "Enter Book Name:";
    $v = <STDIN>;
    chomp $v;
    # todo: make sure this is a UTF8 string;
    $book = $v;
    my $i = 0;
    while ($i < 4) {
      $i++;
      say "Enter Specialty Name:";
      $v = <STDIN>;
      chomp $v;
      # todo: make sure this is a UTF8 string;
      push(@$specialties, $v);
    }
    say "Enter ascending level from one of "
      . join(', ', $self->AscendingAttributeLevelValues()->@*);
    $v = <STDIN>;
    chomp $v;
    if (any { $_ eq $v } $self->AscendingAttributeLevelValues()->@*) {
      $stars     = $v;
      $ascending = 1;
    }
  }

  method printYAML {
    my $data = {
      name             => $name,
      basic_attributes => {
        leadership => {
          base      => $basic->leadership->base,
          increment => $basic->leadership->increment,
        },
        attack => {
          base      => $basic->attack->base,
          increment => $basic->attack->increment,
        },
        defense => {
          base      => $basic->defense->base,
          increment => $basic->defense->increment,
        },
        politics => {
          base      => $basic->politics->base,
          increment => $basic->politics->increment,
        },
      },
      book        => $book,
      specialties => $specialties,
      ascending   => $ascending,
      stars       => $stars,
    };
    $self->logger->debug(sprintf('general is %s', Data::Printer::np($data)));
    say YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->dump($data);

  }

  method execute {

    $self->getPrimaryFields();
    $self->printYAML();
  }

}
1;
__END__
