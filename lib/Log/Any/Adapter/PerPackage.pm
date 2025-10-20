use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Log::Config;

package Log::Any::Adapter::PerPackage;
use Log::Any::Adapter::Util qw(make_method );
use base                    qw(Log::Any::Adapter::Base);

my %LEVELS = (
  TRACE     => 0,
  DEBUG     => 1,
  INFO      => 2,
  NOTICE    => 3,
  WARNING   => 4,
  ERROR     => 5,
  CRITICAL  => 6,
  ALERT     => 7,
  EMERGENCY => 8
);

sub init {
  my ($self) = @_;
  $self->{logger} = $self->{logger} or die "logger required";

}

sub _should_log {
  my ($self, $method) = @_;
  my $category = $self->{category} || '';
  state $log_config = Game::EvonyTKR::Log::Config->logLevels();

  # Get level for this category (or default to INFO)
  my $category_level = $log_config->{$category} // 'INFO';
  my $method_level   = uc($method);

  return ($LEVELS{$method_level} || 0) >= ($LEVELS{$category_level} || 2);
}

# Generate all logging methods
foreach my $method (
  qw(trace debug info notice warning error critical alert emergency)) {
  make_method(
    $method,
    sub {
      my ($self, $text) = @_;
      return unless $self->_should_log($method);

      my $category = $self->{category} || 'main';
      $self->{logger}->info("[$category] $text");
    }
  );

  make_method(
    "is_$method",
    sub {
      my ($self) = @_;
      return $self->_should_log($method);
    }
  );
}

1;
__END__
