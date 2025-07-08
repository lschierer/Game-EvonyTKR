use v5.42.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
require YAML::PP;
require Hash::Util;
require Text::CSV;
require Storable;
require UUID;
require Game::EvonyTKR::Model::General::ConflictGroup;
require Encode;
use namespace::clean;

class Game::EvonyTKR::Model::General::ConflictGroup::Importer :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;
  use List::AllUtils qw( any none uniq );
  our $VERSION = 'v0.0.1';

  field $outputDir : reader : param;
  field $generals_file : param //= Path::Tiny::path('EvansFullGeneralInfo.csv');
  field $groups_file : param //=
    Path::Tiny::path('EvAnsConflictGroupDefinitions.csv');

  field $myUUIDBase : reader;
  field $conflictGroups : reader = {};

  ADJUST {
    $myUUIDBase = UUID::uuid5($self->UUID5_base, 'General Conflict Group');
  }

  ADJUST {
# ensure that $outputDir is a Path::Tiny object - do not trust the user who might pass in a string
    $outputDir = Path::Tiny::path($outputDir);
    if ($outputDir->exists()) {
      if ($outputDir->is_dir()) {
        $self->logger->trace("$outputDir is a directory as expected");
      }
      else {
        $self->logger->logcroak("$outputDir is not a directory!!");
      }
    }
    else {
      $self->logger->logcroak("$outputDir does not exist!");
    }
  }

  method _generate_uuid($text) {
    return UUID::uuid5($myUUIDBase, $text);
  }

  method loadGenerals() {
    # -----------------------------
    # Load generals and their BSC groups
    # -----------------------------
    my %group_members;
    my $gen_csv = Text::CSV->new({ binary => 1, auto_diag => 1 });

    open my $gen_fh, '<:encoding(UTF-8)', $generals_file
      or die "Cannot open generals file: $!";
    my $gen_header = $gen_csv->getline($gen_fh);
    $gen_csv->column_names(@$gen_header);

    while (my $row = $gen_csv->getline_hr($gen_fh)) {
      my $name = $row->{Name} // next;
      $name = $self->normalize_name($name, $row->{Name});

      my $bsc_codes = $row->{'BSC Codes'} // '';
      $bsc_codes =~ s/\s+//g;
      next unless $bsc_codes =~ /\w/;

      push @{ $group_members{$bsc_codes} }, $name;
    }
    close $gen_fh;

    # Normalize member lists
    $_ = [uniq @{$_}] for values %group_members;

    return \%group_members;
  }

  method loadGroupLinkDefinitions() {
    # -----------------------------
    # Load group link definitions
    # -----------------------------

    my %code_parts;
    my $grp_csv = Text::CSV->new({ binary => 1, auto_diag => 1 });

    open my $grp_fh, '<:encoding(UTF-8)', $groups_file
      or die "Cannot open group definitions file: $!";
    my $grp_header = $grp_csv->getline($grp_fh);
    $grp_csv->column_names(@$grp_header);

    while (my $row = $grp_csv->getline_hr($grp_fh)) {
      my $key   = $row->{Skill}                    // next;
      my $combo = $row->{'Troop Combination Code'} // next;
      $code_parts{$key} = [
        $row->{'Troop Combination Code'} // '',
        $row->{'Skill Combination Code'} // '',
        $row->{'Action Code'}            // '',
      ];
    }
    close $grp_fh;

    return \%code_parts;
  }

  method importAndMap() {
    my $group_members = $self->loadGenerals();
    my $group_links   = $self->loadGroupLinkDefinitions();

    my %part_index;    # key = part value, value = [ group keys... ]

    # First, index all parts of all groups
    foreach my $group_name (keys %$group_members) {
      my @parts = split /-/, $group_name;
      for my $part (@parts) {
        push @{ $part_index{$part} }, $group_name;
      }
    }

    foreach my $group_name (keys %$group_members) {
      my @parts = split /-/, $group_name;
      my %linked;
      my $uuid    = $self->_generate_uuid($group_name);
      my @members = @{ $group_members->{$group_name} };

      $self->logger->debug("Checking $group_name parts: @parts");

      # Check for conflicts based on any matching part, regardless of position
      for my $part (@parts) {
        my $linked_groups = $part_index{$part} // [];
        $self->logger->debug("  Part = $part, linked → @$linked_groups");

        foreach my $conflict (@$linked_groups) {
          next if $conflict eq $group_name;
          $self->logger->debug("    Linked: $group_name ↔ $conflict");
          $linked{$conflict} = 1;
        }
      }

      my @linked_uuids = map { $self->_generate_uuid($_) } sort keys %linked;

      my $conflict = {
        id      => $uuid,
        name    => $group_name,
        members => \@members,
        others  => \@linked_uuids,
      };

      $conflictGroups->{$group_name} = $conflict;
    }

    $self->logger->info(
      "Generated " . scalar(keys %{$conflictGroups}) . " conflict groups");
    return $conflictGroups;
  }

  method normalize_name ($raw_name, $original = '') {

    # Remove square bracket metadata completely
    $raw_name =~ s/\s*\[[^\]]+\]\s*//g;

    # Replace (content) with just the content — keep the word "Prime" etc.
    $raw_name =~ s/\(([^)]+)\)/ $1/g;

    # Normalize whitespace: collapse multiple spaces and trim
    $raw_name =~ s/^\s+|\s+$//g;
    $raw_name =~ s/\s{2,}/ /g;

    my $name = $raw_name;
    if (length $original) {

      $self->logger->debug("Normalized '$original' → '$name'")
        if $name ne $original;
    }
    return $name;
  }

  method map_bsc_to_useCategory ($bsc) {
    if ($bsc =~ /^GR/) {
      return 'Ground';
    }
    if ($bsc =~ /^RA/) {
      return 'Ranged';
    }
    if ($bsc =~ /^MT/) {
      return 'Mounted';
    }
    if ($bsc =~ /^SG/) {
      return 'Siege';
    }
    return 0;
  }

  method _write_yaml_file ($filepath, $content) {
    my $ypp = YAML::PP->new(schema => [qw/ + Perl /]);

    my $yaml = $ypp->dump_string($content);
    $filepath->spew_utf8($yaml);
  }

}
1;
