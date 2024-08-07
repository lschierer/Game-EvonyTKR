package Game::EvonyTKR::REST;
use Dancer2;

our $VERSION = '0.1';

get '/' => sub {
    template 'index' => { 'title' => 'Game::EvonyTKR::REST' };
};

true;
