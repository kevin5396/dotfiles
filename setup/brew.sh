# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# install stow, for dotfiles
brew install stow git tree

# casks
brew cask install dash flux the-unarchiver google-chrome iina
