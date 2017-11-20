DOT_PATH=~/.dotfiles
# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update

# install stow, for dotfiles
brew install stow
brew install git tree autojump
brew install proxychains-ng

# casks
brew cask install dash flux the-unarchiver google-chrome iina
brew cask install shadowsocksx-ng
brew cask install transmission
brew cask install virtualbox
brew install vim

# cask emacs
brew cask install emacs

# font
brew tap caskroom/fonts
brew cask install font-source-code-pro font-source-code-pro-for-powerline


# zsh & oh-my-zsh & pure
brew install zsh zsh-completions zsh-syntax-highlighting
echo $(which zsh) | sudo tee -a /etc/shells
chsh -s $(which zsh)

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

cd $DOT_PATH
git submodule update --init --recursive
cd pure
mkdir -p $HOME/.zfunctions
ln -s $PWD/pure.zsh $HOME/.zfunctions/prompt_pure_setup
ln -s $PWD/async.zsh $HOME/.zfunctions/async



# anaconda, python
brew cask install anaconda

# clang, make sure to install xcode first
brew install --with-toolchain llvm

# rust !
curl https://sh.rustup.rs -sSf | sh
