DOT_PATH=~/.dotfiles

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
