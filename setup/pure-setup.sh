DOT_PATH=~/.dotfiles

cd $DOT_PATH
git submodule update --init --recursive
cd pure
mkdir -p $HOME/.zfunctions
ln -s $PWD/pure.zsh $HOME/.zfunctions/prompt_pure_setup
ln -s $PWD/async.zsh $HOME/.zfunctions/async


