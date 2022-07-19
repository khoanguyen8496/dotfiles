#! /bin/sh
cp -r nvim ~/.config
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim
python -m pip install --user neovim
python3 -m pip install --user neovim
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
