#! /bin/sh
cp -r nvim ~/.config
python -m pip install --user neovim
python3 -m pip install --user neovim
mkdir build
git clone --recursive https://github.com/cquery-project/cquery
cd cquery && mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_EXPORT_COMPILE_COMMANDS=YES || exit 1
make -j32
sudo make install
cd ../../
sudo pip install python-language-server
sudo npm install javascript-typescript-langserver
