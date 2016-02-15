export PATH=${PATH}:${TRAVIS_BUILD_DIR}/.lua:$HOME/.local/bin:${TRAVIS_BUILD_DIR}/install/luarocks/bin
bash .travis/setup_lua.sh
eval `$HOME/.lua/luarocks path`
