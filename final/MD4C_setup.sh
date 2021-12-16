#MD4C VERSION is 0.2.4
git clone https://github.com/mity/md4c
git checkout 0d1a41a4d25d57e41b19f8c6abbabbf4d40d09ae
cd md4c
mkdir build
cd build
cmake -DCMAKE_C_COMPILER=/usr/bin/afl-clang-fast ..
make

#AFL command for fuzzing MD4C
#afl-fuzz -i ~/md4c/test/fuzz-input -o ~/md4c_3hours/ -- ./md2html