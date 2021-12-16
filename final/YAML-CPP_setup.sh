git clone https://github.com/jbeder/yaml-cpp.git
cd yaml-cpp
git checkout yaml-cpp-0.6.0
mkdir build
cd build
cmake -DCMAKE_CXX_COMPILER=afl-clang-fast++ ..
make

#AFL command for fuzzing YAML-CPP
#-i for input seed path
#-o for output result path
#-- for target executable path
#afl-fuzz -i ~/yaml_cpp_input_seeds/ -o ~/yaml_cpp_3hours/ -- ./parse