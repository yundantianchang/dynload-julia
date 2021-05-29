# dynload-julia

Dynamically load Julia from Fortran.

## Setup
Assuming that the [GFortran](https://gcc.gnu.org/fortran/) compiler, the [Fortran Package Manager](https://github.com/fortran-lang/fpm)
and `Git` are installed and available in the PATH environment variable, we need only install Julia then `git-clone` this repository and run `fpm`.
See below for example setups for Linux and Windows.

## Debian Linux
```sh
wget https://julialang-s3.julialang.org/bin/linux/x64/1.6/julia-1.6.1-linux-x86_64.tar.gz
tar -xzf julia-1.6.1-linux-x86_64.tar.gz
export LD_LIBRARY_PATH=`pwd`/julia-1.6.1/lib
git clone https://github.com/brocolis/dynload-julia
cd dynload-julia
fpm test --flag "-fPIC -ldl"
```

## Windows
- Download and install https://julialang-s3.julialang.org/bin/winnt/x64/1.6/julia-1.6.1-win64.exe
- Add `C:\path\to\Julia-1.6.1\bin` to the `PATH` environment variable, for example
```bat
set PATH=C:\Julia-1.6.1\bin;%PATH%
git clone https://github.com/brocolis/dynload-julia
cd dynload-julia
fpm test
```

## macOS
- Not tested
