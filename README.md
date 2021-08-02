# dynload-julia

Dynamically load Julia from Fortran.

## Setup
This guide assumes that [GFortran](https://gcc.gnu.org/fortran/), [Fortran Package Manager](https://github.com/fortran-lang/fpm)
and `Git` are installed already.

## Debian Linux
```sh
wget https://julialang-s3.julialang.org/bin/linux/x64/1.6/julia-1.6.1-linux-x86_64.tar.gz
tar -xzf julia-1.6.1-linux-x86_64.tar.gz
export LD_LIBRARY_PATH=`pwd`/julia-1.6.1/lib
# ☝️ This one is important!
```

## Windows
- Download and install https://julialang-s3.julialang.org/bin/winnt/x64/1.6/julia-1.6.1-win64.exe
- Add `C:\path\to\Julia-1.6.1\bin` to the `PATH` environment variable, for example
```bat
set PATH=C:\Julia-1.6.1\bin;%PATH%
rem ☝️ This one is important!
```

## macOS
- Not tested

## Usage
(1) Create a new _fpm_ project and _cd_ into it
```sh
fpm new my-julia
cd my-julia
```
(2) Open file _fpm.toml_ in your text editor and append this code:
```toml
[dependencies]
dynload-julia = { git = "https://github.com/brocolis/dynload-julia.git" }
```
(3) Open file _app/main.f90_ and replace everything with the following code:
```fortran
program main
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_double
implicit none
real(kind=c_double), allocatable :: matf64(:,:)

call julia_init()
call julia_run("script.jl", matf64)
print *, 'Fortran: value computed by Julia:', matf64

call julia_destroy()
end program
```
(4) Create file _script.jl_ in the project root directory
```julia
using LinearAlgebra
# Do stuff ...
return [1.1 2.2; 4.4 5.5]
```

(5) Build and run the project
```sh
# On Linux
fpm run --flag "-fPIC -ldl"
# On Windows
fpm run
```