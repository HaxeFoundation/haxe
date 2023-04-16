# Building Haxe from source

## Obtaining the source

The Haxe compiler source files are hosted on GitHub under the [HaxeFoundation account](https://github.com/HaxeFoundation). The [Haxe repository](https://github.com/HaxeFoundation/haxe) has several submodules, so cloning it should be done with the `--recursive` flag like so:

```
git clone --recursive https://github.com/HaxeFoundation/haxe.git
```

Alternatively, source .zip archives or tarballs can be obtained from the [GitHub Haxe release overview](https://github.com/HaxeFoundation/haxe/releases). However, the git submodules are not included, so you will have to manually place the source code of [submodules](https://github.com/HaxeFoundation/haxe/blob/development/.gitmodules) into appropriate sub-folders.

## Setting up OCaml

The Haxe compiler is written in OCaml, so you have to set up an OCaml development environment. In addition, we make use of a number of OCaml libraries. We recommend using OPAM, which is an OCaml package manager that can also manage OCaml installations.

The Haxe compiler requires OCaml version 4.02 or higher. Since some of the OCaml libraries Haxe depends on were uploaded in the OPAM 2 format, you should use OPAM 2.x instead of OPAM 1.x.

To install OPAM on Unix (e.g. Mac, Linux) systems, follow the [instruction given by OPAM](https://opam.ocaml.org/doc/Install.html). On Windows, we recommend using the [Cygwin/MinGW-based OPAM environment provided by fdopen](https://fdopen.github.io/opam-repository-mingw/installation/), choose the 64-bit versions of everything, also make sure to [use the OPAM 2 version](https://github.com/fdopen/opam-repository-mingw/issues/48).

In case you messed up the OPAM installation, you can uninstall OPAM and remove `~/.opam`, which contains the OCaml switches (OCaml compilers and libraries), and start over.

Also note that since OPAM 2 on Linux will try to use bubblewrap, which uses Linux user namespaces, which might not be available on environments like Docker or Windows Subsystem for Linux (WSL). In case of encountering related errors, use `--disable-sandboxing` during `opam init`.

## Installing dependencies

You need to install some native libraries as well as some OCaml libraries.

 * Native libraries
    * PCRE
    * zlib
    * Neko (for building haxelib)
 * OCaml libraries
    * listed in the `opam` file at the repository root

To install the native libraries, use the appropriate system package manager.

 * Mac OS X
    * Use [Homebrew](https://brew.sh/), `brew install zlib pcre2 mbedtls@2`.
 * Debian / Ubuntu
    * `sudo apt install libpcre2-dev zlib1g-dev libmbedtls-dev`.
 * Windows (Cygwin)
    * Run the Cygwin [setup-x86_64.exe](https://cygwin.com/install.html) against the Cygwin installation directory. Install `make`, `git`, `zlib-devel`, `libpcre2-devel`, `mingw64-x86_64-gcc-core`, `mingw64-x86_64-zlib`, and `mingw64-x86_64-pcre2`. You may need to select "Not Installed" in the dropdown list to see the packages. Copy `zlib1.dll` and `libpcre2-8-0.dll` from `path/to/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin` to the checked out Haxe source directory.
    * Install Neko by either
      * Download the [Neko binaries](https://nekovm.org/download/), and add the extracted directory to the beginning of PATH.
      * Install the [Chocolatey Neko package](https://chocolatey.org/packages/neko).

To install the OCaml libraries, use OPAM as follows:

```sh
# pin the haxe package to the checked out Haxe source directory
opam pin add haxe path/to/haxe --kind=path --no-action

# install the haxe package dependencies (as listed in the `opam` file)
opam install haxe --deps-only
```

## Compile

In the checked out Haxe source directory,
```sh
# On Unix
make

# On Windows (Cygwin)
make -f Makefile.win
```

## Install

Generally, you should remove any existing Haxe installation to avoid conflict. You should at least make sure that the `HAXE_STD_PATH` environment variable is not set.

To install the freshly built Haxe,

 * On Unix (e.g. Mac and Linux),
    ```sh
    sudo make install
    ```
 * On Windows, add the checked out Haxe source directory to the beginning of PATH.
