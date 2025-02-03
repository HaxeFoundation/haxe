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

To install OPAM, follow the [instruction given by OPAM](https://opam.ocaml.org/doc/Install.html).

In case you messed up the OPAM installation, you can uninstall OPAM and remove `~/.opam` (or `%LOCALAPPDATA%\opam` on Windows), which contains the OCaml switches (OCaml compilers and libraries), and start over.

Also note that since OPAM 2 on Linux will try to use bubblewrap, which uses Linux user namespaces, which might not be available on environments like Docker or Windows Subsystem for Linux (WSL). In case of encountering related errors, use `--disable-sandboxing` during `opam init`.

## Installing dependencies

You need to install some native libraries as well as some OCaml libraries.

* Native libraries
  * PCRE
  * zlib
  * mbedtls
  * Neko (for building haxelib)
* OCaml libraries
  * listed in the `opam` file at the repository root

To install the native libraries, use the appropriate system package manager.

 * Mac OS X
    * Use [Homebrew](https://brew.sh/), `brew install zlib pcre2 mbedtls`.
 * Debian / Ubuntu
    * `sudo apt install libpcre2-dev zlib1g-dev libmbedtls-dev`.
 * Windows (Cygwin)
    * Run `opam init` to ensure cygwin is configured with the base packages required. Additional cygwin packages will be installed during the `opam install` further below.

    * Manually install [mingw64-mbedtls](https://github.com/Simn/mingw64-mbedtls) into the cygwin root. First download the correct package from [the releases tab](https://github.com/Simn/mingw64-mbedtls/releases/latest), and then run:

      ```pwsh
      & $(opam exec -- cygpath -w "/bin/tar") -C / -xvf path/to/package.tar.xz
      ```

    * Install Neko by either
      * Download the [Neko binaries](https://nekovm.org/download/), and add the extracted directory to the beginning of PATH.
      * Install the [Chocolatey Neko package](https://chocolatey.org/packages/neko).

On Windows, add the following PATH entry prior to installing libraries:

```pwsh
$env:PATH="$(opam exec -- cygpath -w /usr/x86_64-w64-mingw32/bin);$env:PATH"
```

To install the OCaml libraries, use OPAM as follows:

```sh
# pin the haxe package to the checked out Haxe source directory
opam pin add haxe path/to/haxe --kind=path --no-action

# install the haxe package dependencies (as listed in the `opam` file)
opam install haxe --deps-only
```

## Compile

On Windows, cygwin's make must be available in PATH. To do this, run:

```pwsh
$env:PATH="$(opam exec -- cygpath -w /bin);$env:PATH"
```

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
 * On Windows, first copy the required dlls next to the haxe executable:

   ```pwsh
   make -f Makefile.win copy_mingw_dlls
   ```

   Then add the checked out Haxe source directory to the beginning of PATH.
