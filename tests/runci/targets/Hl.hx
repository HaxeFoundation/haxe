package runci.targets;

import haxe.io.Path;
import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Hl {
    static public function getHlDependencies() {
        if (commandSucceed("hl", ["--version"])) {
            infoMsg('hl has already been installed.');
            return;
        }
        switch (systemName) {
            case "Linux":
                Linux.requireAptPackages(["libpng-dev", "libjpeg-turbo8-dev", "libturbojpeg", "zlib1g-dev", "libvorbis-dev"]);
            case "Mac":
                runCommand("brew", ["install", "libpng", "jpeg-turbo", "libvorbis"], true);
            case "Windows":
                //pass
        }
        var hlSrc = Path.join([Sys.getEnv("HOME"), "hashlink"]);
        runCommand("git", ["clone", "https://github.com/HaxeFoundation/hashlink.git", hlSrc]);
        var hlBuild = Path.join([Sys.getEnv("HOME"), "hashlink_build"]);
        FileSystem.createDirectory(hlBuild);
        var generator = systemName == "Windows" ? [] : ["-GNinja"];
        runCommand("cmake", generator.concat([
            "-DBUILD_TESTING=OFF",
            "-DWITH_BULLET=OFF",
            "-DWITH_DIRECTX=OFF",
            "-DWITH_FMT=ON",
            "-DWITH_OPENAL=OFF",
            "-DWITH_SDL=OFF",
            "-DWITH_SQLITE=OFF",
            "-DWITH_SSL=OFF",
            "-DWITH_UI=OFF",
            "-DWITH_UV=OFF",
            "-DWITH_VIDEO=OFF",
            "-B" + hlBuild,
            "-H" + hlSrc
        ]));
        runCommand("cmake", [
            "--build", hlBuild
        ]);
        
        addToPATH(Path.join([hlBuild, "bin"]));
        runCommand("hl", ["--version"]);
    }

    static public function run(args:Array<String>) {
        getHlDependencies();
        runCommand("haxe", ["compile-hl.hxml"].concat(args));
        runCommand("hl", ["bin/unit.hl"]);

        // TODO sys test
    }
}