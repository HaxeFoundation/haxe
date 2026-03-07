class Test {
    static function error(msg, code) {
        Sys.stderr().writeString(msg);
        Sys.exit(code);
    }

    static function main() {
        var proc = new sys.io.Process("haxe", ["-cp", ".", "--display", "Main.hx@38@toplevel"]);
        var stderr = proc.stderr.readAll().toString();
        var exit = proc.exitCode();
        if (exit != 0) {
            error(stderr, exit);
        } else {
            if (stderr.indexOf('<i k="package">_pkg</i>') != -1)
                error("The package starting with `_` is not filtered out for completion:\n" + stderr, 1);
        }
    }
}
