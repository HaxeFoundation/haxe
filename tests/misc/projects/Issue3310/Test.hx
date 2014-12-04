class Test {
    static function error(msg, code) {
        Sys.stderr().writeString(msg);
        Sys.exit(code);
    }

    static function main() {
        var proc = new sys.io.Process("haxe", ["--display", "Main.hx@62@toplevel"]);
        var exit = proc.exitCode();
        var stderr = proc.stderr.readAll().toString();
        if (exit != 0)
            error(stderr, exit);
        else if (stderr.indexOf("<il>") != 0)
            error("Invalid toplevel completion output\n", 1);
        else if (stderr.indexOf("_Main.A_Impl_") != -1)
            error("Abstract implementation class is included in the output:\n\n" + stderr, 1);
    }
}
