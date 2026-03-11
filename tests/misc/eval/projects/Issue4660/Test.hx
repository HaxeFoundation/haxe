class Test {
    static function error(msg, code) {
        Sys.stderr().writeString(msg);
        Sys.exit(code);
    }

    static function main() {
        var proc = new sys.io.Process("haxe", [
            "-js", "out.js",
            "--macro", "Include.use()"
        ]);
        var stderr = proc.stderr.readAll().toString();
        var exit = proc.exitCode();
        if (exit != 0) {
            error(stderr, exit);
        } else {
            var out = sys.io.File.getContent("out.js");
            if (out.indexOf("THIS IS INCLUDED") == -1)
                error("File is NOT included\n", 1);
        }
    }
}
