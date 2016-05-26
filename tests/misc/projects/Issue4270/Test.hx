class Test {
    static function error(msg, code) {
        Sys.stderr().writeString(msg);
        Sys.exit(code);
    }

    static function test(file:String, pos:Int, shouldExist:Bool) {
        var arg = '$file@$pos';
        var proc = new sys.io.Process("haxe", ["--display", arg]);
        var stderr = proc.stderr.readAll().toString();
        var exit = proc.exitCode();
        if (exit != 0) {
            error(arg + ":\n" + stderr, exit);
        } else {
            var exist = stderr.indexOf("<i n=\"code\">") != -1;
            if (shouldExist && !exist)
                error(arg + ":\nNo 'code' field found in the completion output:\n\n" + stderr, 1);
            else if (!shouldExist && exist)
                error(arg + ":\nThe 'code' field should not present in the completion output:\n\n" + stderr, 1);
        }
    }

    static function main() {
        test("EmptyString.hx", 60, false);
        test("MultiChar.hx", 60, false);
        test("SingleChar.hx", 60, true);
    }
}
