class Main {
    static function main() {
    }

    function f(v:Int):String {
        switch (v) {
            case 1:
                var f = function() return 1;
                if (Math.random() > 0.5)
                    return "";
            default:
                return null;
        }
    }
}