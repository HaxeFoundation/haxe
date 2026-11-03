import haxe.ds.Option;

class Main {
    static function main() {
        var a = Some(10);
        switch (a) {
            case None:
            case Some(v):
        }
    }
}