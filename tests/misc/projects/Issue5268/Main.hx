import haxe.ds.Option;

class Main {
    public static function main() {
        return switch outcome() {
            case Some(v):
                var value = switch v {
                    case 1: 1;
                    case m: return None;
                }
                value;
            case None:
                2;
        }
    }

    static function outcome()
        return Some(1);
}