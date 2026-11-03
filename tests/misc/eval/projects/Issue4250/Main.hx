typedef Node = { parent: Node };

class SomeNode {
    public var parent:SomeNode;
    public function new() { }
}

class Main {
    static function main() {
        var someNode = new SomeNode();
        var n:Node = someNode;
        n.parent = { parent: null };
    }
}