class Promise<T> {
    public function then<TOut>():Promise<TOut> { return null; }
}

typedef Thenable<T> = {
    function then():Thenable<T>;
}

class Issue6038 {
    static function main() {
        var p:Promise<Int> = null;
        var t:Thenable<Int> = p;
    }
}