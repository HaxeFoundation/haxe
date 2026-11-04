interface I {
    public function test<T>():Void;
}

class C implements I {
    public function test<T:String>() { }
}

function main() {

}