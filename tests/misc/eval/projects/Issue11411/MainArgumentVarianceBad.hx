class Parent {}
class Child extends Parent {}

interface I {
    public function test<T:Parent>(t:T):Void;
}

class C implements I {
    public function test<T:Child>(t:T) { }
}

function main() {

}