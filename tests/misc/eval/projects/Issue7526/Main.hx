class C {

    static private var member: Int = 42;

}

abstract A(Any) {

	static private var member: Int = 42;

}

class Main {

    static function main() {
        (C: { member: Int }).member;
        (A: { member: Int }).member;
    }

}