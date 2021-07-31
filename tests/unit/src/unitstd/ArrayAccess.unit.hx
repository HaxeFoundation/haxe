package tests.unit.src.unitstd;

extern inline overload function test1(access: ArrayAccess<Int>) return true;
extern inline overload function test1(access: Any) return false;

class Dummy1 implements ArrayAccess<Int> { public function new() {} }
class Dummy2 extends Dummy2 {}

class Dummy3<T> implements ArrayAccess<T> { public function new() {} }
class Dummy4<T> extends Dummy3<T> {}
class Dummy5 extends Dummy3<String> {}

test1([]) == true;
test1([1, 2, 3]) == true;
test1([1.2]) == false;

test1(new Dummy1()) == true;
test1(new Dummy2()) == true;
test1(new Dummy3<Int>()) == true;
test1(new Dummy3<String>()) == false;
test1(new Dummy4<Int>()) == true;
test1(new Dummy4<Float>()) == false;
test1(new Dummy5()) == false;

test1("abc") == false;
test1(null) == false;


extern inline overload function test2<T>(access: ArrayAccess<T>) return true;
extern inline overload function test2(access: Any) return false;

test2([]) == true;
test2([1, 2, 3]) == true;
test2([1.2]) == true;

test2(new Dummy1()) == true;
test2(new Dummy2()) == true;
test2(new Dummy3<Int>()) == true;
test2(new Dummy3<String>()) == true;
test2(new Dummy4<Int>()) == true;
test2(new Dummy4<Float>()) == true;
test2(new Dummy5()) == true;

test2("abc") == false;
test2(null) == false;


extern inline overload function test3<T: ArrayAccess<Int>>(access: T) return true;
extern inline overload function test3(access: Any) return false;

test3([]) == true;
test3([1, 2, 3]) == true;
test3([1.2]) == false;

test3(new Dummy1()) == true;
test3(new Dummy2()) == true;
test3(new Dummy3<Int>()) == true;
test3(new Dummy3<String>()) == false;
test3(new Dummy4<Int>()) == true;
test3(new Dummy4<Float>()) == false;
test3(new Dummy5()) == false;

test3("abc") == false;
test3(null) == false;


extern inline overload function test4<T, U: ArrayAccess<T>>(access: U) return true;
extern inline overload function test4(access: Any) return false;

test4([]) == true;
test4([1, 2, 3]) == true;
test4([1.2]) == true;

test4(new Dummy1()) == true;
test4(new Dummy2()) == true;
test4(new Dummy3<Int>()) == true;
test4(new Dummy3<String>()) == true;
test4(new Dummy4<Int>()) == true;
test4(new Dummy4<Float>()) == true;
test4(new Dummy5()) == true;

test4("abc") == false;
test4(null) == false;