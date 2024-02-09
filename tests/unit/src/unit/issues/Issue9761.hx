package unit.issues;

class Issue9761 extends Test {
  function test() {
    ((null: Array<X>): Array<Y>);
    ((null: Array<Y>): Array<X>);

    ((null: Array<XT<Int>>): Array<YT<Int>>);
    ((null: Array<YT<Int>>): Array<XT<Int>>);

    ((null: Array<XT<XT<Int>>>): Array<XT<YT<Int>>>);
    ((null: Array<XT<XT<Int>>>): Array<YT<XT<Int>>>);
    ((null: Array<XT<XT<Int>>>): Array<YT<YT<Int>>>);

    ((null: Array<XT<YT<Int>>>): Array<XT<XT<Int>>>);
    ((null: Array<XT<YT<Int>>>): Array<YT<XT<Int>>>);
    ((null: Array<XT<YT<Int>>>): Array<YT<YT<Int>>>);

    ((null: Array<YT<XT<Int>>>): Array<XT<YT<Int>>>);
    ((null: Array<YT<XT<Int>>>): Array<XT<XT<Int>>>);
    ((null: Array<YT<XT<Int>>>): Array<YT<YT<Int>>>);

    ((null: Array<YT<YT<Int>>>): Array<XT<YT<Int>>>);
    ((null: Array<YT<YT<Int>>>): Array<YT<XT<Int>>>);
    ((null: Array<YT<YT<Int>>>): Array<XT<XT<Int>>>);

    ((null: X): Z);

    noAssert();
  }
}

private typedef X = {?x: X}
private abstract Y(X) from X to X {}
private abstract Z({?x: Z}) from X to X {}

private typedef XT<T> = {?x: XT<T>, ?t: T}
private abstract YT<T>(XT<T>) from XT<T> to XT<T> {}
