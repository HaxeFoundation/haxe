package unit.issues;

class Issue5090 extends Test {
  public function test() {
    eq(haxe.crypto.Sha1.encode('olá'), '3e16889d494632f3c528fb3a3756435f8aa88788');
    eq(haxe.crypto.Sha1.encode('𝄞'), 'e4a5ca681e4b2fa112844dca54e7b868086aee0b');
  }
}
