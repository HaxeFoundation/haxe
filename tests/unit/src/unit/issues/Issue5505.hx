package unit.issues;

class Issue5505 extends Test {
  function test() {
#if (java || cs)
    eq(StringTools.urlEncode('~'), '~');
    eq(StringTools.urlDecode(StringTools.urlEncode('~')), '~');
    eq(StringTools.urlDecode(StringTools.urlEncode('-')), '-');
    eq(StringTools.urlDecode('%7E'), '~');
    eq(StringTools.urlDecode('%7e'), '~');
#end
  }
}
