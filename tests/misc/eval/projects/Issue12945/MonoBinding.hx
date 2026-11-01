function f(?a:{x:Int, y:Int}, b:{x:String}) {}

function main() {
  var v = null;     // v : M  (unbound monomorph)
  f({x: v});        // {x: M}
  $type(v);
}
