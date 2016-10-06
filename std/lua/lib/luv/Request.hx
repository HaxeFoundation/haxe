package lua.lib.luv;
@:luaRequire("luv")
extern class Request {
  function cancel() : Bool;
}
