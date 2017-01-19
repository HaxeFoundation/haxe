package lua.lib.luv.fs;

@:enum
abstract Open(String) {
  var ReadOnly                 = "r";
  var ReadOnlySync             = "rs";
  var ReadWrite                = "r+";
  var ReadWriteSync            = "rs+";
  var ReadWriteAppend          = "a+";
  var ReadWriteTruncate        = "w+";
  var ReadWriteTruncateNewFile = "wx+";
  var ReadWriteAppendNewFile   = "ax+";
  var WriteOnly                = "w";
  var WriteNewFile             = "wx";
  var Append                   = "a";
  var AppendNewFile            = "ax";
}
