package cpp.link;

@:cppFileCode( 'extern "C" int zlib_register_prims();')
@:buildXml("
<target id='haxe'>
  <lib name='${HXCPP}/lib/${BINDIR}/libzlib${LIBEXTRA}${LIBEXT}'/>
</target>
")
@:keep class StaticZlib
{
   static function __init__()
   {
     untyped __cpp__("zlib_register_prims();");
   }
}

