package cpp.link;

@:cppFileCode( 'extern "C" int regexp_register_prims();')
@:buildXml("
<target id='haxe'>
  <lib name='${HXCPP}/lib/${BINDIR}/libregexp${LIBEXTRA}${LIBEXT}'/>
</target>
")
@:keep class StaticRegexp
{
   static function __init__()
   {
     untyped __cpp__("regexp_register_prims();");
   }
}

