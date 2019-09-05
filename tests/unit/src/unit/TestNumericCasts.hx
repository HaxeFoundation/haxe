// This file is auto-generated from RunCastGenerator.hx - do not edit!
package unit;
#if java
import java.StdTypes;
private typedef Int32 = Int;
private typedef Float32 = Single;
private typedef Float64 = Float;
#else
private typedef Int8 = Int;
private typedef Int16 = Int;
private typedef Int32 = Int;
private typedef Int64 = Int;
private typedef Float32 = Float;
private typedef Float64 = Float;
#end
private class CastHelper {
	static public var nullOr0 = #if target.static 0 #else null #end;
}
class TestNumericCasts extends unit.Test {
	static function Int8_Int16(v:Int8):Int16 return cast v;
	static function Int8_Int32(v:Int8):Int32 return cast v;
	static function Int8_Int64(v:Int8):Int64 return cast v;
	static function Int8_Float32(v:Int8):Float32 return cast v;
	static function Int8_Float64(v:Int8):Float64 return cast v;
	static function Int8_BoxedInt8(v:Int8):Null<Int8> return cast v;
	static function Int8_BoxedInt16(v:Int8):Null<Int16> return cast v;
	static function Int8_BoxedInt32(v:Int8):Null<Int32> return cast v;
	static function Int8_BoxedInt64(v:Int8):Null<Int64> return cast v;
	static function Int8_BoxedFloat32(v:Int8):Null<Float32> return cast v;
	static function Int8_BoxedFloat64(v:Int8):Null<Float64> return cast v;
	static function Int16_Int8(v:Int16):Int8 return cast v;
	static function Int16_Int32(v:Int16):Int32 return cast v;
	static function Int16_Int64(v:Int16):Int64 return cast v;
	static function Int16_Float32(v:Int16):Float32 return cast v;
	static function Int16_Float64(v:Int16):Float64 return cast v;
	static function Int16_BoxedInt8(v:Int16):Null<Int8> return cast v;
	static function Int16_BoxedInt16(v:Int16):Null<Int16> return cast v;
	static function Int16_BoxedInt32(v:Int16):Null<Int32> return cast v;
	static function Int16_BoxedInt64(v:Int16):Null<Int64> return cast v;
	static function Int16_BoxedFloat32(v:Int16):Null<Float32> return cast v;
	static function Int16_BoxedFloat64(v:Int16):Null<Float64> return cast v;
	static function Int32_Int8(v:Int32):Int8 return cast v;
	static function Int32_Int16(v:Int32):Int16 return cast v;
	static function Int32_Int64(v:Int32):Int64 return cast v;
	static function Int32_Float32(v:Int32):Float32 return cast v;
	static function Int32_Float64(v:Int32):Float64 return cast v;
	static function Int32_BoxedInt8(v:Int32):Null<Int8> return cast v;
	static function Int32_BoxedInt16(v:Int32):Null<Int16> return cast v;
	static function Int32_BoxedInt32(v:Int32):Null<Int32> return cast v;
	static function Int32_BoxedInt64(v:Int32):Null<Int64> return cast v;
	static function Int32_BoxedFloat32(v:Int32):Null<Float32> return cast v;
	static function Int32_BoxedFloat64(v:Int32):Null<Float64> return cast v;
	static function Int64_Int8(v:Int64):Int8 return cast v;
	static function Int64_Int16(v:Int64):Int16 return cast v;
	static function Int64_Int32(v:Int64):Int32 return cast v;
	static function Int64_Float32(v:Int64):Float32 return cast v;
	static function Int64_Float64(v:Int64):Float64 return cast v;
	static function Int64_BoxedInt8(v:Int64):Null<Int8> return cast v;
	static function Int64_BoxedInt16(v:Int64):Null<Int16> return cast v;
	static function Int64_BoxedInt32(v:Int64):Null<Int32> return cast v;
	static function Int64_BoxedInt64(v:Int64):Null<Int64> return cast v;
	static function Int64_BoxedFloat32(v:Int64):Null<Float32> return cast v;
	static function Int64_BoxedFloat64(v:Int64):Null<Float64> return cast v;
	static function Float32_Int8(v:Float32):Int8 return cast v;
	static function Float32_Int16(v:Float32):Int16 return cast v;
	static function Float32_Int32(v:Float32):Int32 return cast v;
	static function Float32_Int64(v:Float32):Int64 return cast v;
	static function Float32_Float64(v:Float32):Float64 return cast v;
	static function Float32_BoxedInt8(v:Float32):Null<Int8> return cast v;
	static function Float32_BoxedInt16(v:Float32):Null<Int16> return cast v;
	static function Float32_BoxedInt32(v:Float32):Null<Int32> return cast v;
	static function Float32_BoxedInt64(v:Float32):Null<Int64> return cast v;
	static function Float32_BoxedFloat32(v:Float32):Null<Float32> return cast v;
	static function Float32_BoxedFloat64(v:Float32):Null<Float64> return cast v;
	static function Float64_Int8(v:Float64):Int8 return cast v;
	static function Float64_Int16(v:Float64):Int16 return cast v;
	static function Float64_Int32(v:Float64):Int32 return cast v;
	static function Float64_Int64(v:Float64):Int64 return cast v;
	static function Float64_Float32(v:Float64):Float32 return cast v;
	static function Float64_BoxedInt8(v:Float64):Null<Int8> return cast v;
	static function Float64_BoxedInt16(v:Float64):Null<Int16> return cast v;
	static function Float64_BoxedInt32(v:Float64):Null<Int32> return cast v;
	static function Float64_BoxedInt64(v:Float64):Null<Int64> return cast v;
	static function Float64_BoxedFloat32(v:Float64):Null<Float32> return cast v;
	static function Float64_BoxedFloat64(v:Float64):Null<Float64> return cast v;
	static function BoxedInt8_Int8(v:Null<Int8>):Int8 return cast v;
	static function BoxedInt8_Int16(v:Null<Int8>):Int16 return cast v;
	static function BoxedInt8_Int32(v:Null<Int8>):Int32 return cast v;
	static function BoxedInt8_Int64(v:Null<Int8>):Int64 return cast v;
	static function BoxedInt8_Float32(v:Null<Int8>):Float32 return cast v;
	static function BoxedInt8_Float64(v:Null<Int8>):Float64 return cast v;
	static function BoxedInt8_BoxedInt16(v:Null<Int8>):Null<Int16> return cast v;
	static function BoxedInt8_BoxedInt32(v:Null<Int8>):Null<Int32> return cast v;
	static function BoxedInt8_BoxedInt64(v:Null<Int8>):Null<Int64> return cast v;
	static function BoxedInt8_BoxedFloat32(v:Null<Int8>):Null<Float32> return cast v;
	static function BoxedInt8_BoxedFloat64(v:Null<Int8>):Null<Float64> return cast v;
	static function BoxedInt16_Int8(v:Null<Int16>):Int8 return cast v;
	static function BoxedInt16_Int16(v:Null<Int16>):Int16 return cast v;
	static function BoxedInt16_Int32(v:Null<Int16>):Int32 return cast v;
	static function BoxedInt16_Int64(v:Null<Int16>):Int64 return cast v;
	static function BoxedInt16_Float32(v:Null<Int16>):Float32 return cast v;
	static function BoxedInt16_Float64(v:Null<Int16>):Float64 return cast v;
	static function BoxedInt16_BoxedInt8(v:Null<Int16>):Null<Int8> return cast v;
	static function BoxedInt16_BoxedInt32(v:Null<Int16>):Null<Int32> return cast v;
	static function BoxedInt16_BoxedInt64(v:Null<Int16>):Null<Int64> return cast v;
	static function BoxedInt16_BoxedFloat32(v:Null<Int16>):Null<Float32> return cast v;
	static function BoxedInt16_BoxedFloat64(v:Null<Int16>):Null<Float64> return cast v;
	static function BoxedInt32_Int8(v:Null<Int32>):Int8 return cast v;
	static function BoxedInt32_Int16(v:Null<Int32>):Int16 return cast v;
	static function BoxedInt32_Int32(v:Null<Int32>):Int32 return cast v;
	static function BoxedInt32_Int64(v:Null<Int32>):Int64 return cast v;
	static function BoxedInt32_Float32(v:Null<Int32>):Float32 return cast v;
	static function BoxedInt32_Float64(v:Null<Int32>):Float64 return cast v;
	static function BoxedInt32_BoxedInt8(v:Null<Int32>):Null<Int8> return cast v;
	static function BoxedInt32_BoxedInt16(v:Null<Int32>):Null<Int16> return cast v;
	static function BoxedInt32_BoxedInt64(v:Null<Int32>):Null<Int64> return cast v;
	static function BoxedInt32_BoxedFloat32(v:Null<Int32>):Null<Float32> return cast v;
	static function BoxedInt32_BoxedFloat64(v:Null<Int32>):Null<Float64> return cast v;
	static function BoxedInt64_Int8(v:Null<Int64>):Int8 return cast v;
	static function BoxedInt64_Int16(v:Null<Int64>):Int16 return cast v;
	static function BoxedInt64_Int32(v:Null<Int64>):Int32 return cast v;
	static function BoxedInt64_Int64(v:Null<Int64>):Int64 return cast v;
	static function BoxedInt64_Float32(v:Null<Int64>):Float32 return cast v;
	static function BoxedInt64_Float64(v:Null<Int64>):Float64 return cast v;
	static function BoxedInt64_BoxedInt8(v:Null<Int64>):Null<Int8> return cast v;
	static function BoxedInt64_BoxedInt16(v:Null<Int64>):Null<Int16> return cast v;
	static function BoxedInt64_BoxedInt32(v:Null<Int64>):Null<Int32> return cast v;
	static function BoxedInt64_BoxedFloat32(v:Null<Int64>):Null<Float32> return cast v;
	static function BoxedInt64_BoxedFloat64(v:Null<Int64>):Null<Float64> return cast v;
	static function BoxedFloat32_Int8(v:Null<Float32>):Int8 return cast v;
	static function BoxedFloat32_Int16(v:Null<Float32>):Int16 return cast v;
	static function BoxedFloat32_Int32(v:Null<Float32>):Int32 return cast v;
	static function BoxedFloat32_Int64(v:Null<Float32>):Int64 return cast v;
	static function BoxedFloat32_Float32(v:Null<Float32>):Float32 return cast v;
	static function BoxedFloat32_Float64(v:Null<Float32>):Float64 return cast v;
	static function BoxedFloat32_BoxedInt8(v:Null<Float32>):Null<Int8> return cast v;
	static function BoxedFloat32_BoxedInt16(v:Null<Float32>):Null<Int16> return cast v;
	static function BoxedFloat32_BoxedInt32(v:Null<Float32>):Null<Int32> return cast v;
	static function BoxedFloat32_BoxedInt64(v:Null<Float32>):Null<Int64> return cast v;
	static function BoxedFloat32_BoxedFloat64(v:Null<Float32>):Null<Float64> return cast v;
	static function BoxedFloat64_Int8(v:Null<Float64>):Int8 return cast v;
	static function BoxedFloat64_Int16(v:Null<Float64>):Int16 return cast v;
	static function BoxedFloat64_Int32(v:Null<Float64>):Int32 return cast v;
	static function BoxedFloat64_Int64(v:Null<Float64>):Int64 return cast v;
	static function BoxedFloat64_Float32(v:Null<Float64>):Float32 return cast v;
	static function BoxedFloat64_Float64(v:Null<Float64>):Float64 return cast v;
	static function BoxedFloat64_BoxedInt8(v:Null<Float64>):Null<Int8> return cast v;
	static function BoxedFloat64_BoxedInt16(v:Null<Float64>):Null<Int16> return cast v;
	static function BoxedFloat64_BoxedInt32(v:Null<Float64>):Null<Int32> return cast v;
	static function BoxedFloat64_BoxedInt64(v:Null<Float64>):Null<Int64> return cast v;
	static function BoxedFloat64_BoxedFloat32(v:Null<Float64>):Null<Float32> return cast v;
	public function test() {
		{
			deq(0, Int8_Int16(0));
			deq(1, Int8_Int16(1));
			deq(0, Int8_Int32(0));
			deq(1, Int8_Int32(1));
			deq(0, Int8_Int64(0));
			deq(1, Int8_Int64(1));
			deq(0, Int8_Float32(0));
			deq(1, Int8_Float32(1));
			deq(0, Int8_Float64(0));
			deq(1, Int8_Float64(1));
			deq(0, Int8_BoxedInt8(0));
			deq(1, Int8_BoxedInt8(1));
			deq(0, Int8_BoxedInt16(0));
			deq(1, Int8_BoxedInt16(1));
			deq(0, Int8_BoxedInt32(0));
			deq(1, Int8_BoxedInt32(1));
			deq(0, Int8_BoxedInt64(0));
			deq(1, Int8_BoxedInt64(1));
			deq(0, Int8_BoxedFloat32(0));
			deq(1, Int8_BoxedFloat32(1));
			deq(0, Int8_BoxedFloat64(0));
			deq(1, Int8_BoxedFloat64(1));
			deq(0, Int16_Int8(0));
			deq(1, Int16_Int8(1));
			deq(0, Int16_Int32(0));
			deq(1, Int16_Int32(1));
			deq(0, Int16_Int64(0));
			deq(1, Int16_Int64(1));
			deq(0, Int16_Float32(0));
			deq(1, Int16_Float32(1));
			deq(0, Int16_Float64(0));
			deq(1, Int16_Float64(1));
			deq(0, Int16_BoxedInt8(0));
			deq(1, Int16_BoxedInt8(1));
			deq(0, Int16_BoxedInt16(0));
			deq(1, Int16_BoxedInt16(1));
			deq(0, Int16_BoxedInt32(0));
			deq(1, Int16_BoxedInt32(1));
			deq(0, Int16_BoxedInt64(0));
			deq(1, Int16_BoxedInt64(1));
			deq(0, Int16_BoxedFloat32(0));
			deq(1, Int16_BoxedFloat32(1));
			deq(0, Int16_BoxedFloat64(0));
			deq(1, Int16_BoxedFloat64(1));
			deq(0, Int32_Int8(0));
			deq(1, Int32_Int8(1));
			deq(0, Int32_Int16(0));
			deq(1, Int32_Int16(1));
			deq(0, Int32_Int64(0));
			deq(1, Int32_Int64(1));
			deq(0, Int32_Float32(0));
			deq(1, Int32_Float32(1));
			deq(0, Int32_Float64(0));
			deq(1, Int32_Float64(1));
			deq(0, Int32_BoxedInt8(0));
			deq(1, Int32_BoxedInt8(1));
			deq(0, Int32_BoxedInt16(0));
			deq(1, Int32_BoxedInt16(1));
			deq(0, Int32_BoxedInt32(0));
			deq(1, Int32_BoxedInt32(1));
			deq(0, Int32_BoxedInt64(0));
			deq(1, Int32_BoxedInt64(1));
			deq(0, Int32_BoxedFloat32(0));
			deq(1, Int32_BoxedFloat32(1));
			deq(0, Int32_BoxedFloat64(0));
			deq(1, Int32_BoxedFloat64(1));
			deq(0, Int64_Int8(0));
			deq(1, Int64_Int8(1));
			deq(0, Int64_Int16(0));
			deq(1, Int64_Int16(1));
			deq(0, Int64_Int32(0));
			deq(1, Int64_Int32(1));
			deq(0, Int64_Float32(0));
			deq(1, Int64_Float32(1));
			deq(0, Int64_Float64(0));
			deq(1, Int64_Float64(1));
			deq(0, Int64_BoxedInt8(0));
			deq(1, Int64_BoxedInt8(1));
			deq(0, Int64_BoxedInt16(0));
			deq(1, Int64_BoxedInt16(1));
			deq(0, Int64_BoxedInt32(0));
			deq(1, Int64_BoxedInt32(1));
			deq(0, Int64_BoxedInt64(0));
			deq(1, Int64_BoxedInt64(1));
			deq(0, Int64_BoxedFloat32(0));
			deq(1, Int64_BoxedFloat32(1));
			deq(0, Int64_BoxedFloat64(0));
			deq(1, Int64_BoxedFloat64(1));
			deq(0, Float32_Int8(0));
			deq(1, Float32_Int8(1));
			deq(0., Float32_Int8(0.));
			deq(1., Float32_Int8(1.));
			deq(0, Float32_Int16(0));
			deq(1, Float32_Int16(1));
			deq(0., Float32_Int16(0.));
			deq(1., Float32_Int16(1.));
			deq(0, Float32_Int32(0));
			deq(1, Float32_Int32(1));
			deq(0., Float32_Int32(0.));
			deq(1., Float32_Int32(1.));
			deq(0, Float32_Int64(0));
			deq(1, Float32_Int64(1));
			deq(0., Float32_Int64(0.));
			deq(1., Float32_Int64(1.));
			deq(0, Float32_Float64(0));
			deq(1, Float32_Float64(1));
			deq(0., Float32_Float64(0.));
			deq(1., Float32_Float64(1.));
			deq(0, Float32_BoxedInt8(0));
			deq(1, Float32_BoxedInt8(1));
			deq(0., Float32_BoxedInt8(0.));
			deq(1., Float32_BoxedInt8(1.));
			deq(0, Float32_BoxedInt16(0));
			deq(1, Float32_BoxedInt16(1));
			deq(0., Float32_BoxedInt16(0.));
			deq(1., Float32_BoxedInt16(1.));
			deq(0, Float32_BoxedInt32(0));
			deq(1, Float32_BoxedInt32(1));
			deq(0., Float32_BoxedInt32(0.));
			deq(1., Float32_BoxedInt32(1.));
			deq(0, Float32_BoxedInt64(0));
			deq(1, Float32_BoxedInt64(1));
			deq(0., Float32_BoxedInt64(0.));
			deq(1., Float32_BoxedInt64(1.));
			deq(0, Float32_BoxedFloat32(0));
			deq(1, Float32_BoxedFloat32(1));
			deq(0., Float32_BoxedFloat32(0.));
			deq(1., Float32_BoxedFloat32(1.));
			deq(0, Float32_BoxedFloat64(0));
			deq(1, Float32_BoxedFloat64(1));
			deq(0., Float32_BoxedFloat64(0.));
			deq(1., Float32_BoxedFloat64(1.));
			deq(0, Float64_Int8(0));
			deq(1, Float64_Int8(1));
			deq(0., Float64_Int8(0.));
			deq(1., Float64_Int8(1.));
			deq(0, Float64_Int16(0));
			deq(1, Float64_Int16(1));
			deq(0., Float64_Int16(0.));
			deq(1., Float64_Int16(1.));
			deq(0, Float64_Int32(0));
			deq(1, Float64_Int32(1));
			deq(0., Float64_Int32(0.));
			deq(1., Float64_Int32(1.));
			deq(0, Float64_Int64(0));
			deq(1, Float64_Int64(1));
			deq(0., Float64_Int64(0.));
			deq(1., Float64_Int64(1.));
			deq(0, Float64_Float32(0));
			deq(1, Float64_Float32(1));
			deq(0., Float64_Float32(0.));
			deq(1., Float64_Float32(1.));
			deq(0, Float64_BoxedInt8(0));
			deq(1, Float64_BoxedInt8(1));
			deq(0., Float64_BoxedInt8(0.));
			deq(1., Float64_BoxedInt8(1.));
			deq(0, Float64_BoxedInt16(0));
			deq(1, Float64_BoxedInt16(1));
			deq(0., Float64_BoxedInt16(0.));
			deq(1., Float64_BoxedInt16(1.));
			deq(0, Float64_BoxedInt32(0));
			deq(1, Float64_BoxedInt32(1));
			deq(0., Float64_BoxedInt32(0.));
			deq(1., Float64_BoxedInt32(1.));
			deq(0, Float64_BoxedInt64(0));
			deq(1, Float64_BoxedInt64(1));
			deq(0., Float64_BoxedInt64(0.));
			deq(1., Float64_BoxedInt64(1.));
			deq(0, Float64_BoxedFloat32(0));
			deq(1, Float64_BoxedFloat32(1));
			deq(0., Float64_BoxedFloat32(0.));
			deq(1., Float64_BoxedFloat32(1.));
			deq(0, Float64_BoxedFloat64(0));
			deq(1, Float64_BoxedFloat64(1));
			deq(0., Float64_BoxedFloat64(0.));
			deq(1., Float64_BoxedFloat64(1.));
			deq(0, BoxedInt8_Int8(0));
			deq(1, BoxedInt8_Int8(1));
			deq(CastHelper.nullOr0, BoxedInt8_Int8(null));
			deq(0, BoxedInt8_Int16(0));
			deq(1, BoxedInt8_Int16(1));
			deq(CastHelper.nullOr0, BoxedInt8_Int16(null));
			deq(0, BoxedInt8_Int32(0));
			deq(1, BoxedInt8_Int32(1));
			deq(CastHelper.nullOr0, BoxedInt8_Int32(null));
			deq(0, BoxedInt8_Int64(0));
			deq(1, BoxedInt8_Int64(1));
			deq(CastHelper.nullOr0, BoxedInt8_Int64(null));
			deq(0, BoxedInt8_Float32(0));
			deq(1, BoxedInt8_Float32(1));
			deq(CastHelper.nullOr0, BoxedInt8_Float32(null));
			deq(0, BoxedInt8_Float64(0));
			deq(1, BoxedInt8_Float64(1));
			deq(CastHelper.nullOr0, BoxedInt8_Float64(null));
			deq(0, BoxedInt8_BoxedInt16(0));
			deq(1, BoxedInt8_BoxedInt16(1));
			deq(null, BoxedInt8_BoxedInt16(null));
			deq(0, BoxedInt8_BoxedInt32(0));
			deq(1, BoxedInt8_BoxedInt32(1));
			deq(null, BoxedInt8_BoxedInt32(null));
			deq(0, BoxedInt8_BoxedInt64(0));
			deq(1, BoxedInt8_BoxedInt64(1));
			deq(null, BoxedInt8_BoxedInt64(null));
			deq(0, BoxedInt8_BoxedFloat32(0));
			deq(1, BoxedInt8_BoxedFloat32(1));
			deq(null, BoxedInt8_BoxedFloat32(null));
			deq(0, BoxedInt8_BoxedFloat64(0));
			deq(1, BoxedInt8_BoxedFloat64(1));
			deq(null, BoxedInt8_BoxedFloat64(null));
			deq(0, BoxedInt16_Int8(0));
			deq(1, BoxedInt16_Int8(1));
			deq(CastHelper.nullOr0, BoxedInt16_Int8(null));
			deq(0, BoxedInt16_Int16(0));
			deq(1, BoxedInt16_Int16(1));
			deq(CastHelper.nullOr0, BoxedInt16_Int16(null));
			deq(0, BoxedInt16_Int32(0));
			deq(1, BoxedInt16_Int32(1));
			deq(CastHelper.nullOr0, BoxedInt16_Int32(null));
			deq(0, BoxedInt16_Int64(0));
			deq(1, BoxedInt16_Int64(1));
			deq(CastHelper.nullOr0, BoxedInt16_Int64(null));
			deq(0, BoxedInt16_Float32(0));
			deq(1, BoxedInt16_Float32(1));
			deq(CastHelper.nullOr0, BoxedInt16_Float32(null));
			deq(0, BoxedInt16_Float64(0));
			deq(1, BoxedInt16_Float64(1));
			deq(CastHelper.nullOr0, BoxedInt16_Float64(null));
			deq(0, BoxedInt16_BoxedInt8(0));
			deq(1, BoxedInt16_BoxedInt8(1));
			deq(null, BoxedInt16_BoxedInt8(null));
			deq(0, BoxedInt16_BoxedInt32(0));
			deq(1, BoxedInt16_BoxedInt32(1));
			deq(null, BoxedInt16_BoxedInt32(null));
			deq(0, BoxedInt16_BoxedInt64(0));
			deq(1, BoxedInt16_BoxedInt64(1));
			deq(null, BoxedInt16_BoxedInt64(null));
			deq(0, BoxedInt16_BoxedFloat32(0));
			deq(1, BoxedInt16_BoxedFloat32(1));
			deq(null, BoxedInt16_BoxedFloat32(null));
			deq(0, BoxedInt16_BoxedFloat64(0));
			deq(1, BoxedInt16_BoxedFloat64(1));
			deq(null, BoxedInt16_BoxedFloat64(null));
			deq(0, BoxedInt32_Int8(0));
			deq(1, BoxedInt32_Int8(1));
			deq(CastHelper.nullOr0, BoxedInt32_Int8(null));
			deq(0, BoxedInt32_Int16(0));
			deq(1, BoxedInt32_Int16(1));
			deq(CastHelper.nullOr0, BoxedInt32_Int16(null));
			deq(0, BoxedInt32_Int32(0));
			deq(1, BoxedInt32_Int32(1));
			deq(CastHelper.nullOr0, BoxedInt32_Int32(null));
			deq(0, BoxedInt32_Int64(0));
			deq(1, BoxedInt32_Int64(1));
			deq(CastHelper.nullOr0, BoxedInt32_Int64(null));
			deq(0, BoxedInt32_Float32(0));
			deq(1, BoxedInt32_Float32(1));
			deq(CastHelper.nullOr0, BoxedInt32_Float32(null));
			deq(0, BoxedInt32_Float64(0));
			deq(1, BoxedInt32_Float64(1));
			deq(CastHelper.nullOr0, BoxedInt32_Float64(null));
			deq(0, BoxedInt32_BoxedInt8(0));
			deq(1, BoxedInt32_BoxedInt8(1));
			deq(null, BoxedInt32_BoxedInt8(null));
			deq(0, BoxedInt32_BoxedInt16(0));
			deq(1, BoxedInt32_BoxedInt16(1));
			deq(null, BoxedInt32_BoxedInt16(null));
			deq(0, BoxedInt32_BoxedInt64(0));
			deq(1, BoxedInt32_BoxedInt64(1));
			deq(null, BoxedInt32_BoxedInt64(null));
			deq(0, BoxedInt32_BoxedFloat32(0));
			deq(1, BoxedInt32_BoxedFloat32(1));
			deq(null, BoxedInt32_BoxedFloat32(null));
			deq(0, BoxedInt32_BoxedFloat64(0));
			deq(1, BoxedInt32_BoxedFloat64(1));
			deq(null, BoxedInt32_BoxedFloat64(null));
			deq(0, BoxedInt64_Int8(0));
			deq(1, BoxedInt64_Int8(1));
			deq(CastHelper.nullOr0, BoxedInt64_Int8(null));
			deq(0, BoxedInt64_Int16(0));
			deq(1, BoxedInt64_Int16(1));
			deq(CastHelper.nullOr0, BoxedInt64_Int16(null));
			deq(0, BoxedInt64_Int32(0));
			deq(1, BoxedInt64_Int32(1));
			deq(CastHelper.nullOr0, BoxedInt64_Int32(null));
			deq(0, BoxedInt64_Int64(0));
			deq(1, BoxedInt64_Int64(1));
			deq(CastHelper.nullOr0, BoxedInt64_Int64(null));
			deq(0, BoxedInt64_Float32(0));
			deq(1, BoxedInt64_Float32(1));
			deq(CastHelper.nullOr0, BoxedInt64_Float32(null));
			deq(0, BoxedInt64_Float64(0));
			deq(1, BoxedInt64_Float64(1));
			deq(CastHelper.nullOr0, BoxedInt64_Float64(null));
			deq(0, BoxedInt64_BoxedInt8(0));
			deq(1, BoxedInt64_BoxedInt8(1));
			deq(null, BoxedInt64_BoxedInt8(null));
			deq(0, BoxedInt64_BoxedInt16(0));
			deq(1, BoxedInt64_BoxedInt16(1));
			deq(null, BoxedInt64_BoxedInt16(null));
			deq(0, BoxedInt64_BoxedInt32(0));
			deq(1, BoxedInt64_BoxedInt32(1));
			deq(null, BoxedInt64_BoxedInt32(null));
			deq(0, BoxedInt64_BoxedFloat32(0));
			deq(1, BoxedInt64_BoxedFloat32(1));
			deq(null, BoxedInt64_BoxedFloat32(null));
			deq(0, BoxedInt64_BoxedFloat64(0));
			deq(1, BoxedInt64_BoxedFloat64(1));
			deq(null, BoxedInt64_BoxedFloat64(null));
			deq(0, BoxedFloat32_Int8(0));
			deq(1, BoxedFloat32_Int8(1));
			deq(0., BoxedFloat32_Int8(0.));
			deq(1., BoxedFloat32_Int8(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Int8(null));
			deq(0, BoxedFloat32_Int16(0));
			deq(1, BoxedFloat32_Int16(1));
			deq(0., BoxedFloat32_Int16(0.));
			deq(1., BoxedFloat32_Int16(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Int16(null));
			deq(0, BoxedFloat32_Int32(0));
			deq(1, BoxedFloat32_Int32(1));
			deq(0., BoxedFloat32_Int32(0.));
			deq(1., BoxedFloat32_Int32(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Int32(null));
			deq(0, BoxedFloat32_Int64(0));
			deq(1, BoxedFloat32_Int64(1));
			deq(0., BoxedFloat32_Int64(0.));
			deq(1., BoxedFloat32_Int64(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Int64(null));
			deq(0, BoxedFloat32_Float32(0));
			deq(1, BoxedFloat32_Float32(1));
			deq(0., BoxedFloat32_Float32(0.));
			deq(1., BoxedFloat32_Float32(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Float32(null));
			deq(0, BoxedFloat32_Float64(0));
			deq(1, BoxedFloat32_Float64(1));
			deq(0., BoxedFloat32_Float64(0.));
			deq(1., BoxedFloat32_Float64(1.));
			deq(CastHelper.nullOr0, BoxedFloat32_Float64(null));
			deq(0, BoxedFloat32_BoxedInt8(0));
			deq(1, BoxedFloat32_BoxedInt8(1));
			deq(0., BoxedFloat32_BoxedInt8(0.));
			deq(1., BoxedFloat32_BoxedInt8(1.));
			deq(null, BoxedFloat32_BoxedInt8(null));
			deq(0, BoxedFloat32_BoxedInt16(0));
			deq(1, BoxedFloat32_BoxedInt16(1));
			deq(0., BoxedFloat32_BoxedInt16(0.));
			deq(1., BoxedFloat32_BoxedInt16(1.));
			deq(null, BoxedFloat32_BoxedInt16(null));
			deq(0, BoxedFloat32_BoxedInt32(0));
			deq(1, BoxedFloat32_BoxedInt32(1));
			deq(0., BoxedFloat32_BoxedInt32(0.));
			deq(1., BoxedFloat32_BoxedInt32(1.));
			deq(null, BoxedFloat32_BoxedInt32(null));
			deq(0, BoxedFloat32_BoxedInt64(0));
			deq(1, BoxedFloat32_BoxedInt64(1));
			deq(0., BoxedFloat32_BoxedInt64(0.));
			deq(1., BoxedFloat32_BoxedInt64(1.));
			deq(null, BoxedFloat32_BoxedInt64(null));
			deq(0, BoxedFloat32_BoxedFloat64(0));
			deq(1, BoxedFloat32_BoxedFloat64(1));
			deq(0., BoxedFloat32_BoxedFloat64(0.));
			deq(1., BoxedFloat32_BoxedFloat64(1.));
			deq(null, BoxedFloat32_BoxedFloat64(null));
			deq(0, BoxedFloat64_Int8(0));
			deq(1, BoxedFloat64_Int8(1));
			deq(0., BoxedFloat64_Int8(0.));
			deq(1., BoxedFloat64_Int8(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Int8(null));
			deq(0, BoxedFloat64_Int16(0));
			deq(1, BoxedFloat64_Int16(1));
			deq(0., BoxedFloat64_Int16(0.));
			deq(1., BoxedFloat64_Int16(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Int16(null));
			deq(0, BoxedFloat64_Int32(0));
			deq(1, BoxedFloat64_Int32(1));
			deq(0., BoxedFloat64_Int32(0.));
			deq(1., BoxedFloat64_Int32(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Int32(null));
			deq(0, BoxedFloat64_Int64(0));
			deq(1, BoxedFloat64_Int64(1));
			deq(0., BoxedFloat64_Int64(0.));
			deq(1., BoxedFloat64_Int64(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Int64(null));
			deq(0, BoxedFloat64_Float32(0));
			deq(1, BoxedFloat64_Float32(1));
			deq(0., BoxedFloat64_Float32(0.));
			deq(1., BoxedFloat64_Float32(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Float32(null));
			deq(0, BoxedFloat64_Float64(0));
			deq(1, BoxedFloat64_Float64(1));
			deq(0., BoxedFloat64_Float64(0.));
			deq(1., BoxedFloat64_Float64(1.));
			deq(CastHelper.nullOr0, BoxedFloat64_Float64(null));
			deq(0, BoxedFloat64_BoxedInt8(0));
			deq(1, BoxedFloat64_BoxedInt8(1));
			deq(0., BoxedFloat64_BoxedInt8(0.));
			deq(1., BoxedFloat64_BoxedInt8(1.));
			deq(null, BoxedFloat64_BoxedInt8(null));
			deq(0, BoxedFloat64_BoxedInt16(0));
			deq(1, BoxedFloat64_BoxedInt16(1));
			deq(0., BoxedFloat64_BoxedInt16(0.));
			deq(1., BoxedFloat64_BoxedInt16(1.));
			deq(null, BoxedFloat64_BoxedInt16(null));
			deq(0, BoxedFloat64_BoxedInt32(0));
			deq(1, BoxedFloat64_BoxedInt32(1));
			deq(0., BoxedFloat64_BoxedInt32(0.));
			deq(1., BoxedFloat64_BoxedInt32(1.));
			deq(null, BoxedFloat64_BoxedInt32(null));
			deq(0, BoxedFloat64_BoxedInt64(0));
			deq(1, BoxedFloat64_BoxedInt64(1));
			deq(0., BoxedFloat64_BoxedInt64(0.));
			deq(1., BoxedFloat64_BoxedInt64(1.));
			deq(null, BoxedFloat64_BoxedInt64(null));
			deq(0, BoxedFloat64_BoxedFloat32(0));
			deq(1, BoxedFloat64_BoxedFloat32(1));
			deq(0., BoxedFloat64_BoxedFloat32(0.));
			deq(1., BoxedFloat64_BoxedFloat32(1.));
			deq(null, BoxedFloat64_BoxedFloat32(null));
		};
	}
	function deq(expected:Dynamic, actual:Dynamic, ?p:haxe.PosInfos) {
		eq(expected, actual, p);
	}
}
