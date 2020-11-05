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
	static function DynamicInt8_Int16(v:Int8):Int16 {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_Int32(v:Int8):Int32 return cast v;
	static function DynamicInt8_Int32(v:Int8):Int32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_Int64(v:Int8):Int64 return cast v;
	static function DynamicInt8_Int64(v:Int8):Int64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_Float32(v:Int8):Float32 return cast v;
	static function DynamicInt8_Float32(v:Int8):Float32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_Float64(v:Int8):Float64 return cast v;
	static function DynamicInt8_Float64(v:Int8):Float64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedInt8(v:Int8):Null<Int8> return cast v;
	static function DynamicInt8_BoxedInt8(v:Int8):Null<Int8> {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedInt16(v:Int8):Null<Int16> return cast v;
	static function DynamicInt8_BoxedInt16(v:Int8):Null<Int16> {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedInt32(v:Int8):Null<Int32> return cast v;
	static function DynamicInt8_BoxedInt32(v:Int8):Null<Int32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedInt64(v:Int8):Null<Int64> return cast v;
	static function DynamicInt8_BoxedInt64(v:Int8):Null<Int64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedFloat32(v:Int8):Null<Float32> return cast v;
	static function DynamicInt8_BoxedFloat32(v:Int8):Null<Float32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int8_BoxedFloat64(v:Int8):Null<Float64> return cast v;
	static function DynamicInt8_BoxedFloat64(v:Int8):Null<Float64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_Int8(v:Int16):Int8 return cast v;
	static function DynamicInt16_Int8(v:Int16):Int8 {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_Int32(v:Int16):Int32 return cast v;
	static function DynamicInt16_Int32(v:Int16):Int32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_Int64(v:Int16):Int64 return cast v;
	static function DynamicInt16_Int64(v:Int16):Int64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_Float32(v:Int16):Float32 return cast v;
	static function DynamicInt16_Float32(v:Int16):Float32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_Float64(v:Int16):Float64 return cast v;
	static function DynamicInt16_Float64(v:Int16):Float64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedInt8(v:Int16):Null<Int8> return cast v;
	static function DynamicInt16_BoxedInt8(v:Int16):Null<Int8> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedInt16(v:Int16):Null<Int16> return cast v;
	static function DynamicInt16_BoxedInt16(v:Int16):Null<Int16> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedInt32(v:Int16):Null<Int32> return cast v;
	static function DynamicInt16_BoxedInt32(v:Int16):Null<Int32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedInt64(v:Int16):Null<Int64> return cast v;
	static function DynamicInt16_BoxedInt64(v:Int16):Null<Int64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedFloat32(v:Int16):Null<Float32> return cast v;
	static function DynamicInt16_BoxedFloat32(v:Int16):Null<Float32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int16_BoxedFloat64(v:Int16):Null<Float64> return cast v;
	static function DynamicInt16_BoxedFloat64(v:Int16):Null<Float64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_Int8(v:Int32):Int8 return cast v;
	static function DynamicInt32_Int8(v:Int32):Int8 {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_Int16(v:Int32):Int16 return cast v;
	static function DynamicInt32_Int16(v:Int32):Int16 {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_Int64(v:Int32):Int64 return cast v;
	static function DynamicInt32_Int64(v:Int32):Int64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_Float32(v:Int32):Float32 return cast v;
	static function DynamicInt32_Float32(v:Int32):Float32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_Float64(v:Int32):Float64 return cast v;
	static function DynamicInt32_Float64(v:Int32):Float64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedInt8(v:Int32):Null<Int8> return cast v;
	static function DynamicInt32_BoxedInt8(v:Int32):Null<Int8> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedInt16(v:Int32):Null<Int16> return cast v;
	static function DynamicInt32_BoxedInt16(v:Int32):Null<Int16> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedInt32(v:Int32):Null<Int32> return cast v;
	static function DynamicInt32_BoxedInt32(v:Int32):Null<Int32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedInt64(v:Int32):Null<Int64> return cast v;
	static function DynamicInt32_BoxedInt64(v:Int32):Null<Int64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedFloat32(v:Int32):Null<Float32> return cast v;
	static function DynamicInt32_BoxedFloat32(v:Int32):Null<Float32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int32_BoxedFloat64(v:Int32):Null<Float64> return cast v;
	static function DynamicInt32_BoxedFloat64(v:Int32):Null<Float64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_Int8(v:Int64):Int8 return cast v;
	static function DynamicInt64_Int8(v:Int64):Int8 {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_Int16(v:Int64):Int16 return cast v;
	static function DynamicInt64_Int16(v:Int64):Int16 {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_Int32(v:Int64):Int32 return cast v;
	static function DynamicInt64_Int32(v:Int64):Int32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_Float32(v:Int64):Float32 return cast v;
	static function DynamicInt64_Float32(v:Int64):Float32 {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_Float64(v:Int64):Float64 return cast v;
	static function DynamicInt64_Float64(v:Int64):Float64 {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedInt8(v:Int64):Null<Int8> return cast v;
	static function DynamicInt64_BoxedInt8(v:Int64):Null<Int8> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedInt16(v:Int64):Null<Int16> return cast v;
	static function DynamicInt64_BoxedInt16(v:Int64):Null<Int16> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedInt32(v:Int64):Null<Int32> return cast v;
	static function DynamicInt64_BoxedInt32(v:Int64):Null<Int32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedInt64(v:Int64):Null<Int64> return cast v;
	static function DynamicInt64_BoxedInt64(v:Int64):Null<Int64> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedFloat32(v:Int64):Null<Float32> return cast v;
	static function DynamicInt64_BoxedFloat32(v:Int64):Null<Float32> {
		var x:Dynamic = v;
		return x;
	}
	static function Int64_BoxedFloat64(v:Int64):Null<Float64> return cast v;
	static function DynamicInt64_BoxedFloat64(v:Int64):Null<Float64> {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_Int8(v:Float32):Int8 return cast v;
	static function DynamicFloat32_Int8(v:Float32):Int8 {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_Int16(v:Float32):Int16 return cast v;
	static function DynamicFloat32_Int16(v:Float32):Int16 {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_Int32(v:Float32):Int32 return cast v;
	static function DynamicFloat32_Int32(v:Float32):Int32 {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_Int64(v:Float32):Int64 return cast v;
	static function DynamicFloat32_Int64(v:Float32):Int64 {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_Float64(v:Float32):Float64 return cast v;
	static function DynamicFloat32_Float64(v:Float32):Float64 {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_BoxedInt8(v:Float32):Null<Int8> return cast v;
	static function DynamicFloat32_BoxedInt8(v:Float32):Null<Int8> {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_BoxedInt16(v:Float32):Null<Int16> return cast v;
	static function DynamicFloat32_BoxedInt16(v:Float32):Null<Int16> {
		var x:Dynamic = v;
		return x;
	}
	static function Float32_BoxedInt32(v:Float32):Null<Int32> return ca