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
