
package flash.utils;

extern class ByteArray implements IDataOutput2 implements IDataInput2 implements ArrayAccess<Int> {
	@:flash.property var bytesAvailable(get,never) : UInt;
	@:flash.property var endian(get,set) : Endian;
	@:flash.property var length(get,set) : UInt;
	@:flash.property var objectEncoding(get,set) : UInt;
	@:flash.property var position(get,set) : UInt;
	@:flash.property @:require(flash11_4) var shareable(get,set) : Bool;
	function new() : Void;
	@:require(flash11_4) function atomicCompareAndSwapIntAt(byteIndex : Int, expectedValue : Int, newValue : Int) : Int;
	@:require(flash11_4) function atomicCompareAndSwapLength(expectedLength : Int, newLength : Int) : Int;
	@:require(flash10) function clear() : Void;
	function compress(?algorithm : CompressionAlgorithm) : Void;
	@:require(flash10) function deflate() : Void;
	private function get_bytesAvailable() : UInt;
	private function get_endian() : Endian;
	private function get_length() : UInt;
	private function get_objectEncoding() : UInt;
	private function get_position() : UInt;
	private function get_shareable() : Bool;
	@:require(flash10) function inflate() : Void;
	function readBoolean() : Bool;
	function readByte() : Int;
	function readBytes(bytes : ByteArray, offset : UInt = 0, length : UInt = 0) : Void;
	function readDouble() : Float;
	function readFloat() : Float;
	function readInt() : Int;
	function readMultiByte(length : UInt, charSet : String) : String;
	function readObject() : Dynamic;
	function readShort() : Int;
	function readUTF() : String;
	function readUTFBytes(length : UInt) : String;
	function readUnsignedByte() : UInt;
	function readUnsignedInt() : UInt;
	function readUnsignedShort() : UInt;
	private function set_endian(value : Endian) : Endian;
	private function set_length(value : UInt) : UInt;
	private function set_objectEncoding(value : UInt) : UInt;
	private function set_position(value : UInt) : UInt;
	private function set_shareable(value : Bool) : Bool;
	function toString() : String;
	function uncompress(?algorithm : CompressionAlgorithm) : Void;
	function writeBoolean(value : Bool) : Void;
	function writeByte(value : Int) : Void;
	function writeBytes(bytes : ByteArray, offset : UInt = 0, length : UInt = 0) : Void;
	function writeDouble(value : Float) : Void;
	function writeFloat(value : Float) : Void;
	function writeInt(value : Int) : Void;
	function writeMultiByte(value : String, charSet : String) : Void;
	function writeObject(object : Dynamic) : Void;
	function writeShort(value : Int) : Void;
	function writeUTF(value : String) : Void;
	function writeUTFBytes(value : String) : Void;
	function writeUnsignedInt(value : UInt) : Void;
	@:flash.property static var defaultObjectEncoding(get,set) : UInt;
	private static function get_defaultObjectEncoding() : UInt;
	private static function set_defaultObjectEncoding(value : UInt) : UInt;
}