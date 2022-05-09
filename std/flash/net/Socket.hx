package flash.net;

extern class Socket extends flash.events.EventDispatcher implements flash.utils.IDataOutput implements flash.utils.IDataInput {
	@:flash.property var bytesAvailable(get,never) : UInt;
	@:flash.property @:require(flash11) var bytesPending(get,never) : UInt;
	@:flash.property var connected(get,never) : Bool;
	@:flash.property var endian(get,set) : flash.utils.Endian;
	@:flash.property var objectEncoding(get,set) : UInt;
	@:flash.property @:require(flash10) var timeout(get,set) : UInt;
	function new(?host : String, port : Int = 0) : Void;
	function close() : Void;
	function connect(host : String, port : Int) : Void;
	function flush() : Void;
	private function get_bytesAvailable() : UInt;
	private function get_bytesPending() : UInt;
	private function get_connected() : Bool;
	private function get_endian() : flash.utils.Endian;
	private function get_objectEncoding() : UInt;
	private function get_timeout() : UInt;
	function readBoolean() : Bool;
	function readByte() : Int;
	function readBytes(bytes : flash.utils.ByteArray, offset : UInt = 0, length : UInt = 0) : Void;
	function readDouble() : Float;
	function readFloat() : F