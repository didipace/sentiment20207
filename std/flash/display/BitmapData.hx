package flash.display;

extern class BitmapData implements IBitmapDrawable {
	@:flash.property var height(get,never) : Int;
	@:flash.property var rect(get,never) : flash.geom.Rectangle;
	@:flash.property var transparent(get,never) : Bool;
	@:flash.property var width(get,never) : Int;
	function new(width : Int, height : Int, transparent : Bool = true, fillColor : UInt = 0xFFFFFFFF) : Void;
	function applyFilter(sourceBitmapData : BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, filter : flash.filters.BitmapFilter) : Void;
	function clone() : BitmapData;
	function colorTransform(rect : flash.geom.Rectangle, colorTransform : flash.geom.ColorTransform) : Void;
	function compare(otherBitmapData : BitmapData) : flash.utils.Object;
	function copyChannel(sourceBitmapData : BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, sourceChannel : UInt, destChannel : UInt) : Void;
	function copyPixels(sourceBitmapData : BitmapData, sourceRect : flash.geom.Rectangle, destPoint : flash.geom.Point, ?alphaBitmapData : BitmapData, ?alphaPoint : flash.geom.Point, mergeAlpha : Bool = false) : Void;
	@:require(flash11_4) function copyPixelsToByteArray(rect : flash.geom.Rectangle, data : flash.utils.ByteArray) : Void;
	function dispose() : Void;
	function draw(source : IBitmapDrawable, ?matrix : flash.geom.Matrix, ?colorTransform : flash.geom.ColorTransform, ?blendMode : BlendMode, ?clipRect : flash.geom.Rectangle, smoothing : Bool = false) : Void;
	@:require(flash11_3) function drawWithQuality(source : IBitmapDrawable, ?matrix : flash.geom.Matrix, ?colorTransform : flash.geom.ColorTransform, ?blendMode : BlendMode, ?clipRect : flash.geom.Rectangle, smoothing : Bool = false, ?quality : StageQuality) : Void;
	@:require(flash11_3) function encode(rect : flash.geom.Rectangle, compressor : flash.utils.Object, ?byteArray : flash.utils.ByteArray) : flash.utils.ByteArray;
	function fillRect(rect : flash.geom.Rectangle, color : UInt) : Void;
	function floodFill(x : Int, y : Int, color : UInt) : Void;
	function generateFilterRect(sourceRect : flash.geom.Rectangle, filter : flash.filters.BitmapFilter) : flash.geom.Rectangle;
	function getColorBoundsRect(mask : UInt, color : UInt, findColor : Bool = true) : flash.geom.Rectangle;
	function getPixel(x : Int, y : Int) : UInt;
	function getPixel32(x : Int, y : Int) : UInt;
	function getPixels(rect : flash.geom.Rectangle) : flash.utils.ByteArray;
	@:require(flash10) function getVector(rect : flash.geom.Rectangle) : flash.Vector<UInt>;
	private function get_height() : Int;
	