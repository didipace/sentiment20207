package flash.filters;

extern class ConvolutionFilter extends BitmapFilter {
	@:flash.property var alpha(get,set) : Float;
	@:flash.property var bias(get,set) : Float;
	@:flash.property var clamp(get,set) : Bool;
	@:flash.property var color(get,set) : UInt;
	@:flash.property var divisor(get,set) : Float;
	@:flash.property var matrix(get,set) : Array<Dynamic>;
	@:flash.property var matrixX(get,set) : Float;
	@:flash.property var matrixY(get,set) : Float;
	@:flash.property var preserveAlpha(get,set) : Bool;
	function new(matrixX : Float = 0, matrixY : Float = 0, ?matrix : Array<Dynamic>, divisor : Float = 1, bias : Float = 0, preserveAlpha : Bool = true, clamp : Bool = true, color : UInt = 