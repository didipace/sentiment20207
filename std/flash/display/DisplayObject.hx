package flash.display;

extern class DisplayObject extends flash.events.EventDispatcher implements IBitmapDrawable {
	@:flash.property var accessibilityProperties(get,set) : flash.accessibility.AccessibilityProperties;
	@:flash.property var alpha(get,set) : Float;
	@:flash.property var blendMode(get,set) : BlendMode;
	@:flash.property @:require(flash10) var blendShader(never,set) : Shader;
	@:flash.property var cacheAsBitmap(get,set) : Bool;
	@:flash.property var filters(get,set) : Array<flash.filters.BitmapFilter>;
	@:flash.property var height(get,set) : Float;
	@:flash.property var loaderInfo(get,never) : LoaderInfo;
	@:flash.property var mask(get,set) : DisplayObject;
	@:flash.property var mouseX(get,never) : Float;
	@:flash.property var mouseY(get,never) : Float;
	@:flash.property var name(get,set) : String;
	@:flash.property var opaqueBackground(get,set) : Null<UInt>;
	@:flash.property var parent(get,never) : DisplayObjectContainer;
	@:flash.property var root(get,never) : DisplayObject;
	@:flash.property var rotation(get,set) : Float;
	@:flash.property @:require(flash10) var rotationX(get,s