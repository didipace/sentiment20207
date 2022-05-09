package flash.events;

extern class AVManifestLoadEvent extends Event {
	@:flash.property var duration(get,never) : Float;
	@:flash.property var handle(get,never) : Int;
	@:flash.property var result(get,never) : flash.media.AVResult;
	@:flash.property var userData(get,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Boo