package flash.text.engine;

extern final class GroupElement extends ContentElement {
	@:flash.property var elementCount(get,never) : Int;
	function new(?elements : flash.Vector<ContentElement>, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	function getElementAt(index : Int) : ContentElement;
	function getElementAtCharIndex(charIndex : Int) : Con