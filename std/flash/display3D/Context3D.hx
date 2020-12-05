package flash.display3D;

extern final class Context3D extends flash.events.EventDispatcher {
	@:flash.property var backBufferHeight(get,never) : Int;
	@:flash.property var backBufferWidth(get,never) : Int;
	@:flash.property var driverInfo(get,never) : String;
	@:flash.property var enableErrorChecking(get,set) : Bool;
	@:flash.property var maxBackBufferHeight(get,set) : Int;
	@:flash.property var maxBackBufferWidth(get,set) : Int;
	@:flash.property @:require(flash12) var profile(get,never) : String;
	@:flash.property var totalGPUMemory(get,never) : Float;
	function clear(red : Float = 0, green : Float = 0, blue : Float = 0, alpha : Float = 1, depth : Float = 1, stencil : UInt = 0, mask : UInt = 0xFFFFFFFF) : Void;
	function configureBackBuffer(width : Int, height : Int, antiAlias : Int, enableDepthAndStencil : Bool = true, wantsBestResolution : Bool = false, wantsBestResolutionOnBrowserZoom : Bool = false) : Void;
	function createCubeTexture(size : Int, format : Context3DTextureFormat, optimizeForRenderToTexture : Bool, streamingLevels : Int = 0) : flash.display3D.textures.CubeTexture;
	function createIndexBuffer(numIndices : Int, ?bufferUsage : Context3DBufferUsage) : IndexBuffer3D;
	function createProgram() : Program3D;
	@:require(flash11_8) function createRectangleTexture(width : Int, height : Int, format : Context3DTextureFormat, optimizeForRenderToTexture : Bool) : flash.display3D.textures.RectangleTexture;
	function createTexture(width : Int, height : Int, format : Context3DTextureFormat, optimizeForRenderToTexture : Bool, streamingLevels : Int = 0) : flash.display3D.textures.Texture;
	function createVertexBuffer(numVertices : Int, data32PerVertex : Int, ?bufferUsage : Context3DBufferUsage) : VertexBuffer3D;
	function createVideoTexture() : flash.display3D.textures.VideoTexture;
	function dispose(recreate : Bool = true) : Void;
	function drawToBitmapData(destination : flash.display.BitmapData) : Void;
	function drawTriangles(indexBuffer : IndexBuffer3D, firstIndex : Int = 0, numTriangles : Int = -1) : Void;
	private function get_backBufferHeight() : Int;
	private function get_backBufferWidth() : Int;
	private function get_driverInfo() : String;
	private function get_enableErrorChecking() : Bool;
	private function get_maxBackBufferHeight() : Int;
	private function get_maxBackBufferWidth() : Int;
	private function get_profile() : String;
	private function get_totalGPUMemory() : Float;
	function present() : Void;
	function setBlendFactors(sourceFactor : Context3DBlendFactor, destinationFactor : Context3DBlendFactor) : Void;
	function setColorMask(red : Bool, green : Bool, blue : Bool, alpha : Bool) : Void;
	function setCulling(triangleFaceToCull : Context3DTriangleFace) : Void;
	function setDepthTest(depthMask : Bool, passCompareMode : Context3DCompareMode) : Void;
	function setProgram(program : Program3D) : Voi