/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.display;

import haxe.display.JsonModuleTypes;
import haxe.display.Position;
import haxe.display.Protocol;

/**
	Methods of the JSON-RPC-based `--display` protocol in Haxe 4.
	A lot of the methods are *inspired* by the Language Server Protocol, but there is **no** intention to be directly compatible with it.
**/
@:publicFields
class DisplayMethods {
	/**
		The completion request is sent from the client to Haxe to request code completion.
		Haxe automatically determines the type of completion to use based on the passed position, see `CompletionResultKind`.
	**/
	static inline var Completion = new HaxeRequestMethod<CompletionParams, CompletionResult>("display/completion");

	/**
		The request is sent from the client to Haxe to resolve additional information for a given completion item.
	**/
	static inline var CompletionItemResolve = new HaxeRequestMethod<CompletionItemResolveParams, CompletionItemResolveResult>("display/completionItem/resolve");

	/**
		The find references request is sent from the client to Haxe to find locations that reference the symbol at a given text document position.
	**/
	static inline var FindReferences = new HaxeRequestMethod<FindReferencesParams, GotoDefinitionResult>("display/references");

	/**
		The goto definition request is sent from the client to Haxe to resolve the definition location(s) of a symbol at a given text document position.
	**/
	static inline var GotoDefinition = new HaxeRequestMethod<PositionParams, GotoDefinitionResult>("display/definition");

	/**
		The goto implementation request is sent from the client to Haxe to resolve the implementation location(s) of a symbol at a given text document position.
	**/
	static inline var GotoImplementation = new HaxeRequestMethod<PositionParams, GotoDefinitionResult>("display/implementation");

	/**
		The goto type definition request is sent from the client to Haxe to resolve the type definition location(s) of a symbol at a given text document position.
	**/
	static inline var GotoTypeDefinition = new HaxeRequestMethod<PositionParams, GotoTypeDefinitionResult>("display/typeDefinition");

	/**
		The hover request is sent from the client to Haxe to request hover information at a given text document position.
	**/
	static inline var Hover = new HaxeRequestMethod<PositionParams, HoverResult>("display/hover");

	/**
		This request is sent from the client to Haxe to determine the package for a given file, based on class paths configuration.
	**/
	static inline var DeterminePackage = new HaxeRequestMethod<FileParams, DeterminePackageResult>("display/package");

	/**
		The signature help request is sent from the client to Haxe to request signature information at a given cursor position.
	**/
	static inline var SignatureHelp = new HaxeRequestMethod<SignatureHelpParams, SignatureHelpResult>("display/signatureHelp");

	/*
		TODO:

		- finish completion
		- diagnostics
		- codeLens
		- workspaceSymbols ("project/symbol"?)
	 */
}

/** Completion **/
typedef CompletionParams = PositionParams & {
	var wasAutoTriggered:Bool;

	/** list of metas to include in responses **/
	var ?meta:Array<String>;
}

typedef FieldResolution = {
	/**
		Whether it's valid to use the unqualified name of the field or not.
		This is `false` if the identifier is shadowed.
	**/
	var isQualified:Bool;

	/**
		The qualifier that has to be inserted to use the field if `!isQualified`.
		Can either be `this` or `super` for instance fields for the type name for `static` fields.
	**/
	var qualifier:String;
}

typedef DisplayLocal<T> = {
	var id:Int;
	var name:String;
	var type:JsonType<T>;
	var origin:LocalOrigin;
	var capture:Bool;
	var ?extra:{
		var params:Array<JsonTypeParameter>;
		var expr:JsonExpr;
	};
	var meta:JsonMetadata;
	var pos:JsonPos;
	var isInline:Bool;
	var isFinal:Bool;
}

enum abstract LocalOrigin(Int) {
	var LocalVariable;
	var Argument;
	var ForVariable;
	var PatternVariable;
	var CatchVariable;
	var LocalFunction;
}

enum abstract ClassFieldOriginKind<T>(Int) {
	/**
		The field is declared on the current type itself.
	**/
	var Self:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is a static field brought into context via a static import
		(`import pack.Module.Type.field`).
	**/
	var StaticImport:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is declared on a parent type, such as:
		- a super class field that is not overriden
		- a forwarded abstract field
	**/
	var Parent:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is a static extension method brought
		into context with the `using` keyword.
	**/
	var StaticExtension:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		This field doesn't belong to any named type, just an anonymous structure.
	**/
	var AnonymousStructure:ClassFieldOriginKind<JsonAnon>;

	/**
		Special fields built into the compiler, such as:
		- `code` on single-character Strings
		- `bind()` on functions.
	**/
	var BuiltIn:ClassFieldOriginKind<NoData>;

	/**
		The origin of this class field is unknown.
	**/
	var Unknown:ClassFieldOriginKind<NoData>;
}

typedef ClassFieldOrigin<T> = {
	var kind:ClassFieldOriginKind<T>;
	var ?args:T;
}

typedef ClassFieldOccurrence<T> = {
	var field:JsonClassField;
	var resolution:FieldResolution;
	var ?origin:ClassFieldOrigin<T>;
}

enum abstract EnumFieldOriginKind<T>(Int) {
	/**
		The enum value is declared on the current type itself.
	**/
	var Self:EnumFieldOriginKind<JsonModuleType<T>>;

	/**
		The enum value is brought into context via a static import
		(`import pack.Module.Enum.Value`).
	**/
	var StaticImport:EnumFieldOriginKind<JsonModuleType<T>>;
}

typedef EnumFieldOrigin<T> = {
	var kind:EnumFieldOriginKind<T>;
	var ?args:T;
}

typedef EnumFieldOccurrence<T> = {
	var field:JsonEnumField;
	var resolution:FieldResolution;
	var ?origin:EnumFieldOrigin<T>;
}

enum abstract Literal(String) {
	var Null = "null";
	var True = "true";
	var False = "false";
	var This = "this";
	var Trace = "trace";
}

enum abstract DisplayModuleTypeKind(Int) {
	var Class;
	var Interface;
	var Enum;
	var Abstract;
	var EnumAbstract;

	/** A `typedef` that is just an alias for another type. **/
	var TypeAlias;

	/** A `typedef` that is an alias for an anonymous structure. **/
	var Struct;

	/** A type name introduced by `import as` / `import in` **/
	// var ImportAlias;
}

typedef DisplayModuleType = {
	var path:JsonTypePath;
	var pos:JsonPos;
	var isPrivate:Bool;
	var params:Array<DisplayModuleTypeParameter>;
	var meta:JsonMetadata;
	var doc:JsonDoc;
	var isExtern:Bool;
	var isFinal:Bool;
	var isAbstract:Bool;
	var kind:DisplayModuleTypeKind;
}

typedef DisplayModuleTypeParameter = {
	var name:String;
	var meta:JsonMetadata;
	var constraints:Array<JsonType<Dynamic>>;
}

typedef DisplayLiteral<T> = {
	var name:String;
}

enum abstract MetadataTarget(String) {
	var Class = "TClass";
	var ClassField = "TClassField";
	var Abstract = "TAbstract";
	var AbstractField = "TAbstractField";
	var Enum = "TEnum";
	var Typedef = "TTypedef";
	var AnyField = "TAnyField";
	var Expr = "TExpr";
	var TypeParameter = "TTypeParameter";
}

enum abstract Platform(String) {
	var Cro