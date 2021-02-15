package;

#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using haxe.macro.ExprTools;
using haxe.macro.TypeTools;
#end

class Bar
{
  public function new()
  {
    trace('New Bar');
  }

  public function nonmacro_func(val:String)
  {
    trace('Hello runtime: $val');
  }

  public macro function macro_func(this_expr:Expr) // :this (should refer to the Bar instance on the Foo)
  {
    var this_ident:String = get_this_ident(this_expr);
    trace('${ this_expr.toString() } computed this_ident as