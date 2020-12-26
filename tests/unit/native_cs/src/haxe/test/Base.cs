namespace haxe.test
{

public class Base
{
	~Base() { someString = null; }
	//some haxe-specific keywords

	public static readonly int inline = 42;
	public static readonly int callback = 43;
	public static readonly int cast = 44;
	public static int untyped = 45;

	//final + static variable = inline var in Haxe
	const int inlineNumber = 42;

	//cannot be inline
	public static int notInlineNumber = 42;

	public string someString;
	private string privateField;
	protected int protectedField;

	//static + nonstatic clash
	public static int nameClash(Base t)
	{
		return -1;
	}

	public string prop 
	{
		get
	