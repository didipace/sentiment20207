package eval.luv;

/**
	Inter-loop communication.

	@see https://aantron.github.io/luv/luv/Luv/Async
**/
@:using(eval.luv.Handle)
@:coreType abstract Async to Handle {
	/**
		Allocates and initializes an async handle.

		The handle sho