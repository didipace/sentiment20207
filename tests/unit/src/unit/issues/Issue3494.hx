package unit.issues;

private abstract E2<T1,T2>(Dynamic)
	from T1 from T2 from E2<T2,T1>
	to T1 to T2 to E2<T2,T1>
{}

private abstract E3<T1,T2>(Dynamic)
from T1 from T2
to T1 to T2
{
	@:from inline static public function 