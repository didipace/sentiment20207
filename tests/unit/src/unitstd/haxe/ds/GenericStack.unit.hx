var gs = new haxe.ds.GenericStack<String>();
gs.isEmpty() == true;
gs.first() == null;
gs.pop() == null;
gs.remove(null) == false;
gs.add("foo");
gs.isEmpty() == false;
gs.first() == "foo";
gs.pop() == "foo";
gs.isEmpty() == true;
gs.first() == null;
gs.pop() == null;
gs.add("foo");
gs.first() == "foo";
gs.remove("foo") == true;
gs.isEmpty() == true;
gs.add("foo");
gs.add("bar");
gs.pop() == "bar";
gs.first() == "foo";
gs.pop() == "foo";
gs.add(null);
gs.add(null);
gs.isEmpty() == false;
gs.first() == null;
gs.pop() == null;
gs.remove(null) == tru