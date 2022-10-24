open Unix

type t = {
	addr : Unix.inet_addr;
	port : int;
	mutable socket : Unix.file_descr option;
	send_mutex : Mutex.t;
}

let create host port =
	let host = Unix.inet_addr_of_string host in
	let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect socket (Unix.ADDR_INET (host,port));
	{
		addr = host;
		port = port;
		socket = Some socket;
		send_mutex = Mutex.create()