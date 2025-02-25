package main

import "jasmin-lsp/rpc"

type EncodingExample struct {
	Aboba bool
}

func main() {
	msg := rpc.EncodeMessage(rpc.EncodingExample{Aboba:false})
	rpc.DecodeMessage([]byte(msg))
}
