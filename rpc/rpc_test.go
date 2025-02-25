package rpc_test

import (
	"jasmin-lsp/rpc"
	"testing"
)

type EncodingExample struct {
	Testing bool
}

func TestEncode(t *testing.T) {
	expected := "Content-Length: 16\r\n\r\n{\"Testing\":true}"

	actual := rpc.EncodeMessage(EncodingExample{Testing: true})

	if expected != actual {
		t.Fatalf("Expected: %s\nActual: %s", expected, actual)
	}
}

func TestDecode(t *testing.T) {
	expected := 16

	incomingMessage := "Content-Length: 16\r\n\r\n{\"Testing\":true}"

	actual, err := rpc.DecodeMessage([]byte(incomingMessage))

	if err != nil {
		t.Fatal(err)
	}

	if expected != actual {
		t.Fatalf("Expected: %d\nActual: %d", expected, actual)
	}
}
