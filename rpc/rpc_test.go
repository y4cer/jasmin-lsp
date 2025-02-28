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
		t.Fatalf("Expected: %s\nActual: %s\n", expected, actual)
	}
}

func TestDecode(t *testing.T) {
	expectedLength := 16

	incomingMessage := "Content-Length: 16\r\n\r\n{\"Method\":\"123\"}"

	method, content, err := rpc.DecodeMessage([]byte(incomingMessage))
	contentLength := len(content)

	if err != nil {
		t.Fatal(err)
	}

	if expectedLength != contentLength {
		t.Fatalf("Expected: %d\nActual: %d\n", expectedLength, contentLength)
	}

	if method != "123" {
		t.Fatalf("Expected: \"%s\"\nActual: \"%s\"\n", "123", method)
	}
}
