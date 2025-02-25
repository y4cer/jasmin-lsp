package rpc

import (
	"encoding/json"
	"fmt"
	"bytes"
	"errors"
	"strconv"
)

func EncodeMessage(msg any) string {
	content, err := json.Marshal(msg)
	if err != nil {
		panic(err)
	}


	return fmt.Sprintf("Content-Length: %d\r\n\r\n%s", len(content), content)
}

func DecodeMessage(msg []byte) (int, error) {
	header, content, found := bytes.Cut(msg, []byte("\r\n\r\n"))

	fmt.Printf("%s, %s, %t", header, content, found)

	if !found {
		return 0, errors.New("Could not find separator!")
	}

	contentLengthString := []byte("Content-Length: ")
	contentLength, err := strconv.Atoi(string(header[len(contentLengthString):]))

	if err != nil {
		return 0, err
	}

	return contentLength, nil
}
