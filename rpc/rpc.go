package rpc

import (
	"encoding/json"
	"fmt"
	"bytes"
	"errors"
	"strconv"
)

// TODO: parse header function

type BaseMessage struct {
	Method string `json:"method"`
}

func EncodeMessage(msg any) string {
	content, err := json.Marshal(msg)
	if err != nil {
		panic(err)
	}


	return fmt.Sprintf("Content-Length: %d\r\n\r\n%s", len(content), content)
}

func DecodeMessage(msg []byte) (int, error) {
	header, content, found := bytes.Cut(msg, []byte("\r\n\r\n"))

	fmt.Printf("%s, %s, %t\n", header, content, found)

	if !found {
		return 0, errors.New("Could not find separator!")
	}

	contentLengthString := []byte("Content-Length: ")
	contentLength, err := strconv.Atoi(string(header[len(contentLengthString):]))

	if err != nil {
		return 0, err
	}

	var baseMessage BaseMessage
	if err := json.Unmarshal(content[:contentLength], &baseMessage); err != nil {
		return 0, err
	}

	fmt.Println(baseMessage.Method)
	

	return contentLength, nil
}

// type SplitFunc func(data []byte, atEOF bool) (advance int, token []byte, err error)
func Split(data []byte, atEOF bool) (advance int, token []byte, err error) {
	header, content, found := bytes.Cut(data, []byte("\r\n\r\n"))

	if !found {
		return 0, nil, nil
	}


	contentLengthString := []byte("Content-Length: ")
	contentLength, err := strconv.Atoi(string(header[len(contentLengthString):]))

	if len(content) < contentLength {
		return 0, nil, nil
	}

	if err != nil {
		return 0, nil, err
	}

	advance = len(header) + 4 + contentLength
	token = data[:advance]

	return advance, token, nil
}
