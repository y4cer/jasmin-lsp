package main

import (
	"bufio"
	"jasmin-lsp/rpc"
	"log"
	"os"
)

type EncodingExample struct {
	Method string
}

func getLogger(filename string) *log.Logger {
	logfile, err := os.OpenFile(filename, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0666)
	if err != nil {
		panic("Wrong logger file!")
	}

	return log.New(logfile, "[jasmin-lsp] ", log.Ldate|log.Ltime|log.Lshortfile)
}

func handleMessage(logger *log.Logger, msg string) {
	logger.Println(msg)
}

func main() {
	logger := getLogger("/tmp/jasmin-lsp.log")

	logger.Println("aboba")

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(rpc.Split)

	for scanner.Scan() {
		msg := scanner.Text()
		handleMessage(logger, msg)
	}
}
