package lsp

type Message struct {
	RPC string `json:"jsonrpc"`
}

type RequestMessage struct {
	*Message
	ID     int    `json:"id"`
	Method string `json:"method"`

	// Params []
}

type ResponseMessage struct {
	*Message
	ID *int `json:"id,omitempty"`

	// Result
	// Error
}
